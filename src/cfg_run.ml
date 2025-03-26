open Prog
open Elang
open Elang_run
open Batteries
open BatList
open Cfg
open Utils
open Builtins

let rec eval_cfgexpr oc st cp (e: expr) : (int * int state) res =
  match e with
  | Ebinop(b, e1, e2) ->
    eval_cfgexpr oc st cp e1 >>= fun (v1, st') ->
    eval_cfgexpr oc st' cp e2 >>= fun (v2, st'') ->
    let v = eval_binop b v1 v2 in
    OK (v, st'')
  | Eunop(u, e) ->
    eval_cfgexpr oc st cp e >>= fun (v1, st') ->
    let v = (eval_unop u v1) in
    OK (v, st')
  | Eint i -> OK (i, st)
  | Evar s ->
    begin match Hashtbl.find_option st.env s with
      | Some v -> OK (v, st)
      | None -> Error (Printf.sprintf "Unknown variable %s\n" s)
    end
  | Ecall (f, args) ->
      List.fold_left (
        fun (acc : (int list * int state) res) (arg : expr) -> 
        match acc with
        | Error msg -> Error msg
        | OK (l, st') -> 
            match eval_cfgexpr oc st' cp arg with
            | Error msg -> Error msg
            | OK (i, st'') -> OK ((l@[i]), st'')
              ) (OK([], st)) args >>= fun (int_args, st') ->
                match find_function cp f with
            | OK found_f -> 
               (match eval_cfgfun oc st' cp f found_f int_args with
               | Error msg -> Error msg
               | OK (None, st'') -> Error (Format.sprintf "Function %s doesn't have a return value.\n" f)
               | OK (Some ret, st'') -> OK (ret, st''))
            | Error msg -> 
               (match do_builtin oc st.mem f int_args with
               | Error msg -> Error msg
               | OK None -> Error (Format.sprintf "Function %s doesn't have a return value.\n" f)
               | OK (Some ret) -> OK (ret, st'))

and eval_cfginstr oc st cp ht (n: int): (int * int state) res =
  match Hashtbl.find_option ht n with
  | None -> Error (Printf.sprintf "Invalid node identifier\n")
  | Some node ->
    match node with
    | Cnop succ ->
      eval_cfginstr oc st cp ht succ
    | Cassign(v, e, succ) ->
      eval_cfgexpr oc st cp e >>= fun (i, st') ->
      Hashtbl.replace st'.env v i;
      eval_cfginstr oc st' cp ht succ
    | Ccmp(cond, i1, i2) ->
      eval_cfgexpr oc st cp cond >>= fun (i, st') ->
      if i = 0 then eval_cfginstr oc st' cp ht i2 else eval_cfginstr oc st' cp ht i1
    | Creturn(e) ->
      eval_cfgexpr oc st cp e >>= fun (e, st') ->
      OK (e, st')
    | Ccall (f, args, succ) ->
      List.fold_left (
        fun (acc : (int list * int state) res) (arg : expr) -> 
        match acc with
        | Error msg -> Error msg
        | OK (l, st') -> 
            match eval_cfgexpr oc st' cp arg with
            | Error msg -> Error msg
            | OK (i, st'') -> OK ((l@[i]), st'')
              ) (OK([], st)) args 
              >>= fun (int_args, st') ->
                match find_function cp f with
                | OK found_f -> 
                   (match eval_cfgfun oc st' cp f found_f int_args with
                   | Error msg -> Error msg
                   | OK (_, st'') -> eval_cfginstr oc st'' cp ht succ)
                | Error msg -> 
                   (match do_builtin oc st'.mem f int_args with
                   | OK _ -> eval_cfginstr oc st' cp ht succ
                   | Error msg -> Error msg )
              

and eval_cfgfun oc st cp cfgfunname { cfgfunargs;
                                      cfgfunbody;
                                      cfgentry} vargs =
  let st' = { st with env = Hashtbl.create 17 } in
  match List.iter2 (fun a v -> Hashtbl.replace st'.env a v) cfgfunargs vargs with
  | () -> eval_cfginstr oc st' cp cfgfunbody cfgentry >>= fun (v, st') ->
    OK (Some v, {st' with env = st.env})
  | exception Invalid_argument _ ->
    Error (Format.sprintf "CFG: Called function %s with %d arguments, expected %d.\n"
             cfgfunname (List.length vargs) (List.length cfgfunargs)
          )

let eval_cfgprog oc cp memsize params =
  let st = init_state memsize in
  find_function cp "main" >>= fun f ->
  let n = List.length f.cfgfunargs in
  let params = take n params in
  eval_cfgfun oc st cp "main" f params >>= fun (v, st) ->
  OK v



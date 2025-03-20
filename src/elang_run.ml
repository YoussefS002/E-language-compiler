open Elang
open Batteries
open Prog
open Utils
open Builtins

let remove_local_vars st local_st =
   let filtered_env = Hashtbl.filteri (fun k v -> if Hashtbl.mem st.env k then (Printf.printf "Not removing %s\n" k; true) else (Printf.printf "removing %s\n" k; false) ) local_st.env
      in {local_st with env = filtered_env}

let binop_bool_to_int f x y = if f x y then 1 else 0

(* [eval_binop b x y] évalue l'opération binaire [b] sur les arguments [x]
   et [y]. *)
let eval_binop (b: binop) : int -> int -> int =
  match b with
   | Eadd -> fun x y -> x + y
   | Emul -> fun x y -> x * y
   | Emod -> fun x y -> x mod y
   | Exor -> fun x y -> x lxor y
   | Ediv -> fun x y -> x / y
   | Esub -> fun x y -> x - y
   | Eclt -> fun x y -> if x < y then 1 else 0
   | Ecle -> fun x y -> if x <= y then 1 else 0
   | Ecgt -> fun x y -> if x > y then 1 else 0
   | Ecge -> fun x y -> if x >= y then 1 else 0
   | Eceq -> fun x y -> if x = y then 1 else 0
   | Ecne -> fun x y -> if x != y then 1 else 0


(* [eval_unop u x] évalue l'opération unaire [u] sur l'argument [x]. *)
let eval_unop (u: unop) : int -> int =
  match u with
   | Eneg -> fun x -> -x

(* [eval_eexpr st e] évalue l'expression [e] dans l'état [st]. Renvoie une
   erreur si besoin. *)
let rec eval_eexpr oc st (ep: eprog) (e : expr) : (int * int state) res =
   match e with
   | Eint i -> OK (i, st)
   | Evar s -> 
      (match Hashtbl.find_option st.env s with
      | Some i -> OK (i, st)
      | None -> Error "Variable is not defined")
   | Ebinop (b, ex, ey) -> 
      eval_eexpr oc st ep ex >>= fun (x, st') ->
      eval_eexpr oc st' ep ey >>= fun (y, st'') ->
      OK (eval_binop b x y, st'')
   | Eunop (u, ex) ->
      eval_eexpr oc st ep ex >>= fun (x, st') ->
      OK (eval_unop u x, st')
   | Ecall (f, args) -> 
      (List.fold_left
            (fun acc arg -> 
            acc >>= fun (l, st') ->
            eval_eexpr oc st' ep arg >>= fun (i, st'') ->
            OK ((l@[i]), st'')) 
               (OK([], st)) args >>= fun(int_args, st') -> 
            match find_function ep f with
            | OK found_f -> 
               (match eval_efun oc st' ep found_f f int_args with
               | Error msg -> Error msg
               | OK (None, st'') -> Error (Format.sprintf "E: Function %s doesn't have a return value.\n" f)
               | OK (Some ret, st'') -> OK (ret, st''))
            | Error msg -> 
               (match do_builtin oc st'.mem f int_args with
               | Error msg -> Error msg
               | OK None -> Error (Format.sprintf "E: Function %s doesn't have a return value.\n" f)
               | OK (Some ret) -> OK (ret, st')))
   | Echar c -> OK (Char.code c, st)
         
(* [eval_einstr oc st ins] évalue l'instruction [ins] en partant de l'état [st].

   Le paramètre [oc] est un "output channel", dans lequel la fonction "print"
   écrit sa sortie, au moyen de l'instruction [Format.fprintf].

   Cette fonction renvoie [(ret, st')] :

   - [ret] est de type [int option]. [Some v] doit être renvoyé lorsqu'une
   instruction [return] est évaluée. [None] signifie qu'aucun [return] n'a eu
   lieu et que l'exécution doit continuer.

   - [st'] est l'état mis à jour. *)
and eval_einstr oc (st: int state) (ep: eprog) (ins: instr) :
  (int option * int state) res =
   match ins with
   | Iassign (s, e) ->
      if Hashtbl.mem st.env s
         then    
            (let replace st s v =
                  let new_env = Hashtbl.copy st.env 
                     in Hashtbl.replace new_env s v; 
                        {st with env = new_env} 
               in match eval_eexpr oc st ep e with
               | Error msg -> Error msg
               | OK (v, st') -> OK (None, replace st' s v))
         else
            Error (Format.sprintf "E: Variable %s was not declared." s)
   | Iif (e, i1, i2) -> 
      (eval_eexpr oc st ep e >>= fun (v, st') -> 
         if v != 0 
            then eval_einstr oc st' ep i1 >>= fun (r_opt, st'') -> 
               OK (r_opt, remove_local_vars st' st'') 
            else eval_einstr oc st' ep i2 >>= fun (r_opt, st'') -> 
               OK (r_opt, remove_local_vars st' st''))
   | Iwhile (e, i) -> 
      (eval_eexpr oc st ep e >>= fun (v, st') ->
         if v != 0 
            then eval_einstr oc st' ep i >>= fun (r_opt, next_st) -> 
               match r_opt with 
               | None -> eval_einstr oc (remove_local_vars st' next_st) ep (Iwhile (e, i))
               | Some r -> OK (r_opt, remove_local_vars st' next_st)
            else OK (None, st'))
   | Iblock i_list -> 
      (match i_list with
      | [] -> OK (None, st)
      | i::rest -> 
         match eval_einstr oc st ep i with
         | Error msg -> Error msg
         | OK (Some r, next_st) -> OK (Some r, next_st)
         | OK (None, next_st) -> eval_einstr oc next_st ep (Iblock rest))
   | Ireturn e -> 
      eval_eexpr oc st ep e >>= fun (v, st') -> 
         OK(Some v, st')
   | Icall (f, args) -> 
      (List.fold_left
         (fun acc arg -> 
         acc >>= fun (l, st') ->
         eval_eexpr oc st' ep arg >>= fun (i, st'') ->
         OK ((l@[i]), st'')) 
            (OK([], st)) args >>= fun(int_args, st') -> 
            match find_function ep f with
            | OK found_f -> 
               (eval_efun oc st' ep found_f f int_args >>= fun (_, st'') -> 
                  OK (None, st''))
            | Error msg -> 
               (do_builtin oc st'.mem f int_args >>= fun _ -> 
                  OK (None, st')))
   | Ideclare (_, s) -> 
      let new_env = Hashtbl.copy st.env
         in Hashtbl.add new_env s 0;
         OK (None, {st with env = new_env})

(* [eval_efun oc st f fname vargs] évalue la fonction [f] (dont le nom est
   [fname]) en partant de l'état [st], avec les arguments [vargs].

   Cette fonction renvoie un couple (ret, st') avec la même signification que
   pour [eval_einstr]. *)
and eval_efun oc (st: int state) ep ({ funargs; funbody}: efun)
    (fname: string) (vargs: int list)
  : (int option * int state) res =
  (* L'environnement d'une fonction (mapping des variables locales vers leurs
     valeurs) est local et un appel de fonction ne devrait pas modifier les
     variables de l'appelant. Donc, on sauvegarde l'environnement de l'appelant
     dans [env_save], on appelle la fonction dans un environnement propre (Avec
     seulement ses arguments), puis on restore l'environnement de l'appelant. *)
  let env_save = Hashtbl.copy st.env in
  let env = Hashtbl.create 17 in
  match List.iter2 (fun (a, t) v -> Hashtbl.replace env a v) funargs vargs with
  | () ->
    eval_einstr oc { st with env } ep funbody >>= fun (v, st') ->
    OK (v, { st' with env = env_save })
  | exception Invalid_argument _ ->
    Error (Format.sprintf
             "E: Called function %s with %d arguments, expected %d.\n"
             fname (List.length vargs) (List.length funargs)
          )

(* [eval_eprog oc ep memsize params] évalue un programme complet [ep], avec les
   arguments [params].

   Le paramètre [memsize] donne la taille de la mémoire dont ce programme va
   disposer. Ce n'est pas utile tout de suite (nos programmes n'utilisent pas de
   mémoire), mais ça le sera lorsqu'on ajoutera de l'allocation dynamique dans
   nos programmes.

   Renvoie:

   - [OK (Some v)] lorsque l'évaluation de la fonction a lieu sans problèmes et renvoie une valeur [v].

   - [OK None] lorsque l'évaluation de la fonction termine sans renvoyer de valeur.

   - [Error msg] lorsqu'une erreur survient.
   *)
let eval_eprog oc (ep: eprog) (memsize: int) (params: int list)
  : int option res =
  let st = init_state memsize in
  find_function ep "main" >>= fun f ->
  (* ne garde que le nombre nécessaire de paramètres pour la fonction "main". *)
  let n = List.length f.funargs in
  let params = take n params in
  eval_efun oc st ep f "main" params >>= fun (v, _) ->
  OK v

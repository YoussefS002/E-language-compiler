open Ast
open Elang
open Prog
open Report
open Options
open Batteries
open Elang_print
open Utils

let tag_is_binop =
  function
    Tadd -> true
  | Tsub -> true
  | Tmul -> true
  | Tdiv -> true
  | Tmod -> true
  | Txor -> true
  | Tcle -> true
  | Tclt -> true
  | Tcge -> true
  | Tcgt -> true
  | Tceq -> true
  | Tne  -> true
  | _    -> false

let binop_of_tag =
  function
    Tadd -> Eadd
  | Tsub -> Esub
  | Tmul -> Emul
  | Tdiv -> Ediv
  | Tmod -> Emod
  | Txor -> Exor
  | Tcle -> Ecle
  | Tclt -> Eclt
  | Tcge -> Ecge
  | Tcgt -> Ecgt
  | Tceq -> Eceq
  | Tne -> Ecne
  | _ -> assert false

let remove_local_vars typ_var local_typ_var =
  Hashtbl.filteri (fun s t -> Hashtbl.mem typ_var s) local_typ_var
      
let rec type_expr (typ_var : (string,typ) Hashtbl.t) (typ_fun : (string, typ list * typ) Hashtbl.t) (e: expr) : typ res =
  match e with
  | Ebinop (b, e1, e2) -> 
      type_expr typ_var typ_fun e1 >>= fun t1 ->
      type_expr typ_var typ_fun e2 >>= fun t2 ->
      if t1 != Tvoid && t2 != Tvoid
        then OK Tint (* à vérifier *)
        else Error "E: Binop is not defined on void type."
  | Eunop (u, e) -> 
      type_expr typ_var typ_fun e >>= fun t ->
        if t != Tvoid
          then OK Tint
          else Error "E: Unop is not defined on void type."
  | Eint i -> OK Tint
  | Echar c -> OK Tchar
  | Evar s -> 
    (match Hashtbl.find_option typ_var s with
    | Some t when t != Tvoid -> OK t
    | _ -> Error (Format.sprintf "E: Expression %s type is not defined." s))
  | Ecall (f, exprs) -> 
    match Hashtbl.find_option typ_fun f with
    | Some (arg_types, ret_type) when ret_type != Tvoid ->
        list_map_res (type_expr typ_var typ_fun) exprs >>= fun types ->
          if types = arg_types
            then OK ret_type
            else Error (Format.sprintf "E: Unvalid argument types in function %s calling." f)
    | _ -> Error "E: Function return type is not defined."

let are_compatible (t1 : typ) (t2 : typ) : bool =
  match t1, t2 with
  | Tint, Tint
  | Tchar, Tchar
  | Tint, Tchar
  | Tchar, Tint -> true
  | _ -> false

(* [make_eexpr_of_ast a] builds an expression corresponding to a tree [a]. If
   the tree is not well-formed, fails with an [Error] message. *)
let rec make_eexpr_of_ast (typ_var : (string,typ) Hashtbl.t) (typ_fun : (string, typ list * typ) Hashtbl.t) (a: tree) : expr res =
  let res =
    match a with
    (* TODO *)
    | IntLeaf i -> OK (Eint i)
    | StringLeaf s -> OK (Evar s) 
    | CharLeaf c -> OK (Echar c)
    | Node(t, [e1; e2]) when tag_is_binop t ->
        (make_eexpr_of_ast typ_var typ_fun e1 >>= fun expr1 ->
        make_eexpr_of_ast typ_var typ_fun e2 >>= fun expr2 ->
        OK (Ebinop (binop_of_tag t, expr1, expr2)))
    | Node(Tneg, [e]) -> 
        make_eexpr_of_ast typ_var typ_fun e >>= fun expr ->
        OK (Eunop (Eneg, expr))
    | Node(Tcall, [StringLeaf f; Node(Targs, args)]) -> 
        list_map_res (make_eexpr_of_ast typ_var typ_fun) args >>= fun exprs -> 
        OK (Ecall (f, exprs))
    | _ -> 
        Error (Printf.sprintf "Unacceptable ast in make_eexpr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> type_expr typ_var typ_fun o >>= fun t -> res
  | Error msg -> Error (Format.sprintf "In make_eexpr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let rec make_einstr_of_ast (typ_var : (string,typ) Hashtbl.t) (typ_fun : (string, typ list * typ) Hashtbl.t) (a: tree) : (instr * (string,typ) Hashtbl.t)res =
  let res =
    match a with
    (* TODO *)
    (* typ_var a été ajouté à la valeur de retour de cette fonction
    pour permettre la gestion des variables locales dans les if et while. *)
    | Node(Tassign, [StringLeaf s; e]) -> 
        make_eexpr_of_ast typ_var typ_fun e >>= fun expr ->
        type_expr typ_var typ_fun expr >>= fun te ->
        type_expr typ_var typ_fun (Evar s) >>= fun ts ->
        if are_compatible te ts 
          then OK (Iassign (s, expr), typ_var)
          else Error (Format.sprintf "E: Types %s and %s are not compatible." (string_of_typ ts) (string_of_typ te))
          | Node(Tif, [e; i1; i2]) -> 
            make_eexpr_of_ast typ_var typ_fun e >>= fun expr ->
            make_einstr_of_ast typ_var typ_fun i1 >>= fun (instr1, new_typ_var) ->
            make_einstr_of_ast typ_var typ_fun i2 >>= fun (instr2, new_typ_var) ->
            OK (Iif (expr, instr1, instr2), typ_var)
        | Node(Twhile, [e; i]) -> 
            make_eexpr_of_ast typ_var typ_fun e >>= fun expr ->
            make_einstr_of_ast typ_var typ_fun i >>= fun (instr, new_typ_var) ->
            OK (Iwhile (expr, instr), typ_var)
        | Node(Tblock, i_list) ->
            List.fold_left (fun acc i ->
              acc >>= fun (cur_i_list, cur_typ_var) ->
              make_einstr_of_ast cur_typ_var typ_fun i >>= fun (instr, new_typ_var) ->
              OK(cur_i_list@[instr], new_typ_var)) 
                (OK([], typ_var)) i_list >>= fun (instr_list, new_typ_var) ->
                  OK(Iblock(instr_list), new_typ_var)
        | Node(Treturn, [e]) -> 
            make_eexpr_of_ast typ_var typ_fun e >>= fun expr ->
            OK (Ireturn expr, typ_var)        
        | Node(Tcall, [StringLeaf f; Node(Targs, args)]) -> 
            (list_map_res (make_eexpr_of_ast typ_var typ_fun) args >>= fun exprs ->
            list_map_res (type_expr typ_var typ_fun) exprs >>= fun types ->
            (match Hashtbl.find_option typ_fun f with
            | None -> Error (Format.sprintf "E: Unknown argument types of function %s." f) 
            | Some (arg_types, ret_type) -> 
              if types = arg_types
                then OK (Icall (f, exprs), typ_var)
                else Error (Format.sprintf "E: Unvalid argument types in function %s calling." f)))
        | Node (Tdeclare, [TypeLeaf t; StringLeaf s]) ->
            (if t != Tvoid 
              then 
                (if Hashtbl.mem typ_var s 
                  then 
                    Error (Format.sprintf "E: Variable %s already declared." s)
                  else 
                    let new_typ_var = Hashtbl.copy typ_var
                      in Hashtbl.add new_typ_var s t;
                      OK (Ideclare (t ,s), new_typ_var))
              else 
                Error (Format.sprintf "E: Can not declare void variable."))
    | NullLeaf -> OK (Iblock [], typ_var)
    | _ -> Error (Printf.sprintf "Unacceptable ast in make_einstr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> res
  | Error msg -> Error (Format.sprintf "In make_einstr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let make_ident (a: tree) : (string * typ) res =
  match a with
  | Node (Targ, [TypeLeaf t; StringLeaf s]) -> OK (s, t)
  | a -> Error (Printf.sprintf "make_ident: unexpected AST: %s"
                  (string_of_ast a))

let make_fundef_of_ast (typ_fun : (string, typ list * typ) Hashtbl.t) (a: tree) : (string * efun) res =
  match a with
  | Node (Tfundef, [Node(Tfuntype, [TypeLeaf t]); Node(Tfunname, [StringLeaf fname]); Node (Tfunargs, fargs); Node(Tfunbody, [fbody])]) ->
    list_map_res make_ident fargs >>= fun fargs ->
      (* TODO *)
      let typ_var = Hashtbl.of_list fargs
        in let arg_types = List.map (fun (arg, typ) -> typ) fargs
          in Hashtbl.add typ_fun fname (arg_types, t);
          make_einstr_of_ast typ_var typ_fun fbody >>= fun (fbody, _) ->
          OK (fname, {funargs = fargs; funbody = fbody; funvartyp = typ_var; funrettype = t})
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tfundef, got %s."
             (string_of_ast a))

let make_eprog_of_ast (a: tree) : eprog res =
  match a with
  | Node (Tlistglobdef, l) ->
    let fun_typ = Hashtbl.create (List.length l) in
    Hashtbl.replace fun_typ "print" ([Tint], Tvoid);
    Hashtbl.replace fun_typ "print_int" ([Tint], Tvoid);
    Hashtbl.replace fun_typ "print_char" ([Tchar], Tvoid);
    List.fold_left (fun acc a -> 
    acc >>= fun f_list ->
    make_fundef_of_ast fun_typ a >>= fun (fname, efun) -> 
    match List.assoc_opt fname f_list with
    | None -> OK (f_list@[fname, Gfun efun])
    | Some (Gfun dec) when dec.funbody = Iblock [] -> OK (List.remove_assoc fname f_list @ [fname, Gfun efun])
    | _ -> Error (Format.sprintf "E: Multiple definitions of function %s." fname)) (OK []) l
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tlistglobdef, got %s."
             (string_of_ast a))

let pass_elang ast =
  match make_eprog_of_ast ast with
  | Error msg ->
    record_compile_result ~error:(Some msg) "Elang";
    Error msg
  | OK  ep ->
    dump !e_dump dump_e ep (fun file () ->
        add_to_report "e" "E" (Code (file_contents file))); OK ep


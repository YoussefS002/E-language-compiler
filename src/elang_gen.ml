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

(* [make_eexpr_of_ast a] builds an expression corresponding to a tree [a]. If
   the tree is not well-formed, fails with an [Error] message. *)
let rec make_eexpr_of_ast (a: tree) : expr res =
  let res =
    match a with
    (* TODO *)
    | IntLeaf i -> OK (Eint i)
    | StringLeaf s -> OK (Evar s) 
    | Node(t, [e1; e2]) when tag_is_binop t ->
        (let res1 = make_eexpr_of_ast e1
          in let res2 = make_eexpr_of_ast e2
            in match res1, res2 with
            | Error msg, _ -> Error msg
            | _, Error msg -> Error msg
            | OK expr1, OK expr2 -> OK (Ebinop (binop_of_tag t, expr1, expr2)))
    | Node(Tneg, [e]) -> 
        (let res = make_eexpr_of_ast e
          in match res with
          | Error msg -> Error msg
          | OK expr -> OK (Eunop (Eneg, expr)))
    | Node(Tcall, [StringLeaf f; Node(Targs, args)]) -> 
        (let res = list_map_res make_eexpr_of_ast args 
          in match res with
          | Error msg -> Error msg
          | OK exprs -> OK (Ecall (f, exprs)))
    | _ -> 
        Error (Printf.sprintf "Unacceptable ast in make_eexpr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> res
  | Error msg -> Error (Format.sprintf "In make_eexpr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let rec make_einstr_of_ast (a: tree) : instr res =
  let res =
    match a with
    (* TODO *)
    | Node(Tassign, [StringLeaf s; e]) -> 
      (let res_of_e = make_eexpr_of_ast e 
        in match res_of_e with 
        | OK exp -> OK (Iassign (s, exp))
        | Error msg -> Error msg)
    | Node(Tif, [e; i1; i2]) -> 
      (let res_of_e = make_eexpr_of_ast e
        in let res_of_i1 = make_einstr_of_ast i1
          in let res_of_i2 = make_einstr_of_ast i2
            in match res_of_e, res_of_i1, res_of_i2 with 
            | Error msg, _, _ -> Error msg
            | _, Error msg, _ -> Error msg
            | _, _, Error msg -> Error msg
            | OK exp, OK inst1, OK inst2 -> OK (Iif (exp, inst1, inst2)))
    | Node(Twhile, [e; i]) -> 
      (let res_of_e = make_eexpr_of_ast e
        in let res_of_i = make_einstr_of_ast i
          in match res_of_e, res_of_i with 
          | Error msg, _ -> Error msg
          | _, Error msg -> Error msg
          | OK exp, OK inst-> OK (Iwhile (exp, inst)))
    | Node(Tblock, i_list) -> 
      (let res_of_i_list = list_map_res make_einstr_of_ast i_list
        in match res_of_i_list with
        | Error msg -> Error msg
        | OK instr_list -> OK (Iblock instr_list))
    | Node(Treturn, [e]) -> 
      (let res_of_e = make_eexpr_of_ast e 
        in match res_of_e with 
        | OK exp -> OK (Ireturn exp)
        | Error msg -> Error msg)
    | Node(Tcall, [StringLeaf f; Node(Targs, args)]) -> 
      (let res = list_map_res make_eexpr_of_ast args 
        in match res with
        | Error msg -> Error msg
        | OK exprs -> OK (Icall (f, exprs)))
    | NullLeaf -> OK (Iblock [])
    | _ -> Error (Printf.sprintf "Unacceptable ast in make_einstr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> res
  | Error msg -> Error (Format.sprintf "In make_einstr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let make_ident (a: tree) : string res =
  match a with
  | Node (Targ, [s]) ->
    OK (string_of_stringleaf s)
  | a -> Error (Printf.sprintf "make_ident: unexpected AST: %s"
                  (string_of_ast a))

let make_fundef_of_ast (a: tree) : (string * efun) res =
  match a with
  | Node (Tfundef, [Node(Tfunname, [StringLeaf fname]); Node (Tfunargs, fargs); Node(Tfunbody, [fbody])]) ->
    list_map_res make_ident fargs >>= fun fargs ->
     (* TODO *)
      make_einstr_of_ast fbody >>= fun fbody ->
        OK (fname, {funargs = fargs; funbody = fbody})
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tfundef, got %s."
             (string_of_ast a))

let make_eprog_of_ast (a: tree) : eprog res =
  match a with
  | Node (Tlistglobdef, l) ->
    list_map_res (fun a -> make_fundef_of_ast a >>= fun (fname, efun) -> OK (fname, Gfun efun)) l
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


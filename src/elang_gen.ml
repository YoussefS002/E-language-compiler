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

let binop_is_cmp =
  function 
    | Ecle 
    | Eclt
    | Ecge
    | Ecgt
    | Eceq
    | Ecne -> true
    | _ -> false

let type_is_ptr =
  function
  | Tptr ty -> true
  | _ -> false

let remove_local_vars typ_var local_typ_var =
  Hashtbl.filteri (fun s t -> Hashtbl.mem typ_var s) local_typ_var
      
let rec type_expr (typ_var : (string,typ) Hashtbl.t) (typ_fun : (string, typ list * typ) Hashtbl.t) (e: expr) : typ res =
  match e with
  | Ebinop (b, e1, e2) -> 
      type_expr typ_var typ_fun e1 >>= fun t1 ->
      type_expr typ_var typ_fun e2 >>= fun t2 ->
      if t1 != Tvoid && t2 != Tvoid
        then 
          match t1, t2 with
          | Tptr ty, Tint 
          | Tptr ty, Tchar ->
              (match b with
              | Eadd -> OK (Tptr ty)
              | Esub -> OK (Tptr ty)
              | _ -> Error "Binop is not defined on pointer.")
          | Tchar, Tptr ty            
          | Tint, Tptr ty -> 
              (match b with
              | Eadd -> OK (Tptr ty)
              | _ -> Error "Binop is not defined on pointer.")
          | Tptr ty1, Tptr ty2 -> 
            if binop_is_cmp b
              then 
                if ty1 = ty2 
                  then OK Tint
                  else Error "Uncomparable pointers."  
              else
                Error "Binop is not defined on pointer type."
          | _ -> OK (Tint) 
        else Error "Binop is not defined on void type."
  | Eunop (u, e) -> 
      type_expr typ_var typ_fun e >>= fun t ->
        if t != Tvoid && t!= Tptr t
          then OK Tint
          else Error "Unop is not defined on void or pointer type."
  | Eint i -> OK Tint
  | Echar c -> OK Tchar
  | Evar s -> 
    (match Hashtbl.find_option typ_var s with
    | Some t when t != Tvoid -> OK t
    | _ -> Error (Format.sprintf "Expression %s type is not defined." s))
  | Ecall (f, exprs) -> 
    (match Hashtbl.find_option typ_fun f with
    | Some (arg_types, ret_type) when ret_type != Tvoid ->
        list_map_res (type_expr typ_var typ_fun) exprs >>= fun types ->
          if types = arg_types
            then OK ret_type
            else Error (Format.sprintf "Unvalid argument types in function %s calling." f)
    | _ -> Error "Function return type is not defined.")
  | Eaddrof e -> 
    type_expr typ_var typ_fun e >>= fun t ->
    OK (Tptr t)
  | Eload e -> 
    type_expr typ_var typ_fun e >>= fun t ->
      match t with
      | Tptr ty -> OK ty
      | _ -> Error "Unvalid loading."
    
let rec addr_taken_expr (e: expr) : string Set.t =
  match e with
  | Ebinop (b, e1, e2) -> Set.union (addr_taken_expr e1) (addr_taken_expr e2)
  | Eunop (b, e) -> addr_taken_expr e
  | Eint _ 
  | Evar _
  | Echar _ -> Set.empty
  | Ecall (_, e_list) -> set_concat (List.map addr_taken_expr e_list)
  | Eaddrof e ->
      (match e with
      | Evar s -> Set.singleton s
      | _ -> Set.empty)
  | Eload e -> addr_taken_expr e

let rec addr_taken_instr (i: instr) : string Set.t =
  match i with
  | Iassign (_, e) -> addr_taken_expr e
  | Iif (e, i1, i2) -> set_concat ([addr_taken_expr e; addr_taken_instr i1; addr_taken_instr i2])
  | Iwhile (e, i) -> Set.union (addr_taken_expr e) (addr_taken_instr i)
  | Iblock i_list -> set_concat (List.map addr_taken_instr i_list)
  | Ireturn e -> addr_taken_expr e
  | Icall (_, e_list) -> set_concat (List.map addr_taken_expr e_list)
  | Ideclare _ -> Set.empty
  | Istore (e1, e2) -> Set.union (addr_taken_expr e1) (addr_taken_expr e2)

let are_compatible (t1 : typ) (t2 : typ) : bool =
  match t1, t2 with
  | Tint, Tint
  | Tchar, Tchar
  | Tint, Tchar
  | Tchar, Tint -> true
  | Tptr ty1, Tptr ty2 when ty1 = ty2 -> true
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
    | Node(Tmem, [MemopsLeaf memops; e]) -> 
        (match memops with
        | [] -> make_eexpr_of_ast typ_var typ_fun e
        | memop::rest -> 
          (make_eexpr_of_ast typ_var typ_fun (Node(Tmem, [MemopsLeaf rest; e])) >>= fun e_rest ->
          match memop with
          | Asterik -> OK (Eload e_rest)
          | Ampersand -> OK (Eaddrof e_rest)))
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
      | Node(Tassign, [id; e]) -> 
        make_eexpr_of_ast typ_var typ_fun id >>= fun id_expr ->
        type_expr typ_var typ_fun id_expr >>= fun tid ->
        make_eexpr_of_ast typ_var typ_fun e >>= fun expr ->
        type_expr typ_var typ_fun expr >>= fun te ->
        (match id_expr with
        | Evar s ->
            if are_compatible te tid
              then OK (Iassign (s, expr), typ_var)
              else Error (Format.sprintf "Types %s and %s are not compatible." (string_of_typ tid) (string_of_typ te))
        | Eload ptr_expr -> OK (Istore (ptr_expr, expr), typ_var)
        | _ -> Error (Printf.sprintf "Unacceptable ast in make_einstr_of_ast %s" (string_of_ast a)))
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
    | Node(Tmem, [MemopsLeaf []; Node(Tcall, [StringLeaf f; Node(Targs, args)])]) ->
        (list_map_res (make_eexpr_of_ast typ_var typ_fun) args >>= fun exprs ->
        list_map_res (type_expr typ_var typ_fun) exprs >>= fun types ->
        (match Hashtbl.find_option typ_fun f with
        | None -> Error (Format.sprintf "Unknown argument types of function %s." f) 
        | Some (arg_types, ret_type) -> 
          if types = arg_types
            then OK (Icall (f, exprs), typ_var)
            else Error (Format.sprintf "Unvalid argument types in function %s calling." f)))
    | Node (Tdeclare, [TypeLeaf t; StringLeaf s]) ->
        (if t != Tvoid 
          then 
            (if Hashtbl.mem typ_var s 
              then
                Error (Format.sprintf "Variable %s already declared." s)
              else
                let new_typ_var = Hashtbl.copy typ_var
                  in Hashtbl.add new_typ_var s t;
                  OK (Ideclare (t ,s), new_typ_var))
          else 
            Error (Format.sprintf "Can not declare void variable."))
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
          make_einstr_of_ast typ_var typ_fun fbody >>= fun (fbody, typ_var) ->
           let ofs = ref 0 
              in let funvarinmem = Hashtbl.create 17
                in List.iter (fun x -> 
                  match Hashtbl.find_option typ_var x with
                  | None -> ()
                  | Some t -> 
                    ofs := !ofs + size_type t;
                    Hashtbl.add funvarinmem x !ofs;) (List.rev (Set.to_list (addr_taken_instr fbody)));
                  OK (fname,
                  {funargs = fargs;
                  funbody = fbody;
                  funvartyp = typ_var;
                  funrettype = t;
                  funvarinmem = funvarinmem;
                  funstksz = !ofs})
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
    | _ -> Error (Format.sprintf "Multiple definitions of function %s." fname)) (OK []) l
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


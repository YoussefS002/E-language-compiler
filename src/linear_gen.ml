open Batteries
open Rtl
open Linear
open Prog
open Utils
open Report
open Linear_print
open Options
open Linear_liveness

let succs_of_rtl_instr (i: rtl_instr) =
  match i with
  | Rtl.Rbranch (_, _, _, s1) -> [s1]
  | Rtl.Rjmp s -> [s]
  | _ -> []

let rec succs_of_rtl_instrs il : int list =
  List.concat (List.map succs_of_rtl_instr il)

(* effectue un tri topologique des blocs.  *)
let sort_blocks (nodes: (int, rtl_instr list) Hashtbl.t) entry =
  let rec add_block order visited n =
    (* TODO *)
    List.of_enum (Hashtbl.keys nodes)
    (*if Set.mem n visited 
      then order
      else let succs = succs_of_rtl_instrs (Hashtbl.find nodes n) 
        in List.concat( (order@[n]) :: List.map (add_block [] (Set.add n visited)) succs )*)
  in
  add_block [] Set.empty entry


(* Supprime les jumps inutiles (Jmp à un label défini juste en dessous). *)
let rec remove_useless_jumps (l: rtl_instr list) =
  (* TODO *)
  l
  (*match l with
  | [] -> []
  | Rjmp l1::Rlabel l2::rest -> 
      if l1=l2 
        then Rlabel l2::remove_useless_jumps rest 
        else Rjmp l1::Rlabel l2::remove_useless_jumps rest
  | i::rest -> i::remove_useless_jumps rest*)


(* Remove labels that are never jumped to. *)
let remove_useless_labels (l: rtl_instr list) =
  (* TODO *)
  l
  (*List.filter (function 
    Rlabel i -> List.exists (
      function 
      Rbranch(_, _, _, j) -> j = i 
      | Rjmp j -> j = i
      | _ -> false) l
    | _ -> true) l*)

let linear_of_rtl_fun
    ({ rtlfunargs; rtlfunbody; rtlfunentry; rtlfuninfo }: rtl_fun) =
  let block_order = sort_blocks rtlfunbody rtlfunentry in
  let linearinstrs =
    Rjmp rtlfunentry ::
    List.fold_left (fun l n ->
        match Hashtbl.find_option rtlfunbody n with
        | None -> l
        | Some li -> l @ Rlabel(n) :: li
      ) [] block_order in
  { linearfunargs = rtlfunargs;
    linearfunbody =
      linearinstrs |> remove_useless_jumps |> remove_useless_labels;
    linearfuninfo = rtlfuninfo;
  }

let linear_of_rtl_gdef = function
    Gfun f -> Gfun (linear_of_rtl_fun f)

let linear_of_rtl r =
  assoc_map linear_of_rtl_gdef r

let pass_linearize rtl =
  let linear = linear_of_rtl rtl in
  let lives = liveness_linear_prog linear in
  dump !linear_dump (fun oc -> dump_linear_prog oc (Some lives)) linear
    (fun file () -> add_to_report "linear" "Linear" (Code (file_contents file)));
  OK (linear, lives)

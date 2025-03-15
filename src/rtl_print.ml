open Batteries
open Elang_print
open Rtl
open Prog
open Utils


let print_reg r =
  Format.sprintf "r%d" r

let print_cmpop (r: rtl_cmp) =
  (match r with
  | Rclt -> "<"
  | Rcle -> "<="
  | Rcgt -> ">"
  | Rcge -> ">="
  | Rceq -> "=="
  | Rcne -> "!=")

let dump_rtl_instr name (live_in, live_out) ?(endl="\n") oc (i: rtl_instr) =
  let print_node s = Format.sprintf "%s_%d" name s in

  let dump_liveness live where =
    match live with
      Some live -> Format.fprintf oc "// Live %s : { %s }\n" where (String.concat ", " (Set.to_list (Set.map string_of_int live)))
    | None -> ()
  in
  dump_liveness live_in "before";
  begin match i with
  | Rbinop (b, rd, rs1, rs2) ->
    Format.fprintf oc "%s <- %s(%s, %s)" (print_reg rd) (dump_binop b) (print_reg rs1) (print_reg rs2)
  | Runop (u, rd, rs) ->
    Format.fprintf oc "%s <- %s(%s)" (print_reg rd) (dump_unop u) (print_reg rs)
  | Rconst (rd, i) ->
    Format.fprintf oc "%s <- %d" (print_reg rd) i
  | Rbranch (cmpop, r1, r2, s1) ->
    Format.fprintf oc "%s %s %s ? jmp %s" (print_reg r1) (print_cmpop cmpop) (print_reg r2) (print_node s1)
  | Rjmp s ->
    Format.fprintf oc "jmp %s" (print_node s)
  | Rmov (rd, rs) -> Format.fprintf oc "%s <- %s" (print_reg rd) (print_reg rs)
  | Rret r -> Format.fprintf oc "ret %s" (print_reg r)
  | Rprint r -> Format.fprintf oc "print %s" (print_reg r)
  | Rlabel n -> Format.fprintf oc "%s_%d:" name n
  | Rcall (rd_opt, f, regs) ->  
    match rd_opt with
    | None -> Format.fprintf oc "call %s with args : %s" f (String.concat ", " (List.map print_reg regs))
    | Some rd -> Format.fprintf oc "%s <- call %s with args : %s" (print_reg rd) f (String.concat ", " (List.map print_reg regs))
  end;
  Format.fprintf oc "%s" endl;
  dump_liveness live_out "after"

let dump_rtl_node name lives =
  print_listi (fun i ->
      dump_rtl_instr name
        (match lives with
           None -> (None, None)
         | Some (lin, lout) ->
           Hashtbl.find_option lin i, Hashtbl.find_option lout i)
        ~endl:"\n"
    ) "" "" ""

let dump_rtl_fun oc rtlfunname ({ rtlfunargs; rtlfunbody; rtlfunentry }: rtl_fun) =
  Format.fprintf oc "%s(%s):\n" rtlfunname
    (String.concat ", " $ List.map print_reg rtlfunargs);
  Hashtbl.iter (fun n node ->
      Format.fprintf oc "%s_%d:\n" rtlfunname n;
      dump_rtl_node rtlfunname None oc node) rtlfunbody

let dump_rtl_prog oc cp =
  dump_prog dump_rtl_fun oc cp

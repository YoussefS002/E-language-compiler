open Batteries
open Cfg
open Utils

(* Analyse de vivacité *)

(* [vars_in_expr e] renvoie l'ensemble des variables qui apparaissent dans [e]. *)
let rec vars_in_expr (e: expr) =
   (* TODO *)
   match e with
   | Eint i -> Set.empty
   | Evar s -> Set.singleton s
   | Ebinop (b, e1, e2) -> Set.union (vars_in_expr e1) (vars_in_expr e2)
   | Eunop (u, e) -> vars_in_expr e

(* [live_after_node cfg n] renvoie l'ensemble des variables vivantes après le
   nœud [n] dans un CFG [cfg]. [lives] est l'état courant de l'analyse,
   c'est-à-dire une table dont les clés sont des identifiants de nœuds du CFG et
   les valeurs sont les ensembles de variables vivantes avant chaque nœud. *)
let live_after_node cfg n (lives: (int, string Set.t) Hashtbl.t) : string Set.t =
   (* TODO *)
   let in_succs_opt = Set.map (fun s -> Hashtbl.find_opt lives s) (succs cfg n)
      in let in_succs = Set.filter_map (fun s -> s) in_succs_opt
         in set_concat (Set.to_list in_succs) 
(* [live_cfg_node node live_after] renvoie l'ensemble des variables vivantes
   avant un nœud [node], étant donné l'ensemble [live_after] des variables
   vivantes après ce nœud. *)
let live_cfg_node (node: cfg_node) (live_after: string Set.t) =
   (* TODO *)
   let use node = 
         match node with
         | Cassign (s, e, i) -> vars_in_expr e
         | Creturn e -> vars_in_expr e
         | Cprint (e, i) -> vars_in_expr e
         | Ccmp (e, i1, i2) -> vars_in_expr e
         | Cnop (i) -> Set.empty
      in let def node =
            match node with
            | Cassign (s, e, i) -> Set.singleton s
            | _ -> Set.empty
         in Set.union (use node) (Set.diff live_after (def node))

(* [live_cfg_nodes cfg lives] effectue une itération du calcul de point fixe.

   Cette fonction met à jour l'état de l'analyse [lives] et renvoie un booléen
   qui indique si le calcul a progressé durant cette itération (i.e. s'il existe
   au moins un nœud n pour lequel l'ensemble des variables vivantes avant ce
   nœud a changé). *)
let live_cfg_nodes cfg (lives : (int, string Set.t) Hashtbl.t) =
   (* TODO *)
   let changed = ref false in
      Hashtbl.iter (fun n node -> 
         let new_alive_vars = live_cfg_node node (live_after_node cfg n lives)
            in match Hashtbl.find_opt lives n with
            | None -> changed := true; Hashtbl.replace lives n new_alive_vars
            | Some alive_vars -> if not (Set.equal alive_vars new_alive_vars) then changed := true; Hashtbl.replace lives n new_alive_vars) cfg; !changed


(* [live_cfg_fun f] calcule l'ensemble des variables vivantes avant chaque nœud
   du CFG en itérant [live_cfg_nodes] jusqu'à ce qu'un point fixe soit atteint.
   *)
let live_cfg_fun (f: cfg_fun) : (int, string Set.t) Hashtbl.t =
   let lives = Hashtbl.create 17 in
     (* TODO *)
      let cfg = f.cfgfunbody
         in while live_cfg_nodes cfg lives do 
            ()
         done; 
         lives

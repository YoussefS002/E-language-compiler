open Elang
open Batteries
open Prog
open Utils
open Builtins
open Elang_gen

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
let rec eval_eexpr oc st (ep: eprog) (typ_var : (string,typ) Hashtbl.t) (typ_fun : (string, typ list * typ) Hashtbl.t) (inmem_var : (string, int) Hashtbl.t) (sp : int) (e : expr) : (int * int state) res =
   match e with
   | Eint i -> OK (i, st)
   | Evar s -> 
      (match Hashtbl.find_opt inmem_var s with
      | None -> OK (Hashtbl.find st.env s, st)
      | Some ofs -> 
         Mem.read_bytes_as_int st.mem (sp - ofs) (size_type (Hashtbl.find typ_var s)) >>= fun v ->
         Printf.printf "Read %d in address %d\n" v (sp - ofs); OK (v, st)) 
   | Ebinop (b, ex, ey) -> 
      eval_eexpr oc st ep typ_var typ_fun inmem_var sp ex >>= fun (x, st') ->
      eval_eexpr oc st' ep typ_var typ_fun inmem_var sp ey >>= fun (y, st'') ->
         type_expr typ_var typ_fun ex >>= fun tx ->
         type_expr typ_var typ_fun ey >>= fun ty ->
         (match tx, ty with
         | Tptr t, Tint -> OK (eval_binop b x (y * size_type t), st'')
         | Tint, Tptr t -> OK (eval_binop b (x * size_type t) y, st'')
         | _  -> OK (eval_binop b x y, st''))
   | Eunop (u, ex) ->
      eval_eexpr oc st ep typ_var typ_fun inmem_var sp ex >>= fun (x, st') ->
      OK (eval_unop u x, st')
   | Ecall (f, args) -> 
      (List.fold_left
            (fun acc arg -> 
            acc >>= fun (l, st') ->
            eval_eexpr oc st' ep typ_var typ_fun inmem_var sp arg >>= fun (i, st'') ->
            OK ((l@[i]), st'')) 
               (OK([], st)) args >>= fun(int_args, st') -> 
            match find_function ep f with
            | OK found_f -> 
               eval_efun oc st' ep found_f f int_args typ_fun sp >>= fun (ret_opt, st'') -> 
                  OK (Option.get ret_opt, st'')
            | Error msg -> 
               do_builtin oc st'.mem f int_args >>= fun (ret_opt) -> 
                  OK (Option.get ret_opt, st'))
   | Echar c -> OK (Char.code c, st)
   | Eload e -> 
      eval_eexpr oc st ep typ_var typ_fun inmem_var sp e >>= fun (p, st') -> 
      type_expr typ_var typ_fun e >>= fun tp ->
      Mem.read_bytes_as_int st'.mem p (size_type tp) >>= fun v ->
      Printf.printf "Read %d in memory address %d\n" v p; OK (v, st')
   | Eaddrof e -> 
      match e with
      | Evar s -> 
         (match Hashtbl.find_option inmem_var s with
         | Some ofs -> Printf.printf "Address of %s evaluated : %d\n" s (sp - ofs); OK (sp - ofs, st)
         | None -> Error (Format.sprintf "Address of %s not found" s))
      | Eload p -> eval_eexpr oc st ep typ_var typ_fun inmem_var sp p
      | _ -> Error "Type cannot have an address"
         
(* [eval_einstr oc st ins] évalue l'instruction [ins] en partant de l'état [st].

   Le paramètre [oc] est un "output channel", dans lequel la fonction "print"
   écrit sa sortie, au moyen de l'instruction [Format.fprintf].

   Cette fonction renvoie [(ret, st')] :

   - [ret] est de type [int option]. [Some v] doit être renvoyé lorsqu'une
   instruction [return] est évaluée. [None] signifie qu'aucun [return] n'a eu
   lieu et que l'exécution doit continuer.

   - [st'] est l'état mis à jour. *)
and eval_einstr oc (st: int state) (ep: eprog) (typ_var : (string,typ) Hashtbl.t) (typ_fun : (string, typ list * typ) Hashtbl.t) (inmem_var : (string, int) Hashtbl.t) (sp : int) (ins: instr) :
  (int option * int state * (string,typ) Hashtbl.t) res =
  (* typ_var a été ajouté à la valeur de retour de cette fonction
    pour permettre la gestion des variables locales dans les if et while. *)   
   match ins with
   | Iassign (s, e) ->   
      (let replace st s v =
            let new_env = Hashtbl.copy st.env 
               in Hashtbl.replace new_env s v; 
                  {st with env = new_env} 
         in match eval_eexpr oc st ep typ_var typ_fun inmem_var sp e with
         | Error msg -> Error msg
         | OK (v, st') -> 
            match Hashtbl.find_opt inmem_var s with
            | None -> OK (None, replace st' s v, typ_var)
            | Some ofs -> 
               Mem.write_bytes st'.mem (sp - ofs) (split_bytes (size_type (Hashtbl.find typ_var s)) v) >>= fun _ ->
               Printf.printf "Assigning %d in address %d\n" v (sp - ofs); OK (None, st', typ_var))
   | Iif (e, i1, i2) -> 
      eval_eexpr oc st ep typ_var typ_fun inmem_var sp e >>= fun (v, st') -> 
         if v != 0 
            then 
               eval_einstr oc st' ep typ_var typ_fun inmem_var sp i1 >>= fun (ret_opt, st', new_typ_var) ->
               OK (ret_opt, st', typ_var)  
            else 
               eval_einstr oc st' ep typ_var typ_fun inmem_var sp i2 >>= fun (ret_opt, st', new_typ_var) ->
               OK (ret_opt, st', typ_var)  
   | Iwhile (e, i) -> 
      (eval_eexpr oc st ep typ_var typ_fun inmem_var sp e >>= fun (v, st') ->
         if v != 0 
            then eval_einstr oc st' ep typ_var typ_fun inmem_var sp i >>= fun (r_opt, next_st, new_typ_var) -> 
               match r_opt with 
               | None -> eval_einstr oc next_st ep typ_var typ_fun inmem_var sp (Iwhile (e, i))
               | Some r -> OK (r_opt, next_st, typ_var)
            else OK (None, st', typ_var))
   | Iblock i_list -> 
      (match i_list with
      | [] -> OK (None, st, typ_var)
      | i::rest -> 
         match eval_einstr oc st ep typ_var typ_fun inmem_var sp i with
         | Error msg -> Error msg
         | OK (Some r, next_st, new_typ_var) -> OK (Some r, next_st, new_typ_var)
         | OK (None, next_st, new_typ_var) -> eval_einstr oc next_st ep new_typ_var typ_fun inmem_var sp (Iblock rest))
   | Ireturn e -> 
      eval_eexpr oc st ep typ_var typ_fun inmem_var sp e >>= fun (v, st') -> 
         OK(Some v, st', typ_var)
   | Icall (f, args) -> 
      (List.fold_left
         (fun acc arg -> 
         acc >>= fun (l, st') ->
         eval_eexpr oc st' ep typ_var typ_fun inmem_var sp arg >>= fun (i, st'') ->
         OK ((l@[i]), st'')) 
            (OK([], st)) args >>= fun(int_args, st') -> 
            match find_function ep f with
            | OK found_f -> 
               (eval_efun oc st' ep found_f f int_args typ_fun sp >>= fun (_, st'') -> 
                  OK (None, st'', typ_var))
            | Error msg -> 
               (do_builtin oc st'.mem f int_args >>= fun _ -> 
                  OK (None, st', typ_var)))
   | Ideclare (t, s) -> 
      let new_typ_var = Hashtbl.copy typ_var
         in Hashtbl.add new_typ_var s t;
         OK (None, st, new_typ_var)
   | Istore (p_expr, e) ->
      eval_eexpr oc st ep typ_var typ_fun inmem_var sp p_expr >>= fun (addr, st') ->
      eval_eexpr oc st ep typ_var typ_fun inmem_var sp e >>= fun (v, st'') ->
      type_expr typ_var typ_fun p_expr >>= fun tp ->
      match tp with
      | Tptr t -> 
         Mem.write_bytes st''.mem addr (split_bytes (size_type t) v) >>= fun u ->
         Printf.printf "Storing %d in address %d\n" v addr; OK (None, st'', typ_var)
      | _ -> 
         Error "Storing in unvalid address"
and print_hashtbl (tbl : (string, int) Hashtbl.t) sp =
            Hashtbl.iter (fun key value -> Printf.printf "%s -> %d\n" key (sp - value)) tbl
          
(* [eval_efun oc st f fname vargs] évalue la fonction [f] (dont le nom est
   [fname]) en partant de l'état [st], avec les arguments [vargs].

   Cette fonction renvoie un couple (ret, st') avec la même signification que
   pour [eval_einstr]. *)
and eval_efun oc (st: int state) ep ({funargs; funbody; funvartyp; funrettype; funvarinmem; funstksz}: efun)
    (fname: string) (vargs: int list) (typ_fun : (string, typ list * typ) Hashtbl.t) (sp : int)
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
   Printf.printf "Stack size = %d\n" funstksz;
   print_hashtbl funvarinmem (sp + funstksz);
   eval_einstr oc { st with env } ep funvartyp typ_fun funvarinmem (sp + funstksz) funbody >>= fun (v, st', _) ->
   OK (v, { st' with env = env_save })
  | exception Invalid_argument _ ->
    Error (Format.sprintf
             "E: Called function %s with %d arguments, expected %d.\n"
             fname (List.length vargs) (List.length funargs)
          )
let create_typ_fun (ep: eprog) : (string, typ list * typ) Hashtbl.t =
   let typ_fun = Hashtbl.create (List.length ep)
      in List.iter (fun (fname, gfun) -> 
         match gfun with
         | Gfun efun -> 
            let arg_types = List.map (fun (arg, typ) -> typ) efun.funargs
               in Hashtbl.add typ_fun fname (arg_types, efun.funrettype)) ep;
               typ_fun

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
  let typ_fun = create_typ_fun ep in
  eval_efun oc st ep f "main" params typ_fun 0 >>= fun (v, _) ->
  OK v

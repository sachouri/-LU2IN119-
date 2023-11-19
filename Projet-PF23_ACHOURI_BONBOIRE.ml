(* 
  ACHOURI Sira Lina 21102770
  BONBOIRE Marie 21100552 
*)

(* Contenu additionnel : README, Rapport et documentation. Voir Moodle *) 

(* *********** Question 1.a *********** *) 
type element = Empty
             | DUP    (* n -- n n *)
             | DROP    (* n --  *)
             | SWAP    (* a b -- b a *)
             | ROT    (* a b c -- b c a *)
             | OPA of string    (* a b -- op(a,b) de type INT *)
             | OPB of string    (* a b -- op(a,b) de type BOOL *)
             | STRING of string
             | INT of int
             | BOOL of bool;;


(* *********** Question 1.b *********** *)

(** [to_string x] retourne la chaine de caracteres 
    correspondante a l'element [x] 
*)
let to_string (x:element) : string = match x with
  | Empty -> ""
  | DUP -> "DUP"
  | DROP -> "DROP"
  | SWAP -> "SWAP"
  | ROT -> "ROT"
  | OPA(a) -> a
  | OPB(a) -> a
  | STRING(a) -> a
  | INT(a) -> string_of_int a
  | BOOL(a) -> if a then "TRUE" else "FALSE";;

(** [of_string s] retourne l'element correspondant
    a la chaine de caracteres [s]
*)
let of_string (s:string) : element = match s with
  | "" -> Empty 
  | "DUP" -> DUP
  | "DROP" -> DROP
  | "SWAP" -> SWAP
  | "ROT" -> ROT
  | ("*" | "/" | "+" | "-" | "mod" ) -> OPA(s) 
  | ("=" | "<>" | "<" | ">" | "<=" | ">=") -> OPB(s) 
  | "TRUE" -> BOOL(true)
  | "FALSE" -> BOOL(false) 
  | s -> match (int_of_string_opt s) with
    | None -> STRING(s) 
    | Some a -> INT(a)


(* *********** Question 1.c *********** *)

(** fonction utilitaire : 
    [split s] decoupe le texte [s] en une suite de mot. 
*)
let split (s:string) : string list =
  (* traite les tabulations et les sauts de lignes comme des espaces *)
  let normalize_s = String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) s in
  let split_s = String.split_on_char ' ' normalize_s in
  (* ignore les espaces successifs *)
  List.filter ((<>) "") split_s ;;

assert (split "  \t  \n " = []) ;;
assert (split " A     \n B\t C  " = ["A";"B";"C"]) ;;

(** transforme un texte (representant un programme ou une pile)
    en une suite de symboles du langage (e.g., "42" et "DUP") 
*)
let parse (s:string) : element list = List.map of_string (split s);;

(** transforme un suite de symbole du langage (representant un programme 
    ou une pile) en un texte equivalent. 
    Par exemple : [text (parse "1 2 +")) = "1 2 +"].
*)
let text (p:element list) : string = String.concat " " (List.map to_string p);; 


(* *********** Question 2 *********** *)

type prog = element list
type stack = element list
  
(** fonction auxiliaire : evaluation d'un operateur binaire 
    [eval_binop op e1 e2] effectue l'operation (e1 e2 -- op(e1,e2))
*)
let eval_binop op (e1:element) (e2:element) : element = match op with
  | OPA x -> (match (e1,e2) with 
      | INT(e1),INT(e2) -> (match x with
          | "+" -> INT(e1+e2)
          | "-" -> INT(e1-e2)
          | "*" -> INT(e1*e2)
          | "mod" -> INT(e1 mod e2)
          | "/" -> if e2!=0 then INT(e1/e2) else raise Division_by_zero
          | _ -> raise (Invalid_argument "eval_binop"))
      | _ -> raise (Invalid_argument "eval_binop"))
  | OPB x -> (match x with
      | "=" -> BOOL(e1=e2)
      | "<>" -> BOOL(e1!=e2)
      | "<" -> BOOL(e1<e2)
      | ">" -> BOOL(e1>e2)
      | "<=" -> BOOL(e1<=e2)
      | ">=" -> BOOL(e1>=e2)
      | _ -> raise (Invalid_argument "eval_binop"))
  | _ -> raise (Invalid_argument "eval_binop")
;;

(** fonction auxiliaire : evaluation d'un operateur binaire
    [eval_stackop stk op] evalue [op] avec les deux premiers
    elements de la pile [stk] et retourne la pile resultante
*)
let eval_stackop (stk:stack) op : stack = match stk with
  | [] | _::[] -> raise (Invalid_argument "eval_stackop")
  | x1::x2::tl -> (eval_binop op x2 x1)::tl;;

(** [step stk e] execute l'element [e] dans la pile [stk] 
    et retourne la pile resultante 
*)
let step (stk:stack) (e:element) : stack = match e with
  | Empty -> stk
  | INT _ | BOOL _ -> e::stk 
  | OPA _ | OPB _ -> eval_stackop stk e
  | SWAP -> (match stk with 
      | [] | _::[] -> raise (Invalid_argument "step")
      | x1::x2::tl -> x2::x1::tl)
  | DUP -> (match stk with
      | [] -> raise (Invalid_argument "step")
      | x::tl -> x::x::tl)
  | DROP -> (match stk with 
      | [] -> raise (Invalid_argument "step")
      | x::tl -> tl)
  | ROT -> (match stk with 
      | [] | _::[] | _::_::[] -> raise (Invalid_argument "step")
      | x::y::z::tl -> y::z::x::tl) 
  | STRING _ -> raise (Invalid_argument "step");;


(* *********** Question 3 *********** *)

(** [calc stk p] execute le programme [p] dans la pile [stk] 
    et retourne la pile resultante 
*)
let rec calc (stk:stack) (p:prog) : stack = match p with
  | [] -> stk
  | x::tl -> calc (step stk x) tl;; 
  

(* *********** Question 4 *********** *)

type name = string
  
(* liste d'association (nom - programme) car structure etudiee
   et bien maitrisee donc cela facilite de l'implementation
*)
type dico = (name*prog) list
let empty : dico = [] 

(** [lookup x dico] retourne le programme associe a [x] 
    si l'association existe dans [dico]
*)
let rec lookup (x:name) (dico:dico) : prog = 
  match dico with 
  | [] -> raise Not_found
  | (n,p)::tl -> if n=x then p else lookup x tl;; 
          
(** [add x def dico] ajoute l'association [(x,def)] dans 
    le dico [dico] et retourne le dico resultant. Si une association
    portant le meme nom existe, nous la supprimons avant d'ajouter 
    la nouvelle definition
*) 
let add (x:name) (def:prog) (dico:dico) : dico = 
  let rec aux dicog dicod = 
    match dicog with
    | [] -> dicod
    | (n,p)::tl -> if n=x then aux tl dicod
        else aux tl dicod@[(n,p)] 
  in let new_dico = aux dico [] in (x,def)::new_dico;;

(** [remove x dico] supprime l'association [(x,def)] du 
    le dico [dico] et retourne le dico resultant 
*) 
let remove (x:name) (dico:dico) : dico = 
  let rec aux dicog dicod = 
    match dicog with
    | [] -> dicod
    | (n,p)::tl -> if n=x then aux tl dicod
        else aux tl dicod@[(n,p)]
                             
  in let new_dico = aux dico [] in 
  if new_dico = dico then raise (Invalid_argument "remove")
  else new_dico;;


(* *********** Question 5 *********** *)

(** [eval dico stk p] execute le programme [p] dans la pile [stk] 
    et retourne la pile resultante (cf. rapport pour le detail)
*)
let rec eval (dico:dico) (stk:stack) (p:prog) : stack = match p with 
  (* cas de base *)
  | [] -> stk
    
  | x::tl -> match x with 
    (* definition *)
    | STRING(":") -> 
        (let rec aux_def (dico:dico) (prog:prog) (def:element list) :
          (dico*(element list)*prog) = match prog with
           | [] -> raise (Invalid_argument "eval")
           | x::tl -> (match x with 
               | STRING(":") -> let nd_imb, def_imb, np_imb = (aux_def dico tl []) 
                   in aux_def nd_imb np_imb (def@[x]@def_imb@[STRING(";")]) 
               | STRING(";") -> (match def with
                   | STRING(name)::exp -> ((add name exp dico),def,tl)
                   | _ -> raise (Invalid_argument "eval"))
               | _ -> aux_def dico tl (def@[x]))
         in let nd,def,np=(aux_def dico tl []) in eval nd stk np) 
  
    (* structure conditionnelle *)
    | STRING("IF") -> 
        (let rec aux_if (prog:prog) (pile:stack) (instr:(element list)) :
          (prog*stack) = match prog with 
  
           (* IF _____ THEN *)
           | e::STRING("THEN")::tl -> 
               (match pile with 
                | BOOL(b)::stk_tl -> 
                    if b then (instr@[e])@tl, stk_tl
                    else tl,stk_tl
                | _ -> raise (Invalid_argument "eval")) 
  
           (* IF _____ ELSE _____ ENDIF *)
           | e::STRING("ELSE")::tl ->
               (match pile with 
                | BOOL(b)::stk_tl -> 
                    if b then aux_if tl (BOOL(false)::stk_tl) (instr@[e])
                    else aux_if tl (BOOL(true)::stk_tl) []
                | _ -> raise (Invalid_argument "eval"))
           | e::STRING("ENDIF")::tl -> 
               (match pile with
                | BOOL(b)::stk_tl -> 
                    if b then ((instr@[e])@tl), (List.rev stk_tl)
                    else (instr@tl), (List.rev stk_tl)
                | _ -> raise (Invalid_argument "eval")) 
  
           (* structures conditionnelles imbriquees *)
           | e::STRING("IF")::tl -> 
               (match pile with
                | BOOL(b)::stk_tl -> 
                    if b then 
                      let np, nstk= 
                        aux_if tl (eval dico (List.rev pile) (instr@[e])) [] 
                      in aux_if np nstk [] 
                    else 
                      let np, nstk=
                        aux_if tl pile [] 
                      in aux_if np pile []
                | _ -> raise (Invalid_argument "eval"))
                
            (* instructions conditionnelles *)      
           | e::tl -> (match pile with
               | BOOL(b)::stk_tl -> if b then aux_if tl pile (instr@[e]) 
                   else aux_if tl pile instr
               | _ -> raise (Invalid_argument "eval"))
              
           | _ -> raise (Invalid_argument "eval")
         in let np, nstk = aux_if tl stk [] in eval dico nstk np)
  
    (* element non defini *)
    | STRING(x) -> 
        let prog = (lookup x dico) in eval dico stk (prog@tl)
  
    (* autres *) 
    | x -> eval dico (step stk x) tl;;
    
let jeux_de_test = [ (": fact dup 1 > if dup 1 - fact * then ; 6 fact", "720") ]


(* *********** Question 6 *********** *) 


(** [carre n] renvoie la chaine de caracteres representant le programme 
    qui calcule le [n]-ieme terme de la suite de Fibonacci
*)
let fib n = 
  Printf.sprintf 
    ": FIB DUP 1 > IF 1 - DUP 1 - FIB SWAP FIB + THEN ; %d FIB" n;;
    
(** [fib n] renvoie la chaine de caracteres representant le programme 
    qui calcule le carre de [n]
*)
let carre (n:int) : string = 
  Printf.sprintf 
    ": CARRE DUP * ; %d CARRE" n
;;

(** [cube n] renvoie la chaine de caracteres representant le programme 
    qui calcule le cube de [n]
*)
let cube (n:int) : string =
  Printf.sprintf 
    ": CARRE DUP * ; 
    : CUBE DUP CARRE * ; %d CUBE " n
;; 

(** [fact n] renvoie la chaine de caracteres representant le programme 
    qui calcule la factorielle de [n]
*) 
let fact (n:int) : string =
  Printf.sprintf 
    ": FACT DUP 1 > IF DUP 1 - FACT * THEN ; %d FACT" n
;; 

(** [if_then_else n] renvoie la chaine de caracteres representant le programme 
    qui teste une instruction conditionnelle imbriquee a partir de [n]
*)
let if_then_else (n:int) : string =
  Printf.sprintf
    ": IFTHENELSE DUP 5 = IF 5 ELSE 4 = IF 0 ELSE 1 ENDIF DUP DROP ENDIF ; %d IFTHENELSE" n
;; 

(** [u n] renvoie la chaine de caracteres representant le programme 
    qui calcule [n]-ieme terme de la suite definie par u(0)=42 et
    u_{n+1} = 3 * u_{n} + 4
*)
let u (n:int) : string =
  Printf.sprintf
    ": U DUP 0 = IF 42 ELSE 1 - U 3 * 4 + ENDIF ; %d U SWAP DROP" n
;;

(** [sum_n n] renvoie la chaine de caracteres representant le programme 
    qui calcule la somme des [n] premiers entiers
*)
let sum_n (n:int) : string =
  Printf.sprintf
    ": SUM DUP 0 > IF DUP 1 - SUM + THEN ; %d SUM " n
;; 

(** [val_abs n] renvoie la chaine de caracteres representant le programme 
    qui calcule la valeur absolue de [n]
*)
let val_abs (n:int) : string =
  Printf.sprintf ": ABS DUP 0 < IF -1 * THEN ; %d ABS " n
;;

(** [f91 n] renvoie la chaine de caracteres representant le programme 
    qui calcule le [n]-ieme element de la fonction 91 de McCarthy
*)
let f91 (n:int) : string =
  Printf.sprintf 
    ": F91 DUP 100 > IF 10 - ELSE 11 + F91 F91 ENDIF ; %d F91" n 
;;

(** [syracuse n] renvoie la chaine de caracteres representant le programme 
    qui calcule le [n]-ieme terme de la suite de Syracuse
*)
let syracuse (n:int) : string =
  Printf.sprintf 
    ": SYRACUSE DUP 1 > IF DUP 2 mod 0 = IF 2 / SYRACUSE ELSE 3 * 1 + 
    SYRACUSE ENDIF THEN ; %d SYRACUSE" n 
;; 


(* *********** Question 7 *********** *)
(** Quelques exemples de tests *)
let jeux_de_test = 
  [ (fib 1,"1");(fib 5, "5");(fib 11, "89");
    (carre 5, "25");(carre 11, "121");(carre 0, "0");
    (cube 0, "0");(cube (-5), "-125");(cube 9, "729");
    (fact 6, "720");(fact 5, "120");(fact 1, "1");
    (if_then_else 5, "5");(if_then_else 4, "0");(if_then_else 3, "1");
    (u 0, "42");(u 1, "130");(u 7, "96226");
    (sum_n 0, "0");(sum_n 7, "28");(sum_n 39, "780");
    (val_abs (-4), "4");(val_abs 0, "0");(val_abs 11, "11");
    (syracuse 1, "1"); (syracuse 100, "1"); (syracuse 100, "1");
    (f91 100, "91");(f91 1, "91");(f91 110, "100")
  ] 
        
  
(** [tester s] renvoie le resultat associe a la chaine de caracteres [s]
    representant un programme 
*)
let tester (s:string) : string = 
  (fun p -> text (eval empty [] (parse p))) (s) 
    

(**
	 Module sat_solve.ml: vérificateur de satisfaisabilité pour une forme normale disjonctive.
	 Il s'agit d'une implantation naïve: on génère toutes les entrées d'une table de vérité pour les n variables du
	 problème.  Ensuite on évalue la forme normale pour chacune de ces entrées, si l'une d'elle génère la valeur true
	 alors le problème est satisfaisable, si toutes les entrées de la table génèrent false, alors le problème n'est
	 pas satisfaisable.
 *)


(**
	type proposition_atomique: il représente une variable booléenne.  Chaque variable est repérée par un numéro ou index
	qui normalement devrait être un entier naturel, et peut instancier la proposition sous jacente, ou sa négation.
*)
type proposition_atomique = Var of int | Notvar  of int ;;


(**
	eval_proposition : proposition_atomique -> bool -> bool
	Retourne la valeur de vérité d'une variable, ou proposition.  Si la proposition est vraie, alors retourne true dans 
	le cas ou la variable représente la proposition, et false dans le cas ou la variable représente la négation de la proposition.
*)
let eval_proposition (p: proposition_atomique) (truth_value: bool) : bool =
    match p with 
    | Var(_)    -> truth_value
    | Notvar(_) -> not truth_value ;;


(**
	numero_proposition : proposition_atomique -> int
	Retourne le numéro de variable de la proposition.
*)
let numero_proposition (p: proposition_atomique) : int =
	match p with Var(n) | Notvar(n) -> n ;;


(**
    type clause : il représente une clause d'une forme conjonctive normale.  Par-exemple la clause suivante:
    (x1 ou x2 ou non x3) sera représentée par la liste suivante: [Var(1); Var(2); Notvar(3)] 
*)
type clause = proposition_atomique list ;;


(**
    type instance_variables : représente une réalisation particulière de n variables booléennes.  Par-exemple,
    dans le cas ou on aurait: x1 = false, x2 = false, x3 = true, la représentation correspondante serait:
    [(1, true); (2, true), (3, false)]
*)
type instance_variables = (int * bool) list ;;



(**
	valeur_proposition_dans_instance : proposition_atomique -> instance_variables -> bool
	Retourne la valeur d'une proposition, ou variable booléenne, dans une réalisation particulière.
	La variable est repérée par son numéro, et ce numéro doit donc absolument être présent dans la 
	réalisation donnée en paramètre, sinon l'exception Not_found sera lancée.
*)
let valeur_proposition_dans_instance (p: proposition_atomique) (d: instance_variables) : bool = 
	List.assoc (numero_proposition p) d ;;


(**
	eval_clause : clause -> instance_variables -> bool
	Retourne la valeur d'une clause pour une réalisation donnée des variables.
	Les variables présentes dans la clause doivent absolument se retrouver dans la réalisation proposée.
*)
let rec eval_clause (c: clause) (d: instance_variables) : bool = 
	List.fold_left (fun acc e -> (eval_proposition e (valeur_proposition_dans_instance e d) || acc)) false c ;;



(**
	type probleme_sat : représente un problème de satisfaisabilité sous forme conjonctive normale.
	Se présente donc sous forme d'une liste de clause, le ET logique entre chaque clause est implicite.
*)
type probleme_sat = clause list ;;


(**
	eval_probleme_sat : probleme_sat -> instance_variables -> bool
	Retourne la valeur booléenne d'une forme conjonctive normale pour une réalisation donnée des variables.
*)
let rec eval_probleme_sat (p: probleme_sat) (d: instance_variables) : bool = 
	List.fold_left (fun acc e -> (eval_clause e d) && acc) true p ;;


(**
	lire_bit_a_position : int -> int -> bool 
	Retourne la valeur booléenne d'un bit à une position donnée dans la représentation binaire d'un entier.
	lire_bit_a_position i n retourne la valeur booléenne du bit à la position i pour l'entier n.  Ici la 
	position 0 représente le bit le moins significatif.
*)
let lire_bit_a_position (i: int) (n: int) : bool = not (Int.logand (Int.shift_left 1 i) n = 0) ;;


(**
	entier_vers_instance : int -> int -> instance_variables
	Transforme un nombre entier en une réalisation de variables booléennes, en transformant chaque bit du nombre 
	en valeur booléenne.  Chaque variable reçoit un numéro correspondant à sa position dans le nombre.
	Exemple: entier_vers_instance 5 6 va retourner une réalisation de 6 variables booléennes numérotées de 0 à 5.  
	Chacune de ces variables recevra la valeur true si le bit correspondant est 1 ou false autrement.  Pour 5 on 
	aura donc, considérant que 5 est représenté par 00101: [(0, true); (1, false); (2, true); (3, false); (4, false); (5, false)]
*)
let entier_vers_instance (n: int) (maxpos: int) : instance_variables = 
	let rec entier_vers_instance_interne (pos: int) (acc: instance_variables) : instance_variables = 
	    if (pos = maxpos) then acc
	    else entier_vers_instance_interne (pos+1) ((pos, lire_bit_a_position pos n)::acc)
	    
	in entier_vers_instance_interne 0 [] ;;


(**
	int_pow : int -> int -> int
	int_pow b e retourne b puissance e
*)
let rec int_pow (b: int) (e: int) : int =
    match e with
    | 0 -> 1
    | 1 -> b
    | n -> let c = int_pow b (e/2) in c * c * (if n mod 2 = 0 then 1 else b) ;;


(**
	generer_instances_pour_n_variables : n -> instance_variables list
	retourne la liste de toutes les réalisations possibles de n variables booléennes.
	n devrait évidemment être un entier naturel.
	On va donc générer une instance pour chacun des nombre entiers de 0 à (2**n) - 1, ce qui donnera
	toutes les combinaisons possibles de n variables.  Celles-ci seront numérotées de 0 à (n-1).
*)
let generer_instances_pour_n_variables (n: int) : instance_variables list = 
	let rec generer_instances_pour_n_variables_interne (i: int) (imax: int) (acc: instance_variables list) : instance_variables list =
	    if (i = imax) then acc 
	    else generer_instances_pour_n_variables_interne (i+1) imax ((entier_vers_instance i n)::acc) 
	in generer_instances_pour_n_variables_interne 0 (int_pow 2 n) [];; 


(**
	Compte le nombre de variables présentes dans une clause. (En-fait, retourne l'index maximal, on ne compte pas vraiment les variables...)
*)
let max_var_in_clause (c: clause) : int = 
	List.fold_left (fun a e -> max a (numero_proposition e)) 0 c ;; 
	
(**
	Compte le nombre de variables présentes dans une forme conjonctive normale.
	On ajoute 1 car les variables sont comptées à-partir de zéro.
*)
let max_var_in_probleme_sat (p: probleme_sat) : int = 
	1 + (List.fold_left (fun a e -> max a (max_var_in_clause e)) 0 p) ;;

(**
	est_satisfaisable : probleme_sat -> bool 
	Retourne true si une réalisation de variables booléennes permet à une forme conjonctive normale d'avoir la valeur true.
*)
let est_satisfaisable (p: probleme_sat) : bool = 
	List.exists (fun e -> eval_probleme_sat p e) (generer_instances_pour_n_variables (max_var_in_probleme_sat p)) ;;






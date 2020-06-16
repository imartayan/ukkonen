(* Construction d'un arbre suffixe *)
(* Igor Martayan *)

(* Pointeur vers un entier *)
type pointer =
  |Null
  |I of int
  |P of int ref;;

let print_pointer = function
  |Null -> print_string "null"
  |I n -> print_int n
  |P r -> print_int !r;;

let pointer_value = function
  |Null -> failwith "Null has no value"
  |I n -> n
  |P r -> !r;;

let freeze_pointer p = match p with
  |P r -> I !r
  |_ -> p;;

(* Caractère hachable *)
module HashChar = struct
  type t = char
  let equal x y = x == y
  let hash c = Char.code c
end;;

(* Table de hachage indexée par des caractères *)
module Dict = Hashtbl.Make(HashChar);;

(* Noeud de l'arbre suffixe *)
type node = {
  mutable start : int;
  mutable stop : pointer;
  mutable link : node;
  edges : node Dict.t
};;

let print_node_id n =
  if n.stop = Null then print_string "R"
  else (
    print_int n.start;
    print_char ':';
    print_pointer n.stop
  );;

let print_node n =
  let rec aux depth c n =
    for _ = 1 to depth - 1 do
      print_string "| "
    done;
    if depth > 0 then print_string "L ";
    print_char c; print_char ' ';
    print_node_id n; print_string " -> ";
    print_node_id n.link; print_newline ();
    Dict.iter (aux (depth+1)) n.edges
  in aux 0 '.' n;;

let freeze_node n =
  let rec aux c n =
    n.stop <- freeze_pointer n.stop;
    Dict.iter aux n.edges
  in aux '.' n;;

(* Table de hachage vide *)
let empty_node_dict () : node Dict.t = Dict.create 4;;

(* Nouveau noeud vide *)
let new_node start stop link = {
  start = start;
  stop = stop;
  link = link;
  edges = empty_node_dict ()
};;

(* Point actif pour les modifications *)
type active_point = {
  mutable node : node;
  mutable edge : char;
  mutable len : int
};;

(* Noeud suivant à partir du point actif *)
let next_node a = if a.len > 0 then
    Dict.find a.node.edges a.edge
  else failwith "pas de branche active";;

(* Longueur de la branche active *)
let edge_len a = let next = next_node a in
  (pointer_value next.stop) - next.start + 1;;

(* -------------------- *)
let debug = false;;

(* Algorithme d'Ukkonen *)
let ukkonen s =
  let s = s ^ "$"
  and n = String.length s in
  (* position dans la chaîne de caractères *)
  let pos = ref (-1) in
  (* noeud racine *)
  let rec root = {
    start = 0;
    stop = Null;
    link = root;
    edges = empty_node_dict ()
  } in
  (* dernier noeud modifié *)
  let last = ref root in
  (* point actif *)
  let a = {
    node = root;
    edge = '$';
    len = 0
  } in
  (* passage au noeud suivant si possible *)
  let move a =
    while a.len > 0 && a.len >= edge_len a do
      let next = next_node a in
      a.len <- a.len - edge_len a;
      a.edge <- s.[!pos - a.len];
      a.node <- next
    done in
  (* insertion de x dans l'arbre *)
  let rec insert x =
    (* Cas 1 : pas de branche active *)
    if a.len = 0 then (
      match Dict.find_opt a.node.edges x with
      (* Cas 1.a : choix d'une branche existante *)
      |Some n -> if debug then (
          print_string "cas 1.a"; print_newline ()
        );
        a.edge <- x;
        a.len <- 1;
        move a
      (* Cas 1.b : création d'une nouvelle branche *)
      |None -> if debug then (
          print_string "cas 1.b"; print_newline ()
        );
        let n = new_node !pos (P pos) root in
        Dict.add a.node.edges x n;
        (* /!\ égalité physique *)
        if !last != root then
          (* /!\ égalité physique *)
          if a.node != root then (
            (* propagation des changements *)
            a.node <- a.node.link;
            insert x
          )
    )
    (* Cas 2 : il y a une branche active *)
    else (
      let next = next_node a in
      let start = next.start in
      (* Cas 2.a : on peut avancer sur la branche *)
      if x = s.[start + a.len] then (
        if debug then (
          print_string "cas 2.a"; print_newline ()
        );
        a.len <- a.len + 1;
        move a
      )
      (* Cas 2.b : il faut séparer la branche en 2 *)
      else (
        if debug then (
          print_string "cas 2.b"; print_newline ()
        );
        let split = new_node start (I (start + a.len - 1)) root in
        next.start <- start + a.len;
        Dict.add split.edges s.[start + a.len] next;
        Dict.add split.edges x (new_node !pos (P pos) root);
        Dict.replace a.node.edges a.edge split;
        (* /!\ égalité physique *)
        if !last != root then
          (* nouveau lien suffixe *)
          !last.link <- split;
        last := split;
        (* propagation des changements *)
        (* /!\ égalité physique *)
        if a.node != root then (
          a.node <- a.node.link;
          move a;
          insert x
        )
        else (
          a.edge <- s.[start + 1];
          a.len <- a.len - 1;
          move a;
          insert x
        )
      )
    ) in
  (* boucle principale *)
  for i = 0 to n do
    incr pos;
    last := root;
    if debug then (
      print_string "----------"; print_newline ();
      if a.node != root then (
        print_string "a.node: "; print_node_id a.node;print_newline ();
      );
      if a.len > 0 then (
        print_string "a.edge: "; print_char a.edge; print_newline ();
        print_string "a.len: "; print_int a.len; print_newline ();
      );
      print_string "new letter: "; print_char s.[i]; print_newline ();
    );
    insert s.[i];
    if debug then print_node root;
  done;
  freeze_node root;
  root;;

(* -------------------- *)

let s1 = "panama_canal";;
let s2 = "abcabxabcd";;
let s3 = "abadefgdehiadefx";;

(* print_node (ukkonen s2);; *)

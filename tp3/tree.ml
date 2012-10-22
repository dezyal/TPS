type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Fonction tests*)
let rec gentree = function
  | 0 -> Empty
  | n ->
      Node(
        gentree (n - 1),
        Random.int 100,
        gentree (n - 1)
      )
 
let treeprint t =
  let rec aux = function
    | Empty -> Format.printf "@;@[<h>Empty@]"
    | Node(ls, k, rs) ->
        Format.printf "@;@[<b>Node(@[<b>";
        aux ls;
        Format.printf "@;%d," k;
        aux rs;
        Format.printf "@]@,)@]"
  in
    Format.printf "@[<b>";
    aux t;
    Format.printf "@]@."

(* Mesures *)
let rec size = function
    Empty -> 0
  | Node(Empty,_,Empty) -> 1
  | Node (l,_,r) -> size l + size r + 1

let rec height = function
  | Empty -> -1
  | Node(l,_,r) -> 1 + (max (height l) (height r))


(* Traductions vers les listes *)
let rec append l1 l2 = match l1 with
    [] -> l2
  | e::l -> e::(append l l2)

let prefix tree = 
  let rec prefix_rec l = function
    | Empty -> l
    | Node(Empty,k,Empty) -> k::l
    | Node(ls,k,rs) ->  k::append (prefix_rec l ls) (prefix_rec l rs)
  in prefix_rec [] tree

let rec infix = function
  | Empty -> []
  | Node(ls,k,rs) -> append (append (infix k) [e]) (infix rs)

let rec suffix = function
  | Empty -> []
  | Node(ls,k,rs) -> append (suffix ls) (append (suffix rs) [k])

(* MAP et ITER *)
let rec map f = function
    [] -> []
  | e::l -> f e :: map f l

let rec iter (f: 'a -> unit) = function
    [] -> ()
  | e::l -> f e; iter f l

let _ = treeprint (gentree 4)

let l =
[
    ("feuille","leaf");
    ("arbre","tree");
    ("noeud","node");
    ("sommet","vertice");
    ("arc","edge");
  ]
 
let _ =
  try
    Printf.printf "%s: %s\n" "fleur" (List.assoc "fleur" l)
  with Not_found -> Printf.printf "Pas de traduction …\n"

let list_assoc e l =
  let b = List.assoc e l in b

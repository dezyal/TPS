let print_int_list l =
  Printf.printf "[\n  ";
  List.iter (Printf.printf "%d; ") l;
  Printf.printf "\n]\n\n%!"
 
let rec gen_random_list range n = 
  let rec aux a = function
    | 0 -> a
    | n -> aux (Random.int range :: a) (n-1)
  in aux [] n
 
let init () =
  if Array.length Sys.argv < 2 then
    begin
      Printf.eprintf "Pas assez d'argument !\n%!";
      exit 1
    end;
  if Array.length Sys.argv < 3 then
    Random.self_init ()
  else
    Random.init (int_of_string Sys.argv.(2));
  int_of_string (Sys.argv.(1))
 
let main () =
  let size = init () in
  let l = gen_random_list 64 size in
  let v = Vector.make (2 * size) 0 in
    begin
      print_int_list l;
      List.iter (fun x -> Vector.add_last x v) l;
      Vector.print_int_vect v;
      let v2 = Vector.map (fun x -> x mod 16) v in
	Vector.print_int_vect v2;
	Vector.inplace_map (fun x -> x mod 32) v;
	Vector.print_int_vect v;
	if (not (Vector.is_sorted v)) then
	  begin
	    Printf.printf "sorting ...\n";
	    Vector.sort compare v;
	  end;
	Vector.print_int_vect v;
	exit 0;
    end
 
let _ = main ()

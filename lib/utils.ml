open Core

let int_of_bool (x : bool) : int = match x with | true -> 1 | false -> 0

let count_of ~f (items : 'a list) : int =
  List.fold ~init:0 ~f:(fun acc x -> acc + int_of_bool (f x)) items  

let unzip (items : (int * int) list) : (int list * int list) =
  List.fold_right ~init:([],[]) ~f:(fun (a,b) (acca, accb) -> (a::acca, b::accb)) items
 
let print_strings items =
  print_string "[";
  List.iter ~f:(fun x -> print_string ("\"" ^ x ^ "\";")) items;
  print_string "]\n"

let print_ints items =
  print_string "[";
  List.iter ~f:(fun x -> print_string ((string_of_int x) ^ ";")) items;
  print_string "]\n"

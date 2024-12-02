open Core

let tuple2_of_list (items : 'a list) : ('a * 'a) = 
  match items with 
    | a::b :: _ -> (a,b)
    | _ -> failwith("should be a list with two elements")

let parse_line (str : string) : (int * int) = 
  Str.split (Str.regexp "[ ]+") str |> (List.map ~f:int_of_string) |> tuple2_of_list 

let read_file (filename : string) : (int list * int list) =
  In_channel.read_lines filename |> List.map ~f:parse_line |> Utils.unzip

let calc_distance (one: int list) (two: int list) : int =
  match List.fold2 ~init:0 ~f:(fun acc a b -> acc + abs(a - b)) one two with
    | Ok a -> a
    | _ -> 0

let calc_similarity (one: int list) (two: int list) : int =
  let count x items = Utils.count_of ~f:(fun a -> a = x) items in
  List.fold ~init:0 ~f:(fun acc a -> acc + a * (count a two)) one

let exec filename = 
  let (left, right) = read_file filename in
  let sort items = List.sort ~compare:compare items in
  printf "%d\n" (calc_distance (sort left) (sort right));
  printf "%d\n" (calc_similarity (left) (right))

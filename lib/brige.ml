open Core

let read_file filename =
  let re = Re.Pcre.regexp {|([\d]+): ([\d ]+)|} in
  let int_list_of_string line = String.split_on_chars ~on:[' '] line |> List.map ~f:int_of_string in
  let parse_line line = match Re.exec_opt re line with 
    | Some group -> Some (int_of_string (Re.Group.get group 1), int_list_of_string (Re.Group.get group 2))
    | None -> None
  in
  In_channel.read_lines filename |> List.filter_map ~f:parse_line

let rec permutations (n:int) (lst:'a list) : 'a list list = 
  let append x items = List.map ~f:(fun xs -> x::xs) items in  
  match n with
  | 0 -> []
  | 1 -> List.map ~f:(fun x -> [x]) lst
  | _ -> List.map ~f:(fun v -> append v (permutations (n - 1) lst)) lst |> List.concat

let rec equation (terms : int list) (ops : string list) : string =
  match terms with
    | [] -> failwith "Not enought terms"
    | x::[] -> string_of_int x 
    | x::xs -> match ops with
      | [] -> failwith "Not enought ops!"
      | op::rest -> (string_of_int x) ^ op ^ (equation xs rest)

let apply_ops (terms : int list) (ops : string list) : int =
  let combine a b = int_of_string ((string_of_int a) ^ (string_of_int b)) in
  match terms with
    | [] -> failwith "Not enought terms"
    | x::[] -> x
    | x::xs -> List.fold ~init:(x,ops) ~f:(
      fun (acc,ops) v -> match ops with
        | [] -> failwith "Not enought ops!"
        | "+"::rest -> (acc + v, rest)
        | "*"::rest -> (acc * v, rest)
        | "||"::rest -> ((combine acc v), rest)
        | _ -> failwith "Unknown operation"
    ) xs |> fst

let check value terms operators : bool =
  let ops_list = permutations (pred (List.length terms)) operators in
  List.exists ~f:(fun ops -> (apply_ops terms ops) = value) ops_list

let sum_of_correct lines operators = 
  List.filter ~f:(fun (value, terms) -> check value terms operators) lines |> List.fold ~init:0 ~f:(fun acc a -> acc + (fst a)) 

let exec filename = 
  let lines = read_file filename in 
  let first = sum_of_correct lines ["+";"*"] in
  let second = sum_of_correct lines ["+";"*";"||"] in
  printf "%d\n%d\n" first second;
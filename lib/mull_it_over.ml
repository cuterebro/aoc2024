
open Core

type cmd = Mul of (int * int) | Do | Dont

let decode group : cmd =
  let int_of_group index = Re.Group.get group index |> int_of_string in 
  match Re.Group.all group |> Array.to_list with
    | _ :: "mul" :: _ -> Mul (int_of_group 2, int_of_group 3)
    | "don't" :: _ -> Dont
    | "do" :: _ -> Do
    | _ -> raise (Invalid_argument "Unknown command")

let parse_code source = 
  let re = Re.Pcre.regexp ~flags:[`MULTILINE] "(mul)\\(([\\d]+),([\\d]+)\\)|(do(n't)?)" in
  List.map ~f:decode (Re.all re source)

let read_file filename =
  let raw = In_channel.read_all filename in 
  parse_code raw

let execute_muls code = 
  let value (x : cmd) : int = match x with Mul(a,b) -> a * b | _ -> 0 in
  List.fold ~init:0 ~f:(fun acc a -> acc + (value a)) code

let execute_all code = 
  let step (sum, flag) c = 
    match (c, flag) with
      (Mul (a, b), true) -> (sum + a*b, flag)
      | (Mul _, _) -> (sum, flag)
      | (Do, _) -> (sum, true)
      | (Dont, _) -> (sum, false)
  in
  let (sum, _) = List.fold ~init:(0,true) ~f:step code in sum     

let exec filename = 
  let code = read_file filename in
  printf "%d\n" (execute_muls code);
  printf "%d\n" (execute_all code);


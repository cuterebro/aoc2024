
open Core
module IntMap = Stdlib.Map.Make(Int)
module IntSet = Stdlib.Set.Make(Int)

let build_rules rules =
  let keys = (List.map ~f:List.hd_exn rules) |> IntSet.of_list in
  let pages key = (List.fold ~init:[] ~f:(
    fun acc rule -> match rule with
      | k::v::_ when k = key -> v::acc
      | _ -> acc
    ) rules) |> IntSet.of_list in
  IntSet.to_list keys |> List.map ~f:(fun key -> (key, pages key)) |> IntMap.of_list

let read_file filename = 
  let lines = In_channel.read_lines filename in
  let split_to_ints (line:string) (sep:char) : int list = String.split_on_chars ~on:[sep] line |> List.map ~f:int_of_string in
  let (rules, updates, _) = List.fold ~init:([], [], false) ~f:(fun (r,u,flag) line -> 
    match line with
      | "" -> (r,u,true)
      | _ -> match flag with
        | false -> ((split_to_ints line '|') :: r, u, flag)
        | true -> (r, (split_to_ints line ',') :: u, flag)
  ) lines in
  ((build_rules rules), updates)

let middle_of lst =
  let rec aux a b : int = 
  match b with
    | _::[] | [] -> List.hd_exn a
    | _::_::xs -> aux (List.tl_exn a) xs
  in aux lst lst

let is_correct rules update =
  let check_prev prev cur = 
    match IntMap.find_opt cur rules with
    | Some rule -> IntSet.is_empty (IntSet.inter prev rule)
    | None -> true
  in 
  let rec aux prev next = 
    match next with
    | [] -> true
    | x::xs -> (check_prev prev x) && aux (IntSet.add x prev) xs
  in aux IntSet.empty update

let fix_update rules update =
  let cmp (a:int) (b:int) : int = 
    match IntMap.find_opt b rules with
      | Some rule -> if IntSet.mem a rule then 1 else 0
      | None -> 0
  in
  List.stable_sort ~compare:cmp update

let exec (filename : string) : unit =
  let (rules, updates) = read_file filename in
(*   IntMap.iter (fun k v -> printf "%d -> " k; Utils.print_ints (IntSet.to_list v)) rules;
  List.iter ~f:Utils.print_ints updates;
 *) 
  let first_result = List.filter ~f:(is_correct rules) updates |> List.map ~f:middle_of |> List.fold ~init:0 ~f:(+) in
  let second_result = List.filter ~f:(fun u -> not (is_correct rules u)) updates |> List.map ~f:(fix_update rules) |> List.map ~f:middle_of |> List.fold ~init:0 ~f:(+) in
  printf "%d\n" first_result;
  printf "%d\n" second_result;

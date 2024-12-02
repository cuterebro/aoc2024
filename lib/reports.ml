open Core

let read_file filename : int list list =
  let parse_line line = List.map ~f:int_of_string (String.split_on_chars ~on:[' '] line) in
  In_channel.read_lines filename |> List.map ~f:parse_line

let correct_step prev sign curr =
  let diff = curr - prev in
  let abs_diff = abs(diff) in
  sign * diff >= 0 && abs_diff >= 1 && abs_diff <= 3

let is_correct (items : int list) : bool =
  match items with
   | x::xs -> 
      let (_, _, res) = List.fold ~init:(x, 0, true) ~f:(fun (x,d,r) a -> (a, a - x, r && (correct_step x d a))) xs in
      res
   | _ -> false

let rec expand_list (items : int list) (head : int list) : int list list =
  match items with
    | x::xs -> (head @ xs) :: (expand_list xs (head @ [x]))
    | _ -> []

let is_almost_correct (items: int list) : bool =
  (is_correct items) || (List.exists ~f:is_correct (expand_list items []))

let exec filename =
  let reports = read_file filename in
  let correct_count = Utils.count_of ~f:is_correct reports in
  let second_correct_count = Utils.count_of ~f:is_almost_correct reports in
  printf "%d\n" correct_count;
  printf "%d\n" second_correct_count


open Core

let count_in_string (pattern:string) (str:string) : int = 
  let pattern_len = String.length pattern in
  let substr (index:int):int = if String.equal (String.sub str ~pos:index ~len:pattern_len) pattern then 1 else 0 in 
  let rec aux (index:int):int = if index + pattern_len > (String.length str) then 0 else (aux (index + 1)) + substr index
  in aux 0

let sum_count (pattern:string) (lines:string array) : int =
  Array.fold ~init:0 ~f:(fun acc line -> acc + (count_in_string pattern line)) lines

let string_reverse str = 
  let len = String.length str in
  String.init len ~f:(fun i -> String.get str (len - i - 1))

let reverse (lines:string array) : string array = 
  Array.map ~f:string_reverse lines

let transpose (lines:string array) : string array =
  let len = Array.length lines in
  Array.init len ~f:(fun i -> String.init len ~f:(fun j -> String.get lines.(j) i))

let diagonale (lines:string array) : string array =
  let len = Array.length lines in
  let height = 1 + (2 * (len - 1)) in
  let width i = min (1 + i) (height - i) in
  let start_i i = min i (len - 1) in
  let start_j i = max 0 (i - len + 1) in
  let char_at i j = lines.((start_i i) - j).[(start_j i) + j] in
  Array.init height ~f:(fun i -> String.init (width i) ~f:(fun j -> char_at i j))

let count_pattern pattern lines = 
  let count lines = (sum_count pattern lines) + (sum_count pattern (reverse lines)) in
  (count lines) + (transpose lines |> count) + (diagonale lines |> count) + (reverse lines |> diagonale |> count)

let count_crosses lines = 
  let len = Array.length lines in
  let slash i j = String.init 3 ~f:(fun x -> lines.(i - 1 + x).[j - 1 + x]) in
  let backslash i j = String.init 3 ~f:(fun x -> lines.(i + 1 - x).[j - 1 + x]) in
  let valid i j = 
    let good s = String.equal s "MAS" || String.equal s "SAM" in
    let pos i = i > 0 && i < len - 1 in
    Utils.int_of_bool ((pos i) && (pos j) && good (slash i j) && good (backslash i j))
  in
  let range = Array.init len ~f:succ in
  Array.fold ~init:0 ~f:(fun acc i -> 
    acc + (Array.fold ~init:0 ~f:(fun acc j -> acc + valid i j) range)
  ) range

let read_file filename =
  In_channel.read_lines filename |> Array.of_list

let exec filename = 
  let lines = read_file filename in
  let first = count_pattern "XMAS" lines in
  let second = count_crosses lines in
  printf "%d\n" first;
  printf "%d\n" second;

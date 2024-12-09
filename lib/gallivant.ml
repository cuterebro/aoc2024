open Core

type journey_result = Finished of int | Looped

let print_field field = 
  Array.iter ~f:(fun row -> print_endline (Bytes.to_string row)) field

let field_copy field = Array.map ~f:Bytes.copy field

let print_result res : unit = match res with
  | Finished count -> printf "Finished in %d\n" count
  | Looped -> print_endline "Looped"

let start_position lines : (int * int) = 
  let aux line = match Stdlib.Bytes.index_opt line '^' with Some x -> x | None -> -1 in
  let res : int array = Array.map ~f:aux lines in
  let row = match Stdlib.Array.find_index ((<) 0) res with Some x -> x | None -> failwith "Start position not found" in
  let col = Array.get res row in
  (row, col)

let read_file filename =
  let lines = In_channel.read_lines filename in
  let field = List.map ~f:Bytes.of_string lines |> Array.of_list in
  (field, (start_position field))

let rotate dir = match dir with
  | (-1, 0) -> (0, 1)
  | (0, 1) -> (1, 0)
  | (1, 0) -> (0, -1)
  | (0, -1) -> (-1, 0)
  | _ -> failwith "Wrong direction"

let char_of_dir dir = match dir with   
  | (-1, 0) -> 'w'
  | (0, 1) -> 'd'
  | (1, 0) -> 's'
  | (0, -1) -> 'a'
  | _ -> 'X'

let get_char (field : bytes array) pos : char = let (row, col) = pos in Bytes.get field.(row) col  
let set_char (field : bytes array) pos (c : char) : unit = let (row, col) = pos in Bytes.set field.(row) col c
let next pos dir = 
  let (row, col) = pos in
  let (dr, dc) = dir in (row + dr, col + dc)

let journey field position : journey_result =
  let rows = Array.length field in
  let cols = Bytes.length (Array.get field 0) in
  let finished pos = let (row, col) = pos in row < 0 || col < 0 || row >= rows || col >= cols in
  let is_obstacle pos = phys_equal (get_char field pos) '#' in
  let dejavu pos trace = phys_equal (get_char field pos) trace in
  let visit pos trace = 
    let count = if String.contains "wdsaX" (get_char field pos) then 0 else 1 in set_char field pos trace; count
  in
  let rec step pos dir count : journey_result =
    let trace = char_of_dir dir in
    let next_pos = next pos dir in
    match finished next_pos with
      | true -> Finished count
      | false -> match dejavu pos trace with
        | true -> Looped
        | false -> match is_obstacle next_pos with
          | true -> step pos (rotate dir) count
          | false -> step next_pos dir (count + (visit pos trace))
   in step position (-1, 0) 1

let count_loops field start : int =
  let points = Array.mapi ~f:(fun i row ->
    (List.init (Bytes.length row) ~f:(fun x -> x)) |> List.filter ~f:(fun j -> phys_equal (Bytes.get row j) '.') |> List.map ~f:(fun j -> (i, j))
  ) field |> Array.to_list |> List.concat in
  List.fold ~init:0 ~f:(fun acc pos -> 
    let test_field = field_copy field in
    set_char test_field pos '#';
    match journey test_field start with Finished _ -> acc | Looped -> acc + 1
  ) points

let exec filename =
  let (field, position) = read_file filename in
  let first_result = journey (field_copy field) position in
  let second_result = count_loops field position in
    print_result first_result;
    printf "%d\n" second_result;

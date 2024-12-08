open Core

type journey_result = Finished of int | Looped

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

let get_char (field : bytes array) pos : char = let (row, col) = pos in Bytes.get field.(row) col  
let set_char (field : bytes array) pos (c : char) : unit = let (row, col) = pos in Bytes.set field.(row) col c
let next pos dir = 
  let (row, col) = pos in
  let (dr, dc) = dir in (row + dr, col + dc)

let journey field position : journey_result =
  let rows = Array.length field in
  let cols = Bytes.length (Array.get field 0) in
  let finished pos = let (row, col) = pos in row < 0 || col < 0 || row >= rows || col >= cols in
  let visit pos = 
    let count = if phys_equal (get_char field pos) 'X' then 0 else 1 in set_char field pos 'X'; count
  in
  let is_obstacle pos = phys_equal (get_char field pos) '#' in
  let rec step pos dir count : int =
    let next_pos = next pos dir in
    match finished next_pos with
      | true -> count
      | false -> match is_obstacle next_pos with
        | true -> step pos (rotate dir) count
        | false -> step next_pos dir count + (visit pos)
   in Finished (step position (-1, 0) 1)

let print_field field = 
  Array.iter ~f:(fun row -> print_endline (Bytes.to_string row)) field

let exec filename =
  let (field, position) = read_file filename in
  let first_result = journey (Array.copy field) position in
  printf "%d\n" first_result
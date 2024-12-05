open Core

let read_file filename = 
  let lines = In_channel.read_lines filename in
  let (rules, updates, _) = List.fold ~init:([], [], false) ~f:(fun (r,u,flag) line -> )

let exec filename =
  let (rules, updates) = read_file filename in  
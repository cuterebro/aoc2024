
open Aoc
open Core

type task = {title: string; run: unit -> unit}

let last arr = arr.((Array.length arr) - 1)

let () =
  let (tasks : task array) = [|
    {title = "Day 1. Historian Hysteria:"; run = (fun () -> Histeria.exec "inputs/input01a.txt")};
    {title = "Day 2. Red-Nosed Reports:";  run = (fun () -> Reports.exec "inputs/input02a.txt")};
    {title = "Day 3. Mull It Over:";       run = (fun () -> Mull_it_over.exec "inputs/input03a.txt")};
    {title = "Day 4. Ceres Search:";       run = (fun () -> Ceres.exec "inputs/input04a.txt")};
    {title = "Day 5. Print Queue:";        run = (fun () -> Print_queue.exec "inputs/input05a.txt")};
  |] in
  let exec task = print_endline task.title; task.run() in

  let args = Sys.get_argv () in 
  match last args  with
  | "all" -> Array.iter ~f:exec tasks
  | "last" -> exec (last tasks)
  | _ -> print_endline "Use 'all' to run all tasks and 'last' to run the last task"

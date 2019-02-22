open ExtPervasives
module Log = (val Logger.create "main" : Logs.LOG)

let get_problems () =
  Sys.readdir !Config.problems
  |> Array.to_list
  |> List.filter (fun file -> file <> "" && file.[0] <> '.')
  |> List.sort compare
  |> List.map (Filename.concat !Config.problems ||> Problem.from_file)

let create_solutions_directories problems =
  let mkdir dir =
    if not (Sys.file_exists dir && Sys.is_directory dir) then
      Unix.mkdir dir 0o755
  in
  mkdir !Config.solutions;
  List.iter
    (fun problem ->
       mkdir (Filename.concat !Config.solutions (Problem.name problem)))
    problems

type process_status = Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
[@@deriving show]

let run_solver problem (name, solver) =
  match Lwt_unix.fork () with
  | 0 ->
    Log.info (fun m -> m "Trying solver %s on problem %s" name (Problem.name problem));
    let solution = solver problem in
    let score = Solution.score solution in
    Log.info (fun m -> m "Solver %s got score %d" name score);
    let file = spf "%09d_%s" score (datetime ()) in
    let file = Filename.(concat !Config.solutions (concat (Problem.name problem) file)) in
    Solution.to_file file solution;
    exit 0
  | pid ->
    Log.info (fun m -> m "Running solver %s on problem %s as pid %d"
                 name (Problem.name problem) pid);
    Lwt_unix.waitpid [] pid >>= fun (_, status) ->
    Log.info (fun m -> m "Solver %s (%d) ended with status %a" name pid pp_process_status status);
    Lwt.return ()

let solve_problem problem =
  Log.info (fun m -> m "Starting on problem %s" (Problem.name problem));
  Lwt_stream.iter_n
    ~max_concurrency:!Config.workers
    (run_solver problem)
    (lwt_stream_of_seq Solvers.all)
  >>= fun () ->
  Log.info (fun m -> m "I did what I could on %s" (Problem.name problem));
  Lwt.return ()

let () =
  Log.info (fun m -> m "Starting up");
  Config.parse_command_line ();
  let problems = get_problems () in
  create_solutions_directories problems;
  Lwt_main.run (Lwt_list.iter_s solve_problem problems);
  Log.info (fun m -> m "The end. Check out the directory %s" !Config.solutions)

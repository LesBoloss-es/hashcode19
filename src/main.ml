open ExtPervasives
module Log = (val Logger.create "main" : Logs.LOG)

let get_problems () =
  Sys.readdir !Config.problems
  |> Array.to_list
  |> List.filter (fun file -> file <> "" && file.[0] <> '.')
  |> List.sort compare
  |> List.map
    (fun problem_name ->
       Problem.from_file
         ~problem_name
         (Filename.concat !Config.problems problem_name))

type process_status = Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
[@@deriving show]

let write_solution_if_better ~problem ~solver_name solution =
  let problem_name = Problem.name problem in
  let problem_file ext =
    Filename.concat !Config.solutions (problem_name ^ ext)
  in
  let lock () =
    try
      let ochan = open_out_gen [Open_creat; Open_excl] 0o644 (problem_file ".lock") in
      close_out ochan; true
    with
      Sys_error _ -> false
  in
  let unlock () =
    Sys.remove (problem_file ".lock")
  in
  let read_score () =
    try
      let ichan = open_in (problem_file ".score") in
      let score = int_of_string (input_line ichan) in
      close_in ichan;
      score
    with
    | Sys_error _ -> 0
    | End_of_file ->
      Log.warn (fun m -> m "Got end of file while reading %s" (problem_file ".score"));
      0
    | Failure _ ->
      Log.warn (fun m -> m "%s doesn't contain an integer" (problem_file ".score"));
      0
  in
  let write_score score =
    let ochan = open_out (problem_file ".score") in
    output_string ochan ((string_of_int score) ^ "\n");
    close_out ochan
  in
  let write_solver_name () =
    let ochan = open_out (problem_file ".solver") in
    output_string ochan (solver_name ^ "\n");
    close_out ochan
  in
  let add_solver_name () =
    let ochan = open_out_gen [Open_append] 0o644 (problem_file ".solver") in
    output_string ochan (solver_name ^ "\n");
    close_out ochan
  in
  let write_solution () =
    Solution.to_file (problem_file "") solution
  in
  let rec write_solution_if_better () =
    if lock () then
      (
        let score = Solution.score problem solution in
        let score' = read_score () in
        if score > score' then
          (
            write_score score;
            write_solver_name ();
            write_solution ();
          )
        else if score = score' then
          add_solver_name ();
        unlock ()
      )
    else
      write_solution_if_better ()
  in
  write_solution_if_better ()

let run_solver_on_problem (problem, (name, solver)) =
  match Lwt_unix.fork () with
  | 0 ->
    (* On the forkee's side, we run the solver on the problem. We then compute
       the score of the obtained solution and look in the solutions directory to
       see if that is an improvement. If yes, we write our solution. *)
    let solution = solver problem in
    write_solution_if_better ~problem ~solver_name:name solution;
    exit 0

  | pid ->
    (* On the forker's side, we log a bit and wait for the forkee to terminate.
       We check its return status and that's it. *)
    Log.info (fun m -> m "Solving %s/%s[%d]"
                 (Problem.name problem) name pid);
    Lwt_unix.waitpid [] pid >>= fun (_, status) ->
    if status = WEXITED 0 then
      Log.info (fun m -> m "%s/%s[%d] stopped with success."
                   (Problem.name problem) name pid)
    else
      Log.warn (fun m -> m "%s/%s[%d] stopped with status: %a"
                   (Problem.name problem) name pid pp_process_status status);
    Lwt.return ()

let () =
  Log.info (fun m -> m "Starting up. Parsing command line.");
  Config.parse_command_line ();
  Log.info (fun m -> m "Getting problems.");
  let problems = get_problems () in
  Log.info (fun m -> m "Found %d problems. Starting solvers."
               (List.length problems));

  Solvers.tasks problems
  |> lwt_stream_of_seq
  |> Lwt_stream.iter_n
    ~max_concurrency:!Config.workers
    run_solver_on_problem
  |> Lwt_main.run;

  Log.info (fun m -> m "The end. See you! :-)")

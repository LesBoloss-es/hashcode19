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

let run_solver_on_problem (problem, (name, solver)) =
  match Lwt_unix.fork () with
  | 0 ->
    (* On the forkee's side, we run the solver on the problem. We then compute
       the score of the obtained solution and look in the solutions directory to
       see if that is an improvement. If yes, we write our solution. *)
    let solution = solver (Problem.copy problem) in
    Io.Solution.write_if_better ~problem ~solver_name:name solution;
    exit 0

  | pid ->
    (* On the forker's side, we log a bit and wait for the forkee to terminate.
       We check its return status and that's it. *)
    Log.debug (fun m -> m "Solving %s/%s[%d]"
                 (Problem.name problem) name pid);
    Lwt_unix.waitpid [] pid >>= fun (_, status) ->
    if status = WEXITED 0 then
      Log.debug (fun m -> m "%s/%s[%d] stopped with success."
                   (Problem.name problem) name pid)
    else
      Log.warn (fun m -> m "%s/%s[%d] stopped with status: %a"
                   (Problem.name problem) name pid pp_process_status status);
    Lwt.return ()

let log_total_score problems =
  let open Io.Solution in
  let total_score =
    problems
    |> List.map
      (fun problem ->
         let name = Problem.name problem in
         with_lock ~name @@ fun () ->
         read_score ~name)
    |> List.fold_left (+) 0
  in
  Log.info (fun m -> m "Total score: %d" total_score)

let rec log_total_score_every ~time problems =
  Lwt_unix.sleep time >>= fun () ->
  log_total_score problems;
  log_total_score_every ~time problems

let () = Log.info (fun m -> m "Starting up.")

let () = Sys.(set_signal sigint (Signal_handle (fun _ ->
    Log.info (fun m -> m "Received SIGINT (^C). Stopping gracefully.");
    Config.stop := true)))

let () =
  let seed = 135801055 in
  Log.debug (fun m -> m "Setting seed to %d." seed);
  Random.init seed

let start_time = Unix.gettimeofday ()

let () =
  Log.debug (fun m -> m "Parsing command line.");
  Config.parse_command_line ();
  Logger.set_level !Config.loglevel

let problems =
  Log.debug (fun m -> m "Getting problems.");
  let problems = get_problems () in
  Log.info (fun m -> m "Found %d problems." (List.length problems));
  problems

let () = Lwt.async (fun () -> log_total_score_every ~time:5. problems)

let () =
  Log.info (fun m -> m "Starting solvers.");
  Solvers.tasks problems
  |> ExtSeq.to_lwt_stream
  |> Lwt_stream.iter_n
    ~max_concurrency:!Config.workers
    run_solver_on_problem
  |> Lwt_main.run;

  log_total_score problems;
  Log.info (fun m -> m "Total time: %.2fs."
               (Unix.gettimeofday () -. start_time));
  Log.info (fun m -> m "The end. See you! :-)")

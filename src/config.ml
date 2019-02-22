open ExtPervasives

let loglevel = ref Logs.Debug
let workers = ref 8
let problems = ref "problems"
let solutions = ref "solutions"

let specs =
  let open Arg in
  align [
    "--problems",  Set_string problems, spf "DIR Sets the problems directory to DIR (default: %s)" !problems ;
    "--solutions", Set_string problems, spf "DIR Sets the solutions directory to DIR (default: %s)" !solutions ;
    "--workers",   Set_int workers,     spf "NB Sets the number of workers to NB (default: %d)" !workers ;
  ]

let handle_file _ = raise (Arg.Bad "")
let usage = (spf "Usage: %s [--problems DIR] [--solutions DIR] [--workers NB]" Sys.argv.(0))

let parse_command_line () =
  Arg.parse specs handle_file usage

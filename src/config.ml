open ExtPervasives

let loglevel = ref Logs.Debug
let problems = ref "problems"
let solutions = ref "solutions"
let strict = ref true
let workers = ref 8

let set_level level =
  match Logs.level_of_string level with
  | Ok (Some level) -> loglevel := level
  | _ -> raise (Arg.Bad "bad log level")

let set_strict = function
  | "yes" -> strict := true
  | "no" -> strict := false
  | _ -> raise (Arg.Bad "bad strict")

let specs =
  let open Arg in
  align [
    "--log-level", String set_level,     spf "LEVEL Sets log level to LEVEL (default: %s)" (Logs.level_to_string (Some !loglevel)) ;
    "--problems",  Set_string problems,  spf "DIR Sets the problems directory to DIR (default: %s)" !problems ;
    "--solutions", Set_string solutions, spf "DIR Sets the solutions directory to DIR (default: %s)" !solutions ;
    "--strict",    String set_strict,    spf "yes/no Fail on first error" ;
    "--workers",   Set_int workers,      spf "NB Sets the number of workers to NB (default: %d)" !workers ;
  ]

let handle_file _ = raise (Arg.Bad "")
let usage = (spf "Usage: %s [--problems DIR] [--solutions DIR] [--workers NB]" Sys.argv.(0))

let parse_command_line () =
  Arg.parse specs handle_file usage

let stop = ref false

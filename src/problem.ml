(* open ExtPervasives *)
module Log = (val Logger.create "problem" : Logs.LOG)

type t = {
  name : string ;
}

let from_file (file : string) : t =
  Log.info (fun m -> m "Parsing %s" file);
  let ichan = open_in file in
  let rec parse_problem () =
    try
      let _line = input_line ichan in
      (assert false : unit); (* FIXME *)
      parse_problem ()
    with
      End_of_file -> ()
  in
  let _ = parse_problem () in (* FIXME *)
  close_in ichan;
  { name = file } (* FIXME *)

let name problem = problem.name

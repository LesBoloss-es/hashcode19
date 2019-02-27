(* open ExtPervasives *)
module Log = (val Logger.create "problem" : Logs.LOG)

type t = (* FIXME *)
  { name : string }
[@@deriving show]

let from_file ~problem_name (filename : string) : t =
  let ichan = open_in filename in
  ignore ichan;
  ignore problem_name;
  ignore filename;
  assert false (* FIXME *)

let name problem = problem.name

let example =
  { name = "example" } (* FIXME *)

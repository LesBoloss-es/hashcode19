(* open ExtPervasives *)
module Log = (val Logger.create "problem" : Logs.LOG)

type t = (* FIXME *)
  { name : string }
[@@deriving show]

let from_channel ~problem_name (ichan : in_channel) : t =
  ignore ichan;
  ignore problem_name;
  assert false

let from_file ~problem_name (filename : string) : t =
  let ichan = open_in filename in
  let problem = from_channel ~problem_name ichan in
  close_in ichan;
  problem

let from_string ~problem_name (string : string) : t =
  let (ifd, ofd) = Unix.pipe () in
  let ichan = Unix.in_channel_of_descr ifd in
  let ochan = Unix.out_channel_of_descr ofd in
  output_string ochan string;
  let problem = from_channel ~problem_name ichan in
  close_out ochan;
  problem

let name problem = problem.name

let example =
  { name = "example" } (* FIXME *)

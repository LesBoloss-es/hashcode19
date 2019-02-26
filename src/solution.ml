(* open ExtPervasives *)
module Log = (val Logger.create "solution" : Logs.LOG)

type t =
  int list array

let to_file (filename : string) (solution : t) : unit =
  let ochan = open_out filename in
  ignore ochan;
  ignore filename;
  ignore solution;
  assert false (* FIXME *)

let score problem solution =
  ignore problem;
  ignore solution;
  assert false (* FIXME *)

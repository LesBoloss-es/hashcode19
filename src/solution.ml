(* open ExtPervasives *)
module Log = (val Logger.create "solution" : Logs.LOG)

type t = unit (* FIXME *)

let to_file (file : string) (_solution : t) : unit =
  Log.debug (fun m -> m "Printing in %s" file);
  let ochan = open_out file in
  (assert false : unit); (* FIXME *)
  close_out ochan

let score (_solution : t) : int =
  (assert false : int) (* FIXME *)

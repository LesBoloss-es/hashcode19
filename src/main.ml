open ExtPervasives
module Log = (val Logger.create "main" : Logs.LOG)

let specs =
  let open Arg in
  let open Config in
  align [
      "--workers", Set_int workers, spf "NB Sets the number of workers to NB (default: %d)" !workers ;
    ]

let handle_file filename =
  if !Config.file = "" then
    Config.file := filename
  else
    raise (Arg.Bad "only one file expected")

let usage = (spf "Usage: %s [--workers NB] FILE" Sys.argv.(0))

let lwt_stream_of_seq s =
  Lwt_stream.from
    (let s = ref s in
     fun () ->
     match !s () with
     | Seq.Nil ->
        Lwt.return_none
     | Cons (e, s') ->
        s := s';
        Lwt.return_some e)

let () =
  Arg.parse specs handle_file usage;
  Lwt_main.run
    (Lwt_stream.iter_n
       ~max_concurrency:!Config.workers
       (fun (name, solver) ->
         Log.info (fun m -> m "Trying solver %s" name);
         assert false
       )
       (lwt_stream_of_seq Solvers.all))

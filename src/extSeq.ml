let to_lwt_stream s =
  Lwt_stream.from
    (let s = ref s in
     fun () ->
       match !s () with
       | Seq.Nil ->
         Lwt.return_none
       | Cons (e, s') ->
         s := s';
         Lwt.return_some e)

let from_function (f : unit -> 'a option) : 'a Seq.t =
  let open Seq in
  let rec s () =
    match f () with
    | None -> Nil
    | Some x -> Cons (x, s)
  in
  s

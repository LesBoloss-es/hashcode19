open Seq

let to_lwt_stream s =
  Lwt_stream.from
    (let s = ref s in
     fun () ->
       match !s () with
       | Nil ->
         Lwt.return_none
       | Cons (e, s') ->
         s := s';
         Lwt.return_some e)

let from_function (f : unit -> 'a option) : 'a Seq.t =
  let rec s () =
    match f () with
    | None -> Nil
    | Some x -> Cons (x, s)
  in
  s

let from_function_i (f : int -> 'a option) : 'a Seq.t =
  let rec s i () =
    match f i with
    | None -> Nil
    | Some x -> Cons (x, s (i+1))
  in
  s 0

let flatten (s : 'a t t) : 'a t =
  flat_map (fun x -> x) s

let int ?(start=0) ?(end_=max_int) () : int t =
  let rec s i = fun () ->
    if i > end_ then
      Nil
    else
      Cons (i, s (i+1))
  in
  s start

let intertwine (s1 : 'a t) (s2 : 'a t) : 'a t =
  let rec s s1 s2 = fun () ->
    match s1 () with
    | Nil -> s2 ()
    | Cons (x1, s1') ->
      Cons (x1, s s2 s1')
  in
  s s1 s2

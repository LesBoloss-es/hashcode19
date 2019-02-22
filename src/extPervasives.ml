let pf = Format.printf
let epf = Format.eprintf
let fpf = Format.fprintf
let spf = Format.sprintf

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

let (||>) f g = fun x -> f x |> g

let datetime () =
  let tm = Unix.(localtime (gettimeofday ())) in
  spf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let (>>=) = Lwt.bind

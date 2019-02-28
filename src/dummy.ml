open ExtPervasives
module Log = (val Logger.create "dummy" : Logs.LOG)

let dummy (_param : int) (_problem : Problem.t) : Solution.t =
  Log.debug (fun m -> m "Dummy solver, yay o/");
  assert false

let instances =
  ExtSeq.from_function
    (fun () ->
       let i = Random.int 100 in
       let s = "dummy-" ^ (soi i) in
       Some (s, dummy i))

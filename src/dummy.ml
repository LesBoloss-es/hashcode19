let dummy (_param : int) (_problem : Problem.t) : Solution.t =
  ()

let instances =
  ExtSeq.from_function
    (fun () ->
       let i = Random.int 100 in
       let s = "dummy-" ^ (string_of_int i) in
       Some (s, dummy i))

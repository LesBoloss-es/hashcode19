open ExtPervasives

let all : (string * (Problem.t -> Solution.t)) Seq.t =
  seq_from_function
    (fun () ->
       let a = Random.int 100 in
       let b = Random.int 100 in
       let c = Random.int 100 in
       let d = Random.int 100 in

       Some
         (spf "naive-%d-%d-%d-%d" a b c d,
          (Naive.schedule_with_score Naive.naive_choice_better
             (Linear.multipliers a b c d))))

let tasks all_problems =
  let open Seq in
  (* We create a sequence of solvers applied to a problem. We have the first
     solver applied to all problems, then the second applied to all problems,
     etc. *)
  let rec s problems solvers =
    fun () ->
      if !Config.stop then
        Nil
      else
        match solvers () with
        | Nil -> Nil
        | Cons (solver, solvers') ->
          match problems with
          | [] ->
            s all_problems solvers' ()
          | problem :: problems ->
            Cons ((problem, solver), s problems solvers)
  in
  s all_problems all

(* open ExtPervasives *)

let all : (string * (Problem.t -> Solution.t)) Seq.t list =
  [Dummy.instances]

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
  all
  |> List.fold_left ExtSeq.intertwine Seq.empty
  |> s all_problems

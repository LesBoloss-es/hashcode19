let strategies : (string * (Problem.t -> Solution.t)) list =
  let strategies = ref [] in
  let m_bonus = 6 in
  for m_tostart = 1 to 3 do
    let m_tostart = 2 * m_tostart in
    for m_wait = 4 to 6 do
      let m_wait = m_wait in
      strategies :=
        (
          (Format.sprintf "Multipliers %d %d %d %d" m_bonus 0 m_tostart m_wait) ,
          Naive.schedule_with_score Naive.naive_choice_better (Linear.multipliers m_bonus 0 m_tostart m_wait)
        )
        :: !strategies
    done
  done;
!strategies

let all : (string * (Problem.t -> Solution.t)) Seq.t =
  List.to_seq strategies

let tasks all_problems =
  let open Seq in
  (* We create a sequence of solvers applied to a problem. We have the first
     solver applied to all problems, then the second applied to all problems,
     etc. *)
  let rec s problems solvers =
    fun () ->
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

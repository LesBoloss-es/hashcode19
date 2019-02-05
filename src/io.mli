(** {1 Io} *)

type problem

type solution

val score : solution -> int

val parse_problem : string -> problem

val print_solution : string -> solution -> unit

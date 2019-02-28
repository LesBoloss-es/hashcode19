(* open ExtPervasives *)
module Log = (val Logger.create "solution" : Logs.LOG)

type slide =
  | One of Problem.photo
  | Two of Problem.photo * Problem.photo
[@@deriving show]

type t =
  { length : int ;
    slides : slide array }
[@@deriving show]

let to_file (filename : string) (solution : t) : unit =
  let ochan = open_out filename in
  ignore ochan;
  ignore filename;
  ignore solution;
  assert false (* FIXME *)

let tags_of_slide = function
  | One photo -> photo.Problem.tags
  | Two (photo1, photo2) ->
    ExtPervasives.union_sorted photo1.Problem.tags photo2.Problem.tags

let score_of_slides s1 s2 =
  let t1, t2 = tags_of_slide s1, tags_of_slide s2 in
  let common_tags = ExtPervasives.common_elts t1 t2 in
  let score1, score2 =
    ExtPervasives.non_common_elts t1 t2,
    ExtPervasives.non_common_elts t2 t1
  in
  min common_tags (min score1 score2)

let score problem solution =
  ignore problem;
  let rec aux i =
    if i + 1 >= solution.length then 0
    else begin
      score_of_slides solution.slides.(i) solution.slides.(i+1) + (aux (i+1))
    end
  in
  aux 0

let get_random_slide _input _rem : Solution.slide = assert false


let rm_slide_everywhere _slide _input _rem = () (* FIXME *)


let rec fold_int f init = function
  | 0 -> init
  | n -> fold_int f (f init n) (n - 1)


let keep_going _input _rem = assert false


let solver input nb_iterations _horizon =
  let remaining_photos = ref (Array.copy input.Problem.photos) in
  let slides = Array.make 100_000 (Obj.magic 0) in
  let first = get_random_slide input remaining_photos in
  slides.(0) <- first;
  let pos = ref 1 in

  let rec solve () =
    let try_input best _ =
      let (best_score, _) = best in
      let slide = get_random_slide input remaining_photos in
      let new_score = Solution.score_of_slides slides.(!pos - 1) slide in
      if new_score > best_score then
        (new_score, slide)
      else
        best
    in

    let (_, new_slide) = fold_int try_input (-1, slides.(!pos - 1)) nb_iterations in

    slides.(!pos) <- new_slide;
    incr pos;
    rm_slide_everywhere new_slide input remaining_photos;

    if keep_going input remaining_photos then solve ();
  in

  solve ();

  Solution.{ slides ; length = !pos - 1 }

let solver input =
  let input_copy = Problem.copy input in
  solver input_copy

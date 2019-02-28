type available = {
  mutable slides: (Solution.slide * bool) array;
  mutable nb_eligible: int;
}

let rec get_random_slide_aux (rem : available) : Solution.slide * int =
  let n = Random.int (Array.length rem.slides) in
  let (slide, eligible) = rem.slides.(n) in
  if not eligible then
    get_random_slide_aux rem
  else
    (slide, n)

let array_filter p from_ to_ =
  let pos = ref 0 in
  let append x =
    to_.(!pos) <- x;
    incr pos
  in
  Array.iter (fun x -> if p x then append x) from_;
  !pos


let get_random_slide rem =
  let res = get_random_slide_aux rem in
  rem.nb_eligible <- pred rem.nb_eligible;
  if rem.nb_eligible > 10 && (rem.nb_eligible * 2 <= Array.length rem.slides) then begin
    let arr = Array.make rem.nb_eligible rem.slides.(0) in
    let nb_copies = array_filter snd rem.slides arr in
    assert (nb_copies = rem.nb_eligible);
    rem.slides <- arr;
    res
  end else
    res


let discard (rem: available) index =
  let (s, _) = rem.slides.(index) in
  rem.slides.(index) <- (s, false);
  rem.nb_eligible <- rem.nb_eligible - 1

let rec fold_int f init = function
  | 0 -> init
  | n -> fold_int f (f init n) (n - 1)

let keep_going rem =
  rem.nb_eligible > 0

let make_possible_slides input : available =
  let slides = SlidesFromPhotos.stupid input |> Array.of_list in
  { slides = slides ; nb_eligible = Array.length slides }

let solver input nb_iterations =
  let remaining_slides = make_possible_slides input in
  let slides = Array.make 100_000 (Solution.dummy_slide) in
  let first, index = get_random_slide remaining_slides in
  slides.(0) <- first;
  let pos = ref 1 in
  discard remaining_slides index;

  let rec solve last_index =
    let try_input best _ =
      let (best_score, _, _) = best in
      let slide, index = get_random_slide remaining_slides in
      let new_score = Solution.score_of_slides slides.(!pos - 1) slide in
      if new_score > best_score then
        (new_score, slide, index)
      else
        best
    in

    let (_, new_slide, index) =
      let init = (-1, slides.(!pos - 1), last_index) in
      fold_int try_input init nb_iterations
    in

    slides.(!pos) <- new_slide;
    incr pos;
    discard remaining_slides index;

    if keep_going remaining_slides then solve index;
  in

  solve index;

  Solution.{ slides ; length = !pos - 1 }

let solver input =
  let input_copy = Problem.copy input in
  solver input_copy

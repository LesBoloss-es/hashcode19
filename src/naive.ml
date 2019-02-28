open ExtPervasives

module Log = (val Logger.create "naive" : Logs.LOG)

type index = Horz of (Solution.slide * int) | Vert of (Solution.slide * int * int)

type available = {
  mutable verticals: (Problem.photo * bool) array;
  mutable horizontals: (Problem.photo * bool) array;
  mutable nb_v_eligible: int;
  mutable nb_h_eligible: int;
}

let get arr index =
  assert (index < Array.length arr);
  arr.(index)

let set arr index value =
  assert (index < Array.length arr);
  arr.(index) <- value

let slide_of_index = function
  | Horz (s, _) -> s
  | Vert (s, _, _) -> s

let get_random_slide_old (rem : available) : index =
  let w_h = rem.nb_h_eligible in
  let l_v = rem.nb_v_eligible in
  let w_v = l_v * (l_v - 1) / 2 in
  let ratio = (float w_h) /. ((float w_h) +. (float w_v)) in 
  let choose_h = (Random.float 1.) < ratio in
  if (choose_h || w_v = 0) && w_h <> 0 then begin
    let n_h = Array.length rem.horizontals in
    let rec pick_aux () =
      let n = Random.int n_h in
      let (slide, eligible) = get rem.horizontals n in
      if not eligible then
        pick_aux ()
      else
        Horz (Solution.One slide, n)
    in
    pick_aux ()
  end else begin
    let n_v = Array.length rem.verticals in
    let rec pick_n1 () =
      let n = Random.int n_v in
      let (slide, eligible) = get rem.verticals n in
      if not eligible then
        pick_n1 ()
      else
        (slide,n)
    in
    let rec pick_n2 n1 =
      let n = Random.int n_v in
      let (slide, eligible) = get rem.verticals n in
      if (not eligible) || (n1 = n) then
        pick_n2 n1
      else
        (slide, n)
    in
    let slide1, n1 = pick_n1 () in
    let slide2, n2 = pick_n2 n1 in
    Vert (Solution.Two (slide1, slide2), n1, n2)
  end

let get_random_slide cur_slide (rem : available) : index =
  let w_h = rem.nb_h_eligible in
  let l_v = rem.nb_v_eligible in
  let w_v = l_v * (l_v - 1) / 2 in
  let ratio = (float w_h) /. ((float w_h) +. (float w_v)) in 
  let choose_h = (Random.float 1.) < ratio in
  if (choose_h || w_v = 0) && w_h <> 0 then begin
    let rec get_next i last = 
      if i >= Array.length rem.horizontals then begin
        match last with
        | Some index -> index
        | None -> assert false
      end else begin
        let (photo, eligible) = get rem.horizontals i in
        let slide = Solution.One photo in
        let output = Horz (slide, i) in
        let score = Solution.score_of_slides cur_slide slide in
        if eligible && score > 0 then 
          output
        else if eligible then
          get_next (i+1) (Some output)
        else 
          get_next (i+1) last
      end
    in
    get_next 0 None
  end else begin
    let rec get_next_available i = 
      let (photo, eligible) = get rem.verticals i in
      if eligible then (photo, i)
      else get_next_available (i+1)
    in
    let photo1, i1 = get_next_available 0 in
    let rec get_next i last = 
      if i >= Array.length rem.verticals then begin
        match last with
        | Some index -> index
        | None -> assert false
      end else begin
        let (photo, eligible) = get rem.verticals i in
        let slide = Solution.Two (photo1, photo) in
        let output = Vert (slide, i1, i) in
        let score = Solution.score_of_slides cur_slide slide in
        if eligible && score > 0 then 
          output
        else if eligible then
          get_next (i+1) (Some output)
        else 
          get_next (i+1) last
      end
    in
    get_next i1 None
  end

let array_filter p from_ to_ =
  let pos = ref 0 in
  let append x =
    set to_ (!pos) x;
    incr pos
  in
  Array.iter (fun x -> if p x then append x) from_;
  !pos


let discard (rem: available) (index: index) =
  begin match index with
  | Horz (_, idx) ->
    let (ph, _) = get rem.horizontals idx in
    set rem.horizontals idx (ph, false);
    rem.nb_h_eligible <- pred rem.nb_h_eligible;
    if rem.nb_h_eligible > 10 && (rem.nb_h_eligible * 2 <= Array.length rem.horizontals) then begin
      let arr = Array.make rem.nb_h_eligible (get rem.horizontals 0) in
      let nb_copies = array_filter snd rem.horizontals arr in
      assert (nb_copies = rem.nb_h_eligible);
      rem.horizontals <- arr
    end

  | Vert (_, idx1, idx2) ->
    let (ph1, _) = get rem.verticals idx1 in
    let (ph2, _) = get rem.verticals idx2 in
    set rem.verticals idx1 (ph1, false);
    set rem.verticals idx2 (ph2, false);
    rem.nb_v_eligible <- pred (pred rem.nb_v_eligible);
    if rem.nb_v_eligible > 10 && (rem.nb_v_eligible * 2 <= Array.length rem.verticals) then begin
      let arr = Array.make rem.nb_v_eligible (get rem.verticals 0) in
      let nb_copies = array_filter snd rem.verticals arr in
      assert (nb_copies = rem.nb_v_eligible);
      rem.verticals <- arr
    end
  end

let rec fold_int f init = function
  | 0 -> init
  | n -> fold_int f (f init n) (n - 1)

let keep_going rem =
  rem.nb_h_eligible > 0 || rem.nb_v_eligible > 1

let make_possible_slides input : available =
  let verticals = Array.make (Array.length input.Problem.photos_v) (Obj.magic 0) in
  let horizontals = Array.make (Array.length input.Problem.photos_h) (Obj.magic 0) in
  Array.iteri (fun i elt -> verticals.(i) <- (elt, true))
    input.Problem.photos_v;
  Array.iteri (fun i elt -> horizontals.(i) <- (elt, true))
    input.Problem.photos_h;
  { 
    nb_h_eligible = Array.length horizontals ;
    nb_v_eligible = Array.length verticals ;
    verticals;
    horizontals;
  }

let solver input nb_iterations =
  let remaining_slides = make_possible_slides input in
  let slides = Array.make 100_000 (Solution.dummy_slide) in
  let index = get_random_slide_old remaining_slides in
  set slides 0 (slide_of_index index);
  let pos = ref 1 in
  discard remaining_slides index;

  let rec solve last_index =
    let try_input best _ =
      let (best_score, _, _) = best in
      let index = get_random_slide (slide_of_index last_index) remaining_slides in
      let slide = slide_of_index index in
      let new_score = Solution.score_of_slides (get slides (!pos - 1)) slide in
      if new_score > best_score then
        (new_score, slide, index)
      else
        best
    in

    let (_, new_slide, index) =
      let init = (-1, get slides (!pos - 1), last_index) in
      fold_int try_input init nb_iterations
    in

    set slides (!pos) new_slide;
    incr pos;
    discard remaining_slides index;

    if keep_going remaining_slides then solve index;
  in

  solve index;

  Solution.{ slides ; length = !pos - 1 }

let instances =
  ExtSeq.int ~start:20 ()
  |>  Seq.map (fun n ->
      let n = 20 * n in
      let s = "input-" ^ (soi n) in
      (s, fun input -> solver input n))

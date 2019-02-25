open Problem

let end_time t pos p r =
  let ride = p.rides.(r) in
  (max ride.earliest_start (t + distance pos ride.start)) + ride.duration

let find_best (score : time -> intersection -> Problem.t -> ride -> int) t pos p rides_booked =
  let r = ref (-1) in  (* selected ride *)
  let s = ref min_int in  (* best score at the moment *)
  let find i ride =
    if (not rides_booked.(i)) then
      let s' = score t pos p ride in
      if s' > !s then begin
        r := i ;
        s := s'
      end
  in
  Array.iteri find p.rides ;
  !r

let to_solution cars : Solution.t =
  Array.map (fun (_, _, q, _) -> List.rev q) cars

let _schedule_with_score score (p : Problem.t) : Solution.t =
  let cars = Array.make p.vehicles (0, (0, 0), [], false) in
  let rides_booked = Array.make (Array.length p.rides) false in
  while Array.exists (fun (_, _, _, finished) -> not finished) cars do
    for i = 0 to p.vehicles - 1 do
      let t, pos, queue, finished = cars.(i) in
      if not finished then begin
        let r = find_best score t pos p rides_booked in
        if r >= 0 then begin
          rides_booked.(r) <- true;
          let end_t = end_time t pos p r in
          let end_pos = p.rides.(r).finish in
          let queue = r :: queue in
          cars.(i) <- (end_t, end_pos, queue, false) ;
        end else begin
          cars.(i) <- t, pos, queue, true
        end
      end
    done;
  done;
  to_solution cars

let naive_choice _p cars last_chosen =
  let i = ref last_chosen in
  let incr () =
    if !i + 1 = Array.length cars
    then i := 0
    else i := !i + 1
  in
  let break = ref false in
  incr () ;
  while !i <> last_chosen && not !break do
    let _, _, _, finished = cars.(!i) in
    if finished
    then incr ()
    else break := true
  done;
  if !i = last_chosen then -1 else !i

let naive_choice_better p cars last_chosen =
  let i = ref last_chosen in
  let incr () =
    if !i + 1 = Array.length cars
    then i := 0
    else i := !i + 1
  in
  let break = ref false in
  incr () ;
  while !i <> last_chosen && not !break do
    let t, _, _, finished = cars.(!i) in
    let threshold = p.average_ride_size /2 in
    let must_choose = Array.for_all (fun (t', _, _, f) -> f || t' + threshold >= p.steps) cars in
    if finished
    then incr ()
    else begin
      if t + threshold >= p.steps && not must_choose then
        incr ()
      else
        break := true
    end
  done;
  if !i = last_chosen then -1 else !i

let less_advanced _p cars last_chosen =
  let i = ref (-1) in
  let min_t = ref max_int in
  let select j (t, _, _, finished) =
    if not finished && t < !min_t && j <> last_chosen then begin
      i := j;
      min_t := t
    end
  in
  let select' j (t, _, _, finished) =
    if not finished && t < !min_t then begin
      i := j;
      min_t := t
    end
  in
  Array.iteri select cars;
  if !i = -1 then Array.iteri select' cars;
  !i

let schedule_with_score choice score (p : Problem.t) : Solution.t =
  let cars = Array.make p.vehicles (0, (0, 0), [], false) in
  let rides_booked = Array.make (Array.length p.rides) false in
  let i = ref 0 in
  while !i <> - 1 do
    let t, pos, queue, finished = cars.(!i) in
    if not finished then begin
      let r = find_best score t pos p rides_booked in
      if r >= 0 then begin
        rides_booked.(r) <- true;
        let end_t = end_time t pos p r in
        let end_pos = p.rides.(r).finish in
        let queue = r :: queue in
        cars.(!i) <- (end_t, end_pos, queue, false) ;
      end else begin
        cars.(!i) <- t, pos, queue, true
      end
    end;
    i := choice p cars !i
  done;
  to_solution cars

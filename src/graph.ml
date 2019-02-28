module Log = (val Logger.create "victor" : Logs.LOG)

type edge = {
  score : int;
  starts : Solution.slide;
  ends : Solution.slide;
}

type t = {
  neighbours : edge list array;
  indices : (Solution.slide, int) Hashtbl.t
}

let make_from_slides slides limit = 
  let length = List.length slides in
  let neighbours = Array.make length [] in
  let indices = Hashtbl.create 13 in
  List.iteri (fun i slide -> Hashtbl.add indices slide i) slides;
  let rec aux cur_slide next_slides limit = 
    match next_slides with
    | [] -> ()
    | h::t ->
      if limit = 0 then ()
      else begin
        let cur_id = Hashtbl.find indices cur_slide in
        let next_id = Hashtbl.find indices h in
        let score = Solution.score_of_slides cur_slide h in
        if score <> 0 then begin
          neighbours.(cur_id) <- {score; starts = cur_slide; ends = h} :: neighbours.(cur_id);
          neighbours.(next_id) <- {score; starts = h; ends = cur_slide} :: neighbours.(next_id);
        end;
        aux cur_slide t (limit - 1)
      end
  in
  let rec aux2 = function
    | [] -> ()
    | h::t -> aux h t limit; aux2 t
  in
  aux2 slides;
  {neighbours; indices}

let best_neighbour graph cur non_visited = 
  let rec aux = function
    | [] -> None
    | h::t ->
      let h_id = Hashtbl.find graph.indices h.ends in
      if Hashtbl.mem non_visited h_id then Some h.ends
      else aux t
  in
  let cur_id = Hashtbl.find graph.indices cur in
  aux graph.neighbours.(cur_id)

exception Found_it of Solution.slide

let random_slide non_visited =
  try
    Hashtbl.iter (fun _ s -> raise (Found_it s)) non_visited;
    None
  with
    Found_it s -> Some s

let rec traverse graph current non_visited sol =
  match best_neighbour graph current non_visited with
  | Some ngh ->
    let ngh_id = Hashtbl.find graph.indices ngh in
    Hashtbl.remove non_visited ngh_id;
    traverse graph ngh non_visited (ngh::sol)
  | None -> begin
    match random_slide non_visited with
    | Some rnd -> 
      let rnd_id = Hashtbl.find graph.indices rnd in
      Hashtbl.remove non_visited rnd_id;
      traverse graph rnd non_visited (rnd::sol)
    | None -> sol
  end

let solve_graph graph = 
  let non_visited = Hashtbl.create ~random:true 13 in
  Hashtbl.iter (fun slide id -> Hashtbl.add non_visited id slide) graph.indices;
  match random_slide non_visited with
  | Some rnd ->
    let rnd_id = Hashtbl.find graph.indices rnd in
    Hashtbl.remove non_visited rnd_id;
    traverse graph rnd non_visited [rnd]
  | None -> []

let solver limit problem = 
  Log.debug (fun m -> m "Solving problem %s"(Problem.name problem));
  let slides = SlidesFromPhotos.stupid problem in
  Log.debug (fun m -> m "Got slides");
  let graph = make_from_slides slides limit in
  Log.debug (fun m -> m "Made graph");
  let sol = solve_graph graph in
  Log.debug (fun m -> m "Graph solved");
  let slides = Array.of_list sol in
  Solution.{slides; length = Array.length slides}

let instances = ExtSeq.from_function_i (fun i -> 
  let limit = 50 + i in 
  Some ("victor", solver limit))

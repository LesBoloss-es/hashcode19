open ExtPervasives
module Log = (val Logger.create "problem" : Logs.LOG)

type verticality = [`H | `V]
[@@deriving show]

let verticality_of_string = function
  | "H" -> `H
  | "V" -> `V
  | _ -> failwith "verticality_of_string"

type tag = int
[@@deriving show]

type photo =
  { verticality : verticality ;
    tags : tag list ;
    id : int }
[@@deriving show]

let print_tag_to_photo fmt hashtbl =
  fpf fmt "@[<2>[@ ";
  Hashtbl.iter
    (fun tag photo ->
       fpf fmt "%d -> %a@\n" tag pp_photo photo)
    hashtbl;
  fpf fmt "]@]"

type t =
  { name : string ;
    photos_v : photo array ;
    photos_h : photo array ;
    tags : (tag, photo) Hashtbl.t [@printer print_tag_to_photo] }
[@@deriving show]

let dummy_photo = 
  { verticality = `H; tags = []; id = -1 }

let copy problem =
  { name = problem.name ;
    photos_v = Array.copy problem.photos_v ;
    photos_h = Array.copy problem.photos_h ;
    tags = Hashtbl.copy problem.tags }

let from_channel ~problem_name (ichan : in_channel) : t =
  let nb_photos = ios (input_line ichan) in

  let tag_from_string =
    let string_to_tag = Hashtbl.create 100 in
    let next_tag = ref 0 in
    fun string ->
      match Hashtbl.find_opt string_to_tag string with
      | None ->
        incr next_tag;
        Hashtbl.add string_to_tag string !next_tag;
        !next_tag
      | Some tag ->
        tag
  in

  let tag_to_photo = Hashtbl.create 100 in

  let photos_v = Array.make nb_photos (Obj.magic 0) in
  let photos_h = Array.make nb_photos (Obj.magic 0) in
  let next_photo_v = ref 0 in
  let next_photo_h = ref 0 in
  let add_photo_v photo =
    photos_v.(!next_photo_v) <- photo;
    incr next_photo_v
  in
  let add_photo_h photo =
    photos_h.(!next_photo_h) <- photo;
    incr next_photo_h
  in

  for i_photo = 0 to nb_photos - 1 do
    match input_line ichan |> String.split_on_char ' ' with

    | verticality :: nb_tags :: tags ->
      let verticality = verticality_of_string verticality in
      assert (List.length tags = ios nb_tags);
      let tags = List.map tag_from_string tags |> List.sort compare in
      let photo = { verticality ; tags ; id = i_photo } in
      (match verticality with
       | `V -> add_photo_v
       | `H -> add_photo_h) photo;
      List.iter
        (fun tag ->
           Hashtbl.add tag_to_photo tag photo)
        tags;

    | _ ->
      assert false
  done;

  assert (!next_photo_v + !next_photo_h = nb_photos);

  { name = problem_name ;
    photos_v = Array.sub photos_v 0 !next_photo_v ;
    photos_h = Array.sub photos_h 0 !next_photo_h ;
    tags = tag_to_photo }

let from_file ~problem_name (filename : string) : t =
  let ichan = open_in filename in
  let problem = from_channel ~problem_name ichan in
  close_in ichan;
  problem

let from_string ~problem_name (string : string) : t =
  let (ifd, ofd) = Unix.pipe () in
  let ichan = Unix.in_channel_of_descr ifd in
  let ochan = Unix.out_channel_of_descr ofd in
  output_string ochan string;
  let problem = from_channel ~problem_name ichan in
  close_out ochan;
  problem

let name problem = problem.name

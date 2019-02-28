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

type t =
  { name : string ;
    photos : photo array ;
    tags : (tag, photo) Hashtbl.t [@opaque] }
[@@deriving show]

let copy problem =
  { name = problem.name ;
    photos = Array.copy problem.photos ;
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

  let photos = Array.make nb_photos (Obj.magic 0) in
  let next_photo = ref 0 in
  let add_photo photo =
    photos.(!next_photo) <- photo;
    incr next_photo
  in

  for i_photo = 0 to nb_photos - 1 do
    match input_line ichan |> String.split_on_char ' ' with

    | verticality :: nb_tags :: tags ->
      assert (List.length tags = ios nb_tags);
      let tags = List.map tag_from_string tags in
      let photo =
        { verticality = verticality_of_string verticality ;
          tags ; id = i_photo }
      in
      add_photo photo;
      List.iter
        (fun tag ->
           Hashtbl.add tag_to_photo tag photo)
        tags;

    | _ ->
      assert false
  done;

  assert (!next_photo = nb_photos);

  { name = problem_name ; photos ; tags = tag_to_photo }

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

let example = "4\nH 3 cat beach sun\nV 2 selfie smile\nV 2 garden selfie\nH 2 garden cat\n"

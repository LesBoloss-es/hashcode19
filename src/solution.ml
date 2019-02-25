(* open ExtPervasives *)
module Log = (val Logger.create "solution" : Logs.LOG)

type t =
  int list array

let to_file (filename : string) (solution : t) : unit =
  let oc = open_out filename in
  for vehicle = 0 to Array.length solution - 1 do
    output_string oc
      (
        (string_of_int (List.length solution.(vehicle)))
        ^ " "
        ^ (String.concat " " (List.map string_of_int solution.(vehicle)))
        ^ "\n"
      )
  done;
  close_out oc

open Problem

let score problem solution =
  let score = ref 0 in
  let ride_assigned = Array.make (Array.length problem.rides) (-1) in
  for vehicle = 0 to problem.vehicles - 1 do
    let position = ref (0, 0) in
    let time = ref 0 in
    List.iter
      (fun ride_number ->
         if ride_assigned.(ride_number) <> -1 then
           Format.eprintf "ERROR: trying to assign ride %d to vehicles %d and %d@." ride_number vehicle ride_assigned.(ride_number)
         else
           ride_assigned.(ride_number) <- vehicle;

         let ride = problem.rides.(ride_number) in

         (* Go to the starting point *)
         time := !time + (distance !position ride.start);

         (* Are we there on time? If yes, wait for the earliest start *)
         let bonus_deserved =
           if !time <= ride.earliest_start then
             (
               time := ride.earliest_start;
               true
             )
           else
             false
         in

         (* Do the ride *)
         time := !time + ride.duration ;
         position := ride.finish;

         (* Check that we are there in time *)
         if !time <= ride.latest_finish && !time <= problem.steps then
           score := !score + (if bonus_deserved then problem.bonus else 0) + ride.duration
         else
           Format.eprintf "WARNING: vehicle %d finished ride %d too late@." vehicle ride_number
      )
      solution.(vehicle)
  done;
  !score

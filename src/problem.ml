(* open ExtPervasives *)
module Log = (val Logger.create "problem" : Logs.LOG)

type intersection = int * int

type time = int

type ride =
  { start : intersection ;
    finish : intersection ;
    duration : int ;
    earliest_start : time ;
    latest_finish : time }

type t = {
  name : string ;
  vehicles : int ;
  rides : ride array ;
  average_ride_size : int ;
  bonus : int ;
  steps : int ;
}

let distance (x1, y1) (x2, y2) =
  abs (x2 - x1) + abs (y2 - y1)

let from_file ~problem_name (filename : string) : t =
  let ic = open_in filename in
  match String.split_on_char ' ' (input_line ic) with
  | [_rows; _columns; vehicles; number_of_rides; bonus; steps] ->
    (
      let number_of_rides = int_of_string number_of_rides in
      let rides = Array.make number_of_rides (Obj.magic ()) in
      let average_ride_size = ref 0.0 in
      for ride = 0 to number_of_rides - 1 do
        match String.split_on_char ' ' (input_line ic) with
        | [row_start; column_start; row_finish; column_finish; earliest_start; latest_finish] ->
          (
            let start = (int_of_string row_start, int_of_string column_start) in
            let finish = (int_of_string row_finish, int_of_string column_finish) in
            let duration = distance start finish in
            average_ride_size := !average_ride_size +. (float_of_int duration) /. (float_of_int number_of_rides);
            rides.(ride) <-
              { start ; finish ; duration ;
                earliest_start = int_of_string earliest_start;
                latest_finish = int_of_string latest_finish }
          )
        | _ -> assert false
      done;
      close_in ic;
      { name = problem_name ;
        vehicles = int_of_string vehicles ;
        rides ;
        average_ride_size = int_of_float !average_ride_size ;
        bonus = int_of_string bonus ;
        steps = int_of_string steps }
    )
  | _ -> assert false

let name problem = problem.name

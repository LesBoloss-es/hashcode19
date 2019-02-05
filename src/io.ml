
type problem = unit

type solution = unit

let score _output =
  assert false

let parse_problem filename =
  let ichan = open_in filename in
  let rec parse_problem () =
    try
      let _ = input_line ichan in
      assert false;
      parse_problem ()
    with
      End_of_file -> ()
  in
  let problem = parse_problem () in
  close_in ichan;
  problem

let print_solution filename solution =
  let ochan = open_out filename in
  assert false;
  close_out ochan

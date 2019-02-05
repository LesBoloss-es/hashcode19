module Log = (val Logger.create "main" : Logs.LOG)

let parse_problem filename =
  Log.debug (fun m -> m "Parsing %s" filename);
  let ichan = open_in filename in
  let rec parse_problem () =
    try
      let line = input_line ichan in
      assert false;
      parse_problem ()
    with
      End_of_file -> ()
  in
  let problem = parse_problem () in
  close_in ichan;
  problem

let print_solution filename solution =
  Log.debug (fun m -> m "Printing in %s" filename);
  let ochan = open_out filename in
  assert false;
  close_out ochan

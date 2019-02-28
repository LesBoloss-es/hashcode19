open Problem
module Log = (val Logger.create "slides-from-photos" : Logs.LOG)

let stupid problem : Solution.slide list =
  Log.debug (fun m -> m "start stupid");
  let slides = ref [] in
  Array.iter
    (fun photo_h ->
       slides := Solution.One photo_h :: !slides)
    problem.photos_h;
  for i = 0 to (Array.length problem.photos_v - 1) / 2 do
    slides := Solution.Two (
        problem.photos_v.(2*i),
        problem.photos_v.(2*i+1))
                :: !slides
  done;
  Log.debug (fun m -> m "end stupid");
  !slides

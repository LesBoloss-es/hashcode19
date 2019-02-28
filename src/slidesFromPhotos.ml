open Problem

let stupid problem : Solution.slide list =
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
  !slides

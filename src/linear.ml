open Problem
open Solution

let multipliers
    m_bonus m_duration m_tostart m_wait
    time pos problem ride =
  (* time where we can be at start *)
  let time_at_start = time + distance pos ride.start in

  (* consider the ride only if we can make it in time *)
  if time_at_start + ride.duration <= ride.latest_finish then
    (
      m_duration * ride.duration
      +
      (* what we earn *)
      (
        (* bonus (or 0) *)
        (m_bonus * (if time_at_start <= ride.earliest_start then problem.bonus else 0))
      )
      -
      (* what we loose *)
      (
        (* the time it takes to get at start *)
        (m_tostart * (distance pos ride.start))
        (* the time that we wait at start *)
        + (m_wait * (max 0 (ride.earliest_start - time_at_start)))
      )
    )
  else
    min_int

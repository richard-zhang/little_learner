(*
  chapter 4
  The Law of Revision
  new theta = theta - (alpha x rate of change of loss w.r.t theta )
  * why minus
    * we need to move theta in the direction that minimize loss
    * hence opposite direction of gradient
*)

let revise reviser rev = Base.Fn.apply_n_times ~n:rev reviser

let%test "revise 1" =
  revise (List.map (fun x -> x - 3)) 5 [1; 2; 3] = [-14; -13; -12]

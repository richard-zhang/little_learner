(* chapter 1 *)
let line x params =
  match params with
  | [a; b] ->
      (a *. x) +. b
  | _ ->
      failwith "only accept #params = 2"

let line_xs = Tensor.floats [2.0; 1.0; 4.0; 3.0]

let line_ys = Tensor.floats [1.8; 1.2; 4.2; 3.3]

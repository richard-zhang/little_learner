

(*
  At the moment use finite difference method
*)
(* let gradient func parameter *)
(*
  思路有问题   
*)
let gradient (func : Tensor.t array -> float) (parameter : Tensor.t array) : Tensor.t array = 
  let eps = Float.epsilon in
  let base_value = func parameter in
  let increments = Array.map Tensor.small_increments parameter in
  (*
    1. 
    2.     
    3. 
  *)
  (* need to implement a tensor map first *)
  



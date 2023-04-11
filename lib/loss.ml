(* chapter 3 *)

(*
  1. expectant function is a function expecting a input data and coresponding data
  2. target function is function like line which takes input as well parameter and produce output
  3. objective function take parameters as arguments and produces a scalar representing the loss
  4. loss is how far away we from the well-fitted parameters
  5. loss function expects 1. target function, 2. data set 3. parameters and output the loss
*)

(*
  expectant_func = loss_func target
  objective_func = expectant_fun data_set
  loss = objective_func params
*)

let l2_loss target (xs, ys) params =
  let output = List.map (fun x -> target x params) xs in
  List.fold_left2
    (fun init x1 x2 ->
      let diff = (x1 -. x2) *. (x1 -. x2) in
      init +. diff )
    0. output ys

let%expect_test "graident of line at (0.0, 0.0)" =
  let gradient_at_x =
    Tensor.bin_op (fun x y -> -2.0 *. y *. x) Line.line_xs Line.line_ys
    |> Tensor.sum |> Tensor.get_value
  in
  let gradient_at_y =
    Tensor.bin_op (fun _ y -> -2.0 *. y) Line.line_xs Line.line_ys
    |> Tensor.sum |> Tensor.get_value
  in
  Printf.printf "%.1f,%.1f" gradient_at_x gradient_at_y ;
  [%expect {| -63.0,-21.0 |}]

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

let l2_loss target (xs, expected_ys) params =
  Tensor.bin_op
    (fun x y ->
      let diff = target x params -. y in
      diff *. diff )
    xs expected_ys
  |> Tensor.sum |> Tensor.get_value

let%expect_test "l2_loss" =
  let target_func = Line.line in
  let expectant_func = l2_loss target_func in
  let objective_func = expectant_func (Line.line_xs, Line.line_ys) in
  let params = [0.0; 0.0] in
  let loss = objective_func params in
  Printf.printf "%.2f" loss ; [%expect {| 33.21 |}]

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

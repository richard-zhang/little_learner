(* no performance consideration *)
type t = Scalar of float | Tensor of t array

let scalar v = Scalar v

let floats float_list =
  let value = List.map scalar float_list |> Array.of_list in
  Tensor value

let tensor t = Tensor (Array.of_list t)

(* chapter 2 *)
let is_scalar = function Scalar _ -> true | _ -> false

let length = function Scalar _ -> 0 | Tensor array -> Array.length array

let access index = function
  | Scalar v ->
      Scalar v
  | Tensor array ->
      Array.get array index

let head = access 0

let get_value = function
  | Scalar v ->
      v
  | _ ->
      failwith "can only extract value from a scalar"

let rec shape v = if is_scalar v then [] else length v :: shape (head v)

let%test_unit "shape_example" =
  let open Base in
  let singleton_tensor x = Tensor (Array.of_list [Scalar x]) in
  let data_1 =
    Tensor
      (Array.of_list
         [singleton_tensor 5.; singleton_tensor 6.; singleton_tensor 8.] )
  in
  let data_2 =
    Tensor
      (Array.of_list
         [singleton_tensor 7.; singleton_tensor 9.; singleton_tensor 5.] )
  in
  let data = Tensor (Array.of_list [data_1; data_2]) in
  [%test_result: int list] (shape data) ~expect:[2; 3; 1]

let rank =
  let rec go acc v = if is_scalar v then acc else go (acc + 1) (head v) in
  go 0

(* chapter 3 *)
(*
   Note: we use the concept of descending into the higher-ranked tensors to accomplish this, we descend into the higher ranked tensor while repeating the lower-ranked tneor until one or two-argument operation can proceed
*)

(* assume op is commutative *)
let bin_op op t1 t2 =
  let r1 = rank t1 in
  let r2 = rank t2 in
  let rec helper (op : float -> float -> float) r1 r2 t1 t2 =
    match (t1, t2) with
    | Scalar v1, Scalar v2 ->
        Scalar (op v1 v2)
    | Tensor array1, Tensor array2 when r1 = r2 ->
        Tensor
          ( Array.combine array1 array2
          |> Array.map (fun (a, b) -> helper op (r1 - 1) (r2 - 1) a b) )
    | _, Tensor array ->
        Tensor (Array.map (helper op r1 (r2 - 1) t1) array)
    | _, _ ->
        failwith
          "Rank of first argument must be smaller than rank of the second \
           argument"
  in
  helper op r1 r2 t1 t2

let plus = bin_op ( +. )

let%test "plus 1" =
  let t1 = floats [5.; 6.; 7.] in
  let t2 = floats [2.; 0.; 1.] in
  let expected = floats [7.0; 6.; 8.] in
  plus t1 t2 = expected

let%test "plus 2" =
  let t1 = scalar 4. in
  let t2 = floats [3.; 6.; 5.] in
  let expected = floats [7.0; 10.; 9.] in
  plus t1 t2 = expected

let%test "plus 3" =
  let t1 = floats [6.; 9.; 1.] in
  let t2 = floats [4.0; 3.; 8.] in
  let t3 = floats [7.; 4.; 7.] in
  let t4 = tensor [t2; t3] in
  let expected = tensor [floats [10.; 12.; 9.]; floats [13.; 13.; 8.]] in
  plus t1 t4 = expected

let rec unary_op r op t =
  let r1 = rank t in
  assert (r <= r1) ;
  if r1 = r then op t
  else
    match t with
    | Tensor t1 ->
        Tensor (Array.map (unary_op r op) t1)
    | _ ->
        failwith "must be of tensor"

let sum =
  unary_op 1 (function
    | Scalar _ ->
        failwith "must be tensor"
    | Tensor x ->
        Scalar (Array.fold_left ( +. ) 0. (Array.map get_value x)) )

let%test "sum 1" =
  let t1 = floats [1.; 2.] in
  let t2 = floats [3.; 4.] in
  let t3 = floats [5.; 6.] in
  let t4 = floats [7.; 8.] in
  let expected = tensor [floats [3.; 7.]; floats [11.; 15.]] in
  sum (tensor [tensor [t1; t2]; tensor [t3; t4]]) = expected
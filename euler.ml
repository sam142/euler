let rec foldl_range f a b z =
  if b < a then z else foldl_range f (a + 1) b (f z a)

let rec foldr_range f a b z =
  if b < a then z else foldr_range f a (b - 1) (f b z)

let list_of_range a b = foldr_range (fun x xs -> x::xs) a b []

let fibs_upto n =
  let rec aux acc a b =
    if n < b then acc else aux (b::acc) b (a + b)
  in List.rev (aux [] 1 1)

let is_prime p =
  let bound = p |> float_of_int |> sqrt |> int_of_float in
  let rec aux i =
    if i > bound then true else p mod i <> 0 && aux (i + 1)
  in p > 0 && aux 2

let prime_divisor n =
  let rec aux i =
    if n mod i = 0 && is_prime i then i else aux (i + 1)
  in aux 2

let prime_factors n =
  let rec aux acc i =
    if i < 2 then acc else 
      let div = prime_divisor i in
      aux (div::acc) (i / div)
  in aux [] n

let num_rev n = 
  let rec aux n acc =
    if n <= 0 then acc else aux (n / 10) (acc * 10 + n mod 10)
  in aux n 0

let p1 =
  let mul_3_5 n = n mod 3 = 0 || n mod 5 = 0 in
  List.(list_of_range 1 999 |> filter mul_3_5 |> fold_left (+) 0)

let p2 =
  fibs_upto 4_000_000 |>
  List.filter (fun x -> x mod 2 = 0) |>
  List.fold_left (+) 0

let p3 =
  prime_factors 600851475143 |> List.hd

let p4 =
  let is_div_3d n = foldl_range (fun p i -> p || (n mod i == 0 && n / i < 1000)) 100 999 false in
  let prefix = 999 * 999 / 1000 in
  let rec pal_div_3d prefix =
    let pal = prefix * 1000 + num_rev prefix in
    if is_div_3d pal then pal else pal_div_3d (prefix - 1)
  in pal_div_3d prefix

let p5 =
  let step = list_of_range 1 20 |> List.filter is_prime |> List.fold_left ( * ) 1 in
  let is_div_1_20 n = foldl_range (fun p i -> p && (n mod i == 0)) 1 20 true in
  let rec aux i = if is_div_1_20 i then i else aux (i + step) in
  aux step

let p6 =
  let pow2 x = x * x in
  let square_of_sum = foldl_range (+) 1 100 0 |> pow2 in
  let sum_of_squares = foldl_range (fun s i -> s + pow2 i) 1 100 0 in
  square_of_sum - sum_of_squares

let () =
  let open Printf in
  printf "01 %d\n" p1;
  printf "02 %d\n" p2;
  printf "03 %d\n" p3;
  printf "04 %d\n" p4;
  printf "05 %d\n" p5;
  printf "06 %d\n" p6;


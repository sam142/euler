let rec foldl_range a b f z =
  if b < a then z else foldl_range (a + 1) b f (f z a)

let rec foldr_range a b f z =
  if b < a then z else foldr_range a (b - 1) f (f b z)

let list_of_range a b = foldr_range a b (fun x xs -> x::xs) []

let fibs_upto n =
  let rec aux acc a b =
    if n < b then acc else aux (b::acc) b (a + b)
  in List.rev (aux [] 1 1)

let is_prime p =
  let bound = p |> float_of_int |> sqrt |> int_of_float in
  let rec aux i =
    if i > bound then true else p mod i <> 0 && aux (i + 1)
  in p > 1 && aux 2

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

let p1 () =
  let mul_3_5 n = n mod 3 = 0 || n mod 5 = 0 in
  List.(list_of_range 1 999 |> filter mul_3_5 |> fold_left (+) 0)

let p2 () =
  fibs_upto 4_000_000 |>
  List.filter (fun x -> x mod 2 = 0) |>
  List.fold_left (+) 0

let p3 () =
  prime_factors 600851475143 |> List.hd

let p4 () =
  let is_div_3d n = foldl_range 100 999 (fun p i -> p || (n mod i == 0 && n / i < 1000)) false in
  let prefix = 999 * 999 / 1000 in
  let rec pal_div_3d prefix =
    let pal = prefix * 1000 + num_rev prefix in
    if is_div_3d pal then pal else pal_div_3d (prefix - 1)
  in pal_div_3d prefix

let p5 () =
  let common_div = list_of_range 1 20 |> List.filter is_prime |> List.fold_left ( * ) 1 in
  let is_div_1_20 n = foldl_range 1 20 (fun p i -> p && (n mod i == 0)) true in
  let rec aux i = if is_div_1_20 i then i else aux (i + common_div) in
  aux common_div

let p6 () =
  let pow2 x = x * x in
  let square_of_sum = foldl_range 1 100 (+) 0 |> pow2 in
  let sum_of_squares = foldl_range 1 100 (fun s i -> s + pow2 i) 0 in
  square_of_sum - sum_of_squares

let p7 () =
  let rec next_prime = function
    | p when p < 2 -> 2
    | 2 -> 3
    | p -> let next = p + 2 in if is_prime next then next else next_prime next
  in
  let nth_prime n =
    let rec aux p i = if i <= 1 then p else aux (next_prime p) (i - 1)
    in aux 2 n
  in nth_prime 10_001

let p8_data () =
  let list_of_string s =
    let rec aux acc i = if i < 0 then acc else aux (s.[i] :: acc) (i - 1)
    in aux [] (String.length s - 1)
  in
  [
    "73167176531330624919225119674426574742355349194934";
    "96983520312774506326239578318016984801869478851843";
    "85861560789112949495459501737958331952853208805511";
    "12540698747158523863050715693290963295227443043557";
    "66896648950445244523161731856403098711121722383113";
    "62229893423380308135336276614282806444486645238749";
    "30358907296290491560440772390713810515859307960866";
    "70172427121883998797908792274921901699720888093776";
    "65727333001053367881220235421809751254540594752243";
    "52584907711670556013604839586446706324415722155397";
    "53697817977846174064955149290862569321978468622482";
    "83972241375657056057490261407972968652414535100474";
    "82166370484403199890008895243450658541227588666881";
    "16427171479924442928230863465674813919123162824586";
    "17866458359124566529476545682848912883142607690042";
    "24219022671055626321111109370544217506941658960408";
    "07198403850962455444362981230987879927244284909188";
    "84580156166097919133875499200524063689912560717606";
    "05886116467109405077541002256983155200055935729725";
    "71636269561882670428252483600823257530420752963450";
  ] |>
  List.map list_of_string |>
  List.concat |>
  List.map (fun c -> int_of_char c - 48)

let p8 () =
  let take n xs =
    let rec aux acc i = function
      | [] -> []
      | x::xs -> if i <= 0 then acc else aux (x::acc) (i - 1) xs
    in aux [] n xs
  in
  let max_sublist len xs =
    let rec aux max = function
      | [] -> max
      | x::xs as l ->
        let product = take len l |> List.fold_left ( * ) 1 in
        if product > max then aux product xs else aux max xs
    in aux 0 xs
  in p8_data () |> max_sublist 13

let p9 () =
  let pow2 x = x * x in
  let is_solution a b = (let c = 1000 - a - b in pow2 a + pow2 b = pow2 c && c > a && c > b) in
  let nums =
    foldl_range 0 1000 (fun xs i ->
        foldl_range i 1000 (fun ys j ->
            if is_solution i j then (i,j)::ys else ys
          ) xs
      ) []
  in
  List.hd nums |> fun (a,b) -> a * b * (1000 - a - b)

let p10 () =
  let primes = 
    foldl_range 2 2_000_000 (fun xs i ->
        if is_prime i then i::xs else xs
      ) []
  in
  List.fold_left (+) 0 primes

let all () =
  let open Printf in
  p1  () |> printf "01 %d\n";
  p2  () |> printf "02 %d\n";
  p3  () |> printf "03 %d\n";
  p4  () |> printf "04 %d\n";
  p5  () |> printf "05 %d\n";
  p6  () |> printf "06 %d\n";
  p7  () |> printf "07 %d\n";
  p8  () |> printf "08 %d\n";
  p9  () |> printf "09 %d\n";
  p10 () |> printf "10 %d\n";;

let () =
  let open Printf in
  p10 () |> printf "10 %d\n";


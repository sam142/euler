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

let take n xs =
  let rec aux acc i = function
    | [] -> []
    | x::xs -> if i <= 0 then acc else aux (x::acc) (i - 1) xs
  in aux [] n xs

let rec zip xs ys = match (xs, ys) with
  | ([], []) -> []
  | (x::xs, []) -> []
  | ([], y::ys) -> []
  | (x::xs, y::ys) -> (x,y)::zip xs ys

let max_sublist_prod len xs =
  let rec aux max = function
    | [] -> max
    | x::xs as l ->
      let product = take len l |> List.fold_left ( * ) 1 in
      if product > max then aux product xs else aux max xs
  in aux 0 xs

let swap (x,y) = (y,x)

let rec digits n =
  if n < 10 then [n] else n mod 10 :: digits (n / 10)

let mul_digits s xs =
  let rec aux carry = function 
    | [] -> if carry = 0 then [] else digits carry
    | x::xs ->
      let n = s * x + carry in
      let digit = n mod 10 in
      let carry = n / 10 in
      digit :: aux carry xs
  in aux 0 xs

let sum xs = List.fold_left (+) 0 xs

let divisor_sum n =
  foldl_range 1 (n/2) (fun sum i ->
      if n mod i = 0 then sum + i else sum
    ) 0

let rec scalar_add_digits s = function
  | [] -> if s = 0 then [] else digits s
  | x::xs ->
    let n = x + s in
    let digit = n mod 10 in
    let carry = n / 10 in
    digit :: scalar_add_digits carry xs

let add_digits a b =
  let rec aux carry = function
    | ([], y) -> scalar_add_digits carry y
    | (x, []) -> scalar_add_digits carry x
    | (x::xs, y::ys) ->
      let n = x + y + carry in
      let digit = n mod 10 in
      let carry = n / 10 in
      digit :: aux carry (xs,ys)
  in aux 0 (a,b)

let p1 () =
  let mul_3_5 n = n mod 3 = 0 || n mod 5 = 0 in
  List.(list_of_range 1 999 |> filter mul_3_5 |> fold_left (+) 0)

let p2 () =
  fibs_upto 4_000_000 |>
  List.filter (fun x -> x mod 2 = 0) |>
  sum

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
  p8_data () |> max_sublist_prod 13

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
  (* brute force is fast enough, but a proper sieving algorithm would be nicer *)
  let primes = 
    foldl_range 2 2_000_000 (fun xs i ->
        if is_prime i then i::xs else xs
      ) []
  in
  sum primes

let p14 () =
  let count_collatz n =
    let rec aux count n =
      if n <= 1 then count else
      if n mod 2 = 0
      then aux (count + 1) (n / 2)
      else aux (count + 1) (3 * n + 1)
    in aux 0 n
  in
  foldl_range 1 1_000_000 (fun (cnt,start) i ->
      let next_cnt = count_collatz i in
      if next_cnt > cnt
      then (next_cnt, i)
      else (cnt, start)
    ) (0,1)
  |> snd

let p15 () =
  let memo = Hashtbl.create (20*20) in
  let rec path_count size =
    try Hashtbl.find memo size
    with Not_found -> begin match size with
        | (0,0) -> 1
        | (0,h) -> 1
        | (w,0) -> 1
        | (w,h) ->
          let count = path_count ((w - 1), h) + path_count (w, (h - 1)) in
          Hashtbl.add memo size count;
          count
      end
  in
  path_count (20,20)
    
let p16 () =
  let rec pow2_digits n acc =
    if n <= 0 then acc else pow2_digits (n - 1) (mul_digits 2 acc)
  in
  pow2_digits 1000 [1] |> sum

let p20 () =
  let rec fact_digits n acc =
    if n <= 0 then acc else fact_digits (n - 1) (mul_digits n acc)
  in
  fact_digits 100 [1] |> sum

let p21 () =
  foldl_range 0 9999 (fun sum a ->
      let b = divisor_sum a in
      if a > b && divisor_sum b = a
      then sum + a + b
      else sum
    ) 0

let p23 () =
  let bound = 28123 in
  let is_abundant n = divisor_sum n > n in
  let abundants = list_of_range 0 bound |> List.filter is_abundant |> Array.of_list in
  let abundant_sums = Hashtbl.create (10 * Array.length abundants) in
  for i = 0 to (Array.length abundants - 1) do
    for j = i to (Array.length abundants - 1) do
      let sum = abundants.(i) + abundants.(j) in
      if sum <= bound then
        Hashtbl.add abundant_sums sum ();
    done
  done;
  foldl_range 0 bound (fun sum i ->
      try
        let () = Hashtbl.find abundant_sums i in
        sum
      with Not_found -> sum + i
    ) 0

exception Result of int list

let p24 () =
  let rec lex_perm_count cnt perm = 
    if List.length perm < 10 then
      foldl_range 0 9 (fun cnt i ->
          if List.mem i perm
          then cnt
          else lex_perm_count cnt (i::perm)) cnt
    else
    if cnt <= 1 then raise (Result perm) else (cnt - 1)
  in
  try lex_perm_count 1_000_000 []
  with Result perm -> perm |> List.rev |> List.fold_left (fun num i -> num * 10 + i) 0

let p25 () =
  let fib_digits len =
    let rec aux n a b =
      if List.length b >= len then n else aux (n + 1) b (add_digits a b)
    in aux 2 [1] [1]
  in fib_digits 1000

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
  p10 () |> printf "10 %d\n";
  p14 () |> printf "14 %d\n";
  p15 () |> printf "15 %d\n";
  p16 () |> printf "16 %d\n";
  p20 () |> printf "20 %d\n";
  p21 () |> printf "21 %d\n";
  p23 () |> printf "23 %d\n";
  p24 () |> printf "24 %d\n";;

let x =
  let open Printf in
  p25 () |> printf "25 %d\n";;


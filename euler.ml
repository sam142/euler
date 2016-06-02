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

let split n xs =
  let rec aux acc i = function
    | [] -> (acc,[])
    | x::xs as l -> if i <= 0 then (acc,l) else aux (x::acc) (i - 1) xs
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

let int_of_digits ds =
  List.fold_left (fun num i -> num * 10 + i) 0 ds

let scalar_mul_digits s xs =
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

let pow a b =
  let rec pow acc a b =
    if b < 1 then acc else pow (a * acc) a (b - 1)
  in pow 1 a b

let remdups xs =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> xs |> List.filter ((<>) x) |> aux (x::acc)
  in aux [] xs |> List.rev

let next_multiset ds =
  let rec aux i carry = function
    | [] -> foldl_range 0 i (fun tail _ -> 0::tail) []
    | d::ds ->
      let digit = (d + carry) mod 10 and carry = (d + carry) / 10 in
      if carry < 1
      then foldl_range 0 i (fun tail _ -> digit::tail) ds
      else aux (i + 1) carry ds
  in aux 0 1 ds

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
    if n <= 0 then acc else pow2_digits (n - 1) (scalar_mul_digits 2 acc)
  in
  pow2_digits 1000 [1] |> sum

let p20 () =
  let rec fact_digits n acc =
    if n <= 0 then acc else fact_digits (n - 1) (scalar_mul_digits n acc)
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
  with Result perm -> perm |> List.rev |> int_of_digits 

let p25 () =
  let fib_digits len =
    let rec aux n a b =
      if List.length b >= len then n else aux (n + 1) b (add_digits a b)
    in aux 2 [1] [1]
  in fib_digits 1000

let p26 () =
  let rec reci_cycle len rem rems n =
    let rem = rem mod n in
    if List.mem rem rems then len else reci_cycle (len + 1) (rem * 10) (rem::rems) n
  in
  foldl_range 1 999 (fun (max, d) i ->
      let cycle = reci_cycle 1 10 [] i in
      if cycle > max then (cycle, i) else (max, d)
    ) (0, 0)
  |> snd

let p27 () =
  let rec prime_seq a b n =
    if is_prime (n * n + a * n + b) then prime_seq a b (n + 1) else n
  in
  foldl_range (-999) 999 (fun res i ->
      foldl_range (-999) 999 (fun (max, a, b) j ->
          let seq = prime_seq i j 0 in
          if seq > max then (seq, i, j) else (max, a, b)
        ) res
    ) (0, 0, 0)
  |> (fun (_, a, b) -> a * b)

let p28 () =
  let rec diag_sum n =
    if n <= 1 then 1 else
      let area = n * n in
      let sum = foldl_range 0 3 (fun sum i -> sum + area - i * (n - 1)) 0 in
      sum + diag_sum (n - 2)
  in diag_sum 1001

let p29 () =
  let run_length = function 
    | x::xs -> 
      (* ugh :( *)
      let rec aux n cur acc = function
        | [] -> (n, cur)::acc
        | x::xs ->
          if x = cur
          then aux (n + 1) cur acc xs
          else aux 1 x ((n, cur)::acc) xs
      in
      aux 1 x [] xs
    | [] -> []
  in
  let pf_pow factors e = List.map (fun (c,p) -> (c*e, p)) factors in
  let primes =
    list_of_range 2 100 |> List.map (fun i -> i |> prime_factors |> run_length)
  in
  let counter = Hashtbl.create (100*100) in
  for e = 2 to 100 do
    List.iter (fun p ->
        Hashtbl.replace counter (pf_pow p e) ()
      ) primes
  done;
  Hashtbl.length counter

let p30 () =
  let pow5_sum ds = ds |> List.map (fun d -> pow d 5) |> sum in
  let rec is_solution ds =
    let pow5_digits = ds |> pow5_sum |> digits in
    let sort = List.sort compare in
    (sort ds) = (sort pow5_digits)
  in
  let find_solutions max_length =
    let rec aux acc ds = 
      let acc = (if is_solution ds then ds::acc else acc) in
      let next = next_multiset ds in
      if List.length next > max_length then acc else aux acc next
    in aux [] [2]
  in find_solutions 6 |> List.map pow5_sum |> sum

let p31 () =
  let find_solutions target coins =
    let rec aux min acc value =
      if value > target then acc else
      if value = target then (acc + 1) else
        coins |> 
        List.filter ((<=) min) |> 
        List.fold_left (fun acc coin -> aux coin acc (value + coin)) acc
    in aux 0 0 0
  in find_solutions 200 [1;2;5;10;20;50;100;200]

let p32 () =
  let collect_pandigital ds acc =
    foldl_range 1 4 (fun acc i ->
        foldl_range 1 4 (fun acc j ->
            let (a,ds) = split i ds in
            let (b,ds) = split j ds in
            let a = int_of_digits a
            and b = int_of_digits b
            and ds = int_of_digits ds
            in if a * b = ds then ds::acc else acc
          ) acc
      ) acc
  in
  let rec collect_solutions acc perm =
    if List.length perm < 9 then
      foldl_range 1 9 (fun acc i ->
          if List.mem i perm
          then acc
          else collect_solutions acc (i::perm)
        ) acc
    else
      collect_pandigital perm acc
  in collect_solutions [] [] |> remdups |> sum

let p33 () =
  let rec gcd a b =
    let a = max a b and b = min a b in
    if a mod b = 0 then b else gcd (a - b) b
  in
  let id x = x in
  let mem_of f xs ys = List.filter (fun y -> f (List.mem y xs)) ys in
  let not_zero = List.filter ((<>) 0) in
  foldl_range 10 99 (fun acc i ->
      foldl_range (i+1) 99 (fun acc j ->
          let dis = digits i |> not_zero and djs = digits j |> not_zero in
          let common = dis |> mem_of id djs  in
          if common = [] then acc else
            let dis = dis |> mem_of not common in
            let djs = djs |> mem_of not common in
            let ls = List.map (( * ) i) djs in
            let rs = List.map (( * ) j) dis in
            if List.exists (fun l -> List.mem l rs) ls
            then (i,j)::acc
            else acc
        ) acc
    ) [] |>
  List.fold_left (fun (pn, pd) (n, d) -> (pn*n, pd*d)) (1,1) |>
  fun (n, d) -> d / gcd n d

let p34 () =
  let rec fact i = if i <= 1 then 1 else i * fact (i - 1) in
  let fact_sum ds = ds |> List.map fact |> sum in
  let sort = List.sort compare in
  let is_solution ds = (fact_sum ds |> digits |> sort) = (ds |> sort) in
  let rec find_solutions acc ds max_length =
    let next = next_multiset ds in
    if List.length ds > max_length then acc else
      let acc = if is_solution ds then ds::acc else acc
      in find_solutions acc next max_length
  in find_solutions [] [3] 7 |> List.map fact_sum |> sum

let p35 () =
  let rotate = function 
    | [] -> []
    | x::xs -> xs @ [x]
  in
  let is_circular_prime ds =
    let rec aux acc i ds =
      if i <= 0 then acc else
        aux (acc && int_of_digits ds |> is_prime)  (i - 1) (rotate ds)
    in aux true (List.length ds) ds
  in
  foldl_range 1 999_999 (fun cnt i ->
      if is_circular_prime (digits i) then cnt + 1 else cnt
    ) 0

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
  p24 () |> printf "24 %d\n";
  p25 () |> printf "25 %d\n";
  p26 () |> printf "26 %d\n";
  p27 () |> printf "27 %d\n";
  p28 () |> printf "28 %d\n";
  p29 () |> printf "29 %d\n";
  p30 () |> printf "30 %d\n";
  p31 () |> printf "31 %d\n";
  p32 () |> printf "32 %d\n";
  p33 () |> printf "33 %d\n";
  p34 () |> printf "34 %d\n";;

let last =
  let open Printf in
  p35 () |> printf "35 %d\n";;


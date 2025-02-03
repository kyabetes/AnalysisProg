let is_after (m1:int) (d1:int) (y1:int) 
             (m2:int) (d2:int) (y2:int) : bool =
  if y1 > y2 then true
  else if y1 < y2 then false
  else
    (* years are the same so compare months! *)
    if m1 > m2 then true
    else if m1 < m2 then false
    else
      (* months are the same; compare days *)
      d1 > d2

(* number of days in each month, ignoring leap years. 
   index 0 = january, 1 = february, ..., 11 = december. *)
let days_in_month = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]

let days_of_month (m:int) : int =
  days_in_month.(m - 1)

let days_left (month:int) (day:int) : int =
  let rec aux (m:int) (d:int) (acc:int) : int =
    if m > 12 then acc  (* should not really happen if inputs are valid *)
    else if m = 12 then
      (* remaining days in December = 31 - d *)
      acc + (days_of_month m - d)
    else
      (* add the remainder of this month, then recurse to the next one*)
      let remainder = days_of_month m - d in
      aux (m + 1) 0 (acc + remainder)
  in
  aux month day 0


let is_prime (num:int) : bool =
  let rec check_divisor (n:int) (d:int) : bool =
    if d * d > n then true          (* no divisors found => prime *)
    else if (n mod d) = 0 then false
    else check_divisor n (d + 1)
  in
  check_divisor num 2


(* given a row (list of int), produce the next row in Pascal's triangle.
   example: if row = [1; 4; 6; 4; 1], then next_row row = [1; 5; 10; 10; 5; 1] 
    this one was a bit tricky, but I think I figured it out. if there are any 
      inefficiencies here I apologize! *)
let next_row (row:int list) : int list =
  let rec pair_sums (lst:int list) (acc:int list) : int list =
    match lst with
    | x :: y :: tl -> pair_sums (y :: tl) ((x + y) :: acc)
    | _ -> List.rev acc
  in
  match row with
  | [] -> [1]
  | _ ->
      let middle_sums = pair_sums row [] in
      1 :: (middle_sums) @ [1]


let pascal (j:int) (k:int) : int =
  let rec build_row (n:int) (acc:int list) : int list =
    if n = 0 then acc
    else
      (* compute next row from 'acc' and continue *)
      let new_row = next_row acc in
      build_row (n - 1) new_row
  in
  let final_row = build_row j [1] in
  List.nth final_row k


(****************************************************************
  TESTS
****************************************************************)

let () =
  print_endline "tests for is_after";
  assert (is_after 2  1 2025  1 31 2025  = true);   (* Feb 1 2025 after Jan 31 2025 *)
  assert (is_after 1 31 2025  2  1 2025  = false);  (* Jan 31 2025 before Feb 1 2025 *)
  assert (is_after 12 31 2025  1  1 2024 = true);   (* year 2025 after year 2024 *)
  assert (is_after 12 31 2024  1  1 2025 = false);  (* year 2024 before year 2025 *)
  assert (is_after 5  5  2025  5  5  2025  = false); (* same date => not "after" *)
  print_endline "is_after tests passed.";

  print_endline "\n tests for days_left";
  assert (days_left 12 31 = 0);   (* no day left in the year *)
  assert (days_left 1 1   = 364); (* Jan 1 => 364 days remain in a 365-day year *)
  (* some intermediate checks *)
  assert (days_left 12 30 = 1);   (* only Dec 31 left *)
  assert (days_left 11 30 = 31);

  print_endline "days_left tests passed.";

  print_endline "\n tests for is_prime ";
  (* Known primes >= 2 *)
  assert (is_prime 2  = true);
  assert (is_prime 3  = true);
  assert (is_prime 4  = false);
  assert (is_prime 5  = true);
  assert (is_prime 16 = false);
  assert (is_prime 17 = true);
  assert (is_prime 19 = true);
  assert (is_prime 20 = false);
  (* a slightly bigger one, 29 is prime *)
  assert (is_prime 29 = true);
  print_endline "is_prime tests passed.";

  print_endline "\n----- Tests for pascal -----";
  (* as described in the problem statement, the first 6 rows: 
      row 0: [1]
      row 1: [1; 1]
      row 2: [1; 2; 1]
      row 3: [1; 3; 3; 1]
      row 4: [1; 4; 6; 4; 1]
      row 5: [1; 5; 10; 10; 5; 1]
   *)
  assert (pascal 0 0 = 1); 
  assert (pascal 1 0 = 1);
  assert (pascal 1 1 = 1);
  assert (pascal 2 1 = 2);
  assert (pascal 3 2 = 3);
  assert (pascal 4 2 = 6);
  assert (pascal 5 0 = 1);
  assert (pascal 5 2 = 10);
  assert (pascal 5 5 = 1);
  (* another row check: pascal 5 3 = 10 *)
  assert (pascal 5 3 = 10);

  print_endline "pascal tests passed.";

  print_endline "\nAll tests passed successfully!"

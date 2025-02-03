(* 
     given an int list lst, returns a list containing only the odd numbers
     from lst, in the same order. tail-recursive.
*)

let odds lst =
  let rec aux acc = function
    | [] -> List.rev acc  (* this reverses the accumulator to restore order *)
    | x :: xs ->
        if x mod 2 <> 0 then aux (x :: acc) xs
        else aux acc xs
  in
  aux [] lst

(*
     given an int num and an int list lst, returns the index (0-based)
     of the first occurrence of num in lst. if num is not in lst, returns -1.
   tail-recursive implementation. :)
*)
let first_index num lst =
  let rec aux idx = function
    | [] -> -1
    | x :: xs ->
        if x = num then idx else aux (idx + 1) xs
  in
  aux 0 lst

(* 
     given an int list lst, returns a list of the partial sums.
     for example, partial_sum [2; -7; 100] returns [2; -5; 95].
   again, tail-recursive implementation.
*)
let partial_sum lst =
  let rec aux current acc = function
    | [] -> List.rev acc  (* reverse to produce the result in the original order *)
    | x :: xs ->
        let new_sum = current + x in
        aux new_sum (new_sum :: acc) xs
  in
  aux 0 [] lst

  (* helper. given a row (list of ints), produce the next row in pascal's triangle *)
let next_row (row : int list) : int list =
  let rec pair_sums lst acc =
    match lst with
    | x :: y :: tl -> pair_sums (y :: tl) ((x + y) :: acc)
    | _ -> List.rev acc
  in
  1 :: ((pair_sums row []) @ [1])

(* pascal_tr j k is the number at row j and column k in pascal's triangle.
   this function is tail recursive, because the helper function build_row is tail recursive *)
let pascal_tr (j : int) (k : int) : int =
  let rec build_row (n : int) (row : int list) : int list =
    if n = 0 then row
    else
      let new_row = next_row row in
      build_row (n - 1) new_row
  in
  let final_row = build_row j [1] in
  List.nth final_row k

(*
   test cases
   I used assert to verify that the functions behave as expected.
*)
let () =
  (* tests for odds *)
  assert (odds [2; 3; (-2); (-1)] = [3; (-1)]);
  assert (odds [20; 0; (-4)] = []);
  assert (odds [] = []);
  assert (odds [1; 2; 3; 4; 5] = [1; 3; 5]);

  (* tests for first_index *)
  assert (first_index 20 [20; 15; 20] = 0);
  assert (first_index 15 [20; 15; 20] = 1);
  assert (first_index 0 [20; 15; 20] = -1);
  assert (first_index 5 [] = -1);
  assert (first_index (-1) [1; (-1); 2; (-1)] = 1);

  (* tests for partial_sum *)
  assert (partial_sum [2; (-7); 100] = [2; (-5); 95]);
  assert (partial_sum [] = []);
  assert (partial_sum [0; 0; 0] = [0; 0; 0]);
  assert (partial_sum [1; 2; 3; 4] = [1; 3; 6; 10]);
  assert (partial_sum [-1; -2; -3] = [-1; -3; -6]);

  (* tests for pascal_tr *)
  assert (pascal_tr 0 0 = 1);                       (* row 0: [1] *)
  assert (pascal_tr 1 0 = 1);                       (* row 1: [1; 1] *)
  assert (pascal_tr 1 1 = 1);                       (* row 1: [1; 1] *)
  assert (pascal_tr 2 0 = 1);                       (* row 2: [1; 2; 1] *)
  assert (pascal_tr 2 1 = 2);                       (* row 2: [1; 2; 1] *)
  assert (pascal_tr 2 2 = 1);                       (* row 2: [1; 2; 1] *)
  assert (pascal_tr 3 1 = 3);                       (* row 3: [1; 3; 3; 1] *)
  assert (pascal_tr 3 2 = 3);                       (* row 3: [1; 3; 3; 1] *)
  assert (pascal_tr 4 2 = 6);                       (* row 4: [1; 4; 6; 4; 1] *)
  assert (pascal_tr 5 0 = 1);                       (* row 5: [1; 5; 10; 10; 5; 1] *)
  assert (pascal_tr 5 2 = 10);                      (* row 5: [1; 5; 10; 10; 5; 1] *)
  assert (pascal_tr 5 4 = 5);                       (* row 5: [1; 5; 10; 10; 5; 1] *)
  assert (pascal_tr 6 3 = 20);
  
  print_endline "All tests passed!!!" 

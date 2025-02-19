include Json

 open Json_structures.Parsed_complete_bus
 open Json_structures.Parsed_medium_bus
 open Json_structures.Parsed_small_bus
 
 (* provided helper function that deduplicates a list *)
 let dedup xs = List.sort_uniq compare xs
 
 (* provided helper function that sorts a given list *)
 let sort xs = List.sort compare xs
 
 (* provided helper function to convert a float to a string *)
 let json_string_of_float f =
   Printf.sprintf "%g" f
   
 (* 1: this function creates a JSON array of objects. Each object has two fields:
    - "n": a JSON number counting down from i.0 to 1.0 (using float_of_int)
    - "b": always True
 *)
 let rec make_silly_list n =
   match n with
   | 0 -> []
   | _ -> Object [("n", Num (float_of_int n)); ("b", True)] :: make_silly_list (n - 1)
 
 let make_silly_json i =
   Array (make_silly_list i)
 
 (* 2 *)
 (*
   concat_with takes a separator string and a list of strings.
   it returns a single string that concatenates the list's strings,
   inserting the separator only between elements (not at the beginning or end).
*)
 let rec concat_with (sep, ss) =
  match ss with
  | [] -> ""
  | [s] -> s
  | s :: rest -> s ^ sep ^ concat_with (sep, rest)
 
 (* 3: quote_string takes a string and returns a new string with a
 double quote character added to both the beginning and the end *)
 let quote_string s =
  "\"" ^ s ^ "\""
 
 (* 4: string_of_json converts a JSON value into its string representation. *)
 let rec string_of_json j =
  match j with
  | Num f -> json_string_of_float f
  | String s -> quote_string s
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array js ->
      let elements = List.map string_of_json js in
      "[" ^ concat_with (", ", elements) ^ "]"
  | Object kvs ->
      let string_of_pair (k, v) =
        quote_string k ^ " : " ^ string_of_json v
      in
      let pairs = List.map string_of_pair kvs in
      "{" ^ concat_with (", ", pairs) ^ "}"
 
(* 5: take: returns the first n elements of xs in the same order as xs *)
let rec take (n, xs) =
  match n, xs with
  | 0, _ -> []
  | _, [] -> [] 
  | n, x :: xs' -> x :: take (n - 1, xs')

(* 6: firsts: returns a list of the first components of each pair in the same order as the list *)
let rec firsts xs =
  match xs with
  | [] -> []
  | (x, _) :: rest -> x :: firsts rest
 
(* 7: comment regarding firsts (take (n, xs)) vs. take (n, firsts xs) -

   They always yield the same list because taking the first n pairs from xs 
   and then extracting their first components is the same as extracting all 
   first components of xs and then taking the first n of those.

   Firsts (take (n, xs)) might be faster since it processes only the first n pairs,
   whereas firsts xs processes the entire list before taking the first n elements.
*)
 
(* 8: assoc: returns the value associated with key k from a list of pairs.  If k is not found, it returns None.
*)
   let rec assoc (k, xs) =
    match xs with
    | [] -> None
    | (k1, v1) :: rest ->
        match k = k1 with
        | true -> Some v1
        | false -> assoc (k, rest)
 
(* 9: dot: returns the value associated with field f in a JSON object.
   If j is an object that has a field named f, then returns Some v;
   otherwise, returns None.
*)
let dot (j, f) =
  match j with
  | Object kvs -> assoc (f, kvs)
  | _ -> None
 
(* 10: dots: traverses a JSON object along an access path (list of field names).
   It returns Some v if the path exists, or None if any access fails.
*)
   let rec dots (j, fs) =
    match fs with
    | [] -> Some j
    | f :: rest ->
        match dot (j, f) with
        | None -> None
        | Some v -> dots (v, rest)
 
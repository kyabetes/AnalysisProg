open Hw3
open Json

(* Test cases for make_silly_json *)
let test1_1 =
  make_silly_json 1 =
  Array [Object [("n", Num 1.); ("b", True)]]

let test1_2 =
  make_silly_json 2 =
  Array [Object [("n", Num 2.); ("b", True)];
         Object [("n", Num 1.); ("b", True)]]

let test1_3 =
  make_silly_json 3 =
  Array [Object [("n", Num 3.); ("b", True)];
         Object [("n", Num 2.); ("b", True)];
         Object [("n", Num 1.); ("b", True)]]

let test1_4 =
  make_silly_json 0 = Array [] (* Edge case: empty array *)

let test1_5 =
  make_silly_json 4 =
  Array [Object [("n", Num 4.); ("b", True)];
         Object [("n", Num 3.); ("b", True)];
         Object [("n", Num 2.); ("b", True)];
         Object [("n", Num 1.); ("b", True)]]

(* Test cases for concat_with *)
let test2_1 =
  concat_with (";", ["1"; "2"]) = "1;2"

let test2_2 =
  concat_with (", ", ["red"; "orange"; "yellow"]) = "red, orange, yellow"

let test2_3 =
  concat_with (" | ", ["OCaml"; "is"; "odd"]) = "OCaml | is | odd"

let test2_4 =
  concat_with ("-", []) = "" (* Edge case: empty list *)

let test2_5 =
  concat_with ("+", ["single"]) = "single" (* Edge case: single element *)

(* Test cases for quote_string *)
let test3_1 =
  quote_string "hello" = "\"hello\""

let test3_2 =
  quote_string "" = "\"\"" (* Edge case: empty string *)

let test3_3 =
  quote_string "I prefer Python." = "\"I prefer Python.\""

let test3_4 =
  quote_string "123" = "\"123\""

let test3_5 =
  quote_string "OCaml" = "\"OCaml\""

(* Sample JSON object for testing string_of_json *)
let json_obj =
  Object [("foo", Num 3.14159);
          ("bar", Array [Num 1.; String "world"; Null]);
          ("ok", True)]

(* Test cases for string_of_json *)
let test4_1 =
  string_of_json json_obj =
  "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}"

let test4_2 =
  string_of_json (Num 42.0) = "42"

let test4_3 =
  string_of_json (String "Hello") = "\"Hello\""

let test4_4 =
  string_of_json (Array []) = "[]" (* Edge case: empty array *)

let test4_5 =
  string_of_json (Object []) = "{}" (* Edge case: empty object *)

(* Test cases for take *)
let test5_1 = take (2, [4; 5; 6; 7]) = [4; 5]
let test5_2 = take (3, ["a"; "b"; "c"; "d"]) = ["a"; "b"; "c"]
let test5_3 = take (0, [1; 2; 3]) = [] (* Edge case: take 0 elements *)
let test5_4 = take (4, [1; 2; 3; 4]) = [1; 2; 3; 4] (* Full list *)
let test5_5 = take (5, [1; 2; 3]) = [1; 2; 3] (* Count exceeds list length *)

(* Test cases for firsts *)
let test6_1 = firsts [(1,2); (3,4)] = [1; 3]
let test6_2 = firsts [(10,"a"); (20,"b"); (30,"c")] = [10; 20; 30]
let test6_3 = firsts [] = [] (* Edge case: empty list *)
let test6_4 = firsts [(42, true)] = [42] (* Single pair *)
let test6_5 = firsts [(false, "no"); (true, "yes")] = [false; true]

(* Test cases for assoc *)
let test8_1 = assoc ("foo", [("bar", 17); ("foo", 19)]) = Some 19
let test8_2 = assoc ("baz", [("bar", 17); ("foo", 19)]) = None (* Key not found *)
let test8_3 = assoc ("x", []) = None (* Edge case: empty list *)
let test8_4 = assoc ("a", [("a", 1); ("a", 2); ("a", 3)]) = Some 1 (* First match *)
let test8_5 = assoc ("bar", [("bar", 0); ("bar", -1)]) = Some 0

(* Test cases for dot *)
let test9_1 = dot (json_obj, "ok") = Some True
let test9_2 = dot (json_obj, "foo") = Some (Num 3.14159)
let test9_3 = dot (json_obj, "missing") = None (* Non-existent key *)
let test9_4 = dot (Num 5.0, "foo") = None (* Edge case: dot on non-object *)
let test9_5 =
  dot (Object [("nested", Object [("inner", String "value")])], "nested")
  = Some (Object [("inner", String "value")])

(* Test cases for dots *)
let test10_1 =
  dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"])
  = Some (String "gotcha")

let test10_2 =
  dots (Object [("f", Object [("h", String "nope")])], ["f"; "g"])
  = None (* Wrong path *)

let test10_3 =
  dots (Object [("x", Object [("y", Object [("z", Num 99.)])])], ["x"; "y"; "z"])
  = Some (Num 99.)

let test10_4 =
  dots (Num 42., ["x"]) = None (* Edge case: dots on non-object *)

let test10_5 =
  (* Test dots with an empty key list; expect to get the original JSON *)
  dots (json_obj, []) = Some json_obj

(* Run tests *)
let () =
  (* Part 0 tests *)
  assert test1_1;
  assert test1_2;
  assert test1_3;
  assert test1_4;
  assert test1_5;

  (* Part 1 tests *)
  assert test2_1;
  assert test2_2;
  assert test2_3;
  assert test2_4;
  assert test2_5;

  assert test3_1;
  assert test3_2;
  assert test3_3;
  assert test3_4;
  assert test3_5;

  assert test4_1;
  assert test4_2;
  assert test4_3;
  assert test4_4;
  assert test4_5;

  (* Part 2 tests *)
  assert test5_1;
  assert test5_2;
  assert test5_3;
  assert test5_4;
  assert test5_5;

  assert test6_1;
  assert test6_2;
  assert test6_3;
  assert test6_4;
  assert test6_5;

  assert test8_1;
  assert test8_2;
  assert test8_3;
  assert test8_4;
  assert test8_5;

  assert test9_1;
  assert test9_2;
  assert test9_3;
  assert test9_4;
  assert test9_5;

  assert test10_1;
  assert test10_2;
  assert test10_3;
  assert test10_4;
  assert test10_5;

  print_endline "all tests passed!!!!!!!!!"

(* CSE 341, Winter 2021, HW2 Provided Code *)

(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* helper function that deduplicate the list *)
let dedup ls = List.sort_uniq compare ls

(* helper function that sort a given list *)
let sort ls = List.sort compare ls

(* Part 1: Printing JSON values *)
(* 1 *)
let make_silly_json (i: int) : json =
        let rec make_silly_json_helper (i: int) (xs : json) : json= 
                if i = 0 then xs
                else match  xs with 
                        Array ss ->  make_silly_json_helper (i-1) (Array (Object [("n", Num (float_of_int i));("b", True)] :: ss ))
                        | _ -> failwith "illegal argument exception"
        in make_silly_json_helper i (Array [ Object [] ]);;


(* Part 2: Processing JSON values *)
(* Don't forget to write a comment for problems 7 and 20. *)
(* 2 *)
let rec concat_with (sep : string) (ss : string list) : string =
  failwith "Need to implement:  concat_with"

(* 3 *)
let quote_string (s : string) : string =
  failwith "Need to implement:  quote_string"

(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job. *)
let json_string_of_float (f : float) : string =
  Printf.sprintf "%g" f

(* 4 *)
let rec string_of_json (j : json) : string =
  failwith "Need to implement:  string_of_json"

(* 5 *)
let rec take (n: int) (l: 'a list) : 'a list =
  failwith "Need to implement: take"

(* 6 *)
let rec firsts (l: ('a * 'b) list) : 'a list =
  failwith "Need to implement: firsts"

(* 7 *)
(* write your comment here *)

(* 8 *)
let rec assoc (k : 'a) (xs: ('a * 'b) list) : 'b option =
  failwith "Need to implement: assoc"

(* 9 *)
let dot (j: json) (f: string) : json option =
  failwith "Need to implement: dot"

(* 10 *)
let rec dots (j: json) (fs: string list) : json option =
  failwith "Need to implement: dots"

(* 11 *)
let one_fields (j: json) : string list =
  failwith "Need to implement: one_fields"

(* 12 *)
let no_repeats (xs: 'a list) : bool =
  failwith "Need to implement: no_repeats"

(* 13 *)
let rec recursive_no_field_repeats (j: json) : bool =
  failwith "Need to implement: recursive_no_field_repeats"

(* 14 *)
let count_occurrences (xs: 'a list) (e: exn) : (string * int) list =
  failwith "Need to implement: count_occurrences"

(* 15 *)
let rec string_values_for_access_path (fs : string list) (js: json list) 
: (string list) =
  failwith "Need to implement: string_values_for_access_path"

(* 16 *)
let rec filter_access_path_value (fs : string list) (value : string) (js : json list)
: json list =
  failwith "Need to implement: filter_access_path_value"

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r: rect) (p: point) : bool =
  failwith "Need to implement: in_rect"

(* 18 *)
let point_of_json (j: json) : point option =
  failwith "Need to implement: point_of_json"

(* 19 *)
let rec filter_access_path_in_rect (fs : string list) (r: rect) (js: json list)
: json list =
  failwith "Need to implement: filter_access_path_in_rect"

(* Part 3: Analyzing the data *)

(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. *)
exception SortIsBroken

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude = 47.648637;
    min_longitude = -122.322099;
    max_latitude = 47.661176;
    max_longitude = -122.301019
  }

(* Creates a histogram for the given list of strings. 
 * Returns a tuple in which the first element is
 * a string, and the second is the number of times that string
 * is found. *)
let histogram (xs : string list) : (string * int) list =
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences sorted_xs SortIsBroken in
  let compare_counts ((s1 : string), (n1 : int)) ((s2 : string), (n2 : int)) : int =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs: string list) (js: json list) : (string * int) list =
  histogram (string_values_for_access_path fs js)

(* 20 *)
let complete_bus_positions_list =
  match (dot complete_bus_positions "entity") with
  | Some (Array l) -> l
  | _ -> failwith "complete_bus_positions_list"

exception Unimplemented
let route_histogram     = Unimplemented
let top_three_routes    = Unimplemented
let buses_in_ud         = Unimplemented
let ud_route_histogram  = Unimplemented
let top_three_ud_routes = Unimplemented
let all_fourty_fours    = Unimplemented




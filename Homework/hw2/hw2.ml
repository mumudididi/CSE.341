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
let make_silly_json (i : int) : json =
  let rec make_silly_json_helper (i : int) (xs : json) : json =
    if i = 0 then xs
    else
      match xs with
      | Array ss ->
          make_silly_json_helper (i - 1)
            (Array (Object [ ("n", Num (float_of_int i)); ("b", True) ] :: ss))
      | _ -> failwith "illegal argument exception"
  in
  make_silly_json_helper i (Array [])

(* Part 2: Processing JSON values *)
(* Don't forget to write a comment for problems 7 and 20. *)
(* 2 *)
let rec concat_with (sep : string) (ss : string list) : string =
  match ss with
  | [] -> ""
  | [ s ] -> s
  | s :: rest -> s ^ sep ^ concat_with sep rest

(* 3 *)
let quote_string (s : string) : string = "\"" ^ s ^ "\""

(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job. *)
let json_string_of_float (f : float) : string = Printf.sprintf "%g" f

(* 4 *)
(*Array [ Object [("n" , 1), ("b", True)] ; Object [("n" , 1), ("b", True)] ]*)
let rec string_of_json (j : json) : string =
  let rec jsArray_to_list (ll : json list) : string list =
    match ll with
    | [] -> []
    | h :: ll' -> string_of_json h :: jsArray_to_list ll'
  in
  let rec jsObject_to_list (ll : (string * json) list) : string list =
    match ll with
    | [] -> []
    | h :: rest ->
        let str, js_obj = h in
        ((quote_string str ^ " : ") ^ string_of_json js_obj)
        :: jsObject_to_list rest
  in

  match j with
  | Num n -> json_string_of_float n
  | String s -> quote_string s
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array arr -> "[" ^ concat_with ", " (jsArray_to_list arr) ^ "]"
  | Object obj_list -> "{" ^ concat_with ", " (jsObject_to_list obj_list) ^ "}"

(* 5 *)
let rec take (n : int) (l : 'a list) : 'a list =
  match n with
  | 0 -> []
  | x -> ( match l with [] -> [] | xs :: rest -> xs :: take (n - 1) rest)

(* 6 *)
let rec firsts (l : ('a * 'b) list) : 'a list =
  match l with
  | [] -> []
  | xs :: rest ->
      let a, b = xs in
      a :: firsts rest

(* 7 *)
(* write your comment here *)

(* 8 *)
let rec assoc (kba : 'a) (xs : ('a * 'b) list) : 'b option =
  match xs with
  | [] -> None
  | ele :: rest ->
      let p1, p2 = ele in
      if p1 = kba then Some p2 else assoc kba rest

(* 9 *)
let dot (j : json) (f : string) : json option =
  match j with Object obj_list -> assoc f obj_list | _ -> None

(* 10 *)
let rec dots (j : json) (fs : string list) : json option =
  match fs with
  | [] -> None
  | [ h ] -> dot j h
  | h :: rest -> (
      let j_response = dot j h in
      match j_response with None -> None | Some a -> dots a rest)

(* 11 *)


let one_fields (j : json) : string list =
  let rec gather_obj_keys (js : json) =
    match js with
    | Object obj_list -> (
        match obj_list with
        | [] -> []
        | h :: rest ->
            let k, v = h in
            k :: gather_obj_keys (Object rest))
    | _ -> []
  in

  gather_obj_keys j

(* 12 *)
let no_repeats (xs : 'a list) : bool =
  let xs' = dedup xs in
  if List.length xs' = List.length xs then true else false

(*
 *let rec dfs (js : json) : string list=
 *  match js with
 *  | Object js_obj_list -> (
 *      match js_obj_list with
 *      | [] -> []
 *      | h :: rest -> (
 *          let k, v = h in
 *          match v with
 *          | Object v_list -> dfs v @ (k :: dfs (Object rest))
 *          | _ -> k :: dfs (Object rest)))
 *  | Array js_array -> (
 *      match js_array with [] -> [] | h :: rest -> dfs h @ dfs (Array rest))
 *  | _ -> []
 *)

(* 13 *)
let rec recursive_no_field_repeats (j : json) : bool =
        let rec dfs (js : json) : string list=
    match js with
    | Object js_obj_list -> (
        match js_obj_list with
        | [] -> []
        | h :: rest -> (
            let k, v = h in
            match v with
            | Object v_list -> (dfs v )@ (k :: dfs (Object rest))
            | Array v_array ->( dfs v) @ (k :: dfs (Object rest))
            | _ -> k :: dfs (Object rest)))
    | Array js_array -> (
        match js_array with 
                |  [] -> [] 
                | h :: rest -> dfs h @ dfs (Array rest))
    | _ -> []
  in
  let names = dfs j in no_repeats names

(* 14 *)
let count_occurrences (xs : 'a list) (e : exn) : (string * int) list =
        let rec count (xs : string list)  (str: string) (num : int) : ( string * int ) list = 
                match xs with 
                        | [] ->   [(str, num)]

                        | h :: rest -> (
                                if str = h then count rest str (num+1)
                                else  if str < h then (str, num) ::  (count rest h 1)
                                else raise e
                        )
  in match xs with 
       | [] -> []
       | h :: xs' -> count xs' h 1
        
        

(* 15 *)
let rec string_values_for_access_path (fs : string list) (js : json list) :
    string list =
            match js with 
                | [] -> []
                | j :: js' -> (
                        let response  = dots j fs in 
                        match response with 
                                | Some a -> (
                                        match a with 
                                        |String s -> s :: string_values_for_access_path fs js'
                                        | _ -> string_values_for_access_path fs js'
                                
)
                                | None -> string_values_for_access_path fs js'
)

(* 16 *)
let rec filter_access_path_value (fs : string list) (value : string)
    (js : json list) : json list =
            match js with 
                | [] -> []
                | h :: js' -> (
                        let res = dots  h fs in 
                        match res with 
                              | Some s -> ( 
                                      match s with 
                                      |String s -> (if s = value then h :: filter_access_path_value fs value js'
                                                        else filter_access_path_value fs value js'
                                                        )
                                      | _ -> filter_access_path_value fs value js'


                                        )
                              | None -> filter_access_path_value fs value js'

                                )

(* Types for use in problems 17-20. *)
type rect = {
  min_latitude : float;
  max_latitude : float;
  min_longitude : float;
  max_longitude : float;
}

type point = { latitude : float; longitude : float }

(* 17 *)
let in_rect (r : rect) (p : point) : bool =
        let lat = p.latitude  in 
        let long = p.longitude in 
        (lat >= r.min_latitude) && (lat <= r.max_latitude) && (long <= r.max_longitude) && (long >= r.min_longitude)


(* 18 *)
let point_of_json (j : json) : point option =
         match (dot j "latitude" ) with 
                        |Some a -> 
                                        (match a with 
                                        | Num a_num -> (
                                                let long = dot j "longitude" in
                                                    match long with 
                                                      | Some b ->  (
                                                              match b with 
                                                              | Num b_num -> Some {latitude= a_num; longitude = b_num}
                                                                 | _ -> None
         )
                                                      | None -> None
         )
                                        | _ -> None )
                       | None -> None 

(* 19 *)
let rec filter_access_path_in_rect (fs : string list) (r : rect)
    (js : json list) : json list =
            match js with 
            | [] -> [] 
            | h :: rest -> (
                    match h with 
                        | Object obj -> (
                                let v = dots h fs in 
                                match v with 
                                        | Some v_obj -> (
                                                let v_pt = point_of_json v_obj in 
                                                match v_pt with 
                                                  | Some pt -> (
                                                           
                                                          if (in_rect r pt)  then h :: (filter_access_path_in_rect fs r rest)
                                                          else filter_access_path_in_rect fs r rest

                                                        )
                                                  | None -> (filter_access_path_in_rect fs r rest)

    )
                                        | None ->  filter_access_path_in_rect fs r rest
    )

                        | _ ->  filter_access_path_in_rect fs r rest
    )

(* Part 3: Analyzing the data *)

(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. *)
exception SortIsBroken

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  {
    min_latitude = 47.648637;
    min_longitude = -122.322099;
    max_latitude = 47.661176;
    max_longitude = -122.301019;
  }

(* Creates a histogram for the given list of strings. 
 * Returns a tuple in which the first element is
 * a string, and the second is the number of times that string
 * is found. *)
let histogram (xs : string list) : (string * int) list =
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences sorted_xs SortIsBroken in
  let compare_counts ((s1 : string), (n1 : int)) ((s2 : string), (n2 : int)) :
      int =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs : string list) (js : json list) :
    (string * int) list =
  histogram (string_values_for_access_path fs js)

(* 20 *)
let complete_bus_positions_list =
  match dot complete_bus_positions "entity" with Some (Array l) -> l | _ -> []

exception Unimplemented

let route_histogram = Unimplemented

let top_three_routes = Unimplemented

let buses_in_ud = Unimplemented

let ud_route_histogram = Unimplemented

let top_three_ud_routes = Unimplemented

let all_fourty_fours = Unimplemented

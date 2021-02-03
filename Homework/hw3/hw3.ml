(* NOTE: Uncomment the following line if you want to #use this file in utop
 * (optionally, call it from utop directly): *)
(* #mod_use "hw3types.ml";; *)


(* NOTE: to get rid off the red-wiggles in VSCode, first compile the
 * the hw3types module running this 
 * from the command line: 
       ocamlopt hw3types.ml
 *)

open Hw3types

(**** Implement the following functions per rubric: ****)

(* #1 *)
let only_lowercase : string list -> string list =
        List.filter (fun str -> str.[0] = Char.lowercase_ascii str.[0])

(* #2 *)
let longest_string1 : string list -> string =
        List.fold_left  (fun str1 str2 -> if (String.length str1) < (String.length str2) then str2 else str1 ) "" ;;


(* #3 *)
let longest_string2 : string list -> string =
        List.fold_left  (fun str1 str2 -> if (String.length str1) <= (String.length str2) then str2 else str1 ) "" ;;

(* #4 *)
let longest_string_helper (f: int ->  int -> bool) : string list -> string =
        let comp_func  (str1:string) (str2:string) = f (String.length str1) (String.length str2) in 
        List.fold_left (fun str1 str2 -> if (comp_func str1 str2) then str1 else str2)  ""

let longest_string3 : string list -> string =
        let fun_comp (num1: int) (num2: int) : bool =  num1 >= num2 in 
                longest_string_helper fun_comp;;


let longest_string4 : string list -> string =
        let fun_comp (num1: int) (num2: int) : bool =  num1 >= num2 in 
                longest_string_helper fun_comp;;

  
(* #5 *)
let longest_lowercase : string list -> string = longest_string3 % only_lowercase;;


(* #6 *)
let caps_no_X_string : string -> string =
        let split (str:string)  = String.split_on_char 'X' str in 
        let concat (str_list: string list) = String.concat "" str_list in 
        concat % (split % String.uppercase_ascii) 


(* #7 *)
let rec first_answer (f: 'a -> 'b option) (xs: 'a list) : 'b =
        match xs with 
                | [] -> raise NoAnswer
                | hd :: xs' -> (
                        let res = f hd in 
                        match res with 
                        | Some c -> c 
                        | None -> first_answer f xs'
)

(* #8 *)
let all_answers (f: 'a -> 'b list option) (xs: 'a list) : 'b list option =
        let rec unfold (ele_list: 'a list ) (lst : 'b list )  = 
                match ele_list, lst with 
                |[], [] -> (true, [])
                | [], _ -> (false,lst )
                | ele :: rest, _ -> (
                        let ele' = f ele in 
                        match ele' with 
                        | Some e ->  unfold rest (e @ lst)  

                        | None ->  ( false, [])
        )


                        in
                        let ret = unfold xs [] in match ret  with (false, []) -> None  | (_, xx) -> Some xx


(* #9 *)
let count_wildcards : pattern -> int =
  failwith "Need to implement count_wildcards"

let count_wild_and_variable_lengths (p: pattern) : int =
  failwith "Need to implement count_wild_and_variable_lengths"

let count_a_var (s: string) : pattern -> int =
  failwith "Need to implement count_a_var"

(* #10 *)
let check_pat (pat: pattern) : bool =
  failwith "Need to implement check_pat"

(* #11 *)
let rec matches (v: valu) (pat: pattern) : (string * valu) list option =
  failwith "Need to implement matches"

(* #12 *)
let first_match (v: valu) (patlst: pattern list) : (string * valu) list option =
  failwith "Need to implement first_match"

(* optional challenge problem  *)

let typecheck_patterns (t: (string * string * typ) list) (patlst: pattern list) 
: typ option =
  failwith "Need to implement typecheck_patterns"

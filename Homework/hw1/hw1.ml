exception Unimplemented

(**
 * TODO: Complete these function bindings:
 *)

(* 1 *)
(*
 *let is_older (date1 : int * (int * int)) (date2 : int * (int * int)) : bool =
 *  if snd (snd date1) < snd (snd date2) then true
 *  else if snd (snd date1) = snd (snd date2) && fst (snd date1) < fst (snd date2)
 *  then true
 *  else if
 *    snd (snd date1) = snd (snd date2)
 *    && fst (snd date1) = fst (snd date2)
 *    && fst date1 < fst date2
 *  then true
 *  else false
 *)

 let is_older (date1 : int * (int * int)) (date2 : (int * (int * int))) : bool = 
  let ymd1 = fst(date1) + fst(snd date1)*100 + snd(snd date1)*10000 in
  let  ymd2 = fst(date2) + fst(snd date2) * 100 + snd(snd date2) * 10000 in 
  ymd1 < ymd2;; 

(* 2 *)
let rec number_in_month (dates : (int * (int * int)) list) (month : int) : int =
        if dates = [] then 0 
        else if fst(snd (List.hd dates )) = month then 1 + number_in_month (List.tl dates) month
        else number_in_month (List.tl dates) month;;

(* 3 *)
let rec number_in_months (dates : (int * (int * int)) list) (months : int list)
    : int = 
            (*[(1,(1,2001));(1,(2,2001)); (1,(2,2002))] [1,2]*)
            if months = [] || dates = [] then 0
            else (number_in_month dates (List.hd months)) +  number_in_months dates (List.tl months);;

(* 4 *)
let rec dates_in_month (dates : (int * (int * int)) list) (month : int) :
    (int * (int * int)) list =
            (* [3,2,3,4] 3*)
            if dates = [] then []
            else if fst( snd ( List.hd dates )) = month then List.hd dates :: dates_in_month (List.tl dates) month
            else dates_in_month (List.tl dates) month 

(* 5 *)
let rec dates_in_months (dates : (int * (int * int)) list) (months : int list) :
    (int * (int * int)) list =
            if dates=[] || months=[] then [] 
            else( dates_in_month dates (List.hd months) ) @ dates_in_months dates (List.tl months);;

(* 6 *)
let rec get_nth (xs : string list) (n : int) : string = 
        if n =1 then List.hd xs
        else get_nth (List.tl xs) (n-1)

(* 7 *)
let string_of_date (date : int * (int * int)) : string = 
        let months_string = ["January"; "Februray";"March";"April";"May";"June";"July";"August";"September";"October";"November";"December"] in 
        let month = get_nth months_string (fst(snd date))in 
        let day = string_of_int (fst date) in 
        let year = string_of_int (snd ( snd date )) in 
        let sepatator = "-" in
        month ^ sepatator ^ day^ sepatator ^ year

(* 8: *)
let rec number_before_reaching_sum (sum : int) (xs : int list) : int =
        if xs = [] then 0
        (*(10, [11]) -> 0 *)
        else if List.hd xs >=  sum then 0
        (*(10, [9]) -> 0*)
        else if  List.hd xs< sum && List.tl xs = []  then 1
        (*(10, [9,8])*)
        else 1 + number_before_reaching_sum sum ((List.hd xs + List.hd(List.tl xs)):: List.tl(List.tl xs))


(* 9 *)
let what_month (day_of_year : int) : int = 
        let days_in_month = [31;28;31;30;31;30;31;31;30;31;30;31] in 
        1 +  number_before_reaching_sum  day_of_year days_in_month


(* 10 *)
let rec month_range (day1 : int) (day2 : int) : int list = 
        if day1 > day2 then []
        else day1 :: month_range (day1+1) day2

(* 11 *)
let rec oldest (dates : (int * (int * int)) list) : (int * (int * int)) option =
  raise Unimplemented

(* 12 *)
let cumulative_sum (xs : int list) : int list = raise Unimplemented

(**
 * Challenge Problems
 * TODO: Complete these function bindings
 *)

(* 13 *)
let number_in_months_challenge (dates : (int * (int * int)) list)
    (months : int list) : int =
  raise Unimplemented

let dates_in_months_challenge (dates : (int * (int * int)) list)
    (months : int list) : (int * (int * int)) list =
  raise Unimplemented

(* 14 *)
let reasonable_date (date : int * (int * int)) : bool = raise Unimplemented

(*https://gildor478.github.io/ounit/ounit2/index.html*)
(*ocamlfind ocamlc -o test -package ounit2 -linkpkg hw1.ml hw1_test_withLib.ml*)
open OUnit2
open Hw1

let hw1_test =
  "test hw1"
  >::: [
         ( "is_older_tests" >:: fun _ ->
           assert_equal false (is_older (15, (12, 1995)) (14, (12, 1995)));
           assert_equal true (is_older (5, (12, 1995)) (14, (12, 1995)));
           assert_equal false (is_older (15, (12, 1995)) (14, (1, 1995)));
           assert_equal true (is_older (15, (1, 1995)) (14, (12, 1995)));
           assert_equal false (is_older (15, (12, 1997)) (14, (12, 1995)));
           assert_equal true (is_older (15, (12, 1987)) (14, (12, 1995)));
           assert_equal false (is_older (15, (12, 2007)) (14, (12, 1995))) );
         ( "test for number_in_month" >:: fun _ ->
           assert_equal 0 (number_in_month [] 1);
           assert_equal 2
             (number_in_month
                [ (15, (12, 1995)); (14, (12, 1995)); (24, (3, 1994)) ]
                12);
           assert_equal 0
             (number_in_month
                [ (15, (12, 1995)); (14, (12, 1995)); (24, (3, 1994)) ]
                4) );
         ( "test for number_in_months" >:: fun _ ->
           assert_equal 0 (number_in_months [] []);
           assert_equal 0 (number_in_months [] [ 1; 2 ]);
           assert_equal 0
             (number_in_months
                [ (1, (1, 2001)); (1, (2, 2001)); (1, (2, 2002)) ]
                []);
           assert_equal 0
             (number_in_months
                [ (1, (1, 2001)); (1, (2, 2001)); (1, (2, 2002)) ]
                [ 4; 5 ]);
           assert_equal 3
             (number_in_months
                [ (1, (1, 2001)); (1, (2, 2001)); (1, (2, 2002)) ]
                [ 1; 2 ]);
           assert_equal 2
             (number_in_months
                [ (15, (12, 1995)); (14, (12, 1995)); (24, (3, 1994)) ]
                [ 12 ]);
           assert_equal 1
             (number_in_months
                [ (15, (12, 1995)); (14, (12, 1995)); (24, (3, 1994)) ]
                [ 3; 4; 5 ]) );
         ( "test for dates_in_month" >:: fun _ -> 
                 assert_equal [] (dates_in_month [] 5);
                 assert_equal [] (dates_in_month [(15,(2,2022));(15,(3,2023));(15,(2,2023))] 4);
                 assert_equal [(15,(3,2023))] (dates_in_month [(15,(2,2022));(15,(3,2023));(15,(2,2023))] 3);
                 assert_equal [(15,(2,2022));(15,(2,2023))] (dates_in_month [(15,(2,2022));(15,(3,2023));(15,(2,2023))] 2);
  
         );
         ( "test for dates_in_months" >:: fun _ -> 
                 assert_equal [] (dates_in_months [] []);
                 assert_equal [] (dates_in_months [] [1;2]);
                 assert_equal [] (dates_in_months [(15,(2,2022));(15,(3,2023));(15,(2,2023))] []);
                 assert_equal [(15,(3,2023))] (dates_in_months [(15,(2,2022));(15,(3,2023));(15,(2,2023))] [3]);
                 assert_equal [(15,(2,2022));(15,(2,2023));(15,(3,2023))] (dates_in_months [(15,(2,2022));(15,(3,2023));(15,(2,2023))] [2;3]);
                 assert_equal [(15,(2,2022));(15,(2,2023));(15,(5,2023))] (dates_in_months [(15,(2,2022));(15,(3,2023));(15,(2,2023));(15,(5,2023))] [2;4;5]);
  
         );
         ( "test for string_of_date" >:: fun _ -> 
                 assert_equal "September-10-2015" (string_of_date (10,(9,2015)));
                 
                 assert_equal "January-1-2015" (string_of_date (1,(1,2015)));
                 (*print_endline (string_of_date (10,(9,2015)));*)
         );
         ( "test for number_before_reaching_sum" >:: fun _ -> 
                 assert_equal 0 (number_before_reaching_sum 10 []);
                 assert_equal 0 (number_before_reaching_sum 10 [20]);
                 assert_equal 1 (number_before_reaching_sum 10 [1]);
                 assert_equal 1 (number_before_reaching_sum 10 [9;1]);
                 assert_equal 1 (number_before_reaching_sum 10 [9;11]);
                 assert_equal 1 (number_before_reaching_sum 20 [9;11]);
                 assert_equal 3 (number_before_reaching_sum 20 [9;1;1;10]);
                 assert_equal 6(number_before_reaching_sum 27 [2;3;12;1;1;1;10]);
                 (*print_endline (string_of_int (number_before_reaching_sum 26 [31;28;31;1;1;1;10]));*)


         );
         ( "test for string_of_date" >:: fun _ -> 
                 assert_equal 12 (what_month 360 ) ;
                 assert_equal 1 (what_month 20 ) ;
                 assert_equal 2 (what_month 59 ) ;
                 assert_equal 3 (what_month 60 ) ;
         );
          
       ]


(*
 *let test_num_in_month =  "test for number_in_month"  >::: [
 *
 *        "edge case []" >:: (fun _ -> assert_equal 0 (number_in_month [] 1));
 *        "has month" >:: (fun _ -> assert_equal 2 (number_in_month [(15,(12,1995)) ;(14,(12,1995)); (24,(3,1994))] 12));
 *        "no qualified month" >:: (fun _ -> assert_equal 0 (number_in_month [(15,(12,1995)) ;(14,(12,1995)); (24,(3,1994))] 4));
 *]

 *)

let _ = run_test_tt_main  hw1_test

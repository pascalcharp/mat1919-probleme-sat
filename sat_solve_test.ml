#use "sat_solve.ml" ;;

let clause_test = [Var(1); Var(2); Var(3); Notvar(5)] ;;
let value_set_test_1 = [(1, false); (2, false); (3, false); (4, true); (5, true); (6, true)] ;;
let value_set_test_2 = [(1, false); (2, true); (3, false); (4, true); (5, true); (6, true)] ;;
let value_set_test_3 = [(1, false); (2, false); (3, false); (4, true); (5, false); (6, true)] ;;
let value_set_test_4 = [(1, true); (2, false); (3, true); (4, true)] ;;

let probleme_sat_test_1 = [
                           [Var(1); Var(2)];
                           [Var(3)];
                           [Notvar(1); Notvar(3)];
                           [Notvar(2); Notvar(3)]
                         ] ;;

let probleme_sat_test_2 = [
                             [Var(1); Var(2)];
                             [Var(3)];
                             [Var(1); Notvar(3)];
                             [Notvar(2); Notvar(3)]
                         ] ;;

assert (not (eval_clause clause_test value_set_test_1)) ;;
assert (eval_clause clause_test value_set_test_2) ;; 
assert (eval_clause clause_test value_set_test_3) ;;

assert (not (eval_probleme_sat probleme_sat_test_1 value_set_test_1)) ;;
assert (not (eval_probleme_sat probleme_sat_test_1 value_set_test_2)) ;;
assert (not (eval_probleme_sat probleme_sat_test_1 value_set_test_3)) ;;


assert( eval_probleme_sat probleme_sat_test_2 value_set_test_4) ;
assert( not (eval_probleme_sat probleme_sat_test_2 value_set_test_1)) ;;


assert ((max_var_in_clause clause_test) = 5) ;;
assert ((max_var_in_probleme_sat probleme_sat_test_1) = 4) ;;

assert( not(est_satisfaisable probleme_sat_test_1)) ;;
assert( est_satisfaisable probleme_sat_test_2 ) ;;
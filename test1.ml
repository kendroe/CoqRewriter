let exp1 = Exp.parseExp "apply(f,e)" ;;
print_string ("Test 1:" ^ (Exp.prExp exp1) ^ "\n") ;;
let exp2 = Exp.parseExp "fn x => x+2" ;;
print_string ("Test 2:" ^ (Exp.prExp exp2) ^ "\n") ;;
let exp3 = Exp.parseExp "apply((fn x => nplus(x,3)),4)" ;;
print_string ("Test 3:" ^ (Exp.prExp exp3) ^ "\n") ;;

Trace.toggle_trace () ;;
let exp4 = List.hd (Inner.rewrite2 Env.emptyEnv exp3) ;;
print_string ("Test 4: " ^ Exp.prExp exp4 ^ "\n") ;;
(*let (Exp.APPL (f,l)) = exp4 ;;
print_string ("functor " ^ (string_of_int f) ^ "\n");;*)

let exp5 = Exp.parseExp "(fn x => (apply(f(e),x)))" ;;
print_string ("Test 5: " ^ Exp.prExp exp5 ^ "\n") ;;

let exp6 = List.hd (Inner.rewrite2 Env.emptyEnv exp5) ;;
print_string ("Test 6: " ^ Exp.prExp exp6 ^ "\n") ;;

let exp7 = Exp.parseExp "1#2 * x" ;;
print_string ("Test 7: " ^ Exp.prExp exp7 ^ "\n") ;;

(*let exp8 = convert_overloads emptyEnv exp7 ;;
print_string ("Test 8: " ^ Exp.prExp exp8 ^ "\n") ;;*)

let exp9 = Exp.parseExp "1 * x" ;;
print_string ("Test 9: " ^ Exp.prExp exp9 ^ "\n") ;;

(*let exp10 = convert_overloads emptyEnv exp9 ;*)

let exp11 = Exp.parseExp "1 * 1#2" ;;
print_string ("Test 11: " ^ Exp.prExp exp11 ^ "\n") ;;

(*let exp12 = convert_overloads emptyEnv exp11 ;
print (prExp exp12) ;*)


let exp13 = Exp.parseExp "nplus(ntimes(x,2),3)==9" ;;
print_string ("Test 13: " ^ Exp.prExp exp13 ^ "\n") ;;

let exp14 = List.hd (Inner.rewrite2 Env.emptyEnv exp13) ;;
print_string ("Test 14: " ^ Exp.prExp exp14 ^ "\n") ;;

let exp15 = Exp.parseExp "nless(nplus(ntimes(x,2),3),9)" ;;
print_string ("Test 15: " ^ Exp.prExp exp15 ^ "\n") ;;

let exp16 = List.hd (Inner.rewrite2 Env.emptyEnv exp15) ;;
print_string ("Test 16: " ^ Exp.prExp exp16 ^ "\n") ;;

let exp17 = Exp.parseExp "nless(9,nplus(ntimes(x,2),3))" ;;
print_string ("Test 17: " ^ Exp.prExp exp17 ^ "\n") ;;

let exp18 = List.hd (Inner.rewrite2 Env.emptyEnv exp17) ;;
print_string ("Test 18: " ^ Exp.prExp exp18 ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(x,3)==nplus(y,4)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(x,5)==nplus(y,4)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,3)==ntimes(y,4)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,2)==ntimes(y,4)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,4)==ntimes(y,2)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,9)==ntimes(y,6)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,6)==ntimes(y,9)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "6==nplus(x,9)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "9==nplus(x,5)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(x,9)==6" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(x,5)==9" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,9)==6" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,6)==9" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,5)==9" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,3)==9" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "6==ntimes(x,9)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "9==ntimes(x,6)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "9==ntimes(x,5)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "9==ntimes(x,3)";;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "y==ntimes(x,3,y)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,3,y)==y" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "y==nplus(x,3,y)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(x,3,y)==y" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ntimes(x,3,y)==nplus(x,3,ntimes(x,3,y))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(x,3,ntimes(x,3,y))==ntimes(x,3,y)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(nplus(x,3),nplus(y,4))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(nplus(x,5),nplus(y,4))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,3),ntimes(y,4))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,2),ntimes(y,4))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,4),ntimes(y,2))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,9),ntimes(y,6))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(nplus(x,9),6)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(nplus(x,5),9)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,9),6)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,6),9)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,5),9)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(ntimes(x,3),nminus(0,9))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(6,nplus(x,9))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(9,nplus(x,5))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(6,ntimes(x,9))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(9,ntimes(x,6))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(9,ntimes(x,5))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(nminus(0,9),ntimes(x,3))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

(*let exp_test = Exp.parseExp "rless(rminus(0#1,9#1),rtimes(x,3#1))"
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;
print (prExp exp_res) ;

let exp_test = Exp.parseExp "0#1==rtimes(x,3#1))"
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;
print (prExp exp_res) ;

let exp_test = Exp.parseExp "0#1==rplus(x,3#1))"
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;
print (prExp exp_res) ;

let exp_test = Exp.parseExp "rplus(1#1,x)==rplus(y,3#1))"
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;
print (prExp exp_res) ;

let exp_test = Exp.parseExp "rplus(1#3,x)==rplus(y,3#1))"
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;
print (prExp exp_res) ;

let exp_test = Exp.parseExp "rplus(3#1,x)==rplus(y,1#3))"
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;
print (prExp exp_res) ;
*)


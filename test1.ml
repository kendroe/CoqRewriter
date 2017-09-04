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

let exp5 = Exp.parseExp "(fn x => (f(e) x))" ;;
print_string ("Test5: " ^ Exp.prExp exp5 ^ "\n") ;;

let exp6 = List.hd (Inner.rewrite2 Env.emptyEnv exp5) ;;
print_string ("Test6: " ^ Exp.prExp exp6 ^ "\\n") ;;

(*val exp7 = parseExp "1#2 * x" ;

val exp8 = convert_overloads emptyEnv exp7 ;

val exp9 = parseExp "1 * x" ;

val exp10 = convert_overloads emptyEnv exp9 ;

val exp11 = parseExp "1 * 1#2" ;

val exp12 = convert_overloads emptyEnv exp11 ;
print (prExp exp12) ;


val exp13 = parseExp "nplus(ntimes(x,2),3)==9" ;

val exp14 = hd (rewrite2 emptyEnv exp13) ;
print (prExp exp14) ;

TRACEimpl.toggle_trace () ;

val exp15 = parseExp "nless(nplus(ntimes(x,2),3),9)" ;

val exp16 = hd (rewrite2 emptyEnv exp15) ;
print (prExp exp16) ;

val exp17 = parseExp "nless(9,nplus(ntimes(x,2),3))" ;

val exp18 = hd (rewrite2 emptyEnv exp17) ;
print (prExp exp18) ;

val exp_test = parseExp "nplus(x,3)==nplus(y,4)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nplus(x,5)==nplus(y,4)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,3)==ntimes(y,4)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,2)==ntimes(y,4)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,4)==ntimes(y,2)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,9)==ntimes(y,6)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,6)==ntimes(y,9)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "6==nplus(x,9)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "9==nplus(x,5)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nplus(x,9)==6"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nplus(x,5)==9"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,9)==6"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,6)==9"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,5)==9"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,3)==9"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "6==ntimes(x,9)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "9==ntimes(x,6)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "9==ntimes(x,5)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "9==ntimes(x,3)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "y==ntimes(x,3,y)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,3,y)==y"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "y==nplus(x,3,y)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nplus(x,3,y)==y"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "ntimes(x,3,y)==nplus(x,3,ntimes(x,3,y))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nplus(x,3,ntimes(x,3,y))==ntimes(x,3,y)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(nplus(x,3),nplus(y,4))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(nplus(x,5),nplus(y,4))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,3),ntimes(y,4))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,2),ntimes(y,4))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,4),ntimes(y,2))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,9),ntimes(y,6))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(nplus(x,9),6)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(nplus(x,5),9)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,9),6)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,6),9)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,5),9)"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(ntimes(x,3),nminus(0,9))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(6,nplus(x,9))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(9,nplus(x,5))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(6,ntimes(x,9))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(9,ntimes(x,6))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(9,ntimes(x,5))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "nless(nminus(0,9),ntimes(x,3))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "rless(rminus(0#1,9#1),rtimes(x,3#1))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "0#1==rtimes(x,3#1))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "0#1==rplus(x,3#1))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "rplus(1#1,x)==rplus(y,3#1))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "rplus(1#3,x)==rplus(y,3#1))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;

val exp_test = parseExp "rplus(3#1,x)==rplus(y,1#3))"
val exp_res = hd (rewrite2 emptyEnv exp_test) ;
print (prExp exp_res) ;
*)


let exp1 = Exp.parseExp "apply(f,e)" ;;
print_string ("Test 1:" ^ (Exp.prExp exp1) ^ "\n") ;;
let exp2 = Exp.parseExp "fn x => x+2" ;;
print_string ("Test 2:" ^ (Exp.prExp exp2) ^ "\n") ;;
let exp3 = Exp.parseExp "apply((fn x => nplus(x,3)),4)" ;;
print_string ("Test 3:" ^ (Exp.prExp exp3) ^ "\n") ;;

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

let exp_test = Exp.parseExp "rless(rminus(0#1,9#1),rtimes(x,3#1))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "0#1==rtimes(x,3#1))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "0#1==rplus(x,3#1))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "rplus(1#1,x)==rplus(y,3#1))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "rplus(1#3,x)==rplus(y,3#1))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "rplus(3#1,x)==rplus(y,1#3))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "rless(rtimes(rminus(0#1,3#1),x),rtimes(y,1#9)))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "x==3 & y==nplus(x,1)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "EXISTS(x) y==nplus(z,1)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "EXISTS(y) not(y==nplus(z,1))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ALL(x) y==nplus(z,1)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "ALL(y) not(y==nplus(z,1))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "EXISTS(x) x==ntimes(q,r) & y==nplus(x,1)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "EXISTS(x) x==ntimes(q,r)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "CASE x OF 0 => 1 ||| 1 => 2" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "CASE True() OF True() => 1 ||| False() => 2" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let env_1 = Env.addTypeDefinition Env.emptyEnv (Rtype.parseWholeDef "List(t) = Nil | Cons(t,List(t))");;

let env_2 = Env.addFunction env_1 ((Exp.parseExp "append(a,b)"),
            (Rtype.parse "List(a) * List(a) -> List(a)"),
            (Exp.parseExp "True"),
    [
        Exp.parseRule "append(Cons(f,r),b) -> Cons(f,append(r,b))";
        Exp.parseRule "append(Nil,b) -> b)"
    ]) [] ;;

let exp_test = Exp.parseExp "CASE Nil() OF Nil() => nplus(1,10) ||| Cons(a,b) => 2" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 Env.emptyEnv exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "append(Cons(2,Nil),3)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_2 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let env_3 = Env.addProperty env_2 (Exp.parseRule "append(append(a,b),c) -> append(a,append(b,c))") ;;

let exp_test = Exp.parseExp "append(append(a,Cons(1,Nil)),c)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_3 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let env_4 = Env.addFunction env_3 ((Exp.parseExp "yin(x)"),
            (Rtype.parse "Natural -> Natural"),
            (Exp.parseExp "True"),
    [
        (*Exp.parseRule "yin(0) -> 3";
        Exp.parseRule "yin(x) { nless(0,x) } -> nplus(yin(nminus(x,1)),1)"*)
    ]) [] ;;

let env_5 = Env.addFunction env_4 ((Exp.parseExp "yan(x)"),
            (Rtype.parse "Natural -> Natural"),
            (Exp.parseExp "True"),
    [
        (*Exp.parseRule "yan(0) -> 4";
        Exp.parseRule "yan(x) { nless(0,x) } -> nminus(yan(nminus(x,1)),1)"*)
    ]) [] ;;

let env_6 = Env.addProperty env_5 (Exp.parseRule "nplus(yin(x),yan(x)) -> 4") ;;

let exp_test = Exp.parseExp "nplus(yin(x),yan(x))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(yin(x),yan(nplus(x,1)))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(yin(3),yan(4))" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(yin(x),5,yan(x),3)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nplus(yan(x),5,yin(x),3)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(3,5) & nless(5,3)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(x,y) & nless(y,x)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(5,y) & nless(y,3)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

Rtrace.toggle_trace () ;;

let exp_test = Exp.parseExp "nless(x,y) & nless(y,z) & nless(z,x)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;

let exp_test = Exp.parseExp "nless(2,y) & nless(y,10)" ;;
print_string ("exp_test: " ^ Exp.prExp exp_test ^ "\n") ;;
let exp_res = List.hd (Inner.rewrite2 env_6 exp_test) ;;
print_string ("exp_res: " ^ Exp.prExp exp_res ^ "\n") ;;


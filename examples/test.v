Require Export AdvancedRewrite.advancedRewrite.

Inductive id : Type := 
  | Id : nat -> id
  | PrimedId : nat -> id.
 
Inductive Value {ev} : Type := 
    | NatValue : nat -> Value
    | ListValue : list Value -> Value
    | NoValue : Value
    | OtherValue : ev -> Value.

Inductive absExp : Type :=
   | AbsConstVal : (@Value unit) -> absExp
   | AbsVar : id -> absExp
   | AbsQVar : nat -> absExp
   | AbsFun : id -> list absExp -> absExp
   | AbsNothing : absExp.

printAST (AbsVar (Id 2)).
printExp (AbsVar (Id 2)).

printAST (AbsNothing).
printExp (AbsNothing).

printAST (1+2).
printExp (1*2).
printAST (1+2=3).
printExp (1+2=3).
printAST (2=3).
printExp (2=3).
printAST (1+2=2+1).
printExp (1+2=2+1).
printAST (1 < 1).
printExp (1 < 1).


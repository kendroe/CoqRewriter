Require Export AdvancedRewrite.advancedRewrite.

Inductive TriDigit : Type := 
  | TriZero : TriDigit
  | TriOne : TriDigit
  | TriTwo : TriDigit

 
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


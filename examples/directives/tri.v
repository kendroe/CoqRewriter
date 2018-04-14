Require Export List.
Require Export AdvancedRewrite.advancedRewrite.
Require Export triclass.

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

Theorem x: 1 < 2.
Proof.
   arewrite.
Admitted.

Theorem q: forall x, x=(triAdd x (TriZero::nil)).
Proof.
    arewrite.
Admitted.

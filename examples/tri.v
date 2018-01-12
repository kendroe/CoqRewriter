Require Export List.
Require Export AdvancedRewrite.advancedRewrite.

Inductive TriDigit : Type := 
  | TriZero : TriDigit
  | TriOne : TriDigit
  | TriTwo : TriDigit.

Fixpoint triIncrement ( x: list TriDigit) :=
    match x with
    | nil => (TriOne::nil)
    | (TriZero::r) => (TriOne::r)
    | (TriOne::r) => (TriTwo::r)
    | (TriTwo::r) => (TriZero::(triIncrement r))
    end.

Fixpoint triAdd (x : list TriDigit) (y : list TriDigit) :=
    match x with
    | nil => y
    | (TriZero::r1) => match y with
                       | nil => (TriZero::r1)
                       | (f::r2) => (f::(triAdd r1 r2))
                       end
    | (TriOne::r1) => match y with
                      | nil => (TriOne::r1)
                      | (TriZero::r2) => (TriOne::(triAdd r1 r2))
                      | (TriOne::r2) => (TriTwo::(triAdd r1 r2))
                      | (TriTwo::r2) => triIncrement (TriZero::(triAdd r1 r2))
                      end
    | (TriTwo::r1) => match y with
                      | nil => (TriTwo::r1)
                      | (TriZero::r2) => (TriTwo::(triAdd r1 r2))
                      | (TriOne::r2) => triIncrement (TriZero::(triAdd r1 r2))
                      | (TriTwo::r2) => triIncrement (TriOne::(triAdd r1 r2))
                      end
    end.

Theorem trAdd_ac: AC triAdd.
Proof.
    admit.
Admitted.

Theorem x: 1+1=3.
Proof.
    arewrite.
Abort.
 
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


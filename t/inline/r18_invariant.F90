SUBROUTINE R18_INVARIANT (X, KB0, KN)
INTEGER, INTENT (IN) :: KB0
INTEGER, INTENT (INOUT) :: KN
REAL, INTENT (INOUT) :: X (10)
INTEGER :: IB, IC, ID
! IB: same invariant value in caller and callee -> kept
! IC: different values -> renamed
! ID: value depends on KN, which is modified -> renamed
IB = KB0
IC = 1
ID = KN
CALL S (X, KB0, KN)
X (IB) = REAL (IC + ID)
CONTAINS
SUBROUTINE S (P, KB, KM)
REAL, INTENT (INOUT) :: P (10)
INTEGER, INTENT (IN) :: KB
INTEGER, INTENT (INOUT) :: KM
INTEGER :: IB, IC, ID
IB = KB
IC = 2
ID = KM
KM = KM + 1
P (IB) = REAL (IC + ID)
END SUBROUTINE
END SUBROUTINE

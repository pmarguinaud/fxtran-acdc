SUBROUTINE C19_MERGE (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! MERGE with a constant mask is replaced by one of its arguments, parenthesized in an operand
REAL :: Y, Z
X = MERGE (Y, Z, LT)
X = MERGE (Y + 1., Z, LT) * 2.
X = 2. * MERGE (Y, Z - 1., .NOT. LT)
X = MERGE (1., 2., LF) + 1.
IF (MERGE (L1, L2, LT)) X = 0.
IF (MERGE (L1, LT, LF)) X = 1.
X = MERGE (Y, Z, L1)
END SUBROUTINE

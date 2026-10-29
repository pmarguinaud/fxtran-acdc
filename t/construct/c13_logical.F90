SUBROUTINE C13_LOGICAL (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! Logical operators with constant operands (current folding)
IF (LF .AND. L1) THEN
  X = 1.
ELSEIF (.NOT. LF) THEN
  X = 2.
ENDIF
IF (L1 .OR. LT) X = 3.
END SUBROUTINE

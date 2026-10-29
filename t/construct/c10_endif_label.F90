SUBROUTINE C10_ENDIF_LABEL (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! The label of the END IF statement is kept on a CONTINUE statement
IF (LT) THEN
  IF (L1) GOTO 10
  X = 1.
10 ENDIF
X = 2.
END SUBROUTINE

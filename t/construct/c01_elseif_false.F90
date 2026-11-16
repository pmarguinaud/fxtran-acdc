SUBROUTINE C01_ELSEIF_FALSE (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! The only block left has a condition: the construct is kept
IF (L1) THEN
  X = 1.
ELSEIF (LF) THEN
  X = 2.
ENDIF
END SUBROUTINE

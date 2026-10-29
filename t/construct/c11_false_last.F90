SUBROUTINE C11_FALSE_LAST (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! The last block is removed: END IF moves to the previous block
IF (L1) THEN
  X = 1.
ELSEIF (L2) THEN
  X = 2.
ELSEIF (LF) THEN
  X = 3.
ENDIF
END SUBROUTINE

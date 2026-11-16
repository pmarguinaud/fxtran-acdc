SUBROUTINE C12_FIRST_FALSE_NAMED (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! ELSE IF becomes the first block of a named construct
LBL: IF (LF) THEN
  X = 1.
ELSE IF (L1) THEN LBL
  X = 2.
END IF LBL
END SUBROUTINE

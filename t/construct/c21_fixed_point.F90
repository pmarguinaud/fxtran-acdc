SUBROUTINE C21_FIXED_POINT (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! A simplification exposes another one
REAL :: Y, Z
IF (MERGE (KF, 0, LT) == 5) THEN
  X = MERGE (Y, Z, KF > 4)
ELSE
  X = 0.
ENDIF
END SUBROUTINE

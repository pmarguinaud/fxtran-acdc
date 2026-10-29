SUBROUTINE C20_WRITE_CONTEXTS (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! Forced variables are not replaced where they are written, except in CALL arguments
INTEGER :: I
LF = L1
READ (5, *) LF
DO KF = 1, 10
  X = X + 1.
ENDDO
CALL G (LF)
I = KF + 1
END SUBROUTINE

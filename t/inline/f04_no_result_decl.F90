SUBROUTINE F04_NO_RESULT_DECL (X, Z)
REAL, INTENT (IN) :: X
REAL, INTENT (OUT) :: Z
! No RESULT clause, type in a declaration; the result is also read
Z = F (X)
CONTAINS
FUNCTION F (A)
REAL, INTENT (IN) :: A
REAL :: F
F = 2. * A
F = F + 1.
END FUNCTION
END SUBROUTINE

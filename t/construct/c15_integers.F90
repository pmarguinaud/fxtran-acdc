SUBROUTINE C15_INTEGERS (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! Chains are evaluated from left to right, as a whole
INTEGER :: I
I = 1 - 2 + 3
I = 2 * 3 / 4
I = 7 / 2
I = -7 / 2
I = 2 ** 3 ** 2
I = 10 - 2 - 3
I = I + 1 + 2
I = (1 + 2) * I
I = I * (1 - 4)
I = -(1 + 2)
I = 7 / 0
END SUBROUTINE

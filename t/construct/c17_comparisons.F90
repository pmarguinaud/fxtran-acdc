SUBROUTINE C17_COMPARISONS (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
IF (1 < 2) X = 1.
IF (2 >= 3) X = 2.
IF (1.5 .EQ. 1.5) X = 3.
IF (0.1 == 0.1) X = 4.
IF (0.1 /= 0.2) X = 5.
IF (1 + 2 /= 3) X = 6.
IF (2 > 1.5) X = 7.
END SUBROUTINE

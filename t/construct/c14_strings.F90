SUBROUTINE C14_STRINGS (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! Character comparisons pad the shorter string with blanks
CHARACTER (LEN=4) :: C
IF ("NONE" /= 'NONE') X = 1.
IF ('ABC ' == 'ABC') X = 2.
IF (TRIM ('ABC ') // 'D' == 'ABCD') X = 3.
IF (ADJUSTL ('  A') == 'A') X = 4.
IF ('A' .LT. 'B') X = 5.
IF ('IT''S' == "IT'S") X = 6.
C = 'A' // 'B'
END SUBROUTINE

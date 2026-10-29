SUBROUTINE C22_SELECT_CASE (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! SELECT CASE on a constant selector: the matching block is kept
INTEGER :: I
INTEGER, PARAMETER :: IP = 1
SELECT CASE (KF)
CASE (1, 2)
  X = 1.
CASE (3:5)
  X = 2.
CASE DEFAULT
  X = 3.
END SELECT
SELECT CASE (KF + 10)
CASE (:10)
  X = 4.
CASE (11:)
  X = 5.
END SELECT
SELECT CASE (KF)
CASE (1)
  X = 6.
CASE DEFAULT
  X = 7.
END SELECT
SELECT CASE (KF)
CASE (1)
  X = 8.
END SELECT
SELECT CASE (TRIM ('1DIM '))
CASE ('3DIM')
  X = 9.
CASE ('1DIM')
  X = 10.
END SELECT
SELECT CASE (LT)
CASE (.FALSE.)
  X = 11.
CASE (.TRUE.)
  X = 12.
END SELECT
! A case value which is not a literal (named constant): a literal match is taken, since case
! values cannot overlap; with no literal match, the construct is not changed
SELECT CASE (KF)
CASE (IP)
  X = 13.
CASE (5)
  X = 14.
END SELECT
SELECT CASE (KF)
CASE (IP)
  X = 15.
CASE (6)
  X = 16.
END SELECT
! Left by EXIT: not changed
SEL: SELECT CASE (KF)
CASE (5) SEL
  IF (L1) EXIT SEL
  X = 17.
END SELECT SEL
! Selector which is not a constant
SELECT CASE (I)
CASE (5)
  X = 18.
END SELECT
END SUBROUTINE

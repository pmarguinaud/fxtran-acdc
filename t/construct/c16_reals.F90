SUBROUTINE C16_REALS (L1, L2, LT, LF, X)
LOGICAL :: L1, L2, LT, LF
REAL :: X
! Reals are folded only when exact in single precision, with matching kinds
X = 1.5 + 2.
X = 0.1 + 0.2
X = 1._JPRB + 2._JPRB
X = 1._JPRB + 2.
X = 1. / 3.
X = 3._JPRB / 8._JPRB
X = 1.E3 * 2.
X = 1.D0 + 1.D0
X = -0.5
X = (-1.E-20)
X = 2. * 3
END SUBROUTINE

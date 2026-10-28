SUBROUTINE R16_ITERATORS (X, N)
INTEGER, INTENT (IN) :: N
REAL, INTENT (INOUT) :: X (N)
INTEGER :: J, K, I
! J: DO variable in the caller and in S1 -> kept
DO J = 1, N
  X (J) = 0.
ENDDO
CALL S1 (X, N)
! K: CALL inside a loop on K -> renamed
DO K = 1, 2
  CALL S2 (X, N)
ENDDO
! I: ordinary variable in the caller -> renamed
I = 1
CALL S3 (X, N)
X (I) = 1.
CONTAINS
SUBROUTINE S1 (P, M)
INTEGER, INTENT (IN) :: M
REAL, INTENT (INOUT) :: P (M)
INTEGER :: J
REAL :: Z
Z = 2.
DO J = 1, M
  P (J) = P (J) + Z
ENDDO
END SUBROUTINE
SUBROUTINE S2 (P, M)
INTEGER, INTENT (IN) :: M
REAL, INTENT (INOUT) :: P (M)
INTEGER :: K
DO K = 1, M
  P (K) = P (K) + 1.
ENDDO
END SUBROUTINE
SUBROUTINE S3 (P, M)
INTEGER, INTENT (IN) :: M
REAL, INTENT (INOUT) :: P (M)
INTEGER :: I
DO I = 1, M
  P (I) = P (I) + 1.
ENDDO
END SUBROUTINE
END SUBROUTINE

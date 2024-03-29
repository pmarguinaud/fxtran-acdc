INTERFACE
SUBROUTINE CUBIDIAG_SINGLE_COLUMN (KIDIA, KFDIA, KLON,&
& KLEV, KCTOP, LD_LCUMASK, PA, PB, PR, PU, YDSTACK)
!$acc routine (CUBIDIAG_SINGLE_COLUMN) seq
USE PARKIND1,ONLY:JPIM, JPRB
USE STACK_MOD
INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
INTEGER (KIND=JPIM), INTENT (IN)::KLON
INTEGER (KIND=JPIM), INTENT (IN)::KLEV
INTEGER (KIND=JPIM), INTENT (IN)::KCTOP (KLON)
LOGICAL, INTENT (IN)::LD_LCUMASK (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PA (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PB (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PR (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PU (KLON, KLEV)
TYPE(STACK) :: YDSTACK
ENDSUBROUTINE CUBIDIAG_SINGLE_COLUMN

END INTERFACE

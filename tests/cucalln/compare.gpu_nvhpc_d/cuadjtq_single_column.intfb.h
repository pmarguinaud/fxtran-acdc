INTERFACE
#ifdef RS6K
@PROCESS HOT NOSTRICT
#endif
SUBROUTINE CUADJTQ_SINGLE_COLUMN (YDTHF, YDCST, YDEPHLI, KIDIA, KFDIA&
&, KLON, KLEV, KK, PSP, PT, PQ, LDFLAG, KCALL, LDOFLAG, YDSTACK)
!$acc routine (CUADJTQ_SINGLE_COLUMN) seq
USE PARKIND1,ONLY:JPIM, JPRB
USE YOMCST,ONLY:TCST
USE YOETHF,ONLY:TTHF
USE YOEPHLI,ONLY:TEPHLI
USE STACK_MOD
TYPE (TTHF), INTENT (IN)::YDTHF
TYPE (TCST), INTENT (IN)::YDCST
TYPE (TEPHLI), INTENT (IN)::YDEPHLI
INTEGER (KIND=JPIM), INTENT (IN)::KLON
INTEGER (KIND=JPIM), INTENT (IN)::KLEV
INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
INTEGER (KIND=JPIM), INTENT (IN)::KK
REAL (KIND=JPRB), INTENT (IN)::PSP (KLON)
REAL (KIND=JPRB), INTENT (INOUT)::PT (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PQ (KLON, KLEV)
LOGICAL, INTENT (IN)::LDFLAG (KLON)
INTEGER (KIND=JPIM), INTENT (IN)::KCALL
LOGICAL, OPTIONAL, INTENT (IN)::LDOFLAG (KLON)
TYPE(STACK) :: YDSTACK
ENDSUBROUTINE CUADJTQ_SINGLE_COLUMN

END INTERFACE

SUBROUTINE TRIDIA2 (KN,KPROMA,KST,KEND,PM,PRHS,PSOL)

!$ACDC singleblock

!**** *TRIDIA*   SOLVES A NUMBER OF TRIDIAGONAL LINEAR SYSTEMS

!                Solves PM(js)*PSOL = PRHS(js) for js=kst,kend
!                (PM depends on "js", cf. former TRIDIALCZ)

!     Remark: this routine does something similar to routine SGTSL, but:
!      - it has been optimised to invert several tridiagonal linear systems
!        (SGTSL inverts only one tridiagonal linear system).
!      - it does not the additional tests and calculations done in SGTSL
!        (output quantity "kinfo").
!      - it is F90-norm compliant.

!        Explicit arguments :
!        --------------------
!            KN         : Dimension of the systems.                    (in)
!            KPROMA     : Number of systems to be solved.              (in)
!            KST        : First system to be solved.                   (in)
!            KEND       : Last system to be solved.                    (in)
!            PM         : Tridiagonal matrices of the systems          (inout)
!            PRHS       : RHS of the systems                           (in)
!            PSOL       : Solutions of the systems                     (out)

!     Author.
!     -------
!           OLIVIER TALAGRAND  -  16 SEPTEMBER 1988

!     Modifications.
!     --------------
!  MODIFICATION: ROBERTO BUIZZA - 29 JUL 92
!  K. Yessad (Oct 2008): merge of TRIDIALCZ and TRIDIAVSPL.
! -----------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

! -----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KN
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND
REAL(KIND=JPRB)   ,INTENT(IN)    :: PM(KPROMA,KN,-1:1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRHS(KPROMA,KN)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSOL(KPROMA,KN)

! -----------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZRHS(KPROMA,KN)
REAL(KIND=JPRB) :: ZM(KPROMA,KN,-1:1)
INTEGER(KIND=JPIM) :: J, JROF

REAL(KIND=JPRB) :: ZDEN
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIDIA2',0,ZHOOK_HANDLE)
! -----------------------------------------------------------------------------

!     1.   INITIALIZATIONS.
!          ----------------

!     1.1 FILL IN EXTREMITIES OF SECONDARY DIAGONALS

ZM=PM

ZM(KST:KEND,1,-1) = 0.0_JPRB
ZM(KST:KEND,KN,1) = 0.0_JPRB

!     1.2 Copy PRHS

ZRHS(KST:KEND,1:KN)=PRHS(KST:KEND,1:KN)

! -----------------------------------------------------------------------------

!     2.   ASCENDING LOOP.
!          ---------------

DO JROF = KST,KEND
  PSOL(JROF,1) = -ZM(JROF,1,1)/ZM(JROF,1,0)
  ZRHS(JROF,1) =  ZRHS(JROF,1)/ZM(JROF,1,0)
ENDDO

DO J = 2,KN
  DO JROF = KST,KEND
    ZDEN = 1.0_JPRB/(ZM(JROF,J,-1)*PSOL(JROF,J-1) + ZM(JROF,J,0))
    PSOL(JROF,J) = -ZM(JROF,J,1)*ZDEN
    ZRHS(JROF,J) = (ZRHS(JROF,J) - ZRHS(JROF,J-1)*ZM(JROF,J,-1))*ZDEN
  ENDDO
ENDDO

! -----------------------------------------------------------------------------

!     3.   DESCENDING LOOP.
!          ----------------

PSOL(KST:KEND,KN)=ZRHS(KST:KEND,KN)
DO J = KN-1,1,-1
  DO JROF = KST,KEND
    PSOL(JROF,J) = ZRHS(JROF,J) + PSOL(JROF,J)*PSOL(JROF,J+1)
  ENDDO
ENDDO

! -----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIDIA2',1,ZHOOK_HANDLE)
END SUBROUTINE TRIDIA2

SUBROUTINE GPHPRE_EXPL (YDCVER, TOPPRES, YDCST, KPROMA, KFLEV, KST, KEND, YDVAB, PRESH, PRESF, LHSET, &
                     &  LDELP, LALPHA, LRTGR, LRPP, PDELP, PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP)

!$ACDC singlecolumn  --process-pointers


!**** *GPHPRE_EXPL* - Computes half and full level pressure
!                Modern version of former GPPRE.
!                Modern version of former GPPREH+GPXYB+GPPREF

!     Purpose.
!     --------
!           Computes pressures at half and full model levels.

!**   Interface.
!     ----------
!        *CALL* *GPHPRE_EXPL(...)

!        Explicit arguments :
!        --------------------

!          KPROMA    : horizontal dimensioning                                (in)
!          KFLEV     : vertical dimensioning                                  (in)
!          KST    : start of work                                          (in)
!          KEND     : depth of work                                          (in)
!          YDVAB     : contains information about hybrid vertical coordinate  (in)
!          PRESH     : half level pressure                                    (inout)
!          PRESF     : full level pressure                                    (opt out)
!          LDELP,LALPHA,... : activation keys for partial computations        (opt in)

!        Implicit arguments :  NONE.
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.  None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      K. YESSAD (Sep 2011) after GPPRE, GPPREH, GPXYB and GPPREF.

!     Modifications.
!     --------------
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (Mar 2017): Introduce NDLNPR=2 for NHQE model.
!   H Petithomme (Dec 2020): add options, use of pointers, group VFE tests
!     ------------------------------------------------------------------

USE PARKIND1,ONLY: JPIM, JPRB
USE YOMHOOK, ONLY: LHOOK, JPHOOK, DR_HOOK

USE YOMCST,    ONLY: TCST
USE YOMVERT,   ONLY: TVAB
USE YOMCVER,   ONLY: TCVER

IMPLICIT NONE

TYPE(TCVER)        ,INTENT(IN)               :: YDCVER
REAL(KIND=JPRB)    ,INTENT(IN)               :: TOPPRES
TYPE(TCST)         ,INTENT(IN)               :: YDCST
INTEGER(KIND=JPIM) ,INTENT(IN)               :: KPROMA,KFLEV,KST,KEND
TYPE(TVAB)         ,INTENT(IN)               :: YDVAB
REAL(KIND=JPRB)    ,INTENT(INOUT)            :: PRESH(KPROMA,0:KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)   ,OPTIONAL  :: PRESF(KPROMA,KFLEV)
LOGICAL            ,INTENT(IN)    ,OPTIONAL  :: LHSET,LDELP,LALPHA,LRTGR,LRPP
REAL(KIND=JPRB)    ,INTENT(OUT)   ,OPTIONAL  :: PDELP(KPROMA,KFLEV),PLNPR(KPROMA,KFLEV),PRDELP(KPROMA,KFLEV),PALPH(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)   ,OPTIONAL  :: PRTGR(KPROMA,KFLEV),PRPRE(KPROMA,KFLEV),PRPP(KPROMA,KFLEV)

#include "gphpre_expl_vertfe0.intfb.h"
#include "gphpre_expl_vertfe1.intfb.h"

INTEGER(KIND=JPIM) :: JLEV
LOGICAL :: LLHSET

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GPHPRE_EXPL', 0, ZHOOK_HANDLE)

LLHSET = .FALSE.
IF (PRESENT (LHSET)) LLHSET = LHSET

IF (.NOT.LLHSET) THEN
  DO JLEV=0,KFLEV-1
    PRESH(KST:KEND,JLEV) = YDVAB%VAH(JLEV)+YDVAB%VBH(JLEV)*PRESH(KST:KEND,KFLEV)
  ENDDO
ENDIF

IF (YDCVER%LVERTFE) THEN
  CALL GPHPRE_EXPL_VERTFE1 (YDCVER, TOPPRES, YDCST, KPROMA, KFLEV, KST, KEND, YDVAB, PRESH, PRESF, LHSET, &
                          & LDELP, LALPHA, LRTGR, LRPP, PDELP, PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP)
ELSE
  CALL GPHPRE_EXPL_VERTFE0 (YDCVER, TOPPRES, YDCST, KPROMA, KFLEV, KST, KEND, YDVAB, PRESH, PRESF, LHSET, &
                          & LDELP, LALPHA, LRTGR, LRPP, PDELP, PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP)
ENDIF

IF (LHOOK) CALL DR_HOOK('GPHPRE_EXPL', 1, ZHOOK_HANDLE)

END SUBROUTINE GPHPRE_EXPL


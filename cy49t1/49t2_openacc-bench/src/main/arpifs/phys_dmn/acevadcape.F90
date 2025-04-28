SUBROUTINE ACEVADCAPE(YDPHY2, YDCST, KIDIA,KFDIA, KLON, KLEV,&
      & PFPLSL,PFPLSN,PFPLCL,PFPLCN,PAPRS,PAPRSF,PT,PCP,PAPHIF,PAPHI,PDCAPE)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST   , ONLY : TCST
USE YOMPHY2  , ONLY : TPHY2

!**** *ACEVADCAPE*  - Compute DCAPE due to precipitation evaporation.

!     PURPOSE.
!     --------
!**   INTERFACE.
!     ----------
!       *CALL* *ACEVADCAPE*

!        EXPLICIT ARGUMENTS
!        --------------------
!            INPUT :
!        KIDIA   : start of work
!        KFDIA   : end of work
!        KLON    : depth of work
!        KLEV    : number of levels
!            OUTPUT:

!     METHOD.
!     -------

!     EXTERNALS.
!     ----------
   
!     REFERENCE.
!     ----------
!        None

!     AUTHOR.
!     -------
!        J.M. Piriou.

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 2018-10-13
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE (TPHY2),INTENT (IN) :: YDPHY2
TYPE (TCST), INTENT (IN) :: YDCST
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
REAL(KIND=JPRB),INTENT(IN)    :: PFPLSL(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PFPLSN(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PFPLCL(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PFPLCN(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PCP(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAPHIF(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAPHI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PDCAPE(KLON)

REAL(KIND=JPRB)    :: ZSIG,ZTTEND,ZDFP,ZTAU,ZDELTA,ZB
INTEGER(KIND=JPIM) :: ISIG
INTEGER(KIND=JPIM) :: JLON

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

#include "fcttrm.func.h"

IF (LHOOK) CALL DR_HOOK('ACEVADCAPE',0,ZHOOK_HANDLE)
ASSOCIATE(TSPHY=>YDPHY2%TSPHY, RG=>YDCST%RG, RTT=>YDCST%RTT)

! ISIG: level close to the sigma level ZSIG.
ZSIG=0.92_JPRB
ISIG=NINT(ZSIG*REAL(KLEV))

! Time scale for precipitation to fall across PBL: 800m divided by 6 m/s.
ZTAU=130._JPRB

DO JLON=KIDIA,KFDIA
  ZDELTA=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-PT(JLON,KLEV)))
  ! ZDFP: difference between surface precipitation and precipitation at level ISIG.
  ZDFP=PFPLSL(JLON,KLEV)+PFPLSN(JLON,KLEV)+PFPLCL(JLON,KLEV)+PFPLCN(JLON,KLEV) &
    & -PFPLSL(JLON,ISIG)-PFPLSN(JLON,ISIG)-PFPLCL(JLON,ISIG)-PFPLCN(JLON,ISIG)
  ! Mean T tendency due to precipitation ecaporation.
  ZTTEND=FOLH(PT(JLON,KLEV),ZDELTA)/PCP(JLON,KLEV)*RG*MIN(0._JPRB,ZDFP) &
    & /(PAPRS(JLON,KLEV)-PAPRSF(JLON,ISIG))
  ! Buoyancy.
  ZB=RG/PT(JLON,KLEV)*ZTTEND*ZTAU
  ! DCAPE.
  PDCAPE(JLON)=ZB*(PAPHIF(JLON,ISIG)-PAPHI(JLON,KLEV))/RG
ENDDO


END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACEVADCAPE',1,ZHOOK_HANDLE)

END SUBROUTINE ACEVADCAPE


SUBROUTINE GPRT(LDSPRT,KPROMA,KST,KEND,KLEV,PRD,PRV,PR,&
 & PT,PTL,PTM,PQL,PQM,PRT,PRTL,PRTM,PRL,PRM)  

!*    *GPRT* 

!     Purpose
!     -------  To calculate RT and its derivates 
!     Interface
!     ---------

!     Explicit arguments
!     ------------------
!     Input:
!    -------
!              LDSPRT   : .TRUE. if PTL and PTM already contain
!                         the derivatives of 'TV')
!              KPROMA   : Horizontal dimension
!              KSTART   : Start index
!              KEND     : End index
!              KLEV     : number of levels
!              PRD      : Rd
!              PRV      : Rv
!              PR       : R
!              PT       : T
!              PTL ,PTM : Horizontal derivatives of T
!              PQL, PQM : Horizontal derivatives of q
!     Output:
!    --------
!              PRT       : RT
!              PRTL,PRTM : Horizontal derivatives of RT

!     Author
!     ------
!           J.Boutahar *MAROC-METEO*
!     Modifications
!     -------------
!           Original: 97/06/06
!     C. Fischer 02-06-27 : cdlock
!     M.Hamrud      01-Oct-2003 CY28 Cleaning
!     K. Yessad (Dec 2008): remove dummy CDLOCK
!     H Petithomme (Dec 2020): tests simplification and hoisting
!----------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

!----------------------------------------------------------

IMPLICIT NONE

LOGICAL            ,INTENT(IN)             :: LDSPRT 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KPROMA 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KST 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KEND 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KLEV 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PRD 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PRV 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PR(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PT(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PTL(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PTM(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PQL(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PQM(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PRT(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PRTL(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PRTM(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PRL(KPROMA,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PRM(KPROMA,KLEV) 

!----------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF
REAL(KIND=JPRB) :: ZR
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!----------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GPRT',0,ZHOOK_HANDLE)

ZR = PRV-PRD

!---------------------------------------------------------------
!*       1. Compute RT and its derivatives

DO JLEV = 1, KLEV
  DO JROF = KST, KEND
    PRT(JROF,JLEV)=PR(JROF,JLEV)*PT(JROF,JLEV)
  ENDDO
ENDDO

IF (LDSPRT) THEN
  DO JLEV=1,KLEV
    DO JROF=KST,KEND
      PRTL(JROF,JLEV)=PRD*PTL(JROF,JLEV)
      PRTM(JROF,JLEV)=PRD*PTM(JROF,JLEV)
    ENDDO
  ENDDO
ELSEIF (PRESENT(PRL)) THEN
  DO JLEV=1,KLEV
    DO JROF=KST,KEND
      PRTL(JROF,JLEV)=PRL(JROF,JLEV)*PT(JROF,JLEV)+PR(JROF,JLEV)*PTL(JROF,JLEV)
      PRTM(JROF,JLEV)=PRM(JROF,JLEV)*PT(JROF,JLEV)+PR(JROF,JLEV)*PTM(JROF,JLEV)
    ENDDO
  ENDDO
ELSE
  DO JLEV=1,KLEV
    DO JROF=KST,KEND
      PRTL(JROF,JLEV)=ZR*PT(JROF,JLEV)*PQL(JROF,JLEV)+PR(JROF,JLEV)*PTL(JROF,JLEV)
      PRTM(JROF,JLEV)=ZR*PT(JROF,JLEV)*PQM(JROF,JLEV)+PR(JROF,JLEV)*PTM(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

!----------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GPRT',1,ZHOOK_HANDLE)
END SUBROUTINE GPRT


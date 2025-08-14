SUBROUTINE SIDD (YDCST,YDGEOMETRY, YDDYN, YDDYNA, KLON, KLEV, KIDIA, KFDIA, PDH, PDV, PRNH, PT, PSP)

!$ACDC singleblock 

!**** * SIDD*    - Provide 3D divergence increment in semi-implicit
!                          scheme for the case of nonhydrostatic dynamics

!     Purpose.
!     --------
!           Operators gamma, h, L.

!**   Interface.
!     ----------
!        *CALL* *SIDD(...)

!        Explicit arguments :
!        --------------------
!        YDCST         : structure containing model constants
!        YDGEOMETRY    : structure containing geometry
!        YDDYN         : structure containing dynamics from YOMDYN
!        YDDYNA        : structure containing dynamics from YOMDYNA
!        KLON : TOTAL NUMBER OF COLUMNS IN THE FIVE PARAMETER ARRAYS 
!               (PDH,PDV,PRNH,PT,PSP)
!        KLEV : NUMBER OF VERTICAL LEVELS IN THE 3D PARAMETER ARRAYS,
!                AND PROCESSED
!        KDIA : FIRST COLUMN PROCESSED 
!        KFDIA: LAST COLUMN PROCESSED      
!
!
!        PDH   : HORIZONTAL DIVERGENCE                       - output
!        PDV   : VERTICAL DIVERGENCE VARIABLE                - output
!        PRNH  : NONHYDROSTATIC PRESSURE DEPARTURE VARIABLE  - input
!        PT    : TEMPERATURE                                 - input
!        PSP   : SURFACE PRESSURE                            - input

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        Arpege/Aladin Documentation

!     Author.
!     -------
!      Radmila Bubnova  *GMAP/COMPAS - stage MRE*
!      Original : 93-03-24

!     Modifications.
!     --------------
!      K. Yessad (Sep 2008): update comments + cleanings.
!      K. Yessad (June 2017): Vertical-dependent SITRA.
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST       , ONLY : TCST
USE YOMDYN       , ONLY : TDYN
USE YOMDYNA      , ONLY : TDYNA

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
TYPE(TDYNA)       ,INTENT(IN)    :: YDDYNA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PDH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRNH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSP(KLON) 

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZW3D(KLON,KLEV)
REAL(KIND=JPRB) :: ZW2D(KLON)
REAL(KIND=JPRB) :: ZIN(KLON,KLEV)

INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZDIM, ZKAP_B
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "siseve.intfb.h"
#include "sigam.intfb.h"
#include "sigam_nh.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SIDD',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

!*       1.    SIGAM ON PT AND PSP: AS IN HYDROSTATIC.
!              ---------------------------------------

CALL SIGAM(YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,PDH,PT,PSP)

!     ------------------------------------------------------------------

!*       2.    SIGAM ON PRNH, GAMMA APPLIED ONLY, FINISH PDH.
!              ----------------------------------------------

IF (YDDYNA%LNHEE.AND.(.NOT.YDDYNA%LSI_NHEE)) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLON)
  DO JLON = KIDIA,KFDIA
    ZW2D(JLON) = 0.0_JPRB
  ENDDO
!$OMP END PARALLEL DO

  CALL SIGAM(YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,ZW3D,PRNH,ZW2D)

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON)
  DO JLEV = 1,KLEV
    DO JLON = KIDIA,KFDIA
      PDH(JLON,JLEV) = PDH(JLON,JLEV) &
       & - (1.0_JPRB-YDDYN%SITVAR)*YDDYN%SITR*ZW3D(JLON,JLEV) &
       & + YDCST%RD*YDDYN%SITR*PRNH(JLON,JLEV)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

ELSE

  CALL SIGAM_NH(YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,ZW3D,PRNH)

  ZKAP_B=1.0_JPRB
  IF (YDDYNA%LNHQE.AND.YDDYN%LNHQE_SOLVER_SP) ZKAP_B=YDCST%RKAPPA

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON)
  DO JLEV = 1,KLEV
    DO JLON = KIDIA,KFDIA
      PDH(JLON,JLEV) = PDH(JLON,JLEV) &
       & + YDCST%RD*YDDYN%SITRM(JLEV)*PRNH(JLON,JLEV) &
       & - ZKAP_B*(1.0_JPRB-YDDYN%SITVAR)*YDDYN%SITR*ZW3D(JLON,JLEV)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

ENDIF

!     ------------------------------------------------------------------

!*       3.    SISEVE ON PRNH, FINISH PDV.
!              --------------------------

CALL SISEVE(YDCST,YDGEOMETRY,YDDYN,YDDYNA,KLON,KLEV,KIDIA,KFDIA,PRNH,ZW3D)

ZDIM = YDCST%RG*YDCST%RG/(YDCST%RD*YDDYN%SITR)
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON)
DO JLEV = 1, KLEV
  DO JLON = KIDIA,KFDIA
    PDV(JLON,JLEV) = ZDIM*ZW3D(JLON,JLEV)
  ENDDO
ENDDO
!$OMP END PARALLEL DO

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SIDD',1,ZHOOK_HANDLE)

END SUBROUTINE SIDD


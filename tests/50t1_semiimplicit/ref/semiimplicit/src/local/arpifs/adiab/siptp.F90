SUBROUTINE SIPTP (YDCST, YDGEOMETRY, YDDYN, YDDYNA, KLON, KLEV, KIDIA, KFDIA, PDH, PDV, PRNH, PT, PSP)

!$ACDC singleblock 

!**** *SIPTP* - Counterpart of SITNU in nonhydrostatic

!     Purpose.
!     --------
!           Provide corrections of temperature, surface
!           pressure and nonhydrostatic perturbation from
!           3D divergence in semiimplicit

!**   Interface.
!     ----------
!        *CALL* *SIPTP(...)

!        Explicit arguments :
!        --------------------
!        YDCST         : structure containing model constants
!        YDGEOMETRY    : structure containing geometry
!        YDDYN         : structure containing dynamics from YOMDYN
!
!        KLON : TOTAL NUMBER OF COLUMNS IN THE FIVE PARAMETER ARRAYS 
!               (PDH,PDV,PRNH,PT,PSP)
!        KLEV : NUMBER OF VERTICAL LEVELS IN THE 3D PARAMETER ARRAYS,
!                AND PROCESSED
!        KDIA : FIRST COLUMN PROCESSED 
!        KFDIA: LAST COLUMN PROCESSED      
!
!
!        PDH   : HORIZONTAL DIVERGENCE                       - input
!        PDV   : VERTICAL DIVERGENCE VARIABLE                - input
!        PRNH  : NONHYDROSTATIC PRESSURE DEPARTURE VARIABLE  - output
!        PT    : TEMPERATURE                                 - output
!        PSP   : LOG(SURFACE PRESSURE)                       - output
!
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
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      K. Yessad (Sep 2008): update comments + cleanings.
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDH(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDV(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRNH(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSP(KLON)

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLON, JLEV

REAL(KIND=JPRB) :: Z3DIVC
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "sitnu.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SIPTP',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

!*       1.    SITNU ON PDH TO GET PSP AND PART OF PRNH
!              ----------------------------------------


CALL SITNU (YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,PDH,PRNH,PSP)


!     ------------------------------------------------------------------

!*       2.    FINISH PT AND PRNH CALCULATION.
!              -------------------------------
IF (YDDYNA%LNHEE.AND.(YDDYNA%NTPVAR==1)) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON,Z3DIVC)
DO JLEV = 1, KLEV
!DEC$ IVDEP
  DO JLON = KIDIA,KFDIA
    Z3DIVC = (PDH(JLON,JLEV) + PDV(JLON,JLEV))/YDCST%RCVD
    PT(JLON,JLEV) = YDCST%RD*YDDYN%SITRM(JLEV)*Z3DIVC
    PRNH(JLON,JLEV) = YDCST%RCPD*Z3DIVC - YDCST%RCPD*( &
     & PRNH(JLON,JLEV)*(1.0_JPRB+YDGEOMETRY%YRVERT_GEOM%YRCVER%RFAC1)) &
     & /(YDCST%RD*YDDYN%SITR)
  ENDDO
ENDDO
!$OMP END PARALLEL DO

ELSEIF (YDDYNA%LNHEE.AND.(YDDYNA%NTPVAR==2)) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON,Z3DIVC)
DO JLEV = 1,KLEV
!DEC$ IVDEP
  DO JLON = KIDIA,KFDIA
    Z3DIVC = (PDH(JLON,JLEV) + PDV(JLON,JLEV))/YDCST%RCVD
    PT(JLON,JLEV) = YDDYN%SITRM(JLEV)*( &
     & -YDCST%RCVD*Z3DIVC + (YDCST%RCPD/(YDCST%RD*YDDYN%SITR))*PRNH(JLON,JLEV) )
    PRNH(JLON,JLEV) = YDCST%RCPD*Z3DIVC  &
     & -(YDCST%RCPD/(YDCST%RD*YDDYN%SITR))*PRNH(JLON,JLEV)
  ENDDO
ENDDO
!$OMP END PARALLEL DO

ELSEIF ((YDDYNA%LNHHY.OR.YDDYNA%LNHQE).AND.(YDDYNA%NTPVAR==1)) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON,Z3DIVC)
DO JLEV = 1,KLEV
!DEC$ IVDEP
  DO JLON = KIDIA,KFDIA
    Z3DIVC = (PDH(JLON,JLEV) + PDV(JLON,JLEV))/YDCST%RCVD
    PT(JLON,JLEV) = YDDYN%SITRM(JLEV)*( &
     YDDYN%RNHQE_DELTAM*YDDYN%RNHHY_DELTAM(JLEV)*YDCST%RD*Z3DIVC &
     & +(1.0_JPRB-YDDYN%RNHQE_DELTAM*YDDYN%RNHHY_DELTAM(JLEV))*(PRNH(JLON,JLEV)/YDDYN%SITR) )
    PRNH(JLON,JLEV) = YDDYN%RNHQE_DELTAM*YDDYN%RNHHY_DELTAM(JLEV)*YDCST%RCPD*( &
     & (Z3DIVC - (PRNH(JLON,JLEV)/(YDCST%RD*YDDYN%SITR))) )
  ENDDO
ENDDO
!$OMP END PARALLEL DO

ELSEIF ((YDDYNA%LNHHY.OR.YDDYNA%LNHQE).AND.(YDDYNA%NTPVAR==2)) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON,Z3DIVC)
DO JLEV = 1,KLEV
!DEC$ IVDEP
  DO JLON = KIDIA,KFDIA
    Z3DIVC = (PDH(JLON,JLEV) + PDV(JLON,JLEV))/YDCST%RCVD
    PT(JLON,JLEV) = YDDYN%SITRM(JLEV)*( &
     & YDDYN%RNHQE_DELTAM*YDDYN%RNHHY_DELTAM(JLEV)*( &
     & -YDCST%RCVD*Z3DIVC + (YDCST%RCPD/(YDCST%RD*YDDYN%SITR))*PRNH(JLON,JLEV)) &
     & +(1.0_JPRB-YDDYN%RNHQE_DELTAM*YDDYN%RNHHY_DELTAM(JLEV))*(PRNH(JLON,JLEV)/YDDYN%SITR) )
    PRNH(JLON,JLEV) = YDDYN%RNHQE_DELTAM*YDDYN%RNHHY_DELTAM(JLEV)*YDCST%RCPD*( &
     & (Z3DIVC - (PRNH(JLON,JLEV)/(YDCST%RD*YDDYN%SITR))) )
  ENDDO
ENDDO
!$OMP END PARALLEL DO

ENDIF

!     --------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SIPTP',1,ZHOOK_HANDLE)
END SUBROUTINE SIPTP


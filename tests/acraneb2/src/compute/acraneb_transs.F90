!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_TRANSS(YDPHY,YDPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,LDMASKS,&
! - INPUT 2D
 & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,&
! - INPUT 1D
 & PDM0I,&
! - OUTPUT 2D
 & PDEOSI,PUEOSI)

! Purpose:
! --------
!   ACRANEB_TRANSS - Computation of solar gaseous optical depths.

! Interface:
! ----------
! INPUT:
!   KIDIA   - initial index for horizontal loops
!   KFDIA   - final index for horizontal loops
!   KLON    - horizontal dimension of arrays
!   KTDIA   - initial index for vertical loops (usually 1)
!   KLEV    - vertical dimension of full level arrays
!   KJN     - dimension of arrays containing "daylight" intervals
!   KIIDIA  - array of indices marking start of "daylight" intervals
!   KIFDIA  - array of indices marking end of "daylight" intervals
!   KAUCR   - number of "daylight" intervals
!   PAPRS   - pressure on half-levels
!   PAPRSF  - pressure on full-levels
!   PDELP   - layer thickness in pressure units
!   PR      - gas constant for air
!   PT      - temperature
!   PQ      - specific humidity
!   PQCO2   - specific mass of CO2 with respect to dry air
!   PQO3    - specific mass of ozone with respect to dry air
!   PDM0I   - half inverse of the modified cosine of the zenith angle
! OUTPUT:
!   PDEOSI  - incremental gaseous optical depth (solar descending)
!   PUEOSI  - incremental gaseous optical depth (solar ascending)

! Externals:
! ----------

! Method:
! -------

! Reference:
! ----------

! Author:
! -------
!   1989-12, J.-F. Geleyn (original ACRANEB)

! Modifications:
! --------------
!   2009-10, T. Kral
!   Externalized from ACRANEB.

!   2011-06, R. Brozkova
!   Intermittent call.

!   2013-11, J. Masek
!   New ACRANEB2 gaseous transmissions, modularization. Backphasing to cy38t1.
! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1 ,ONLY : JPIM     ,JPRB     ,JPRD
USE YOMCST   ,ONLY : RPI      ,RD       ,RV
USE YOMPHY   ,ONLY : TPHY
USE YOMPHY3  ,ONLY : TPHY3

IMPLICIT NONE

TYPE(TPHY), INTENT(INOUT):: YDPHY
TYPE(TPHY3),INTENT(INOUT):: YDPHY3
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA
LOGICAL,          INTENT(IN)  :: LDMASKS(KLON)

REAL(KIND=JPRB),INTENT(IN)    :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PR(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQ(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQCO2(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQO3(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PDM0I(KLON)
REAL(KIND=JPRB),INTENT(OUT)   :: PDEOSI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PUEOSI(KLON,0:KLEV)

#include "abor1.intfb.h"

! -----

REAL(KIND=JPRB) :: ZA,ZB,ZARGLI,ZEPSD,ZEPSP,ZEPSU,ZTRLI

REAL(KIND=JPRB) :: ZEPSV,ZGAMV,ZIBV0,ZIZV0,ZMD,ZVOISIM

TYPE TS_RHOZ0V
REAL(KIND=JPRB) :: V(3)
END TYPE

TYPE (TS_RHOZ0V) :: YS_RHOZ0V

REAL(KIND=JPRB) :: ZDELP(KLON)
REAL(KIND=JPRB) :: ZEOSO(KLON)
REAL(KIND=JPRB) :: ZNSOR(KLON)
REAL(KIND=JPRB) :: ZP   (KLON)

REAL(KIND=JPRB) :: ZDU        (KLON,4)
REAL(KIND=JPRB) :: ZS_U       (KLON,3)
REAL(KIND=JPRB) :: ZS_PU      (KLON,3)
REAL(KIND=JPRB) :: ZS_TU      (KLON,3)
REAL(KIND=JPRB) :: ZS_UW      (KLON,3)
REAL(KIND=JPRB) :: ZS_US      (KLON,3)
REAL(KIND=JPRB) :: ZS_US_IRHOV(KLON,3)
REAL(KIND=JPRB) :: ZS_UC      (KLON)

REAL(KIND=JPRB) :: ZQ     (KLON,  KLEV)
REAL(KIND=JPRB) :: ZIRHOV (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZDEOSA (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZUEOSA (KLON,0:KLEV)

REAL(KIND=JPRB) :: ZS_FW(KLON,0:KLEV,3)
REAL(KIND=JPRB) :: ZS_FS(KLON,0:KLEV,3)
REAL(KIND=JPRB) :: ZS_FC(KLON,0:KLEV)

REAL(KIND=JPRB) :: Z4BU_,ZA2B_,ZAFVOI_,ZVOIEMP_,ZVOIGT_
REAL(KIND=JPRB) :: ZAUX_,ZLOG_,ZP_AVG_,ZT_AVG_

REAL(KIND=JPRB) :: ZMD_W_ (KLON)
REAL(KIND=JPRB) :: ZMD_S_ (KLON)
REAL(KIND=JPRB) :: ZDELTA_(KLON,3)
REAL(KIND=JPRB) :: ZA_    (KLON,3)
REAL(KIND=JPRB) :: ZAR_   (KLON,3)
REAL(KIND=JPRB) :: ZCOEF_ (KLON,3)
REAL(KIND=JPRB) :: ZTAU_  (KLON,3)


! local integer scalars
INTEGER(KIND=JPIM) :: JG,JLEV,JLON,JN,JO

! local logical scalars
LOGICAL :: LL_DESC,LL_OZONE


ASSOCIATE(FGTS_DELTA0=>YDPHY3%FGTS_DELTA0, FGTS_OC=>YDPHY3%FGTS_OC, &
 & FGTS_OB=>YDPHY3%FGTS_OB, FGTS_OA=>YDPHY3%FGTS_OA, FGTS_OD=>YDPHY3%FGTS_OD, &
 & FGTS_P00=>YDPHY3%FGTS_P00, FGTS_Q=>YDPHY3%FGTS_Q, FGTS_P=>YDPHY3%FGTS_P, &
 & FGTS_ALPHA=>YDPHY3%FGTS_ALPHA, FGTS_D=>YDPHY3%FGTS_D, FGTS_A=>YDPHY3%FGTS_A, &
 & FGTS_C=>YDPHY3%FGTS_C, FGTS_B=>YDPHY3%FGTS_B, &
 & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX)

! -----
! security constants, derived parameters for Voigt effect and
! Malkmus formula
! -----

! security constants
IF (JPRB == JPRD) THEN
  ZARGLI=-250._JPRB
ELSE
  ZARGLI=-80._JPRB
ENDIF
ZTRLI =EXP(ZARGLI)
ZEPSP =1.E-03_JPRB
IF (JPRB == JPRD) THEN
  ZEPSD =1.E-20_JPRB
ELSE
  ZEPSD =1.E-10_JPRB
ENDIF
ZEPSU =ZTRLI

! Voigt coefficients
YS_RHOZ0V%V(1)=0.296_JPRB
YS_RHOZ0V%V(2)=0.046_JPRB
YS_RHOZ0V%V(3)=0.202_JPRB

ZGAMV       =0.36_JPRB
ZEPSV       =0.014_JPRB
ZIBV0       =1._JPRB/0.74_JPRB
ZIZV0       =1._JPRB/0.0027_JPRB
ZVOISIM     =1.21_JPRB

! ratio of diffusivity factors sqrt(e) and 2 (the latter is used in
! weak line limit)
ZMD=0.5_JPRB*EXP(0.5_JPRB)

! special treatment of solar ozone (temporary fix)
LL_OZONE=.TRUE.

! -----
! preparations
! -----

! safety - truncate specific humidity to interval [0, 1]
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZQ(JLON,JLEV)=MAX(0._JPRB,MIN(1._JPRB,PQ(JLON,JLEV)))
  ENDDO
ENDDO

! determine pressure for computations at model top
DO JLON=KIDIA,KFDIA
  ZDELP(JLON)=MAX(ZEPSP,PAPRS(JLON,KTDIA-1))
  ZP   (JLON)=0.5_JPRB*ZDELP(JLON)
ENDDO

! computation of doubled ozone quantity above starting layer KTDIA
DO JLON=KIDIA,KFDIA
  ZNSOR(JLON)=2._JPRB*MAX(ZEPSP,PAPRS(JLON,0))*PQO3(JLON,0)
ENDDO
DO JLEV=1,KTDIA-1
  DO JLON=KIDIA,KFDIA
    ZNSOR(JLON)=ZNSOR(JLON)+2._JPRB*PDELP(JLON,JLEV)*PQO3(JLON,JLEV)
  ENDDO
ENDDO

! compute inverse air density
DO JLON=KIDIA,KFDIA
  ZIRHOV(JLON,KTDIA-1)=(PR(JLON,KTDIA)*PT(JLON,KTDIA))/ZP(JLON)
ENDDO
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZIRHOV(JLON,JLEV)=(PR(JLON,JLEV)*PT(JLON,JLEV))/PAPRSF(JLON,JLEV)
  ENDDO
ENDDO

! loop through gases
DO JG=1,3

  ! computation of pressure/temperature factors for u_w, u_s
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZA=FGTS_A(JG,0)*(1._JPRB+FGTS_A(JG,1)*PT(JLON,KTDIA))/&
       &              (1._JPRB+FGTS_A(JG,2)*PT(JLON,KTDIA))
      ZB=FGTS_B(JG,0)*(1._JPRB+FGTS_B(JG,1)*PT(JLON,KTDIA))/&
       &              (1._JPRB+FGTS_B(JG,2)*PT(JLON,KTDIA))
      ZS_FW(JLON,KTDIA-1,JG)=ZA
      ZS_FS(JLON,KTDIA-1,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*ZP(JLON)
    !ENDIF
  ENDDO
  DO JLEV=KTDIA,KLEV
		DO JLON=KIDIA,KFDIA
			!IF ( LDMASKS(JLON) ) THEN
        ZA=FGTS_A(JG,0)*(1._JPRB+FGTS_A(JG,1)*PT(JLON,JLEV))/&
         &              (1._JPRB+FGTS_A(JG,2)*PT(JLON,JLEV))
        ZB=FGTS_B(JG,0)*(1._JPRB+FGTS_B(JG,1)*PT(JLON,JLEV))/&
         &              (1._JPRB+FGTS_B(JG,2)*PT(JLON,JLEV))
        ZS_FW(JLON,JLEV,JG)=ZA
        ZS_FS(JLON,JLEV,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*PAPRSF(JLON,JLEV)
      !ENDIF
    ENDDO
  ENDDO

ENDDO

! initialize pressure/temperature factors for H2O e-type continuum
IF ( FGTS_C(1) == 0._JPRB ) THEN
  ! daand: added explicit loops
  DO JLEV=KTDIA-1,KLEV
	  DO JLON=KIDIA,KFDIA
		  ZS_FC(JLON,JLEV)=0._JPRB
		ENDDO
	ENDDO
ELSE
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZS_FC(JLON,KTDIA-1)=FGTS_C(1)*EXP(-FGTS_C(2)*PT(JLON,KTDIA))*&
       & ZP(JLON)
    !ENDIF
  ENDDO
  DO JLEV=KTDIA,KLEV
		DO JLON=KIDIA,KFDIA
			!IF ( LDMASKS(JLON) ) THEN
        ZS_FC(JLON,JLEV)=FGTS_C(1)*EXP(-FGTS_C(2)*PT(JLON,JLEV))*&
         & PAPRSF(JLON,JLEV)
      !ENDIF
    ENDDO
  ENDDO
ENDIF

! -----
! computation of gaseous optical depths:
! - descending calculations are symmetrical between solar and thermal band
!   (except for diffusivity factor)
! - ascending calculations (diffuse in both cases) are for a reflected
!   solar flux and for thermal flux corresponding to the exchange with
!   surface
! -----

! -----
! model top
! -----

! use 1/m0 in solar band instead of diffusivity factor
LL_DESC=.TRUE.

! compute unscaled absorber amounts 2.du (2 stands for diffusivity factor
! in weak line limit) and inverse air density; absorber amount for H2O
! e-type continuum is multiplied by ratio e/p (water vapor pressure
! to total pressure)
DO JLON=KIDIA,KFDIA
  ZDU(JLON,1)=2._JPRB*ZDELP(JLON)*ZQ   (JLON,KTDIA)
  ZDU(JLON,2)=2._JPRB*ZDELP(JLON)*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,3)=                    ZNSOR(JLON)      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))
ENDDO

! initialize auxiliary quantities u_w, u_s, u_s_rho and u_c
! daand: added explicit loops
DO JN=1,3
	DO JLON=KIDIA,KFDIA
		ZS_UW      (JLON,JN)=ZEPSU
		ZS_US      (JLON,JN)=ZEPSU
		ZS_US_IRHOV(JLON,JN)=ZEPSU
		ZS_U       (JLON,JN)=ZEPSU
		ZS_PU      (JLON,JN)=ZEPSU
		ZS_TU      (JLON,JN)=ZEPSU
	ENDDO
ENDDO
DO JLON=KIDIA,KFDIA
  ZS_UC      (JLON)  =ZEPSU
ENDDO

! compute total and incremental optical depths
! (solar incremental multiplied by 2.mu0)
! daand: a bit worried about PT and ZDEOSA being passed as scalars here ...
CALL DELTA_S(KTDIA-1,LL_DESC,ZP,PT(:,KTDIA),ZDU,&
 & ZS_UW,ZS_US,ZS_US_IRHOV,ZS_UC,ZS_U,ZS_PU,ZS_TU,&
 & ZDEOSA(:,KTDIA-1))
DO JLON=KIDIA,KFDIA
	!IF ( LDMASKS(JLON) ) THEN
    PDEOSI(JLON,KTDIA-1)=ZDEOSA(JLON,KTDIA-1)/PDM0I(JLON)
  !ENDIF
ENDDO

! -----
! descending vertical loop
! -----
! - temporary 1D arrays:
! ZEOSO     : "old" solar   optical depth (for computing "new" one)
! -----

DO JLEV=KTDIA,KLEV

	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZEOSO(JLON)=ZDEOSA(JLON,JLEV-1)
    !ENDIF
  ENDDO

  ! compute unscaled absorber amounts 2.du
  DO JLON=KIDIA,KFDIA
    ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  ENDDO

  ! compute total and incremental optical depths
  ! (solar incremental multiplied by 2.mu0)
  CALL DELTA_S(JLEV,LL_DESC,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
   & ZS_UW,ZS_US,ZS_US_IRHOV,ZS_UC,ZS_U,ZS_PU,ZS_TU,&
   & ZDEOSA(:,JLEV))
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      PDEOSI(JLON,JLEV)=&
       & MAX((ZDEOSA(JLON,JLEV)-ZEOSO(JLON))/PDM0I(JLON),0._JPRB)
    !ENDIF
  ENDDO

ENDDO

! -----
! surface condition
! -----

! solar depths computed from top to surface and up to given level
DO JLON=KIDIA,KFDIA
	!IF ( LDMASKS(JLON) ) THEN
    ZEOSO(JLON)=ZDEOSA(JLON,KLEV)
  !ENDIF
ENDDO

! -----
! ascending vertical loop
! -----

! use diffusivity factor in solar band instead of 1/mu0
LL_DESC=.FALSE.

DO JLEV=KLEV,KTDIA,-1

  ! compute unscaled absorber amounts 2.du
  DO JLON=KIDIA,KFDIA
    ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  ENDDO

  ! compute total and incremental optical depths
	! daand: a bit worried about PT and ZUEOSA being passed as scalars here ...
  CALL DELTA_S(JLEV,LL_DESC,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
   & ZS_UW,ZS_US,ZS_US_IRHOV,ZS_UC,ZS_U,ZS_PU,ZS_TU,&
   & ZUEOSA(:,JLEV))
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      PUEOSI(JLON,JLEV)=MAX(ZUEOSA(JLON,JLEV)-ZEOSO(JLON),0._JPRB)
      ZEOSO(JLON)      =ZUEOSA(JLON,JLEV)
    !ENDIF
  ENDDO

ENDDO

END ASSOCIATE

! -----
! private procedures
! -----

CONTAINS

! computation of solar optical depths
SUBROUTINE DELTA_S(KL,LD_DESC,PRES,PT,PDU,PUW,PUS,PUS_IRHOV,PUC,&
 & P_U,P_PU,P_TU,PDELTA)

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN)    :: KL                 ! current level
LOGICAL           , INTENT(IN)    :: LD_DESC            ! selector for
                                                        ! diffusivity factor
REAL   (KIND=JPRB), INTENT(IN)    :: PRES     (KLON)    ! pressure
REAL   (KIND=JPRB), INTENT(IN)    :: PT       (KLON)    ! temperature
REAL   (KIND=JPRB), INTENT(IN)    :: PDU      (KLON,4)  ! 2.du
REAL   (KIND=JPRB), INTENT(INOUT) :: PUW      (KLON,3)  ! u_w
REAL   (KIND=JPRB), INTENT(INOUT) :: PUS      (KLON,3)  ! u_s
REAL   (KIND=JPRB), INTENT(INOUT) :: PUS_IRHOV(KLON,3)  ! u_s/rho
REAL   (KIND=JPRB), INTENT(INOUT) :: PUC      (KLON)    ! u_c
REAL   (KIND=JPRB), INTENT(INOUT) :: P_U      (KLON,3)  ! u
REAL   (KIND=JPRB), INTENT(INOUT) :: P_PU     (KLON,3)  ! p_avg.u
REAL   (KIND=JPRB), INTENT(INOUT) :: P_TU     (KLON,3)  ! T_avg.u
REAL   (KIND=JPRB), INTENT(OUT)   :: PDELTA   (KLON)    ! optical depth


ASSOCIATE(FGTS_DELTA0=>YDPHY3%FGTS_DELTA0, FGTS_OC=>YDPHY3%FGTS_OC, &
 & FGTS_OB=>YDPHY3%FGTS_OB, FGTS_OA=>YDPHY3%FGTS_OA, FGTS_OD=>YDPHY3%FGTS_OD, &
 & FGTS_P00=>YDPHY3%FGTS_P00, FGTS_Q=>YDPHY3%FGTS_Q, FGTS_P=>YDPHY3%FGTS_P, &
 & FGTS_ALPHA=>YDPHY3%FGTS_ALPHA, FGTS_D=>YDPHY3%FGTS_D, FGTS_A=>YDPHY3%FGTS_A, &
 & FGTS_C=>YDPHY3%FGTS_C, FGTS_B=>YDPHY3%FGTS_B, &
 & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX)

! differentiate between solar descending and ascending loop
IF ( LD_DESC ) THEN

  ! descending loop => 2.du will be multiplied by 1/(2.mu0)
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_(JLON)=PDM0I(JLON)
      ZMD_S_(JLON)=PDM0I(JLON)
    !ENDIF
  ENDDO

ELSE

  ! ascending loop => 2.du will be multiplied by 1 or sqrt(e)/2
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_(JLON)=1._JPRB
      ZMD_S_(JLON)=ZMD
    !ENDIF
  ENDDO

ENDIF

! compute optical depths including narrowband saturation
DO JG=1,3
  IF (LL_OZONE.AND.JG == 3 ) CYCLE  ! skip solar ozone
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      PUW(JLON,JG)=PUW(JLON,JG)+ZS_FW(JLON,KL,JG)*PDU(JLON,JG)*ZMD_W_(JLON)
      PUS(JLON,JG)=PUS(JLON,JG)+ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S_(JLON)
      PUS_IRHOV(JLON,JG)=PUS_IRHOV(JLON,JG)+ZIRHOV(JLON,KL)*&
       &                        ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S_(JLON)

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =PUS(JLON,JG)/(2._JPRB*PUW(JLON,JG))
      Z4BU_  =4._JPRB*PUW(JLON,JG)*PUW(JLON,JG)/PUS(JLON,JG)
      ZAFVOI_=1._JPRB
      IF (LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*PUS_IRHOV(JLON,JG)/PUS(JLON,JG)
        IF (LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JLON,JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)
    !ENDIF
  ENDDO
ENDDO

! special treatment of solar ozone because of almost zero 'b'
IF ( LL_OZONE ) THEN
  JG=3
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      PUW(JLON,JG)=PUW(JLON,JG)+ZS_FW(JLON,KL,JG)*PDU(JLON,JG)*ZMD_W_(JLON)
      PUS(JLON,JG)=PUS(JLON,JG)+ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S_(JLON)
      PUS_IRHOV(JLON,JG)=PUS_IRHOV(JLON,JG)+ZIRHOV(JLON,KL)*&
       &                        ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S_(JLON)

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =PUS(JLON,JG)/(2._JPRB*PUW(JLON,JG))
      Z4BU_  =4._JPRB*PUW(JLON,JG)*PUW(JLON,JG)/PUS(JLON,JG)
      ZAFVOI_=1._JPRB
      IF (LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*PUS_IRHOV(JLON,JG)/PUS(JLON,JG)
        IF (LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JLON,JG)=ZAFVOI_*PUW(JLON,JG)
    !ENDIF
  ENDDO
ENDIF

! add H2O e-type continuum
IF ( FGTS_C(1) /= 0._JPRB ) THEN
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      PUC(JLON)=PUC(JLON)+ZS_FC(JLON,KL)*PDU(JLON,4)*ZMD_S_(JLON)
      ZDELTA_(JLON,1)=ZDELTA_(JLON,1)+PUC(JLON)/&
       & (1._JPRB+FGTS_C(3)*PUC(JLON))+FGTS_C(4)*PUC(JLON)**FGTS_C(5)
    !ENDIF
  ENDDO
ENDIF

! compute broadband saturation
DO JG=1,3
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN

      ! rescaling in order to account for broadband saturation
      ZDELTA_(JLON,JG)=(FGTS_DELTA0(JG)/FGTS_ALPHA(JG))*((1._JPRB+&
       & ZDELTA_(JLON,JG)/FGTS_DELTA0(JG))**FGTS_ALPHA(JG)-1._JPRB)

      ! update u, p_avg.u and T_avg.u, compute p_avg and T_avg
      P_U (JLON,JG)=P_U (JLON,JG)+           PDU(JLON,JG)*ZMD_S_(JLON)
      P_PU(JLON,JG)=P_PU(JLON,JG)+PRES(JLON)*PDU(JLON,JG)*ZMD_S_(JLON)
      P_TU(JLON,JG)=P_TU(JLON,JG)+PT  (JLON)*PDU(JLON,JG)*ZMD_S_(JLON)
      ZP_AVG_=P_PU(JLON,JG)/P_U(JLON,JG)
      ZT_AVG_=P_TU(JLON,JG)/P_U(JLON,JG)

      ! corrective secondary fit
      ZAUX_=ZDELTA_(JLON,JG)/(ZDELTA_(JLON,JG)+FGTS_D(JG))
      ZLOG_=LOG(MAX(ZDELTA_(JLON,JG),ZTRLI))
      ZDELTA_(JLON,JG)=ZDELTA_(JLON,JG)*MAX(0._JPRB,&
       & FGTS_P00(JG,0)+ZT_AVG_*(FGTS_P00(JG,1)+ZT_AVG_*FGTS_P00(JG,2))+&
       & ZAUX_*(FGTS_P(JG,0,0)+ZT_AVG_*(FGTS_P(JG,0,1)+ZT_AVG_*FGTS_P(JG,0,2))+&
       & ZLOG_*(FGTS_P(JG,1,0)+ZT_AVG_*(FGTS_P(JG,1,1)+ZT_AVG_*FGTS_P(JG,1,2))+&
       & ZLOG_*(FGTS_P(JG,2,0)+ZT_AVG_*(FGTS_P(JG,2,1)+ZT_AVG_*FGTS_P(JG,2,2))+&
       & ZLOG_*(FGTS_P(JG,3,0)+ZT_AVG_*(FGTS_P(JG,3,1)+ZT_AVG_*FGTS_P(JG,3,2))+&
       & ZLOG_*(FGTS_P(JG,4,0)+ZT_AVG_*(FGTS_P(JG,4,1)+ZT_AVG_*FGTS_P(JG,4,2))+&
       & ZLOG_*(FGTS_P(JG,5,0)+ZT_AVG_*(FGTS_P(JG,5,1)+ZT_AVG_*FGTS_P(JG,5,2))&
       & )))))))
      ZDELTA_(JLON,JG)=ZDELTA_(JLON,JG)*MAX(0._JPRB,1._JPRB+&
       & (FGTS_Q(JG,0)+ZP_AVG_*(FGTS_Q(JG,1)+ZP_AVG_*FGTS_Q(JG,2)))/&
       & (1._JPRB+ZDELTA_(JLON,JG)))
    !ENDIF
  ENDDO
ENDDO

! compute gaseous overlaps
IF ( ANY(FGTS_OA /= 0._JPRB) ) THEN

  ! individual absorptivities
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZA_(JLON,1)=1._JPRB-EXP(-ZDELTA_(JLON,1))
      ZA_(JLON,2)=1._JPRB-EXP(-ZDELTA_(JLON,2))
      ZA_(JLON,3)=1._JPRB-EXP(-ZDELTA_(JLON,3))
    !ENDIF
  ENDDO

  ! absorptivities for pairs of gases assuming random overlaps and
  ! modulation factors for the fits
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZAR_(JLON,1)=ZA_(JLON,1)+ZA_(JLON,2)-ZA_(JLON,1)*ZA_(JLON,2)
      ZAR_(JLON,2)=ZA_(JLON,1)+ZA_(JLON,3)-ZA_(JLON,1)*ZA_(JLON,3)
      ZAR_(JLON,3)=ZA_(JLON,2)+ZA_(JLON,3)-ZA_(JLON,2)*ZA_(JLON,3)
      ZCOEF_(JLON,1)=2._JPRB*ZA_(JLON,1)*ZA_(JLON,2)/&
       & (ZEPSD+ZA_(JLON,1)*ZA_(JLON,1)+ZA_(JLON,2)*ZA_(JLON,2))
      ZCOEF_(JLON,2)=2._JPRB*ZA_(JLON,1)*ZA_(JLON,3)/&
       & (ZEPSD+ZA_(JLON,1)*ZA_(JLON,1)+ZA_(JLON,3)*ZA_(JLON,3))
      ZCOEF_(JLON,3)=2._JPRB*ZA_(JLON,2)*ZA_(JLON,3)/&
       & (ZEPSD+ZA_(JLON,2)*ZA_(JLON,2)+ZA_(JLON,3)*ZA_(JLON,3))
    !ENDIF
  ENDDO

  ! transmissions for pairs of gases containing fitted contribution
  ! of non-random overlaps
  DO JO=1,3
		DO JLON=KIDIA,KFDIA
			!IF ( LDMASKS(JLON) ) THEN
        ZTAU_(JLON,JO)=1._JPRB-ZAR_(JLON,JO)-ZCOEF_(JLON,JO)*FGTS_OA(JO)*&
         & (1._JPRB-ZAR_(JLON,JO))**FGTS_OB(JO)*ZAR_(JLON,JO)**FGTS_OC(JO)*&
         & (1._JPRB-FGTS_OD(JO)*ZAR_(JLON,JO))
      !ENDIF
    ENDDO
  ENDDO

  ! final optical depth (cannot go below maximum of individual optical depths)
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      PDELTA(JLON)=-LOG(MAX(ZTRLI,ZTAU_(JLON,1)*ZTAU_(JLON,2)*ZTAU_(JLON,3)))-&
       & (ZDELTA_(JLON,1)+ZDELTA_(JLON,2)+ZDELTA_(JLON,3))
    !ENDIF
  ENDDO
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      PDELTA(JLON)=MAX(PDELTA(JLON),&
       & ZDELTA_(JLON,1),ZDELTA_(JLON,2),ZDELTA_(JLON,3))
    !ENDIF
  ENDDO

ELSE

  ! gaseous overlaps are off => sum optical depths
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      PDELTA(JLON)=ZDELTA_(JLON,1)+ZDELTA_(JLON,2)+ZDELTA_(JLON,3)
    !ENDIF
  ENDDO

ENDIF

END ASSOCIATE

END SUBROUTINE DELTA_S

! -----

END SUBROUTINE ACRANEB_TRANSS

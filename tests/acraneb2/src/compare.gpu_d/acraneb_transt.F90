!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_TRANST(YDPHY,YDPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,LDAUTO,&
! - INPUT 2D
 & PAPRS,PAPRSF,PDELP,PR,PT,PTS,PQ,PQCO2,PQO3,&
! - OUTPUT 2D
 & PDEOTI,PDEOTI2,PUEOTI,PUEOTI2,&
 & PEOLT,PEOXT,PPNER0,PPNER1,PRPROX,PRSURF, YDSTACK)
!$acc routine (ACRANEB_TRANST) seq

! Purpose:
! --------
!   ACRANEB_TRANST - Computation of thermal gaseous optical depths.

! Interface:
! ----------
! INPUT:
!   KIDIA   - initial index for horizontal loops
!   KFDIA   - final index for horizontal loops
!   KLON    - horizontal dimension of arrays
!   KTDIA   - initial index for vertical loops (usually 1)
!   KLEV    - vertical dimension of full level arrays
!   LDAUTO  - key for computing level to level transmissions needed
!             for autoevaluation of bracketing weights
!   PAPRS   - pressure on half-levels
!   PAPRSF  - pressure on full-levels
!   PDELP   - layer thickness in pressure units
!   PR      - gas constant for air
!   PT      - temperature
!   PTS     - surface temperature
!   PQ      - specific humidity
!   PQCO2   - specific mass of CO2 with respect to dry air
!   PQO3    - specific mass of ozone with respect to dry air
! OUTPUT:
!   PDEOTI  - incremental gaseous optical depth (thermal descending),
!             using dB/dT(T0) based weights
!   PDEOTI2 - incremental gaseous optical depth (thermal descending, CTS),
!             including linear correction in (T_emit - T_local)
!   PUEOTI  - incremental gaseous optical depth (thermal ascending)
!             using dB/dT(T0) based weights
!   PUEOTI2 - incremental gaseous optical depth (thermal ascending),
!             including linear correction in (T_emit - T_local)
!   PEOLT   - local gaseous optical depth, dB/dT(T0) weights
!   PEOXT   - maximum gaseous optical depth for EBL, resp. EBL-EAL flux,
!             dB/dT(T0) weights
!   PPNER0  - transmission term for exact ebl computation, B(T0) weights
!   PPNER1  - transmission term for exact ebl computation, dB/dT(T0) weights
!   PRPROX  - correction term for adjacent exchanges
!   PRSURF  - corrective ratio for surface CTS contribution

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
USE STACK_MOD
#include "stack.h"

IMPLICIT NONE

TYPE(TPHY)        ,INTENT(INOUT):: YDPHY
TYPE(TPHY3)       ,INTENT(INOUT):: YDPHY3
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA

LOGICAL,INTENT(IN) :: LDAUTO

REAL(KIND=JPRB),INTENT(IN)    :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PR(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PTS(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PQ(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQCO2(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQO3(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PDEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PDEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PUEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PUEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PEOLT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PEOXT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PPNER0(KLON,KLEV,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PPNER1(KLON,KLEV,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PRPROX(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PRSURF(KLON)
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK

#include "abor1.intfb.h"

! -----

REAL(KIND=JPRB) :: ZA,ZB,ZARGLI,ZEPSD,ZEPSP,ZEPSU,ZTRLI,ZTCORR,ZTT

REAL(KIND=JPRB) :: ZEPSV,ZGAMV,ZIBV0,ZIZV0,ZMD,ZVOISIM

TYPE TT_RHOZ0V
REAL(KIND=JPRB) :: V(3)
END TYPE

TYPE (TT_RHOZ0V) :: YT_RHOZ0V

REAL(KIND=JPRB) :: ZDELP
REAL(KIND=JPRB) :: ZEOTO
REAL(KIND=JPRB) :: ZNSOR
REAL(KIND=JPRB) :: ZP   

REAL(KIND=JPRB) :: ZDU        (4)
REAL(KIND=JPRB) :: ZC_U       (3)
REAL(KIND=JPRB) :: ZC_PU      (3)
REAL(KIND=JPRB) :: ZC_TU      (3)
REAL(KIND=JPRB) :: ZC_UW      (3)
REAL(KIND=JPRB) :: ZC_US      (3)
REAL(KIND=JPRB) :: ZC_US_IRHOV(3)
REAL(KIND=JPRB) :: ZC_UC      
REAL(KIND=JPRB) :: ZT_U       (3)
REAL(KIND=JPRB) :: ZT_PU      (3)
REAL(KIND=JPRB) :: ZT_TU      (3)
REAL(KIND=JPRB) :: ZT_UW      (3)
REAL(KIND=JPRB) :: ZT_US      (3)
REAL(KIND=JPRB) :: ZT_US_IRHOV(3)
REAL(KIND=JPRB) :: ZT_UC      

temp (REAL(KIND=JPRB), ZQ, (KLON,  KLEV))
temp (REAL(KIND=JPRB), ZIRHOV, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTA0, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTA1, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTA2, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTA0, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTA1, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTA2, (KLON,0:KLEV+1))

temp (REAL(KIND=JPRB), ZC_FW, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZC_FS, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZC_FC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZT_FW, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZT_FS, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZT_FC, (KLON,0:KLEV))
REAL(KIND=JPRB) :: ZDEL0
REAL(KIND=JPRB) :: ZDEL1
temp (REAL(KIND=JPRB), ZDEL1D, (KLON,KLEV-1))
REAL(KIND=JPRB) :: ZTAU  
REAL(KIND=JPRB) :: ZTAU0A
REAL(KIND=JPRB) :: ZTAU1A
REAL(KIND=JPRB) :: ZTAU0B
REAL(KIND=JPRB) :: ZTAU1B
temp (REAL(KIND=JPRB), ZTAU0, (KLON,0:KLEV,0:KLEV))
temp (REAL(KIND=JPRB), ZTAU1, (KLON,0:KLEV,0:KLEV))


INTEGER(KIND=JPIM) :: JO_

REAL(KIND=JPRB) :: Z4BU_,ZA2B_,ZAFVOI_,ZVOIEMP_,ZVOIGT_
REAL(KIND=JPRB) :: ZAUX_,ZLOG_,ZP_AVG_,ZT_AVG_

REAL(KIND=JPRB) :: ZDELTA_(3)
REAL(KIND=JPRB) :: ZA_    (3)
REAL(KIND=JPRB) :: ZAR_   (3)
REAL(KIND=JPRB) :: ZCOEF_ (3)
REAL(KIND=JPRB) :: ZTAU_  (3)



! local integer scalars
INTEGER(KIND=JPIM) :: JG,JLEV,JLEV1,JLEV2,JLON,ILEV

YLSTACK = YDSTACK

alloc (ZQ)
alloc (ZIRHOV)
alloc (ZDEOTA0)
alloc (ZDEOTA1)
alloc (ZDEOTA2)
alloc (ZUEOTA0)
alloc (ZUEOTA1)
alloc (ZUEOTA2)
alloc (ZC_FW)
alloc (ZC_FS)
alloc (ZC_FC)
alloc (ZT_FW)
alloc (ZT_FS)
alloc (ZT_FC)
alloc (ZDEL1D)
alloc (ZTAU0)
alloc (ZTAU1)



JLON = KIDIA



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
YT_RHOZ0V%V(1)=0.031_JPRB
YT_RHOZ0V%V(2)=0.013_JPRB
YT_RHOZ0V%V(3)=0.012_JPRB

ZGAMV       =0.36_JPRB
ZEPSV       =0.014_JPRB
ZIBV0       =1._JPRB/0.74_JPRB
ZIZV0       =1._JPRB/0.0027_JPRB
ZVOISIM     =1.21_JPRB

! ratio of diffusivity factors sqrt(e) and 2 (the latter is used in
! weak line limit)
ZMD=0.5_JPRB*EXP(0.5_JPRB)

! -----
! preparations
! -----

! safety - truncate specific humidity to interval [0, 1]
DO JLEV=KTDIA,KLEV
  
    ZQ(JLON,JLEV)=MAX(0._JPRB,MIN(1._JPRB,PQ(JLON,JLEV)))
  
ENDDO

! determine pressure for computations at model top

  ZDELP=MAX(ZEPSP,PAPRS(JLON,KTDIA-1))
  ZP   =0.5_JPRB*ZDELP


! computation of doubled ozone quantity above starting layer KTDIA

  ZNSOR=2._JPRB*MAX(ZEPSP,PAPRS(JLON,0))*PQO3(JLON,0)

DO JLEV=1,KTDIA-1
  
    ZNSOR=ZNSOR+2._JPRB*PDELP(JLON,JLEV)*PQO3(JLON,JLEV)
  
ENDDO

! compute inverse air density

  ZIRHOV(JLON,KTDIA-1)=(PR(JLON,KTDIA)*PT(JLON,KTDIA))/ZP

DO JLEV=KTDIA,KLEV
  
    ZIRHOV(JLON,JLEV)=(PR(JLON,JLEV)*PT(JLON,JLEV))/PAPRSF(JLON,JLEV)
  
ENDDO

! loop through gases
DO JG=1,3

  ! computation of pressure/temperature factors for u_w, u_s
  
    ZA=YDPHY3%FGTC_A(JG,0)*(1._JPRB+YDPHY3%FGTC_A(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+YDPHY3%FGTC_A(JG,2)*PT(JLON,KTDIA))
    ZB=YDPHY3%FGTC_B(JG,0)*(1._JPRB+YDPHY3%FGTC_B(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+YDPHY3%FGTC_B(JG,2)*PT(JLON,KTDIA))
    ZC_FW(JLON,KTDIA-1,JG)=ZA
    ZC_FS(JLON,KTDIA-1,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*ZP
    ZA=YDPHY3%FGTT_A(JG,0)*(1._JPRB+YDPHY3%FGTT_A(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+YDPHY3%FGTT_A(JG,2)*PT(JLON,KTDIA))
    ZB=YDPHY3%FGTT_B(JG,0)*(1._JPRB+YDPHY3%FGTT_B(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+YDPHY3%FGTT_B(JG,2)*PT(JLON,KTDIA))
    ZT_FW(JLON,KTDIA-1,JG)=ZA
    ZT_FS(JLON,KTDIA-1,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*ZP
  
  DO JLEV=KTDIA,KLEV
    
      ZA=YDPHY3%FGTC_A(JG,0)*(1._JPRB+YDPHY3%FGTC_A(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+YDPHY3%FGTC_A(JG,2)*PT(JLON,JLEV))
      ZB=YDPHY3%FGTC_B(JG,0)*(1._JPRB+YDPHY3%FGTC_B(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+YDPHY3%FGTC_B(JG,2)*PT(JLON,JLEV))
      ZC_FW(JLON,JLEV,JG)=ZA
      ZC_FS(JLON,JLEV,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*PAPRSF(JLON,JLEV)
      ZA=YDPHY3%FGTT_A(JG,0)*(1._JPRB+YDPHY3%FGTT_A(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+YDPHY3%FGTT_A(JG,2)*PT(JLON,JLEV))
      ZB=YDPHY3%FGTT_B(JG,0)*(1._JPRB+YDPHY3%FGTT_B(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+YDPHY3%FGTT_B(JG,2)*PT(JLON,JLEV))
      ZT_FW(JLON,JLEV,JG)=ZA
      ZT_FS(JLON,JLEV,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*PAPRSF(JLON,JLEV)
    
  ENDDO

ENDDO

! initialize pressure/temperature factors for H2O e-type continuum

  ZC_FC(JLON,KTDIA-1)=YDPHY3%FGTC_C(1)*EXP(-YDPHY3%FGTC_C(2)*PT(JLON,KTDIA))*&
   & ZP

DO JLEV=KTDIA,KLEV
  
    ZC_FC(JLON,JLEV)=YDPHY3%FGTC_C(1)*EXP(-YDPHY3%FGTC_C(2)*PT(JLON,JLEV))*&
     & PAPRSF(JLON,JLEV)
  
ENDDO

  ZT_FC(JLON,KTDIA-1)=YDPHY3%FGTT_C(1)*EXP(-YDPHY3%FGTT_C(2)*PT(JLON,KTDIA))*&
   & ZP

DO JLEV=KTDIA,KLEV
  
    ZT_FC(JLON,JLEV)=YDPHY3%FGTT_C(1)*EXP(-YDPHY3%FGTT_C(2)*PT(JLON,JLEV))*&
     & PAPRSF(JLON,JLEV)
  
ENDDO

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

! compute unscaled absorber amounts 2.du (2 stands for diffusivity factor
! in weak line limit) and inverse air density; absorber amount for H2O
! e-type continuum is multiplied by ratio e/p (water vapor pressure
! to total pressure)

  ZDU(1)=2._JPRB*ZDELP*ZQ   (JLON,KTDIA)
  ZDU(2)=2._JPRB*ZDELP*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(3)=                    ZNSOR      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(4)=ZDU(1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))


! initialize auxiliary quantities u_w, u_s, u_s_rho and u_c
! daand: added explicit loops here
DO JG=1,3
	
		ZC_UW      (JG)=ZEPSU
		ZC_US      (JG)=ZEPSU
		ZC_US_IRHOV(JG)=ZEPSU
		ZC_U       (JG)=ZEPSU
		ZC_PU      (JG)=ZEPSU
		ZC_TU      (JG)=ZEPSU
		ZT_UW      (JG)=ZEPSU
		ZT_US      (JG)=ZEPSU
		ZT_US_IRHOV(JG)=ZEPSU
		ZT_U       (JG)=ZEPSU
		ZT_PU      (JG)=ZEPSU
		ZT_TU      (JG)=ZEPSU
	
ENDDO

	ZC_UC        =ZEPSU
	ZT_UC        =ZEPSU


! compute total and incremental optical depths
! daand: a bit worried about PT and ZDEOTA0/ZDEOTA1 being passed as scalars here ...





! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZC_UW(JG)=ZC_UW(JG)+ZC_FW(JLON,(KTDIA-1),JG)*ZDU(JG)
    ZC_US(JG)=ZC_US(JG)+ZC_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD
    ZC_US_IRHOV(JG)=ZC_US_IRHOV(JG)+ZIRHOV(JLON,(KTDIA-1))*&
     &                        ZC_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZC_US(JG)/(2._JPRB*ZC_UW(JG))
    Z4BU_  =4._JPRB*ZC_UW(JG)*ZC_UW(JG)/ZC_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZC_US_IRHOV(JG)/ZC_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZC_UC=ZC_UC+ZC_FC(JLON,(KTDIA-1))*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZC_UC/&
   & (1._JPRB+YDPHY3%FGTC_C(3)*ZC_UC)+YDPHY3%FGTC_C(4)*ZC_UC**YDPHY3%FGTC_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTC_DELTA0(JG)/YDPHY3%FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTC_DELTA0(JG))**YDPHY3%FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZC_U (JG)=ZC_U(JG)+            ZDU(JG)
    ZC_PU(JG)=ZC_PU(JG)+ZP*ZDU(JG)
    ZC_TU(JG)=ZC_TU(JG)+PT  (JLON,KTDIA)*ZDU(JG)
    ZP_AVG_=ZC_PU(JG)/ZC_U(JG)
    ZT_AVG_=ZC_TU(JG)/ZC_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTC_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTC_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTC_P00(JG,1)+ZT_AVG_*YDPHY3%FGTC_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTC_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTC_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTC_Q(JG,1)+ZP_AVG_*YDPHY3%FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))

  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTC_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTC_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTC_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTC_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTC_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEOTA0(JLON,KTDIA-1)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEOTA0(JLON,KTDIA-1)=MAX(ZDEOTA0(JLON,KTDIA-1),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))











! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZT_UW(JG)=ZT_UW(JG)+ZT_FW(JLON,(KTDIA-1),JG)*ZDU(JG)
    ZT_US(JG)=ZT_US(JG)+ZT_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD
    ZT_US_IRHOV(JG)=ZT_US_IRHOV(JG)+ZIRHOV(JLON,(KTDIA-1))*&
     &                        ZT_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZT_US(JG)/(2._JPRB*ZT_UW(JG))
    Z4BU_  =4._JPRB*ZT_UW(JG)*ZT_UW(JG)/ZT_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZT_US_IRHOV(JG)/ZT_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZT_UC=ZT_UC+ZT_FC(JLON,(KTDIA-1))*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZT_UC/&
   & (1._JPRB+YDPHY3%FGTT_C(3)*ZT_UC)+YDPHY3%FGTT_C(4)*ZT_UC**YDPHY3%FGTT_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTT_DELTA0(JG)/YDPHY3%FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTT_DELTA0(JG))**YDPHY3%FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZT_U (JG)=ZT_U (JG)+           ZDU(JG)
    ZT_PU(JG)=ZT_PU(JG)+ZP*ZDU(JG)
    ZT_TU(JG)=ZT_TU(JG)+PT  (JLON,KTDIA)*ZDU(JG)
    ZP_AVG_=ZT_PU(JG)/ZT_U(JG)
    ZT_AVG_=ZT_TU(JG)/ZT_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTT_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTT_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTT_P00(JG,1)+ZT_AVG_*YDPHY3%FGTT_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTT_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTT_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTT_Q(JG,1)+ZP_AVG_*YDPHY3%FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))
  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTT_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTT_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTT_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTT_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTT_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEOTA1(JLON,KTDIA-1)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEOTA1(JLON,KTDIA-1)=MAX(ZDEOTA1(JLON,KTDIA-1),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))







  PDEOTI(JLON,KTDIA-1)=ZDEOTA1(JLON,KTDIA-1)


! -----
! descending vertical loop
! -----
! - temporary 1D arrays:
! ZEOTO     : "old" thermal optical depth (for computing "new" one)
! -----

DO JLEV=KTDIA,KLEV

  ! compute unscaled absorber amounts 2.du
  
    ZDU(1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(4)=ZDU(1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  

  ! compute total and incremental optical depths
  




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZC_UW(JG)=ZC_UW(JG)+ZC_FW(JLON,JLEV,JG)*ZDU(JG)
    ZC_US(JG)=ZC_US(JG)+ZC_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD
    ZC_US_IRHOV(JG)=ZC_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
     &                        ZC_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZC_US(JG)/(2._JPRB*ZC_UW(JG))
    Z4BU_  =4._JPRB*ZC_UW(JG)*ZC_UW(JG)/ZC_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZC_US_IRHOV(JG)/ZC_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZC_UC=ZC_UC+ZC_FC(JLON,JLEV)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZC_UC/&
   & (1._JPRB+YDPHY3%FGTC_C(3)*ZC_UC)+YDPHY3%FGTC_C(4)*ZC_UC**YDPHY3%FGTC_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTC_DELTA0(JG)/YDPHY3%FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTC_DELTA0(JG))**YDPHY3%FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZC_U (JG)=ZC_U(JG)+            ZDU(JG)
    ZC_PU(JG)=ZC_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)
    ZC_TU(JG)=ZC_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)
    ZP_AVG_=ZC_PU(JG)/ZC_U(JG)
    ZT_AVG_=ZC_TU(JG)/ZC_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTC_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTC_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTC_P00(JG,1)+ZT_AVG_*YDPHY3%FGTC_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTC_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTC_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTC_Q(JG,1)+ZP_AVG_*YDPHY3%FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))

  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTC_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTC_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTC_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTC_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTC_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEOTA0(JLON,JLEV)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEOTA0(JLON,JLEV)=MAX(ZDEOTA0(JLON,JLEV),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))






  




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZT_UW(JG)=ZT_UW(JG)+ZT_FW(JLON,JLEV,JG)*ZDU(JG)
    ZT_US(JG)=ZT_US(JG)+ZT_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD
    ZT_US_IRHOV(JG)=ZT_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
     &                        ZT_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZT_US(JG)/(2._JPRB*ZT_UW(JG))
    Z4BU_  =4._JPRB*ZT_UW(JG)*ZT_UW(JG)/ZT_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZT_US_IRHOV(JG)/ZT_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZT_UC=ZT_UC+ZT_FC(JLON,JLEV)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZT_UC/&
   & (1._JPRB+YDPHY3%FGTT_C(3)*ZT_UC)+YDPHY3%FGTT_C(4)*ZT_UC**YDPHY3%FGTT_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTT_DELTA0(JG)/YDPHY3%FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTT_DELTA0(JG))**YDPHY3%FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZT_U (JG)=ZT_U (JG)+           ZDU(JG)
    ZT_PU(JG)=ZT_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)
    ZT_TU(JG)=ZT_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)
    ZP_AVG_=ZT_PU(JG)/ZT_U(JG)
    ZT_AVG_=ZT_TU(JG)/ZT_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTT_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTT_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTT_P00(JG,1)+ZT_AVG_*YDPHY3%FGTT_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTT_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTT_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTT_Q(JG,1)+ZP_AVG_*YDPHY3%FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))
  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTT_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTT_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTT_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTT_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTT_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEOTA1(JLON,JLEV)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEOTA1(JLON,JLEV)=MAX(ZDEOTA1(JLON,JLEV),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))






  
    PDEOTI(JLON,JLEV)=MAX(ZDEOTA1(JLON,JLEV)-ZDEOTA1(JLON,JLEV-1),0._JPRB)
  

ENDDO

! -----
! temperature correction for CTS
! -----


  ZTCORR=4._JPRB*(PT(JLON,KTDIA)/YDPHY3%RTL-1._JPRB)
  ZTAU0A=EXP(MAX(-ZDEOTA0(JLON,KTDIA-1),ZARGLI))
  ZTAU1A=EXP(MAX(-ZDEOTA1(JLON,KTDIA-1),ZARGLI))
  ZTAU=ZTAU0A+ZTCORR*(ZTAU1A-ZTAU0A)
  ZDEOTA2(JLON,KTDIA-1)=-LOG(MAX(ZTAU,ZTRLI))
  PDEOTI2(JLON,KTDIA-1)=ZDEOTA2(JLON,KTDIA-1)

DO JLEV=KTDIA,KLEV
  
    ZTCORR=4._JPRB*(PT(JLON,JLEV)/YDPHY3%RTL-1._JPRB)
    ZTAU0B=EXP(MAX(-ZDEOTA0(JLON,JLEV),ZARGLI))
    ZTAU1B=EXP(MAX(-ZDEOTA1(JLON,JLEV),ZARGLI))
    ZTAU=ZTAU+ZTAU0B-ZTAU0A+&
     & ZTCORR*(ZTAU1B-ZTAU1A-ZTAU0B+ZTAU0A)
    ZDEOTA2(JLON,JLEV)=-LOG(MAX(ZTAU,ZTRLI))
    PDEOTI2(JLON,JLEV)=MAX(ZDEOTA2(JLON,JLEV)-ZDEOTA2(JLON,JLEV-1),0._JPRB)
    ZTAU0A=ZTAU0B
    ZTAU1A=ZTAU1B
  
ENDDO

! -----
! surface condition
! -----

! thermal depths computed from surface up to given level
! daand: added explicit loops here
DO JG=1,3
	
		ZC_UW      (JG)=ZEPSU
		ZC_US      (JG)=ZEPSU
		ZC_US_IRHOV(JG)=ZEPSU
		ZC_U       (JG)=ZEPSU
		ZC_PU      (JG)=ZEPSU
		ZC_TU      (JG)=ZEPSU
		ZT_UW      (JG)=ZEPSU
		ZT_US      (JG)=ZEPSU
		ZT_US_IRHOV(JG)=ZEPSU
		ZT_U       (JG)=ZEPSU
		ZT_PU      (JG)=ZEPSU
		ZT_TU      (JG)=ZEPSU
	
ENDDO

  ZEOTO        =0._JPRB
	ZC_UC        =ZEPSU
	ZT_UC        =ZEPSU



! -----
! ascending vertical loop
! -----

DO JLEV=KLEV,KTDIA,-1

  ! compute unscaled absorber amounts 2.du
  
    ZDU(1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(4)=ZDU(1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  

  ! compute total and incremental optical depths
	! daand: a bit worried about PT and ZUEOTA0/ZUEOTA1 being passed as scalars here ...
  




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZC_UW(JG)=ZC_UW(JG)+ZC_FW(JLON,JLEV,JG)*ZDU(JG)
    ZC_US(JG)=ZC_US(JG)+ZC_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD
    ZC_US_IRHOV(JG)=ZC_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
     &                        ZC_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZC_US(JG)/(2._JPRB*ZC_UW(JG))
    Z4BU_  =4._JPRB*ZC_UW(JG)*ZC_UW(JG)/ZC_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZC_US_IRHOV(JG)/ZC_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZC_UC=ZC_UC+ZC_FC(JLON,JLEV)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZC_UC/&
   & (1._JPRB+YDPHY3%FGTC_C(3)*ZC_UC)+YDPHY3%FGTC_C(4)*ZC_UC**YDPHY3%FGTC_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTC_DELTA0(JG)/YDPHY3%FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTC_DELTA0(JG))**YDPHY3%FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZC_U (JG)=ZC_U(JG)+            ZDU(JG)
    ZC_PU(JG)=ZC_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)
    ZC_TU(JG)=ZC_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)
    ZP_AVG_=ZC_PU(JG)/ZC_U(JG)
    ZT_AVG_=ZC_TU(JG)/ZC_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTC_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTC_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTC_P00(JG,1)+ZT_AVG_*YDPHY3%FGTC_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTC_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTC_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTC_Q(JG,1)+ZP_AVG_*YDPHY3%FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))

  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTC_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTC_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTC_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTC_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTC_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZUEOTA0(JLON,JLEV)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZUEOTA0(JLON,JLEV)=MAX(ZUEOTA0(JLON,JLEV),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))






  




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZT_UW(JG)=ZT_UW(JG)+ZT_FW(JLON,JLEV,JG)*ZDU(JG)
    ZT_US(JG)=ZT_US(JG)+ZT_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD
    ZT_US_IRHOV(JG)=ZT_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
     &                        ZT_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZT_US(JG)/(2._JPRB*ZT_UW(JG))
    Z4BU_  =4._JPRB*ZT_UW(JG)*ZT_UW(JG)/ZT_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZT_US_IRHOV(JG)/ZT_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZT_UC=ZT_UC+ZT_FC(JLON,JLEV)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZT_UC/&
   & (1._JPRB+YDPHY3%FGTT_C(3)*ZT_UC)+YDPHY3%FGTT_C(4)*ZT_UC**YDPHY3%FGTT_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTT_DELTA0(JG)/YDPHY3%FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTT_DELTA0(JG))**YDPHY3%FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZT_U (JG)=ZT_U (JG)+           ZDU(JG)
    ZT_PU(JG)=ZT_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)
    ZT_TU(JG)=ZT_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)
    ZP_AVG_=ZT_PU(JG)/ZT_U(JG)
    ZT_AVG_=ZT_TU(JG)/ZT_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTT_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTT_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTT_P00(JG,1)+ZT_AVG_*YDPHY3%FGTT_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTT_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTT_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTT_Q(JG,1)+ZP_AVG_*YDPHY3%FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))
  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTT_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTT_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTT_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTT_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTT_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZUEOTA1(JLON,JLEV)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZUEOTA1(JLON,JLEV)=MAX(ZUEOTA1(JLON,JLEV),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))






  
    PUEOTI(JLON,JLEV)=MAX(ZUEOTA1(JLON,JLEV)-ZEOTO,0._JPRB)
    ZEOTO      =ZUEOTA1(JLON,JLEV)
  

ENDDO

! -----
! model top (arbitrarily small pressure value for thermal EBL computations)
! -----

! compute unscaled absorber amounts 2.du

  ZDU(1)=2._JPRB*ZDELP*ZQ   (JLON,KTDIA)
  ZDU(2)=2._JPRB*ZDELP*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(3)=                    ZNSOR      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(4)=ZDU(1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))


! compute total and incremental optical depths
! daand: a bit worried about PT and ZUEOTA0/ZUEOTA1 being passed as scalars here ...





! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZC_UW(JG)=ZC_UW(JG)+ZC_FW(JLON,(KTDIA-1),JG)*ZDU(JG)
    ZC_US(JG)=ZC_US(JG)+ZC_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD
    ZC_US_IRHOV(JG)=ZC_US_IRHOV(JG)+ZIRHOV(JLON,(KTDIA-1))*&
     &                        ZC_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZC_US(JG)/(2._JPRB*ZC_UW(JG))
    Z4BU_  =4._JPRB*ZC_UW(JG)*ZC_UW(JG)/ZC_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZC_US_IRHOV(JG)/ZC_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZC_UC=ZC_UC+ZC_FC(JLON,(KTDIA-1))*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZC_UC/&
   & (1._JPRB+YDPHY3%FGTC_C(3)*ZC_UC)+YDPHY3%FGTC_C(4)*ZC_UC**YDPHY3%FGTC_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTC_DELTA0(JG)/YDPHY3%FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTC_DELTA0(JG))**YDPHY3%FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZC_U (JG)=ZC_U(JG)+            ZDU(JG)
    ZC_PU(JG)=ZC_PU(JG)+ZP*ZDU(JG)
    ZC_TU(JG)=ZC_TU(JG)+PT  (JLON,KTDIA)*ZDU(JG)
    ZP_AVG_=ZC_PU(JG)/ZC_U(JG)
    ZT_AVG_=ZC_TU(JG)/ZC_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTC_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTC_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTC_P00(JG,1)+ZT_AVG_*YDPHY3%FGTC_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTC_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTC_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTC_Q(JG,1)+ZP_AVG_*YDPHY3%FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))

  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTC_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTC_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTC_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTC_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTC_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZUEOTA0(JLON,KTDIA-1)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZUEOTA0(JLON,KTDIA-1)=MAX(ZUEOTA0(JLON,KTDIA-1),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))











! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZT_UW(JG)=ZT_UW(JG)+ZT_FW(JLON,(KTDIA-1),JG)*ZDU(JG)
    ZT_US(JG)=ZT_US(JG)+ZT_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD
    ZT_US_IRHOV(JG)=ZT_US_IRHOV(JG)+ZIRHOV(JLON,(KTDIA-1))*&
     &                        ZT_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZT_US(JG)/(2._JPRB*ZT_UW(JG))
    Z4BU_  =4._JPRB*ZT_UW(JG)*ZT_UW(JG)/ZT_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZT_US_IRHOV(JG)/ZT_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZT_UC=ZT_UC+ZT_FC(JLON,(KTDIA-1))*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZT_UC/&
   & (1._JPRB+YDPHY3%FGTT_C(3)*ZT_UC)+YDPHY3%FGTT_C(4)*ZT_UC**YDPHY3%FGTT_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTT_DELTA0(JG)/YDPHY3%FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTT_DELTA0(JG))**YDPHY3%FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZT_U (JG)=ZT_U (JG)+           ZDU(JG)
    ZT_PU(JG)=ZT_PU(JG)+ZP*ZDU(JG)
    ZT_TU(JG)=ZT_TU(JG)+PT  (JLON,KTDIA)*ZDU(JG)
    ZP_AVG_=ZT_PU(JG)/ZT_U(JG)
    ZT_AVG_=ZT_TU(JG)/ZT_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTT_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTT_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTT_P00(JG,1)+ZT_AVG_*YDPHY3%FGTT_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTT_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTT_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTT_Q(JG,1)+ZP_AVG_*YDPHY3%FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))
  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTT_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTT_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTT_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTT_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTT_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZUEOTA1(JLON,KTDIA-1)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZUEOTA1(JLON,KTDIA-1)=MAX(ZUEOTA1(JLON,KTDIA-1),&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))







  PUEOTI(JLON,KTDIA-1)=&
   & MAX(ZUEOTA1(JLON,KTDIA-1)-ZUEOTA1(JLON,KTDIA),0._JPRB)


! -----
! temperature correction for EWS
! -----


  ZTAU0A        =1._JPRB
  ZTAU1A        =1._JPRB
  ZTAU          =1._JPRB
  ZUEOTA2(JLON,KLEV+1)=0._JPRB

DO JLEV=KLEV,KTDIA-1,-1
  ILEV=MAX(KTDIA,JLEV)
  
    ZTT=PT(JLON,ILEV)/PTS(JLON)
    ZTCORR=4._JPRB*((PTS(JLON)/YDPHY3%RTL)*&
     & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT))))/&
     & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT)))-1._JPRB)
    ZTAU0B=EXP(MAX(-ZUEOTA0(JLON,JLEV),ZARGLI))
    ZTAU1B=EXP(MAX(-ZUEOTA1(JLON,JLEV),ZARGLI))
    ZTAU=ZTAU+ZTAU0B-ZTAU0A+&
     & ZTCORR*(ZTAU1B-ZTAU1A-ZTAU0B+ZTAU0A)
    ZUEOTA2 (JLON,JLEV)=-LOG(MAX(ZTAU,ZTRLI))
    PUEOTI2(JLON,JLEV)=MAX(ZUEOTA2(JLON,JLEV)-ZUEOTA2(JLON,JLEV+1),0._JPRB)
    ZTAU0A=ZTAU0B
    ZTAU1A=ZTAU1B
  
ENDDO

IF ( .NOT.LDAUTO ) THEN

  ! -----
  ! local transmissions and optical depths for single and double layers
  ! -----

  DO JLEV1=KTDIA-1,KLEV-1  ! initial half level

		! daand: added explicit loops here
		DO JG=1,3
			
				ZC_UW      (JG)=ZEPSU
				ZC_US      (JG)=ZEPSU
				ZC_US_IRHOV(JG)=ZEPSU
				ZC_U       (JG)=ZEPSU
				ZC_PU      (JG)=ZEPSU
				ZC_TU      (JG)=ZEPSU
				ZT_UW      (JG)=ZEPSU
				ZT_US      (JG)=ZEPSU
				ZT_US_IRHOV(JG)=ZEPSU
				ZT_U       (JG)=ZEPSU
				ZT_PU      (JG)=ZEPSU
				ZT_TU      (JG)=ZEPSU
			
		ENDDO
		
			ZC_UC        =ZEPSU
			ZT_UC        =ZEPSU
		


    IF ( YDPHY%LRPROX ) THEN
      ILEV=MIN(JLEV1+2,KLEV)
    ELSE
      ILEV=JLEV1+1
    ENDIF

    DO JLEV2=JLEV1+1,ILEV  ! final half level

      ! compute unscaled absorber amounts 2.du
      
        ZDU(1)=2._JPRB*PDELP(JLON,JLEV2)*ZQ   (JLON,JLEV2)
        ZDU(2)=2._JPRB*PDELP(JLON,JLEV2)*PQCO2(JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(3)=2._JPRB*PDELP(JLON,JLEV2)*PQO3 (JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(4)=ZDU(1)*RV*ZQ(JLON,JLEV2)/&
         &          (RD+(RV-RD)*ZQ(JLON,JLEV2))
      

      ! compute optical depths
      




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZC_UW(JG)=ZC_UW(JG)+ZC_FW(JLON,JLEV2,JG)*ZDU(JG)
    ZC_US(JG)=ZC_US(JG)+ZC_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD
    ZC_US_IRHOV(JG)=ZC_US_IRHOV(JG)+ZIRHOV(JLON,JLEV2)*&
     &                        ZC_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZC_US(JG)/(2._JPRB*ZC_UW(JG))
    Z4BU_  =4._JPRB*ZC_UW(JG)*ZC_UW(JG)/ZC_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZC_US_IRHOV(JG)/ZC_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZC_UC=ZC_UC+ZC_FC(JLON,JLEV2)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZC_UC/&
   & (1._JPRB+YDPHY3%FGTC_C(3)*ZC_UC)+YDPHY3%FGTC_C(4)*ZC_UC**YDPHY3%FGTC_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTC_DELTA0(JG)/YDPHY3%FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTC_DELTA0(JG))**YDPHY3%FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZC_U (JG)=ZC_U(JG)+            ZDU(JG)
    ZC_PU(JG)=ZC_PU(JG)+PAPRSF(JLON,JLEV2)*ZDU(JG)
    ZC_TU(JG)=ZC_TU(JG)+PT  (JLON,JLEV2)*ZDU(JG)
    ZP_AVG_=ZC_PU(JG)/ZC_U(JG)
    ZT_AVG_=ZC_TU(JG)/ZC_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTC_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTC_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTC_P00(JG,1)+ZT_AVG_*YDPHY3%FGTC_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTC_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTC_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTC_Q(JG,1)+ZP_AVG_*YDPHY3%FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))

  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTC_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTC_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTC_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTC_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTC_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEL0=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEL0=MAX(ZDEL0,&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))






      




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZT_UW(JG)=ZT_UW(JG)+ZT_FW(JLON,JLEV2,JG)*ZDU(JG)
    ZT_US(JG)=ZT_US(JG)+ZT_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD
    ZT_US_IRHOV(JG)=ZT_US_IRHOV(JG)+ZIRHOV(JLON,JLEV2)*&
     &                        ZT_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZT_US(JG)/(2._JPRB*ZT_UW(JG))
    Z4BU_  =4._JPRB*ZT_UW(JG)*ZT_UW(JG)/ZT_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZT_US_IRHOV(JG)/ZT_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZT_UC=ZT_UC+ZT_FC(JLON,JLEV2)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZT_UC/&
   & (1._JPRB+YDPHY3%FGTT_C(3)*ZT_UC)+YDPHY3%FGTT_C(4)*ZT_UC**YDPHY3%FGTT_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTT_DELTA0(JG)/YDPHY3%FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTT_DELTA0(JG))**YDPHY3%FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZT_U (JG)=ZT_U (JG)+           ZDU(JG)
    ZT_PU(JG)=ZT_PU(JG)+PAPRSF(JLON,JLEV2)*ZDU(JG)
    ZT_TU(JG)=ZT_TU(JG)+PT  (JLON,JLEV2)*ZDU(JG)
    ZP_AVG_=ZT_PU(JG)/ZT_U(JG)
    ZT_AVG_=ZT_TU(JG)/ZT_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTT_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTT_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTT_P00(JG,1)+ZT_AVG_*YDPHY3%FGTT_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTT_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTT_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTT_Q(JG,1)+ZP_AVG_*YDPHY3%FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))
  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTT_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTT_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTT_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTT_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTT_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEL1=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEL1=MAX(ZDEL1,&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))







      ! compute transmissions
      IF ( YDPHY%LRPROX ) THEN
        
          ZTAU0(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL0,ZARGLI))
          ZTAU1(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL1,ZARGLI))
        
      ENDIF

      ! store local optical depths of single and double layers
      IF ( JLEV2 == JLEV1+1 ) THEN
        
          PEOLT(JLON,JLEV1+1)=ZDEL1   ! single layer JLEV1+1
        
      ELSEIF ( JLEV2 == JLEV1+2 ) THEN      ! only when LRPROX
        
          ZDEL1D(JLON,JLEV1+1)=ZDEL1  ! double layer JLEV1+1,JLEV1+2
        
      ENDIF

    ENDDO
  ENDDO

  ! -----
  ! top to surface gaseous transmissions
  ! -----
 
	! daand: added explicit loops here
	DO JG=1,3
		
			ZC_UW      (JG)=ZEPSU
			ZC_US      (JG)=ZEPSU
			ZC_US_IRHOV(JG)=ZEPSU
			ZC_U       (JG)=ZEPSU
			ZC_PU      (JG)=ZEPSU
			ZC_TU      (JG)=ZEPSU
			ZT_UW      (JG)=ZEPSU
			ZT_US      (JG)=ZEPSU
			ZT_US_IRHOV(JG)=ZEPSU
			ZT_U       (JG)=ZEPSU
			ZT_PU      (JG)=ZEPSU
			ZT_TU      (JG)=ZEPSU
		
	ENDDO
	
		ZC_UC        =ZEPSU
		ZT_UC        =ZEPSU
	

  DO JLEV=KTDIA,KLEV

    ! compute unscaled absorber amounts 2.du
    
      ZDU(1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
      ZDU(2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
       &          (1._JPRB-ZQ(JLON,JLEV))
      ZDU(3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
       &          (1._JPRB-ZQ(JLON,JLEV))
      ZDU(4)=ZDU(1)*RV*ZQ(JLON,JLEV)/&
       &          (RD+(RV-RD)*ZQ(JLON,JLEV))
    

    ! compute optical depths
    




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZC_UW(JG)=ZC_UW(JG)+ZC_FW(JLON,JLEV,JG)*ZDU(JG)
    ZC_US(JG)=ZC_US(JG)+ZC_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD
    ZC_US_IRHOV(JG)=ZC_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
     &                        ZC_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZC_US(JG)/(2._JPRB*ZC_UW(JG))
    Z4BU_  =4._JPRB*ZC_UW(JG)*ZC_UW(JG)/ZC_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZC_US_IRHOV(JG)/ZC_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZC_UC=ZC_UC+ZC_FC(JLON,JLEV)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZC_UC/&
   & (1._JPRB+YDPHY3%FGTC_C(3)*ZC_UC)+YDPHY3%FGTC_C(4)*ZC_UC**YDPHY3%FGTC_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTC_DELTA0(JG)/YDPHY3%FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTC_DELTA0(JG))**YDPHY3%FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZC_U (JG)=ZC_U(JG)+            ZDU(JG)
    ZC_PU(JG)=ZC_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)
    ZC_TU(JG)=ZC_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)
    ZP_AVG_=ZC_PU(JG)/ZC_U(JG)
    ZT_AVG_=ZC_TU(JG)/ZC_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTC_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTC_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTC_P00(JG,1)+ZT_AVG_*YDPHY3%FGTC_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTC_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTC_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTC_Q(JG,1)+ZP_AVG_*YDPHY3%FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))

  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTC_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTC_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTC_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTC_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTC_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEL0=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEL0=MAX(ZDEL0,&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))






    




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZT_UW(JG)=ZT_UW(JG)+ZT_FW(JLON,JLEV,JG)*ZDU(JG)
    ZT_US(JG)=ZT_US(JG)+ZT_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD
    ZT_US_IRHOV(JG)=ZT_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
     &                        ZT_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZT_US(JG)/(2._JPRB*ZT_UW(JG))
    Z4BU_  =4._JPRB*ZT_UW(JG)*ZT_UW(JG)/ZT_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZT_US_IRHOV(JG)/ZT_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZT_UC=ZT_UC+ZT_FC(JLON,JLEV)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZT_UC/&
   & (1._JPRB+YDPHY3%FGTT_C(3)*ZT_UC)+YDPHY3%FGTT_C(4)*ZT_UC**YDPHY3%FGTT_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTT_DELTA0(JG)/YDPHY3%FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTT_DELTA0(JG))**YDPHY3%FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZT_U (JG)=ZT_U (JG)+           ZDU(JG)
    ZT_PU(JG)=ZT_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)
    ZT_TU(JG)=ZT_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)
    ZP_AVG_=ZT_PU(JG)/ZT_U(JG)
    ZT_AVG_=ZT_TU(JG)/ZT_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTT_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTT_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTT_P00(JG,1)+ZT_AVG_*YDPHY3%FGTT_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTT_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTT_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTT_Q(JG,1)+ZP_AVG_*YDPHY3%FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))
  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTT_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTT_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTT_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTT_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTT_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEL1=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEL1=MAX(ZDEL1,&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))







  ENDDO

  ! convert top to surface optical depths to transmissions
  
    ZTAU0(JLON,KTDIA-1,KLEV)=EXP(MAX(-ZDEL0,ZARGLI))
    ZTAU1(JLON,KTDIA-1,KLEV)=EXP(MAX(-ZDEL1,ZARGLI))
  

ELSE

  ! -----
  ! double vertical loop for the auto-evaluation of EBL flux with
  ! gaseous absorption only
  ! -----

  DO JLEV1=KTDIA-1,KLEV    ! initial half level

		! daand: added explicit loops here
		DO JG=1,3
			
				ZC_UW      (JG)=ZEPSU
				ZC_US      (JG)=ZEPSU
				ZC_US_IRHOV(JG)=ZEPSU
				ZC_U       (JG)=ZEPSU
				ZC_PU      (JG)=ZEPSU
				ZC_TU      (JG)=ZEPSU
				ZT_UW      (JG)=ZEPSU
				ZT_US      (JG)=ZEPSU
				ZT_US_IRHOV(JG)=ZEPSU
				ZT_U       (JG)=ZEPSU
				ZT_PU      (JG)=ZEPSU
				ZT_TU      (JG)=ZEPSU
			
		ENDDO
		
			ZC_UC        =ZEPSU
			ZT_UC        =ZEPSU
		

    
      ZTAU0(JLON,JLEV1,JLEV1)=1._JPRB
      ZTAU1(JLON,JLEV1,JLEV1)=1._JPRB
    

    DO JLEV2=JLEV1+1,KLEV  ! final half level

      ! compute unscaled absorber amounts 2.du
      
        ZDU(1)=2._JPRB*PDELP(JLON,JLEV2)*ZQ   (JLON,JLEV2)
        ZDU(2)=2._JPRB*PDELP(JLON,JLEV2)*PQCO2(JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(3)=2._JPRB*PDELP(JLON,JLEV2)*PQO3 (JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(4)=ZDU(1)*RV*ZQ(JLON,JLEV2)/&
         &          (RD+(RV-RD)*ZQ(JLON,JLEV2))
      

      ! compute optical depths
      




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZC_UW(JG)=ZC_UW(JG)+ZC_FW(JLON,JLEV2,JG)*ZDU(JG)
    ZC_US(JG)=ZC_US(JG)+ZC_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD
    ZC_US_IRHOV(JG)=ZC_US_IRHOV(JG)+ZIRHOV(JLON,JLEV2)*&
     &                        ZC_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZC_US(JG)/(2._JPRB*ZC_UW(JG))
    Z4BU_  =4._JPRB*ZC_UW(JG)*ZC_UW(JG)/ZC_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZC_US_IRHOV(JG)/ZC_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZC_UC=ZC_UC+ZC_FC(JLON,JLEV2)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZC_UC/&
   & (1._JPRB+YDPHY3%FGTC_C(3)*ZC_UC)+YDPHY3%FGTC_C(4)*ZC_UC**YDPHY3%FGTC_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTC_DELTA0(JG)/YDPHY3%FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTC_DELTA0(JG))**YDPHY3%FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZC_U (JG)=ZC_U(JG)+            ZDU(JG)
    ZC_PU(JG)=ZC_PU(JG)+PAPRSF(JLON,JLEV2)*ZDU(JG)
    ZC_TU(JG)=ZC_TU(JG)+PT  (JLON,JLEV2)*ZDU(JG)
    ZP_AVG_=ZC_PU(JG)/ZC_U(JG)
    ZT_AVG_=ZC_TU(JG)/ZC_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTC_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTC_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTC_P00(JG,1)+ZT_AVG_*YDPHY3%FGTC_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTC_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTC_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTC_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTC_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTC_Q(JG,1)+ZP_AVG_*YDPHY3%FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))

  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTC_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTC_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTC_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTC_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTC_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEL0=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEL0=MAX(ZDEL0,&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))






      




! compute optical depths including narrowband saturation
DO JG=1,3
  

    ! update quantities u_w, u_s and u_s_rho
    ZT_UW(JG)=ZT_UW(JG)+ZT_FW(JLON,JLEV2,JG)*ZDU(JG)
    ZT_US(JG)=ZT_US(JG)+ZT_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD
    ZT_US_IRHOV(JG)=ZT_US_IRHOV(JG)+ZIRHOV(JLON,JLEV2)*&
     &                        ZT_FS(JLON,JLEV2,JG)*ZDU(JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B_  =ZT_US(JG)/(2._JPRB*ZT_UW(JG))
    Z4BU_  =4._JPRB*ZT_UW(JG)*ZT_UW(JG)/ZT_US(JG)
    ZAFVOI_=1._JPRB
    IF (YDPHY%LVOIGT) THEN
      ZVOIGT_=YT_RHOZ0V%V(JG)*ZT_US_IRHOV(JG)/ZT_US(JG)
      IF (YDPHY%LVFULL) THEN
        ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
      ELSE
        ZVOIEMP_=ZVOISIM
      ENDIF
      ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
       & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
    ENDIF
    ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)

  
ENDDO

! add H2O e-type continuum

  ZT_UC=ZT_UC+ZT_FC(JLON,JLEV2)*ZDU(4)*ZMD
  ZDELTA_(1)=ZDELTA_(1)+ZT_UC/&
   & (1._JPRB+YDPHY3%FGTT_C(3)*ZT_UC)+YDPHY3%FGTT_C(4)*ZT_UC**YDPHY3%FGTT_C(5)


! compute broadband saturation
DO JG=1,3
  

    ! rescaling in order to account for broadband saturation
    ZDELTA_(JG)=(YDPHY3%FGTT_DELTA0(JG)/YDPHY3%FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA_(JG)/YDPHY3%FGTT_DELTA0(JG))**YDPHY3%FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    ZT_U (JG)=ZT_U (JG)+           ZDU(JG)
    ZT_PU(JG)=ZT_PU(JG)+PAPRSF(JLON,JLEV2)*ZDU(JG)
    ZT_TU(JG)=ZT_TU(JG)+PT  (JLON,JLEV2)*ZDU(JG)
    ZP_AVG_=ZT_PU(JG)/ZT_U(JG)
    ZT_AVG_=ZT_TU(JG)/ZT_U(JG)

    ! corrective secondary fit
    ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTT_D(JG))
    ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
     & YDPHY3%FGTT_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTT_P00(JG,1)+ZT_AVG_*YDPHY3%FGTT_P00(JG,2))+&
     & ZAUX_*(YDPHY3%FGTT_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,0,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,1,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,2,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,3,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,4,2))+&
     & ZLOG_*(YDPHY3%FGTT_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTT_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
     & (YDPHY3%FGTT_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTT_Q(JG,1)+ZP_AVG_*YDPHY3%FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA_(JG)))
  
ENDDO

! compute gaseous overlaps

! individual absorptivities

  ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
  ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
  ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))


! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits

  ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
  ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
  ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
  ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
  ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
   & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
  ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
   & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))


! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO_=1,3
  
    ZTAU_(JO_)=1._JPRB-ZAR_(JO_)
  
  IF ( YDPHY3%FGTT_OA(JO_) /= 0._JPRB ) THEN
    
      ZTAU_(JO_)=ZTAU_(JO_)-ZCOEF_(JO_)*YDPHY3%FGTT_OA(JO_)*&
       & (1._JPRB-ZAR_(JO_))**YDPHY3%FGTT_OB(JO_)*ZAR_(JO_)**YDPHY3%FGTT_OC(JO_)*&
       & (1._JPRB-YDPHY3%FGTT_OD(JO_)*ZAR_(JO_))
    
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)

  ZDEL1=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
   & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))


  ZDEL1=MAX(ZDEL1,&
   & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))







      ! compute transmissions
      
        ZTAU0(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL0,ZARGLI))
        ZTAU1(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL1,ZARGLI))
      

      ! store local optical depths of single and double layers
      IF ( JLEV2 == JLEV1+1 ) THEN
        
          PEOLT(JLON,JLEV1+1)=ZDEL1   ! single layer JLEV1+1
        
      ELSEIF ( YDPHY%LRPROX.AND.(JLEV2 == JLEV1+2) ) THEN
        
          ZDEL1D(JLON,JLEV1+1)=ZDEL1  ! double layer JLEV1+1,JLEV1+2
        
      ENDIF

    ENDDO
  ENDDO

  ! compute quantities needed for T_e corrected EBL, resp. EBL-EAL flux
  DO JLEV1=KTDIA,KLEV      ! exchanging layer 1
    IF ( YDPHY%LRPROX ) THEN
      ILEV=JLEV1+2         ! exclude exchange between adjacent layers
    ELSE
      ILEV=JLEV1+1         ! include exchange between adjacent layers
    ENDIF
    DO JLEV2=ILEV,KLEV     ! exchanging layer 2
      
        PPNER0(JLON,JLEV1,JLEV2)=&
         & ZTAU0(JLON,JLEV1,JLEV2  )-ZTAU0(JLON,JLEV1-1,JLEV2  )-&
         & ZTAU0(JLON,JLEV1,JLEV2-1)+ZTAU0(JLON,JLEV1-1,JLEV2-1)
        PPNER1(JLON,JLEV1,JLEV2)=&
         & ZTAU1(JLON,JLEV1,JLEV2  )-ZTAU1(JLON,JLEV1-1,JLEV2  )-&
         & ZTAU1(JLON,JLEV1,JLEV2-1)+ZTAU1(JLON,JLEV1-1,JLEV2-1)
      
    ENDDO
  ENDDO

ENDIF

! -----
! compute maximum optical depths for EBL, resp. EBL-EAL flux and 
! correction factor for tau12 /= tau1.tau2
! -----

IF ( YDPHY%LRPROX ) THEN

  ! compute maximum optical depths for EBL-EAL
  
    PEOXT(JLON,KTDIA  )=ZDEL1D(JLON,KTDIA  )-PEOLT(JLON,KTDIA  )
    PEOXT(JLON,KTDIA+1)=ZDEL1D(JLON,KTDIA+1)-PEOLT(JLON,KTDIA+1)
    PEOXT(JLON,KLEV -1)=ZDEL1D(JLON,KLEV -2)-PEOLT(JLON,KLEV -1)
    PEOXT(JLON,KLEV   )=ZDEL1D(JLON,KLEV -1)-PEOLT(JLON,KLEV   )
  
  DO JLEV=KTDIA+2,KLEV-2
    
      PEOXT(JLON,JLEV)=MAX(ZDEL1D(JLON,JLEV-1)-PEOLT(JLON,JLEV),&
       &                   ZDEL1D(JLON,JLEV  )-PEOLT(JLON,JLEV))
    
  ENDDO

  ! compute correction factor for tau12 /= tau1.tau2
  ! daand: added explicit loops here
	DO JLEV=0,KLEV
	  
	    PRPROX(JLON,JLEV)=0._JPRB
		
	ENDDO
  DO JLEV=KTDIA,KLEV-1
    
      ZTT=PT(JLON,JLEV)/PT(JLON,JLEV+1)
      ZTCORR=4._JPRB*((PT(JLON,JLEV+1)/YDPHY3%RTL)*&
       & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT))))/&
       & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT)))-1._JPRB)
      PRPROX(JLON,JLEV)=((1._JPRB-ZTCORR)*(ZTAU0(JLON,JLEV-1,JLEV+1)-&
       & ZTAU0(JLON,JLEV-1,JLEV)-ZTAU0(JLON,JLEV,JLEV+1))+&
       & ZTCORR*(ZTAU1(JLON,JLEV-1,JLEV+1)-&
       & ZTAU1(JLON,JLEV-1,JLEV)-ZTAU1(JLON,JLEV,JLEV+1))+&
       & ZTAU1(JLON,JLEV-1,JLEV)+ZTAU1(JLON,JLEV,JLEV+1))/&
       & MAX(ZTAU1(JLON,JLEV-1,JLEV)*ZTAU1(JLON,JLEV,JLEV+1),ZTRLI)
    
  ENDDO

ELSE

  ! fill maximum optical depths for EBL with local values
  DO JLEV=KTDIA,KLEV
    
      PEOXT(JLON,JLEV)=PEOLT(JLON,JLEV)
    
  ENDDO

ENDIF

! -----
! compute corrective ratio for surface CTS contribution
! -----

! add T_e corrected optical depths
! daand: added explicit loops here

	PRSURF(JLON)=0._JPRB


DO JLEV=KTDIA,KLEV
  
    PRSURF(JLON)=PRSURF(JLON)+PDEOTI2(JLON,JLEV)
  
ENDDO

! compute corrective ratio for KTDIA-1 to KLEV transmission

  ZTCORR=4._JPRB*(PTS(JLON)/YDPHY3%RTL-1._JPRB)
  PRSURF(JLON)=(ZTAU0(JLON,KTDIA-1,KLEV)+&
   &    ZTCORR*(ZTAU1(JLON,KTDIA-1,KLEV)-ZTAU0(JLON,KTDIA-1,KLEV)))/&
   &    EXP(MAX(-PRSURF(JLON),ZARGLI))




! -----
! private procedures
! -----



! computation of thermal optical depths (B weights)


! -----

! computation of thermal optical depths (dB/dT weights)


! -----

END SUBROUTINE ACRANEB_TRANST

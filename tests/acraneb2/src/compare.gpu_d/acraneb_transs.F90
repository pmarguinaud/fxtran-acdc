!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_TRANSS(YDPHY,YDPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,LDMASKS,&
! - INPUT 2D
 & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,&
! - INPUT 1D
 & PDM0I,&
! - OUTPUT 2D
 & PDEOSI,PUEOSI, YDSTACK)
!$acc routine (ACRANEB_TRANSS) seq

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
USE STACK_MOD
#include "stack.h"

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
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK

#include "abor1.intfb.h"

! -----

REAL(KIND=JPRB) :: ZA,ZB,ZARGLI,ZEPSD,ZEPSP,ZEPSU,ZTRLI

REAL(KIND=JPRB) :: ZEPSV,ZGAMV,ZIBV0,ZIZV0,ZMD,ZVOISIM

TYPE TS_RHOZ0V
REAL(KIND=JPRB) :: V(3)
END TYPE

TYPE (TS_RHOZ0V) :: YS_RHOZ0V

REAL(KIND=JPRB) :: ZDELP
REAL(KIND=JPRB) :: ZEOSO
REAL(KIND=JPRB) :: ZNSOR
REAL(KIND=JPRB) :: ZP   

REAL(KIND=JPRB) :: ZDU        (4)
REAL(KIND=JPRB) :: ZS_U       (3)
REAL(KIND=JPRB) :: ZS_PU      (3)
REAL(KIND=JPRB) :: ZS_TU      (3)
REAL(KIND=JPRB) :: ZS_UW      (3)
REAL(KIND=JPRB) :: ZS_US      (3)
REAL(KIND=JPRB) :: ZS_US_IRHOV(3)
REAL(KIND=JPRB) :: ZS_UC      

temp (REAL(KIND=JPRB), ZQ, (KLON,  KLEV))
temp (REAL(KIND=JPRB), ZIRHOV, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOSA, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOSA, (KLON,0:KLEV))

temp (REAL(KIND=JPRB), ZS_FW, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZS_FS, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZS_FC, (KLON,0:KLEV))

REAL(KIND=JPRB) :: Z4BU_,ZA2B_,ZAFVOI_,ZVOIEMP_,ZVOIGT_
REAL(KIND=JPRB) :: ZAUX_,ZLOG_,ZP_AVG_,ZT_AVG_

REAL(KIND=JPRB) :: ZMD_W_ 
REAL(KIND=JPRB) :: ZMD_S_ 
REAL(KIND=JPRB) :: ZDELTA_(3)
REAL(KIND=JPRB) :: ZA_    (3)
REAL(KIND=JPRB) :: ZAR_   (3)
REAL(KIND=JPRB) :: ZCOEF_ (3)
REAL(KIND=JPRB) :: ZTAU_  (3)


! local integer scalars
INTEGER(KIND=JPIM) :: JG,JLEV,JLON,JN,JO

! local logical scalars
LOGICAL :: LL_DESC,LL_OZONE

YLSTACK = YDSTACK

alloc (ZQ)
alloc (ZIRHOV)
alloc (ZDEOSA)
alloc (ZUEOSA)
alloc (ZS_FW)
alloc (ZS_FS)
alloc (ZS_FC)



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
	
		!IF ( LDMASKS(JLON) ) THEN
      ZA=YDPHY3%FGTS_A(JG,0)*(1._JPRB+YDPHY3%FGTS_A(JG,1)*PT(JLON,KTDIA))/&
       &              (1._JPRB+YDPHY3%FGTS_A(JG,2)*PT(JLON,KTDIA))
      ZB=YDPHY3%FGTS_B(JG,0)*(1._JPRB+YDPHY3%FGTS_B(JG,1)*PT(JLON,KTDIA))/&
       &              (1._JPRB+YDPHY3%FGTS_B(JG,2)*PT(JLON,KTDIA))
      ZS_FW(JLON,KTDIA-1,JG)=ZA
      ZS_FS(JLON,KTDIA-1,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*ZP
    !ENDIF
  
  DO JLEV=KTDIA,KLEV
		
			!IF ( LDMASKS(JLON) ) THEN
        ZA=YDPHY3%FGTS_A(JG,0)*(1._JPRB+YDPHY3%FGTS_A(JG,1)*PT(JLON,JLEV))/&
         &              (1._JPRB+YDPHY3%FGTS_A(JG,2)*PT(JLON,JLEV))
        ZB=YDPHY3%FGTS_B(JG,0)*(1._JPRB+YDPHY3%FGTS_B(JG,1)*PT(JLON,JLEV))/&
         &              (1._JPRB+YDPHY3%FGTS_B(JG,2)*PT(JLON,JLEV))
        ZS_FW(JLON,JLEV,JG)=ZA
        ZS_FS(JLON,JLEV,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*PAPRSF(JLON,JLEV)
      !ENDIF
    
  ENDDO

ENDDO

! initialize pressure/temperature factors for H2O e-type continuum
IF ( YDPHY3%FGTS_C(1) == 0._JPRB ) THEN
  ! daand: added explicit loops
  DO JLEV=KTDIA-1,KLEV
	  
		  ZS_FC(JLON,JLEV)=0._JPRB
		
	ENDDO
ELSE
	
		!IF ( LDMASKS(JLON) ) THEN
      ZS_FC(JLON,KTDIA-1)=YDPHY3%FGTS_C(1)*EXP(-YDPHY3%FGTS_C(2)*PT(JLON,KTDIA))*&
       & ZP
    !ENDIF
  
  DO JLEV=KTDIA,KLEV
		
			!IF ( LDMASKS(JLON) ) THEN
        ZS_FC(JLON,JLEV)=YDPHY3%FGTS_C(1)*EXP(-YDPHY3%FGTS_C(2)*PT(JLON,JLEV))*&
         & PAPRSF(JLON,JLEV)
      !ENDIF
    
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

  ZDU(1)=2._JPRB*ZDELP*ZQ   (JLON,KTDIA)
  ZDU(2)=2._JPRB*ZDELP*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(3)=                    ZNSOR      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(4)=ZDU(1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))


! initialize auxiliary quantities u_w, u_s, u_s_rho and u_c
! daand: added explicit loops
DO JN=1,3
	
		ZS_UW      (JN)=ZEPSU
		ZS_US      (JN)=ZEPSU
		ZS_US_IRHOV(JN)=ZEPSU
		ZS_U       (JN)=ZEPSU
		ZS_PU      (JN)=ZEPSU
		ZS_TU      (JN)=ZEPSU
	
ENDDO

  ZS_UC        =ZEPSU


! compute total and incremental optical depths
! (solar incremental multiplied by 2.mu0)
! daand: a bit worried about PT and ZDEOSA being passed as scalars here ...



! diffusivity factor


! differentiate between solar descending and ascending loop
IF ( LL_DESC ) THEN

  ! descending loop => 2.du will be multiplied by 1/(2.mu0)
	
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_=PDM0I(JLON)
      ZMD_S_=PDM0I(JLON)
    !ENDIF
  

ELSE

  ! ascending loop => 2.du will be multiplied by 1 or sqrt(e)/2
	
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_=1._JPRB
      ZMD_S_=ZMD
    !ENDIF
  

ENDIF

! compute optical depths including narrowband saturation
DO JG=1,3
  IF (LL_OZONE.AND.JG == 3 )  THEN
    CYCLE
  ENDIF  ! skip solar ozone
	
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      ZS_UW(JG)=ZS_UW(JG)+ZS_FW(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD_W_
      ZS_US(JG)=ZS_US(JG)+ZS_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD_S_
      ZS_US_IRHOV(JG)=ZS_US_IRHOV(JG)+ZIRHOV(JLON,(KTDIA-1))*&
       &                        ZS_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD_S_

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =ZS_US(JG)/(2._JPRB*ZS_UW(JG))
      Z4BU_  =4._JPRB*ZS_UW(JG)*ZS_UW(JG)/ZS_US(JG)
      ZAFVOI_=1._JPRB
      IF (YDPHY%LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*ZS_US_IRHOV(JG)/ZS_US(JG)
        IF (YDPHY%LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)
    !ENDIF
  
ENDDO

! special treatment of solar ozone because of almost zero 'b'
IF ( LL_OZONE ) THEN
  JG=3
	
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      ZS_UW(JG)=ZS_UW(JG)+ZS_FW(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD_W_
      ZS_US(JG)=ZS_US(JG)+ZS_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD_S_
      ZS_US_IRHOV(JG)=ZS_US_IRHOV(JG)+ZIRHOV(JLON,(KTDIA-1))*&
       &                        ZS_FS(JLON,(KTDIA-1),JG)*ZDU(JG)*ZMD_S_

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =ZS_US(JG)/(2._JPRB*ZS_UW(JG))
      Z4BU_  =4._JPRB*ZS_UW(JG)*ZS_UW(JG)/ZS_US(JG)
      ZAFVOI_=1._JPRB
      IF (YDPHY%LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*ZS_US_IRHOV(JG)/ZS_US(JG)
        IF (YDPHY%LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JG)=ZAFVOI_*ZS_UW(JG)
    !ENDIF
  
ENDIF

! add H2O e-type continuum
IF ( YDPHY3%FGTS_C(1) /= 0._JPRB ) THEN
	
		!IF ( LDMASKS(JLON) ) THEN
      ZS_UC=ZS_UC+ZS_FC(JLON,(KTDIA-1))*ZDU(4)*ZMD_S_
      ZDELTA_(1)=ZDELTA_(1)+ZS_UC/&
       & (1._JPRB+YDPHY3%FGTS_C(3)*ZS_UC)+YDPHY3%FGTS_C(4)*ZS_UC**YDPHY3%FGTS_C(5)
    !ENDIF
  
ENDIF

! compute broadband saturation
DO JG=1,3
	
		!IF ( LDMASKS(JLON) ) THEN

      ! rescaling in order to account for broadband saturation
      ZDELTA_(JG)=(YDPHY3%FGTS_DELTA0(JG)/YDPHY3%FGTS_ALPHA(JG))*((1._JPRB+&
       & ZDELTA_(JG)/YDPHY3%FGTS_DELTA0(JG))**YDPHY3%FGTS_ALPHA(JG)-1._JPRB)

      ! update u, p_avg.u and T_avg.u, compute p_avg and T_avg
      ZS_U (JG)=ZS_U (JG)+           ZDU(JG)*ZMD_S_
      ZS_PU(JG)=ZS_PU(JG)+ZP*ZDU(JG)*ZMD_S_
      ZS_TU(JG)=ZS_TU(JG)+PT  (JLON,KTDIA)*ZDU(JG)*ZMD_S_
      ZP_AVG_=ZS_PU(JG)/ZS_U(JG)
      ZT_AVG_=ZS_TU(JG)/ZS_U(JG)

      ! corrective secondary fit
      ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTS_D(JG))
      ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
      ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
       & YDPHY3%FGTS_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTS_P00(JG,1)+ZT_AVG_*YDPHY3%FGTS_P00(JG,2))+&
       & ZAUX_*(YDPHY3%FGTS_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,0,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,1,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,2,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,3,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,4,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,5,2))&
       & )))))))
      ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
       & (YDPHY3%FGTS_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTS_Q(JG,1)+ZP_AVG_*YDPHY3%FGTS_Q(JG,2)))/&
       & (1._JPRB+ZDELTA_(JG)))
    !ENDIF
  
ENDDO

! compute gaseous overlaps
IF ( ANY(YDPHY3%FGTS_OA /= 0._JPRB) ) THEN

  ! individual absorptivities
	
		!IF ( LDMASKS(JLON) ) THEN
      ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
      ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
      ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))
    !ENDIF
  

  ! absorptivities for pairs of gases assuming random overlaps and
  ! modulation factors for the fits
	
		!IF ( LDMASKS(JLON) ) THEN
      ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
      ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
      ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
      ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
       & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
      ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
       & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
      ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
       & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))
    !ENDIF
  

  ! transmissions for pairs of gases containing fitted contribution
  ! of non-random overlaps
  DO JO=1,3
		
			!IF ( LDMASKS(JLON) ) THEN
        ZTAU_(JO)=1._JPRB-ZAR_(JO)-ZCOEF_(JO)*YDPHY3%FGTS_OA(JO)*&
         & (1._JPRB-ZAR_(JO))**YDPHY3%FGTS_OB(JO)*ZAR_(JO)**YDPHY3%FGTS_OC(JO)*&
         & (1._JPRB-YDPHY3%FGTS_OD(JO)*ZAR_(JO))
      !ENDIF
    
  ENDDO

  ! final optical depth (cannot go below maximum of individual optical depths)
	
		!IF ( LDMASKS(JLON) ) THEN
      ZDEOSA(JLON,KTDIA-1)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
       & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))
    !ENDIF
  
	
		!IF ( LDMASKS(JLON) ) THEN
      ZDEOSA(JLON,KTDIA-1)=MAX(ZDEOSA(JLON,KTDIA-1),&
       & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))
    !ENDIF
  

ELSE

  ! gaseous overlaps are off => sum optical depths
	
		!IF ( LDMASKS(JLON) ) THEN
      ZDEOSA(JLON,KTDIA-1)=ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3)
    !ENDIF
  

ENDIF






	!IF ( LDMASKS(JLON) ) THEN
    PDEOSI(JLON,KTDIA-1)=ZDEOSA(JLON,KTDIA-1)/PDM0I(JLON)
  !ENDIF


! -----
! descending vertical loop
! -----
! - temporary 1D arrays:
! ZEOSO     : "old" solar   optical depth (for computing "new" one)
! -----

DO JLEV=KTDIA,KLEV

	
		!IF ( LDMASKS(JLON) ) THEN
      ZEOSO=ZDEOSA(JLON,JLEV-1)
    !ENDIF
  

  ! compute unscaled absorber amounts 2.du
  
    ZDU(1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(4)=ZDU(1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  

  ! compute total and incremental optical depths
  ! (solar incremental multiplied by 2.mu0)
  


! diffusivity factor


! differentiate between solar descending and ascending loop
IF ( LL_DESC ) THEN

  ! descending loop => 2.du will be multiplied by 1/(2.mu0)
	
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_=PDM0I(JLON)
      ZMD_S_=PDM0I(JLON)
    !ENDIF
  

ELSE

  ! ascending loop => 2.du will be multiplied by 1 or sqrt(e)/2
	
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_=1._JPRB
      ZMD_S_=ZMD
    !ENDIF
  

ENDIF

! compute optical depths including narrowband saturation
DO JG=1,3
  IF (LL_OZONE.AND.JG == 3 )  THEN
    CYCLE
  ENDIF  ! skip solar ozone
	
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      ZS_UW(JG)=ZS_UW(JG)+ZS_FW(JLON,JLEV,JG)*ZDU(JG)*ZMD_W_
      ZS_US(JG)=ZS_US(JG)+ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_
      ZS_US_IRHOV(JG)=ZS_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
       &                        ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =ZS_US(JG)/(2._JPRB*ZS_UW(JG))
      Z4BU_  =4._JPRB*ZS_UW(JG)*ZS_UW(JG)/ZS_US(JG)
      ZAFVOI_=1._JPRB
      IF (YDPHY%LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*ZS_US_IRHOV(JG)/ZS_US(JG)
        IF (YDPHY%LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)
    !ENDIF
  
ENDDO

! special treatment of solar ozone because of almost zero 'b'
IF ( LL_OZONE ) THEN
  JG=3
	
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      ZS_UW(JG)=ZS_UW(JG)+ZS_FW(JLON,JLEV,JG)*ZDU(JG)*ZMD_W_
      ZS_US(JG)=ZS_US(JG)+ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_
      ZS_US_IRHOV(JG)=ZS_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
       &                        ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =ZS_US(JG)/(2._JPRB*ZS_UW(JG))
      Z4BU_  =4._JPRB*ZS_UW(JG)*ZS_UW(JG)/ZS_US(JG)
      ZAFVOI_=1._JPRB
      IF (YDPHY%LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*ZS_US_IRHOV(JG)/ZS_US(JG)
        IF (YDPHY%LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JG)=ZAFVOI_*ZS_UW(JG)
    !ENDIF
  
ENDIF

! add H2O e-type continuum
IF ( YDPHY3%FGTS_C(1) /= 0._JPRB ) THEN
	
		!IF ( LDMASKS(JLON) ) THEN
      ZS_UC=ZS_UC+ZS_FC(JLON,JLEV)*ZDU(4)*ZMD_S_
      ZDELTA_(1)=ZDELTA_(1)+ZS_UC/&
       & (1._JPRB+YDPHY3%FGTS_C(3)*ZS_UC)+YDPHY3%FGTS_C(4)*ZS_UC**YDPHY3%FGTS_C(5)
    !ENDIF
  
ENDIF

! compute broadband saturation
DO JG=1,3
	
		!IF ( LDMASKS(JLON) ) THEN

      ! rescaling in order to account for broadband saturation
      ZDELTA_(JG)=(YDPHY3%FGTS_DELTA0(JG)/YDPHY3%FGTS_ALPHA(JG))*((1._JPRB+&
       & ZDELTA_(JG)/YDPHY3%FGTS_DELTA0(JG))**YDPHY3%FGTS_ALPHA(JG)-1._JPRB)

      ! update u, p_avg.u and T_avg.u, compute p_avg and T_avg
      ZS_U (JG)=ZS_U (JG)+           ZDU(JG)*ZMD_S_
      ZS_PU(JG)=ZS_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)*ZMD_S_
      ZS_TU(JG)=ZS_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)*ZMD_S_
      ZP_AVG_=ZS_PU(JG)/ZS_U(JG)
      ZT_AVG_=ZS_TU(JG)/ZS_U(JG)

      ! corrective secondary fit
      ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTS_D(JG))
      ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
      ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
       & YDPHY3%FGTS_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTS_P00(JG,1)+ZT_AVG_*YDPHY3%FGTS_P00(JG,2))+&
       & ZAUX_*(YDPHY3%FGTS_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,0,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,1,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,2,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,3,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,4,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,5,2))&
       & )))))))
      ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
       & (YDPHY3%FGTS_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTS_Q(JG,1)+ZP_AVG_*YDPHY3%FGTS_Q(JG,2)))/&
       & (1._JPRB+ZDELTA_(JG)))
    !ENDIF
  
ENDDO

! compute gaseous overlaps
IF ( ANY(YDPHY3%FGTS_OA /= 0._JPRB) ) THEN

  ! individual absorptivities
	
		!IF ( LDMASKS(JLON) ) THEN
      ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
      ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
      ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))
    !ENDIF
  

  ! absorptivities for pairs of gases assuming random overlaps and
  ! modulation factors for the fits
	
		!IF ( LDMASKS(JLON) ) THEN
      ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
      ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
      ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
      ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
       & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
      ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
       & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
      ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
       & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))
    !ENDIF
  

  ! transmissions for pairs of gases containing fitted contribution
  ! of non-random overlaps
  DO JO=1,3
		
			!IF ( LDMASKS(JLON) ) THEN
        ZTAU_(JO)=1._JPRB-ZAR_(JO)-ZCOEF_(JO)*YDPHY3%FGTS_OA(JO)*&
         & (1._JPRB-ZAR_(JO))**YDPHY3%FGTS_OB(JO)*ZAR_(JO)**YDPHY3%FGTS_OC(JO)*&
         & (1._JPRB-YDPHY3%FGTS_OD(JO)*ZAR_(JO))
      !ENDIF
    
  ENDDO

  ! final optical depth (cannot go below maximum of individual optical depths)
	
		!IF ( LDMASKS(JLON) ) THEN
      ZDEOSA(JLON,JLEV)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
       & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))
    !ENDIF
  
	
		!IF ( LDMASKS(JLON) ) THEN
      ZDEOSA(JLON,JLEV)=MAX(ZDEOSA(JLON,JLEV),&
       & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))
    !ENDIF
  

ELSE

  ! gaseous overlaps are off => sum optical depths
	
		!IF ( LDMASKS(JLON) ) THEN
      ZDEOSA(JLON,JLEV)=ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3)
    !ENDIF
  

ENDIF





	
		!IF ( LDMASKS(JLON) ) THEN
      PDEOSI(JLON,JLEV)=&
       & MAX((ZDEOSA(JLON,JLEV)-ZEOSO)/PDM0I(JLON),0._JPRB)
    !ENDIF
  

ENDDO

! -----
! surface condition
! -----

! solar depths computed from top to surface and up to given level

	!IF ( LDMASKS(JLON) ) THEN
    ZEOSO=ZDEOSA(JLON,KLEV)
  !ENDIF


! -----
! ascending vertical loop
! -----

! use diffusivity factor in solar band instead of 1/mu0
LL_DESC=.FALSE.

DO JLEV=KLEV,KTDIA,-1

  ! compute unscaled absorber amounts 2.du
  
    ZDU(1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(4)=ZDU(1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  

  ! compute total and incremental optical depths
	! daand: a bit worried about PT and ZUEOSA being passed as scalars here ...
  


! diffusivity factor


! differentiate between solar descending and ascending loop
IF ( LL_DESC ) THEN

  ! descending loop => 2.du will be multiplied by 1/(2.mu0)
	
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_=PDM0I(JLON)
      ZMD_S_=PDM0I(JLON)
    !ENDIF
  

ELSE

  ! ascending loop => 2.du will be multiplied by 1 or sqrt(e)/2
	
		!IF ( LDMASKS(JLON) ) THEN
      ZMD_W_=1._JPRB
      ZMD_S_=ZMD
    !ENDIF
  

ENDIF

! compute optical depths including narrowband saturation
DO JG=1,3
  IF (LL_OZONE.AND.JG == 3 )  THEN
    CYCLE
  ENDIF  ! skip solar ozone
	
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      ZS_UW(JG)=ZS_UW(JG)+ZS_FW(JLON,JLEV,JG)*ZDU(JG)*ZMD_W_
      ZS_US(JG)=ZS_US(JG)+ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_
      ZS_US_IRHOV(JG)=ZS_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
       &                        ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =ZS_US(JG)/(2._JPRB*ZS_UW(JG))
      Z4BU_  =4._JPRB*ZS_UW(JG)*ZS_UW(JG)/ZS_US(JG)
      ZAFVOI_=1._JPRB
      IF (YDPHY%LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*ZS_US_IRHOV(JG)/ZS_US(JG)
        IF (YDPHY%LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JG)=ZA2B_*(SQRT(1._JPRB+ZAFVOI_*Z4BU_)-1._JPRB)
    !ENDIF
  
ENDDO

! special treatment of solar ozone because of almost zero 'b'
IF ( LL_OZONE ) THEN
  JG=3
	
		!IF ( LDMASKS(JLON) ) THEN

      ! update quantities u_w, u_s and u_s_rho
      ZS_UW(JG)=ZS_UW(JG)+ZS_FW(JLON,JLEV,JG)*ZDU(JG)*ZMD_W_
      ZS_US(JG)=ZS_US(JG)+ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_
      ZS_US_IRHOV(JG)=ZS_US_IRHOV(JG)+ZIRHOV(JLON,JLEV)*&
       &                        ZS_FS(JLON,JLEV,JG)*ZDU(JG)*ZMD_S_

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B_  =ZS_US(JG)/(2._JPRB*ZS_UW(JG))
      Z4BU_  =4._JPRB*ZS_UW(JG)*ZS_UW(JG)/ZS_US(JG)
      ZAFVOI_=1._JPRB
      IF (YDPHY%LVOIGT) THEN
        ZVOIGT_=YS_RHOZ0V%V(JG)*ZS_US_IRHOV(JG)/ZS_US(JG)
        IF (YDPHY%LVFULL) THEN
          ZVOIEMP_=EXP(ZEPSV*LOG(ZIBV0*(Z4BU_/ZVOIGT_))*LOG(ZIZV0*ZVOIGT_))
        ELSE
          ZVOIEMP_=ZVOISIM
        ENDIF
        ZAFVOI_=1._JPRB+ZVOIGT_/(RPI*(Z4BU_/ZVOIGT_)/(1.5_JPRB*SQRT(Z4BU_))+&
         & 4._JPRB/(Z4BU_/ZVOIGT_)+5._JPRB+ZGAMV*SQRT(Z4BU_/ZVOIGT_)*ZVOIEMP_)
      ENDIF
      ZDELTA_(JG)=ZAFVOI_*ZS_UW(JG)
    !ENDIF
  
ENDIF

! add H2O e-type continuum
IF ( YDPHY3%FGTS_C(1) /= 0._JPRB ) THEN
	
		!IF ( LDMASKS(JLON) ) THEN
      ZS_UC=ZS_UC+ZS_FC(JLON,JLEV)*ZDU(4)*ZMD_S_
      ZDELTA_(1)=ZDELTA_(1)+ZS_UC/&
       & (1._JPRB+YDPHY3%FGTS_C(3)*ZS_UC)+YDPHY3%FGTS_C(4)*ZS_UC**YDPHY3%FGTS_C(5)
    !ENDIF
  
ENDIF

! compute broadband saturation
DO JG=1,3
	
		!IF ( LDMASKS(JLON) ) THEN

      ! rescaling in order to account for broadband saturation
      ZDELTA_(JG)=(YDPHY3%FGTS_DELTA0(JG)/YDPHY3%FGTS_ALPHA(JG))*((1._JPRB+&
       & ZDELTA_(JG)/YDPHY3%FGTS_DELTA0(JG))**YDPHY3%FGTS_ALPHA(JG)-1._JPRB)

      ! update u, p_avg.u and T_avg.u, compute p_avg and T_avg
      ZS_U (JG)=ZS_U (JG)+           ZDU(JG)*ZMD_S_
      ZS_PU(JG)=ZS_PU(JG)+PAPRSF(JLON,JLEV)*ZDU(JG)*ZMD_S_
      ZS_TU(JG)=ZS_TU(JG)+PT  (JLON,JLEV)*ZDU(JG)*ZMD_S_
      ZP_AVG_=ZS_PU(JG)/ZS_U(JG)
      ZT_AVG_=ZS_TU(JG)/ZS_U(JG)

      ! corrective secondary fit
      ZAUX_=ZDELTA_(JG)/(ZDELTA_(JG)+YDPHY3%FGTS_D(JG))
      ZLOG_=LOG(MAX(ZDELTA_(JG),ZTRLI))
      ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,&
       & YDPHY3%FGTS_P00(JG,0)+ZT_AVG_*(YDPHY3%FGTS_P00(JG,1)+ZT_AVG_*YDPHY3%FGTS_P00(JG,2))+&
       & ZAUX_*(YDPHY3%FGTS_P(JG,0,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,0,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,0,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,1,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,1,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,1,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,2,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,2,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,2,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,3,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,3,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,3,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,4,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,4,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,4,2))+&
       & ZLOG_*(YDPHY3%FGTS_P(JG,5,0)+ZT_AVG_*(YDPHY3%FGTS_P(JG,5,1)+ZT_AVG_*YDPHY3%FGTS_P(JG,5,2))&
       & )))))))
      ZDELTA_(JG)=ZDELTA_(JG)*MAX(0._JPRB,1._JPRB+&
       & (YDPHY3%FGTS_Q(JG,0)+ZP_AVG_*(YDPHY3%FGTS_Q(JG,1)+ZP_AVG_*YDPHY3%FGTS_Q(JG,2)))/&
       & (1._JPRB+ZDELTA_(JG)))
    !ENDIF
  
ENDDO

! compute gaseous overlaps
IF ( ANY(YDPHY3%FGTS_OA /= 0._JPRB) ) THEN

  ! individual absorptivities
	
		!IF ( LDMASKS(JLON) ) THEN
      ZA_(1)=1._JPRB-EXP(-ZDELTA_(1))
      ZA_(2)=1._JPRB-EXP(-ZDELTA_(2))
      ZA_(3)=1._JPRB-EXP(-ZDELTA_(3))
    !ENDIF
  

  ! absorptivities for pairs of gases assuming random overlaps and
  ! modulation factors for the fits
	
		!IF ( LDMASKS(JLON) ) THEN
      ZAR_(1)=ZA_(1)+ZA_(2)-ZA_(1)*ZA_(2)
      ZAR_(2)=ZA_(1)+ZA_(3)-ZA_(1)*ZA_(3)
      ZAR_(3)=ZA_(2)+ZA_(3)-ZA_(2)*ZA_(3)
      ZCOEF_(1)=2._JPRB*ZA_(1)*ZA_(2)/&
       & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(2)*ZA_(2))
      ZCOEF_(2)=2._JPRB*ZA_(1)*ZA_(3)/&
       & (ZEPSD+ZA_(1)*ZA_(1)+ZA_(3)*ZA_(3))
      ZCOEF_(3)=2._JPRB*ZA_(2)*ZA_(3)/&
       & (ZEPSD+ZA_(2)*ZA_(2)+ZA_(3)*ZA_(3))
    !ENDIF
  

  ! transmissions for pairs of gases containing fitted contribution
  ! of non-random overlaps
  DO JO=1,3
		
			!IF ( LDMASKS(JLON) ) THEN
        ZTAU_(JO)=1._JPRB-ZAR_(JO)-ZCOEF_(JO)*YDPHY3%FGTS_OA(JO)*&
         & (1._JPRB-ZAR_(JO))**YDPHY3%FGTS_OB(JO)*ZAR_(JO)**YDPHY3%FGTS_OC(JO)*&
         & (1._JPRB-YDPHY3%FGTS_OD(JO)*ZAR_(JO))
      !ENDIF
    
  ENDDO

  ! final optical depth (cannot go below maximum of individual optical depths)
	
		!IF ( LDMASKS(JLON) ) THEN
      ZUEOSA(JLON,JLEV)=-LOG(MAX(ZTRLI,ZTAU_(1)*ZTAU_(2)*ZTAU_(3)))-&
       & (ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3))
    !ENDIF
  
	
		!IF ( LDMASKS(JLON) ) THEN
      ZUEOSA(JLON,JLEV)=MAX(ZUEOSA(JLON,JLEV),&
       & ZDELTA_(1),ZDELTA_(2),ZDELTA_(3))
    !ENDIF
  

ELSE

  ! gaseous overlaps are off => sum optical depths
	
		!IF ( LDMASKS(JLON) ) THEN
      ZUEOSA(JLON,JLEV)=ZDELTA_(1)+ZDELTA_(2)+ZDELTA_(3)
    !ENDIF
  

ENDIF





	
		!IF ( LDMASKS(JLON) ) THEN
      PUEOSI(JLON,JLEV)=MAX(ZUEOSA(JLON,JLEV)-ZEOSO,0._JPRB)
      ZEOSO      =ZUEOSA(JLON,JLEV)
    !ENDIF
  

ENDDO



! -----
! private procedures
! -----



! computation of solar optical depths


! -----

END SUBROUTINE ACRANEB_TRANSS

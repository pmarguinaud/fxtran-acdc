!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_TRANSS(YDCST,YDPHY,YDPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,KJN,&
 & KIIDIA,KIFDIA,KAUCR,&
! - INPUT 2D
 & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,&
! - INPUT 1D
 & PDM0I,&
! - OUTPUT 2D
 & PDEOSI,PUEOSI)

!$ACDC singlecolumn  


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

!   2020-11, J. Masek
!   Initialization of PUEOSI at KTDIA-1.
! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1 ,ONLY : JPIM     ,JPRB
USE YOMHOOK  ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMCST   ,ONLY : TCST
USE YOMPHY   ,ONLY : TPHY
USE YOMPHY3  ,ONLY : TPHY3

IMPLICIT NONE

TYPE(TCST)        ,INTENT(IN) :: YDCST
TYPE(TPHY)        ,INTENT(IN ):: YDPHY
TYPE(TPHY3)       ,INTENT(IN ):: YDPHY3
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KJN
INTEGER(KIND=JPIM),INTENT(IN) :: KIIDIA(KJN)
INTEGER(KIND=JPIM),INTENT(IN) :: KIFDIA(KJN)
INTEGER(KIND=JPIM),INTENT(IN) :: KAUCR

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

! -----

REAL(KIND=JPRB) :: ZA,ZB,ZARGLI,ZEPSD,ZEPSP,ZEPSU,ZTRLI

REAL(KIND=JPRB) :: ZEPSV,ZGAMV,ZIBV0,ZIZV0,ZMD,ZVOISIM

REAL(KIND=JPRB) :: ZS_RHOZ0V(3)

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

! local integer scalars
INTEGER(KIND=JPIM) :: JG,JLEV,JLON,JN

! local logical scalars
LOGICAL :: LL_DESC,LL_OZONE

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANSS',0,ZHOOK_HANDLE)
ASSOCIATE(FGTS_DELTA0=>YDPHY3%FGTS_DELTA0, FGTS_OC=>YDPHY3%FGTS_OC, &
 & FGTS_OB=>YDPHY3%FGTS_OB, FGTS_OA=>YDPHY3%FGTS_OA, FGTS_OD=>YDPHY3%FGTS_OD, &
 & FGTS_P00=>YDPHY3%FGTS_P00, FGTS_Q=>YDPHY3%FGTS_Q, FGTS_P=>YDPHY3%FGTS_P, &
 & FGTS_ALPHA=>YDPHY3%FGTS_ALPHA, FGTS_D=>YDPHY3%FGTS_D, FGTS_A=>YDPHY3%FGTS_A, &
 & FGTS_C=>YDPHY3%FGTS_C, FGTS_B=>YDPHY3%FGTS_B, &
 & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX, &
 & RPI=>YDCST%RPI, RD=>YDCST%RD, RV=>YDCST%RV)
! -----
! security constants, derived parameters for Voigt effect and
! Malkmus formula
! -----

! security constants
ZARGLI=-250._JPRB
ZTRLI =EXP(ZARGLI)
ZEPSP =1.E-03_JPRB
ZEPSD =1.E-20_JPRB
ZEPSU =ZTRLI

! Voigt coefficients
ZS_RHOZ0V(1)=0.296_JPRB
ZS_RHOZ0V(2)=0.046_JPRB
ZS_RHOZ0V(3)=0.202_JPRB
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
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZA=FGTS_A(JG,0)*(1._JPRB+FGTS_A(JG,1)*PT(JLON,KTDIA))/&
       &              (1._JPRB+FGTS_A(JG,2)*PT(JLON,KTDIA))
      ZB=FGTS_B(JG,0)*(1._JPRB+FGTS_B(JG,1)*PT(JLON,KTDIA))/&
       &              (1._JPRB+FGTS_B(JG,2)*PT(JLON,KTDIA))
      ZS_FW(JLON,KTDIA-1,JG)=ZA
      ZS_FS(JLON,KTDIA-1,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*ZP(JLON)
    ENDDO
  ENDDO
  DO JLEV=KTDIA,KLEV
    DO JN=1,KAUCR
      DO JLON=KIIDIA(JN),KIFDIA(JN)
        ZA=FGTS_A(JG,0)*(1._JPRB+FGTS_A(JG,1)*PT(JLON,JLEV))/&
         &              (1._JPRB+FGTS_A(JG,2)*PT(JLON,JLEV))
        ZB=FGTS_B(JG,0)*(1._JPRB+FGTS_B(JG,1)*PT(JLON,JLEV))/&
         &              (1._JPRB+FGTS_B(JG,2)*PT(JLON,JLEV))
        ZS_FW(JLON,JLEV,JG)=ZA
        ZS_FS(JLON,JLEV,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*PAPRSF(JLON,JLEV)
      ENDDO
    ENDDO
  ENDDO

ENDDO

! initialize pressure/temperature factors for H2O e-type continuum
IF ( FGTS_C(1) == 0._JPRB ) THEN
  ZS_FC(:,:)=0._JPRB
ELSE
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZS_FC(JLON,KTDIA-1)=FGTS_C(1)*EXP(-FGTS_C(2)*PT(JLON,KTDIA))*&
       & ZP(JLON)
    ENDDO
  ENDDO
  DO JLEV=KTDIA,KLEV
    DO JN=1,KAUCR
      DO JLON=KIIDIA(JN),KIFDIA(JN)
        ZS_FC(JLON,JLEV)=FGTS_C(1)*EXP(-FGTS_C(2)*PT(JLON,JLEV))*&
         & PAPRSF(JLON,JLEV)
      ENDDO
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
ZS_UW      (:,:)=ZEPSU
ZS_US      (:,:)=ZEPSU
ZS_US_IRHOV(:,:)=ZEPSU
ZS_UC      (:)  =ZEPSU
ZS_U       (:,:)=ZEPSU
ZS_PU      (:,:)=ZEPSU
ZS_TU      (:,:)=ZEPSU

! compute total and incremental optical depths
! (solar incremental multiplied by 2.mu0)
CALL DELTA_S(KTDIA-1,LL_DESC,ZP,PT(:,KTDIA),ZDU,&
 & ZS_UW,ZS_US,ZS_US_IRHOV,ZS_UC,ZS_U,ZS_PU,ZS_TU,&
 & ZDEOSA(:,KTDIA-1))
DO JN=1,KAUCR
  DO JLON=KIIDIA(JN),KIFDIA(JN)
    PDEOSI(JLON,KTDIA-1)=ZDEOSA(JLON,KTDIA-1)/PDM0I(JLON)
  ENDDO
ENDDO

! -----
! descending vertical loop
! -----
! - temporary 1D arrays:
! ZEOSO     : "old" solar   optical depth (for computing "new" one)
! -----

DO JLEV=KTDIA,KLEV

  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZEOSO(JLON)=ZDEOSA(JLON,JLEV-1)
    ENDDO
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
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PDEOSI(JLON,JLEV)=&
       & MAX((ZDEOSA(JLON,JLEV)-ZEOSO(JLON))/PDM0I(JLON),0._JPRB)
    ENDDO
  ENDDO

ENDDO

! -----
! surface condition
! -----

! solar depths computed from top to surface and up to given level
DO JN=1,KAUCR
  DO JLON=KIIDIA(JN),KIFDIA(JN)
    ZEOSO(JLON)=ZDEOSA(JLON,KLEV)
  ENDDO
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
  CALL DELTA_S(JLEV,LL_DESC,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
   & ZS_UW,ZS_US,ZS_US_IRHOV,ZS_UC,ZS_U,ZS_PU,ZS_TU,&
   & ZUEOSA(:,JLEV))
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PUEOSI(JLON,JLEV)=MAX(ZUEOSA(JLON,JLEV)-ZEOSO(JLON),0._JPRB)
      ZEOSO(JLON)      =ZUEOSA(JLON,JLEV)
    ENDDO
  ENDDO

ENDDO

! PUEOSI has no meaning at KTDIA-1, this is to enable its intermittent
! treatment in common loops with PDEOSI.
DO JLON=KIDIA,KFDIA
  PUEOSI(JLON,KTDIA-1)=0._JPRB
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANSS',1,ZHOOK_HANDLE)

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

INTEGER(KIND=JPIM) :: JG,JLON,JN,JO

REAL(KIND=JPRB) :: Z4BU,ZA2B,ZAFVOI,ZVOIEMP,ZVOIGT
REAL(KIND=JPRB) :: ZAUX,ZLOG,ZP_AVG,ZT_AVG

REAL(KIND=JPRB) :: ZMD_W (KLON)
REAL(KIND=JPRB) :: ZMD_S (KLON)
REAL(KIND=JPRB) :: ZDELTA(KLON,3)
REAL(KIND=JPRB) :: ZAS   (KLON,3)
REAL(KIND=JPRB) :: ZAR   (KLON,3)
REAL(KIND=JPRB) :: ZCOEF (KLON,3)
REAL(KIND=JPRB) :: ZTAU  (KLON,3)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANSS:DELTA_S',0,ZHOOK_HANDLE)
ASSOCIATE(FGTS_DELTA0=>YDPHY3%FGTS_DELTA0, FGTS_OC=>YDPHY3%FGTS_OC, &
 & FGTS_OB=>YDPHY3%FGTS_OB, FGTS_OA=>YDPHY3%FGTS_OA, FGTS_OD=>YDPHY3%FGTS_OD, &
 & FGTS_P00=>YDPHY3%FGTS_P00, FGTS_Q=>YDPHY3%FGTS_Q, FGTS_P=>YDPHY3%FGTS_P, &
 & FGTS_ALPHA=>YDPHY3%FGTS_ALPHA, FGTS_D=>YDPHY3%FGTS_D, FGTS_A=>YDPHY3%FGTS_A, &
 & FGTS_C=>YDPHY3%FGTS_C, FGTS_B=>YDPHY3%FGTS_B, &
 & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX, RPI=>YDCST%RPI)

! differentiate between solar descending and ascending loop
IF ( LD_DESC ) THEN

  ! descending loop => 2.du will be multiplied by 1/(2.mu0)
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZMD_W(JLON)=PDM0I(JLON)
      ZMD_S(JLON)=PDM0I(JLON)
    ENDDO
  ENDDO

ELSE

  ! ascending loop => 2.du will be multiplied by 1 or sqrt(e)/2
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZMD_W(JLON)=1._JPRB
      ZMD_S(JLON)=ZMD
    ENDDO
  ENDDO

ENDIF

! compute optical depths including narrowband saturation
DO JG=1,3
  IF (LL_OZONE.AND.JG == 3 ) CYCLE  ! skip solar ozone
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)

      ! update quantities u_w, u_s and u_s_rho
      PUW(JLON,JG)=PUW(JLON,JG)+ZS_FW(JLON,KL,JG)*PDU(JLON,JG)*ZMD_W(JLON)
      PUS(JLON,JG)=PUS(JLON,JG)+ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S(JLON)
      PUS_IRHOV(JLON,JG)=PUS_IRHOV(JLON,JG)+ZIRHOV(JLON,KL)*&
       &                        ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S(JLON)

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B  =PUS(JLON,JG)/(2._JPRB*PUW(JLON,JG))
      Z4BU  =4._JPRB*PUW(JLON,JG)*PUW(JLON,JG)/PUS(JLON,JG)
      ZAFVOI=1._JPRB
      IF (LVOIGT) THEN
        ZVOIGT=ZS_RHOZ0V(JG)*PUS_IRHOV(JLON,JG)/PUS(JLON,JG)
        IF (LVFULL) THEN
          ZVOIEMP=EXP(ZEPSV*LOG(ZIBV0*(Z4BU/ZVOIGT))*LOG(ZIZV0*ZVOIGT))
        ELSE
          ZVOIEMP=ZVOISIM
        ENDIF
        ZAFVOI=1._JPRB+ZVOIGT/(RPI*(Z4BU/ZVOIGT)/(1.5_JPRB*SQRT(Z4BU))+&
         & 4._JPRB/(Z4BU/ZVOIGT)+5._JPRB+ZGAMV*SQRT(Z4BU/ZVOIGT)*ZVOIEMP)
      ENDIF
      ZDELTA(JLON,JG)=ZA2B*(SQRT(1._JPRB+ZAFVOI*Z4BU)-1._JPRB)
    ENDDO
  ENDDO
ENDDO

! special treatment of solar ozone because of almost zero 'b'
IF ( LL_OZONE ) THEN
  JG=3
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)

      ! update quantities u_w, u_s and u_s_rho
      PUW(JLON,JG)=PUW(JLON,JG)+ZS_FW(JLON,KL,JG)*PDU(JLON,JG)*ZMD_W(JLON)
      PUS(JLON,JG)=PUS(JLON,JG)+ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S(JLON)
      PUS_IRHOV(JLON,JG)=PUS_IRHOV(JLON,JG)+ZIRHOV(JLON,KL)*&
       &                        ZS_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD_S(JLON)

      ! Malkmus-Voigt formula with Curtis-Godson approximation
      ZA2B  =PUS(JLON,JG)/(2._JPRB*PUW(JLON,JG))
      Z4BU  =4._JPRB*PUW(JLON,JG)*PUW(JLON,JG)/PUS(JLON,JG)
      ZAFVOI=1._JPRB
      IF (LVOIGT) THEN
        ZVOIGT=ZS_RHOZ0V(JG)*PUS_IRHOV(JLON,JG)/PUS(JLON,JG)
        IF (LVFULL) THEN
          ZVOIEMP=EXP(ZEPSV*LOG(ZIBV0*(Z4BU/ZVOIGT))*LOG(ZIZV0*ZVOIGT))
        ELSE
          ZVOIEMP=ZVOISIM
        ENDIF
        ZAFVOI=1._JPRB+ZVOIGT/(RPI*(Z4BU/ZVOIGT)/(1.5_JPRB*SQRT(Z4BU))+&
         & 4._JPRB/(Z4BU/ZVOIGT)+5._JPRB+ZGAMV*SQRT(Z4BU/ZVOIGT)*ZVOIEMP)
      ENDIF
      ZDELTA(JLON,JG)=ZAFVOI*PUW(JLON,JG)
    ENDDO
  ENDDO
ENDIF

! add H2O e-type continuum
IF ( FGTS_C(1) /= 0._JPRB ) THEN
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PUC(JLON)=PUC(JLON)+ZS_FC(JLON,KL)*PDU(JLON,4)*ZMD_S(JLON)
      ZDELTA(JLON,1)=ZDELTA(JLON,1)+PUC(JLON)/&
       & (1._JPRB+FGTS_C(3)*PUC(JLON))+FGTS_C(4)*PUC(JLON)**FGTS_C(5)
    ENDDO
  ENDDO
ENDIF

! compute broadband saturation
DO JG=1,3
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)

      ! rescaling in order to account for broadband saturation
      ZDELTA(JLON,JG)=(FGTS_DELTA0(JG)/FGTS_ALPHA(JG))*((1._JPRB+&
       & ZDELTA(JLON,JG)/FGTS_DELTA0(JG))**FGTS_ALPHA(JG)-1._JPRB)

      ! update u, p_avg.u and T_avg.u, compute p_avg and T_avg
      P_U (JLON,JG)=P_U (JLON,JG)+           PDU(JLON,JG)*ZMD_S(JLON)
      P_PU(JLON,JG)=P_PU(JLON,JG)+PRES(JLON)*PDU(JLON,JG)*ZMD_S(JLON)
      P_TU(JLON,JG)=P_TU(JLON,JG)+PT  (JLON)*PDU(JLON,JG)*ZMD_S(JLON)
      ZP_AVG=P_PU(JLON,JG)/P_U(JLON,JG)
      ZT_AVG=P_TU(JLON,JG)/P_U(JLON,JG)

      ! corrective secondary fit
      ZAUX=ZDELTA(JLON,JG)/(ZDELTA(JLON,JG)+FGTS_D(JG))
      ZLOG=LOG(MAX(ZDELTA(JLON,JG),ZTRLI))
      ZDELTA(JLON,JG)=ZDELTA(JLON,JG)*MAX(0._JPRB,&
       & FGTS_P00(JG,0)+ZT_AVG*(FGTS_P00(JG,1)+ZT_AVG*FGTS_P00(JG,2))+&
       & ZAUX*(FGTS_P(JG,0,0)+ZT_AVG*(FGTS_P(JG,0,1)+ZT_AVG*FGTS_P(JG,0,2))+&
       & ZLOG*(FGTS_P(JG,1,0)+ZT_AVG*(FGTS_P(JG,1,1)+ZT_AVG*FGTS_P(JG,1,2))+&
       & ZLOG*(FGTS_P(JG,2,0)+ZT_AVG*(FGTS_P(JG,2,1)+ZT_AVG*FGTS_P(JG,2,2))+&
       & ZLOG*(FGTS_P(JG,3,0)+ZT_AVG*(FGTS_P(JG,3,1)+ZT_AVG*FGTS_P(JG,3,2))+&
       & ZLOG*(FGTS_P(JG,4,0)+ZT_AVG*(FGTS_P(JG,4,1)+ZT_AVG*FGTS_P(JG,4,2))+&
       & ZLOG*(FGTS_P(JG,5,0)+ZT_AVG*(FGTS_P(JG,5,1)+ZT_AVG*FGTS_P(JG,5,2))&
       & )))))))
      ZDELTA(JLON,JG)=ZDELTA(JLON,JG)*MAX(0._JPRB,1._JPRB+&
       & (FGTS_Q(JG,0)+ZP_AVG*(FGTS_Q(JG,1)+ZP_AVG*FGTS_Q(JG,2)))/&
       & (1._JPRB+ZDELTA(JLON,JG)))
    ENDDO
  ENDDO
ENDDO

! compute gaseous overlaps
IF ( ANY(FGTS_OA /= 0._JPRB) ) THEN

  ! individual absorptivities
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZAS(JLON,1)=1._JPRB-EXP(-ZDELTA(JLON,1))
      ZAS(JLON,2)=1._JPRB-EXP(-ZDELTA(JLON,2))
      ZAS(JLON,3)=1._JPRB-EXP(-ZDELTA(JLON,3))
    ENDDO
  ENDDO

  ! absorptivities for pairs of gases assuming random overlaps and
  ! modulation factors for the fits
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZAR(JLON,1)=ZAS(JLON,1)+ZAS(JLON,2)-ZAS(JLON,1)*ZAS(JLON,2)
      ZAR(JLON,2)=ZAS(JLON,1)+ZAS(JLON,3)-ZAS(JLON,1)*ZAS(JLON,3)
      ZAR(JLON,3)=ZAS(JLON,2)+ZAS(JLON,3)-ZAS(JLON,2)*ZAS(JLON,3)
      ZCOEF(JLON,1)=2._JPRB*ZAS(JLON,1)*ZAS(JLON,2)/&
       & (ZEPSD+ZAS(JLON,1)*ZAS(JLON,1)+ZAS(JLON,2)*ZAS(JLON,2))
      ZCOEF(JLON,2)=2._JPRB*ZAS(JLON,1)*ZAS(JLON,3)/&
       & (ZEPSD+ZAS(JLON,1)*ZAS(JLON,1)+ZAS(JLON,3)*ZAS(JLON,3))
      ZCOEF(JLON,3)=2._JPRB*ZAS(JLON,2)*ZAS(JLON,3)/&
       & (ZEPSD+ZAS(JLON,2)*ZAS(JLON,2)+ZAS(JLON,3)*ZAS(JLON,3))
    ENDDO
  ENDDO

  ! transmissions for pairs of gases containing fitted contribution
  ! of non-random overlaps
  DO JO=1,3
    DO JN=1,KAUCR
      DO JLON=KIIDIA(JN),KIFDIA(JN)
        ZTAU(JLON,JO)=1._JPRB-ZAR(JLON,JO)-ZCOEF(JLON,JO)*FGTS_OA(JO)*&
         & (1._JPRB-ZAR(JLON,JO))**FGTS_OB(JO)*ZAR(JLON,JO)**FGTS_OC(JO)*&
         & (1._JPRB-FGTS_OD(JO)*ZAR(JLON,JO))
      ENDDO
    ENDDO
  ENDDO

  ! final optical depth (cannot go below maximum of individual optical depths)
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PDELTA(JLON)=-LOG(MAX(ZTRLI,ZTAU(JLON,1)*ZTAU(JLON,2)*ZTAU(JLON,3)))-&
       & (ZDELTA(JLON,1)+ZDELTA(JLON,2)+ZDELTA(JLON,3))
    ENDDO
  ENDDO
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PDELTA(JLON)=MAX(PDELTA(JLON),&
       & ZDELTA(JLON,1),ZDELTA(JLON,2),ZDELTA(JLON,3))
    ENDDO
  ENDDO

ELSE

  ! gaseous overlaps are off => sum optical depths
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PDELTA(JLON)=ZDELTA(JLON,1)+ZDELTA(JLON,2)+ZDELTA(JLON,3)
    ENDDO
  ENDDO

ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANSS:DELTA_S',1,ZHOOK_HANDLE)

END SUBROUTINE DELTA_S

! -----

END SUBROUTINE ACRANEB_TRANSS

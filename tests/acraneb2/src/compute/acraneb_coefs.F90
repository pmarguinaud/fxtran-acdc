!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_COEFS(YDPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,LDMASKS,&
! - INPUT 2D
 & PAPRSF,PDELP,PQICE,PQLI,PDEOS,PEODSI,PEODSN,&
 & PEOSI,PEOSIUN,PEOSN,PEOSNUN,&
 & PEO1SI,PEO1SN,PEO2SI,PEO2SN,PUEOS,PUSI,PUSN,&
 & PEOSA,PEOSAUN,PEO1SA,PEO2SA,PEO3SA,PEO4SA,&
! - INPUT 1D
 & PMU0I,&
! - OUTPUT 2D
 & PA1C,PA1CUN,PA2C,PA3C,PA4C,PA5C,PA1N,PA1NUN,PA2N,PA3N,PA4N,PA5N)

! Purpose:
! --------
!   ACRANEB_COEFS - Computes coeficients for delta-two stream adding
!                   system in solar band.

! Interface:
! ----------
! INPUT:
!   KLON    - horizontal dimension of arrays
!   KTDIA   - initial index for vertical loops (usually 1)
!   KLEV    - vertical dimension of full level arrays
!   KJN     - dimension of arrays containing "daylight" intervals
!   KIIDIA  - array of indices marking start of "daylight" intervals
!   KIFDIA  - array of indices marking end of "daylight" intervals
!   KAUCR   - number of "daylight" intervals
!   PAPRSF  - pressure on full-levels
!   PDELP   - pressure thickness of layers
!   PQICE   - specific mass of cloud ice INSIDE CLOUD
!   PQLI    - specific mass of cloud liquid INSIDE CLOUD
!   PDEOS   - descending gaseous optical depth of layers (absorption only)
!   PEODSI  - scattering coefficient for cloud ice
!   PEODSN  - scattering coefficient for cloud liquid
!   PEOSI   - extinction coefficient for cloud ice
!   PEOSIUN - extinction coefficient for cloud ice, delta-unscaled
!   PEOSN   - extinction coefficient for cloud liquid
!   PEOSNUN - extinction coefficient for cloud liquid, delta-unscaled
!   PEO1SI  - alpha1 for cloud ice
!   PEO1SN  - alpha1 for cloud liquid
!   PEO2SI  - alpha2 for cloud ice
!   PEO2SN  - alpha2 for cloud liquid
!   PUEOS   - ascending gaseous optical depth of layers (absorption only)
!   PUSI    - upscatter fraction for cloud ice
!   PUSN    - upscatter fraction for cloud liquid
!   PEOSA   - aerosol optical depth of layers
!   PEOSAUN - aerosol optical depth of layers, delta-unscaled
!   PEO1SA  - alpha1 x optical depth for aerosols
!   PEO2SA  - alpha2 x optical depth for aerosols
!   PEO3SA  - alpha3 x optical depth for aerosols
!   PEO4SA  - alpha4 x optical depth for aerosols
!   PMU0I   - inverse of the modified cosine of the zenith angle
! OUTPUT:
!   PA1C    - clearsky parallel transmissivity
!   PA1CUN  - clearsky parallel transmissivity, delta-unscaled
!   PA2C    - clearsky parallel/diffuse transmissivity
!   PA3C    - clearsky parallel/diffuse reflectivity
!   PA4C    - clearsky diffuse transmissivity
!   PA5C    - clearsky diffuse reflectivity
!   PA1N    - cloudy parallel transmissivity
!   PA1NUN  - cloudy parallel transmissivity, delta-unscaled
!   PA2N    - cloudy parallel/diffuse transmissivity
!   PA3N    - cloudy parallel/diffuse reflectivity
!   PA4N    - cloudy diffuse transmissivity
!   PA5N    - cloudy diffuse reflectivity

! Externals:
! ----------

! Method:
! -------

! Reference:
! ----------

! Author:
! -------
!   1989-12, J.F. Geleyn (original ACRANEB)

! Modifications:
! --------------
!   2009-10, T. Kral
!   Externalized from ACRANEB.

!   2013-11, J. Masek
!   Improved numerical safety, saturation for Rayleigh scattering.
!   Phasing to cy40t1.

!   2016-04, J. Masek
!   Computation of true direct solar flux.

!   2016-09, J. Masek
!   Proper naming of variables containing delta-unscaled values.
! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB    ,JPRD
USE YOMPHY3   ,ONLY : TPHY3

IMPLICIT NONE

TYPE(TPHY3)       ,INTENT(INOUT):: YDPHY3
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA
LOGICAL,          INTENT(IN)  :: LDMASKS(KLON)

REAL(KIND=JPRB),INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PDELP(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQICE(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQLI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PDEOS(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEODSI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEODSN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEOSI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEOSIUN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEOSN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEOSNUN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO1SI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO1SN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO2SI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO2SN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PUEOS(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PUSI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PUSN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEOSA(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEOSAUN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO1SA(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO2SA(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO3SA(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO4SA(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PMU0I(KLON)
REAL(KIND=JPRB),INTENT(OUT)   :: PA1C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA1CUN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA2C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA3C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA4C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA5C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA1N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA1NUN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA2N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA3N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA4N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA5N(KLON,KLEV)

#include "abor1.intfb.h"

! local real scalars
REAL(KIND=JPRB) :: ZARGLI,ZEORAY
REAL(KIND=JPRB) :: ZEO,ZEODIR,ZEO1,ZEO2,ZEO3,ZEO4,ZEO5,ZEPS,ZEPS3
REAL(KIND=JPRB) :: ZG1,ZG2,ZMU0IC,ZMU0IN,ZRHO,ZTAU,ZTRLI

! local integer scalars
INTEGER(KIND=JPIM) :: JLEV, JLON, JN

ASSOCIATE(FRS_K_SCAT0=>YDPHY3%FRS_K_SCAT0, FRS_BETA=>YDPHY3%FRS_BETA, &
 & FRS_P_CRIT=>YDPHY3%FRS_P_CRIT)
! security constants
IF (JPRB == JPRD) THEN
  ZARGLI=-250._JPRB
ELSE
  ZARGLI=-80._JPRB
ENDIF
ZTRLI =EXP(ZARGLI)

IF (JPRB == JPRD) THEN
  ZEPS3 =1.E-12_JPRB
ELSE
  ZEPS3 =1.E-6_JPRB
ENDIF

DO JLEV=KTDIA,KLEV
	DO JLON=KIDIA,KFDIA
		!IF ( LDMASKS(JLON) ) THEN
      ZEORAY=FRS_K_SCAT0*(1._JPRB+PMU0I(JLON)*PAPRSF(JLON,JLEV)/&
       &     FRS_P_CRIT)**(FRS_BETA-1._JPRB)
      ZEO1=PUEOS(JLON,JLEV)+PEO1SA(JLON,JLEV)+(ZEORAY*PDELP(JLON,JLEV))
      ZEO2=PEO2SA(JLON,JLEV)+(ZEORAY*PDELP(JLON,JLEV))
      ZEPS=SQRT(ZEO1*ZEO1-ZEO2*ZEO2)
      ZTAU=EXP(MAX(-ZEPS,ZARGLI))
      ZRHO=ZEO2/(ZEO1+ZEPS+ZTRLI)
      PA4C(JLON,JLEV)=(ZTAU*(1._JPRB-ZRHO*ZRHO)+ZTRLI)&
       & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
      PA5C(JLON,JLEV)=ZRHO*(1._JPRB-ZTAU*ZTAU)&
       & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
      ZEO   =0.5_JPRB*PDEOS(JLON,JLEV)+ZEORAY*PDELP(JLON,JLEV)
      ZEODIR=ZEO+PEOSAUN(JLON,JLEV)
      ZEO   =ZEO+PEOSA  (JLON,JLEV)
      ZMU0IC=(ZEPS/(ZEO+ZTRLI))+SIGN(MAX(ABS(PMU0I(JLON)-(ZEPS&
       & /(ZEO+ZTRLI))),ZEPS3),(PMU0I(JLON)-(ZEPS/(ZEO+ZTRLI))))
      ZEO3=(PEO3SA(JLON,JLEV)+(0.5_JPRB*(ZEORAY*PDELP(JLON,JLEV))))*ZMU0IC
      ZEO4=(PEO4SA(JLON,JLEV)+(0.5_JPRB*(ZEORAY*PDELP(JLON,JLEV))))*ZMU0IC
      ZEO5=ZEO*ZMU0IC
      PA1C  (JLON,JLEV)=EXP(MAX(-ZEO5,ZARGLI))
      PA1CUN(JLON,JLEV)=EXP(MAX(-ZEODIR*PMU0I(JLON),ZARGLI))
      ZG1=(ZEO3*(ZEO5-ZEO1)-ZEO2*ZEO4)*(1._JPRB/(ZEO5*ZEO5-ZEPS*ZEPS+ZTRLI))
      ZG2=-(ZEO4*(ZEO5+ZEO1)+ZEO2*ZEO3)*(1._JPRB/(ZEO5*ZEO5-ZEPS*ZEPS+ZTRLI))
      PA2C(JLON,JLEV)=ZG2*(PA1C(JLON,JLEV)-PA4C(JLON,JLEV))&
       & -ZG1*PA5C(JLON,JLEV)*PA1C(JLON,JLEV)
      PA3C(JLON,JLEV)=ZG1*(1._JPRB-PA4C(JLON,JLEV)*PA1C(JLON,JLEV))&
       & -ZG2*PA5C(JLON,JLEV)
      ZEO1=ZEO1+PEO1SN(JLON,JLEV)*(PDELP(JLON,JLEV)*PQLI(JLON,JLEV))&
       & +PEO1SI(JLON,JLEV)*(PDELP(JLON,JLEV)*PQICE(JLON,JLEV))
      ZEO2=ZEO2+PEO2SN(JLON,JLEV)*(PDELP(JLON,JLEV)*PQLI(JLON,JLEV))&
       & +PEO2SI(JLON,JLEV)*(PDELP(JLON,JLEV)*PQICE(JLON,JLEV))
      ZEPS=SQRT(ZEO1*ZEO1-ZEO2*ZEO2)
      ZTAU=EXP(MAX(-ZEPS,ZARGLI))
      ZRHO=ZEO2/(ZEO1+ZEPS+ZTRLI)
      PA4N(JLON,JLEV)=(ZTAU*(1._JPRB-ZRHO*ZRHO)+ZTRLI)&
       & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
      PA5N(JLON,JLEV)=ZRHO*(1._JPRB-ZTAU*ZTAU)&
       & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
      ZEO=ZEO+PEOSN(JLON,JLEV)*(PDELP(JLON,JLEV)*PQLI(JLON,JLEV))&
       & +PEOSI(JLON,JLEV)*(PDELP(JLON,JLEV)*PQICE(JLON,JLEV))
      ZEODIR=ZEODIR+PEOSNUN(JLON,JLEV)*(PDELP(JLON,JLEV)*PQLI(JLON,JLEV))&
       & +PEOSIUN(JLON,JLEV)*(PDELP(JLON,JLEV)*PQICE(JLON,JLEV))
      ZMU0IN=(ZEPS/(ZEO+ZTRLI))+SIGN(MAX(ABS(PMU0I(JLON)-(ZEPS&
       & /(ZEO+ZTRLI))),ZEPS3),(PMU0I(JLON)-(ZEPS/(ZEO+ZTRLI))))
      ZEO3=(ZEO3/ZMU0IC+PUSN(JLON,JLEV)*((PDELP(JLON,JLEV)&
       & *PQLI(JLON,JLEV))*PEODSN(JLON,JLEV))&
       & +PUSI(JLON,JLEV)*((PDELP(JLON,JLEV)&
       & *PQICE(JLON,JLEV))*PEODSI(JLON,JLEV)))*ZMU0IN
      ZEO4=(ZEO4/ZMU0IC+(1._JPRB-PUSN(JLON,JLEV))*((PDELP(JLON,JLEV)&
       & *PQLI(JLON,JLEV))*PEODSN(JLON,JLEV))&
       & +(1._JPRB-PUSI(JLON,JLEV))*((PDELP(JLON,JLEV)&
       & *PQICE(JLON,JLEV))*PEODSI(JLON,JLEV)))*ZMU0IN
      ZEO5=ZEO*ZMU0IN
      PA1N  (JLON,JLEV)=EXP(MAX(-ZEO5,ZARGLI))
      PA1NUN(JLON,JLEV)=EXP(MAX(-ZEODIR*PMU0I(JLON),ZARGLI))
      ZG1=(ZEO3*(ZEO5-ZEO1)-ZEO2*ZEO4)*(1._JPRB/(ZEO5*ZEO5-ZEPS*ZEPS+ZTRLI))
      ZG2=-(ZEO4*(ZEO5+ZEO1)+ZEO2*ZEO3)*(1._JPRB/(ZEO5*ZEO5-ZEPS*ZEPS+ZTRLI))
      PA2N(JLON,JLEV)=ZG2*(PA1N(JLON,JLEV)-PA4N(JLON,JLEV))&
       & -ZG1*PA5N(JLON,JLEV)*PA1N(JLON,JLEV)
      PA3N(JLON,JLEV)=ZG1*(1._JPRB-PA4N(JLON,JLEV)&
       & *PA1N(JLON,JLEV))-ZG2*PA5N(JLON,JLEV)
    !ENDIF
  ENDDO
ENDDO

END ASSOCIATE
END SUBROUTINE ACRANEB_COEFS

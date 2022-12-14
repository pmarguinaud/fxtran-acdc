!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_COEFT (KIDIA,KFDIA,KLON,KTDIA,KLEV0,KLEV,&
! - INPUT 2D
 & PDELP,PEOPT,PEO1TI,PEO2TI,PEO1TN,PEO2TN,PQICE,PQLI,PEO1TA,PEO2TA,&
! - OUTPUT 2D
 & PA4C,PA5C,PA4N,PA5N)

! Purpose:
! --------
!   ACRANEB_COEFT - Computes coeficients for delta-two stream adding
!                   system in thermal band.

! Interface:
! ----------
! INPUT:
!   KIDIA  - initial index for horizontal loops
!   KFDIA  - final index for horizontal loops
!   KLON   - horizontal dimension of arrays
!   KTDIA  - initial index for vertical loops (usually 1)
!   KLEV   - vertical dimension of full level arrays
!   PDELP  - pressure thickness of layers
!   PEOPT  - gaseous layer optical depth
!   PEO1TI - alpha1 for cloud ice
!   PEO2TI - alpha2 for cloud ice
!   PEO1TN - alpha1 for cloud liquid
!   PEO2TN - alpha2 for cloud liquid
!   PQICE  - specific mass of cloud ice INSIDE CLOUD
!   PQLI   - specific mass of cloud liquid INSIDE CLOUD
!   PEO1TA - alpha1 x optical depth for aerosols
!   PEO2TA - alpha2 x optical depth for aerosols
! OUTPUT:
!   PA4C   - clearsky diffuse transmissivity
!   PA5C   - clearsky diffuse reflectivity
!   PA4N   - cloudy diffuse transmissivity
!   PA5N   - cloudy diffuse reflectivity

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
!
!   2013-11, J. Masek
!   Improved numerical safety. Phasing to cy40t1.
! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPRD

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV0
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA

REAL(KIND=JPRB),INTENT(IN)    :: PDELP(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEOPT(KLON,KLEV0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO1TI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO2TI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO1TN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO2TN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQICE(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQLI(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO1TA(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PEO2TA(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA4C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA5C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA4N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PA5N(KLON,KLEV)

#include "abor1.intfb.h"

! local real scalars
REAL(KIND=JPRB) :: ZARGLI,ZEO1,ZEO2,ZEPS,ZRHO,ZTAU,ZTRLI

! local integer scalars
INTEGER(KIND=JPIM) :: JLEV,JLON

!-----------------------------------------------------------------------


! security constants
IF (JPRB == JPRD) THEN
  ZARGLI=-250._JPRB
ELSE
  ZARGLI=-80._JPRB
ENDIF
ZTRLI =EXP(ZARGLI)

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZEO1=PEOPT(JLON,JLEV)+PEO1TA(JLON,JLEV)
    ZEO2=PEO2TA(JLON,JLEV)
    ZEPS=SQRT(ZEO1*ZEO1-ZEO2*ZEO2)
    ZTAU=EXP(MAX(-ZEPS,ZARGLI))
    ZRHO=ZEO2/(ZEO1+ZEPS+ZTRLI)
    PA4C(JLON,JLEV)=(ZTAU*(1._JPRB-ZRHO*ZRHO)+ZTRLI)&
     & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
    PA5C(JLON,JLEV)=ZRHO*(1._JPRB-ZTAU*ZTAU)&
     & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
    ZEO1=ZEO1+PEO1TN(JLON,JLEV)*(PDELP(JLON,JLEV)*PQLI(JLON,JLEV))&
     & +PEO1TI(JLON,JLEV)*(PDELP(JLON,JLEV)*PQICE(JLON,JLEV))
    ZEO2=ZEO2+PEO2TN(JLON,JLEV)*(PDELP(JLON,JLEV)*PQLI(JLON,JLEV))&
     & +PEO2TI(JLON,JLEV)*(PDELP(JLON,JLEV)*PQICE(JLON,JLEV))
    ZEPS=SQRT(ZEO1*ZEO1-ZEO2*ZEO2)
    ZTAU=EXP(MAX(-ZEPS,ZARGLI))
    ZRHO=ZEO2/(ZEO1+ZEPS+ZTRLI)
    PA4N(JLON,JLEV)=(ZTAU*(1._JPRB-ZRHO*ZRHO)+ZTRLI)&
     & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
    PA5N(JLON,JLEV)=ZRHO*(1._JPRB-(ZTAU*ZTAU))&
     & /(1._JPRB-(ZRHO*ZRHO)*(ZTAU*ZTAU)+ZTRLI)
  ENDDO
ENDDO

END SUBROUTINE ACRANEB_COEFT

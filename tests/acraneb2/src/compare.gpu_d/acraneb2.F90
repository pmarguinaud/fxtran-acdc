!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB2( &
! - INPUT
 & YDERDI,YDRIP,YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,KJN,KSTEP, &
! - INPUT 2D
 & PAPRS,PAPRSF,PCP,PR,PDELP,PNEB,PQ,PQCO2,PQICE,PQLI,PQO3,PT, &
! - INPUT 1D
 & PALB,PALBDIR,PEMIS,PGELAM,PGEMU,PMU0,PMU0LU,PTS,PDECRD,PCLCT, &
! - INPUT/OUTPUT
 & PGDEOSI,PGUEOSI,PGMU0,PGMU0_MIN,PGMU0_MAX, &
 & PGDEOTI,PGDEOTI2,PGUEOTI,PGUEOTI2,PGEOLT,PGEOXT, &
 & PGRPROX,PGMIXP,PGFLUXC,PGRSURF,PSDUR, &
! - OUTPUT 2D
 & PFRSO,PFRTH, &
! - OUTPUT 1D
 & PFRSODS,PFRSOPS,PFRSOLU,PFRTHDS, &
! - INPUT 2D x 6
 & PDAER, YDSTACK)
!$acc routine (ACRANEB2) seq

! Purpose:
! --------
!   ACRANEB2 - Computes radiative fluxes and associated surface diagnostics.
!   Upgraded and modularized version of original ACRANEB routine.

! Interface:
! ----------
! INPUT:
!   KIDIA     - initial index for horizontal loops
!   KFDIA     - final index for horizontal loops
!   KLON      - horizontal dimension of arrays
!   KTDIA     - initial index for vertical loops (usually 1)
!   KLEV      - vertical dimension of full level arrays
!   KJN       - maximum number of day/night intervals within NPROMA slice
!   KSTEP     - current timestep
!   PAPRS     - half level pressure
!   PAPRSF    - full level pressure
!   PCP       - specific heat of moist air at constant pressure
!   PR        - gas constant of moist air
!   PDELP     - pressure thickness of the layer
!   PNEB      - cloud fraction
!   PQ        - specific humidity
!   PQCO2     - specific mass of CO2 with respect to dry air
!   PQICE     - specific mass of cloud ice
!   PQLI      - specific mass of cloud water
!   PQO3      - specific mass of ozone with respect to dry air
!               (element 0 contains ozone column above model top)
!   PT        - temperature
!   PALB      - diffuse surface albedo
!   PALBDIR   - direct (parallel) surface albedo
!   PEMIS     - surface emissivity
!   PGELAM    - longitude
!   PGEMU     - sine of latitude
!   PMU0      - cosine of solar zenithal angle (instantaneous value)
!   PMU0LU    - cosine of lunar zenithal angle (instantaneous value)
!   PTS       - surface temperature
!   PDECRD    - decorrelation depth for cloud overlaps [Pa]
!   PCLCT     - total cloud cover
! INPUT/OUTPUT (for solar/thermal intermittency):
!   PGDEOSI   - min/max descending incremental optical depths, solar
!   PGUEOSI   - min/max ascending incremental optical depths, solar
!   PGMU0     - cosine of solar zenith angle, approximate actual value
!   PGMU0_MIN - cosine of solar zenith angle, min value
!   PGMU0_MAX - cosine of solar zenith angle, max value
!   PGDEOTI   - descending incremental optical depths, dB/dT(T0) weights
!   PGDEOTI2  - descending incremental optical depths, B weights with
!               linear T_e correction
!   PGUEOTI   - ascending incremental optical depths, dB/dT(T0) weights
!   PGUEOTI2  - ascending incremental optical depths, B weights with
!               linear T_e correction
!   PGEOLT    - local optical depths, dB/dT(T0) weights
!   PGEOXT    - maximum optical depths for EBL-EAL, dB/dT(T0) weights
!   PGRPROX   - correction term for adjacent exchanges
!   PGMIXP    - non-statistical weights for bracketing
!   PGFLUXC   - out of bracket part of EBL, RESP. EBL-EAL flux
!   PGRSURF   - corrective ratio for surface CTS contribution
!   PSDUR     - sunshine duration in seconds
! OUTPUT:
!   PFRSO     - net solar   flux (positive downwards)
!   PFRTH     - net thermal flux (positive downwards)
!   PFRSODS   - downward diffuse           solar flux at surface
!   PFRSOPS   - downward direct (parallel) solar flux at surface
!   PFRSOLU   - downward direct + diffuse  lunar flux at surface
!   PFRTHDS   - downward diffuse         thermal flux at surface
! INPUT again:
!   PDAER     - layer optical depths of 6 aerosol types
 
! Externals:
! ----------
!   AC_CLOUD_MODEL2
!   ACRANEB_COEFS
!   ACRANEB_COEFT
!   ACRANEB_SOLVS
!   ACRANEB_SOLVT
!   ACRANEB_SOLVT3
!   ACRANEB_TRANSS
!   ACRANEB_TRANST

! Method:
! -------

! Reference:
! ----------

! Author:
! -------
!   1989-12, J.-F. Geleyn (original ACRANEB)

! Modifications:
! --------------
!   2009-10, T. Kral      Removed obsolete keys LREWS, LRSTAB.
!                         Removed obsolete solar eclipse computation.
!                         Externalized code of gaseous transmissions
!                         computation, preparation of adding system
!                         coefficients and adding system solvers.
!   2011-06, R. Brozkova  Intermittency for thermal gaseous transmissions.
!   2013-11, J. Masek     New gaseous transmissions and statistical model,
!                         intermittency for bracketing weights, cleaning.
!   2014-11, J. Masek     Solar band computation intermittency.
!   2016-04, J. Masek     True direct solar flux, exponential-random cloud
!                         overlap.
!   2016-09, J. Masek     Revised true direct solar flux, proper calculation
!                         of sunshine duration.
!   2017-09, J. Masek     Optimized SW/LW intermittency in the last timestep,
!                         prevented extrapolation of SW gaseous transmissions.
! End Modifications
!-------------------------------------------------------------------------------

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1 ,ONLY : JPIM     ,JPRB     ,JPRD
USE YOMCST   ,ONLY : RSIGMA   ,RG       ,RPI      ,RDAY

USE YOMRIP   ,ONLY : TRIP
USE YOERDI   ,ONLY : TERDI
USE STACK_MOD
#include "stack.h"

!-------------------------------------------------------------------------------

IMPLICIT NONE

TYPE(TERDI)       ,INTENT(INOUT):: YDERDI
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(INOUT):: YDML_PHY_MF
TYPE(TRIP)        ,INTENT(INOUT):: YDRIP
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KLON 
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN) :: KJN
INTEGER(KIND=JPIM),INTENT(IN) :: KSTEP

REAL(KIND=JPRB),INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PCP(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PR(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PNEB(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQ(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQCO2(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQO3(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PALB(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PALBDIR(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PEMIS(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PGELAM(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PGEMU(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PMU0(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PMU0LU(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PDECRD(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PCLCT(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PGDEOSI(KLON,0:KLEV,2)
REAL(KIND=JPRB),INTENT(INOUT) :: PGUEOSI(KLON,0:KLEV,2)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMU0(KLON,0:YDML_PHY_MF%YRPHY%NSORAYFR-1)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMU0_MIN(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMU0_MAX(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PGDEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGDEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGUEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGUEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGEOLT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGEOXT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGRPROX(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMIXP(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGFLUXC(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGRSURF(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PSDUR(KLON)
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSO(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRTH(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSODS(KLON) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSOPS(KLON) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSOLU(KLON) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRTHDS(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PDAER(KLON,KLEV,6)
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK 

!-------------------------------------------------------------------------------

temp (REAL(KIND=JPRB), ZBB, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPCUN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPNUN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFMN, (KLON,0:KLEV))

! CLOUD GEOMETRY COEFFICIENTS
temp (REAL(KIND=JPRB), ZB1, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZB2, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZB3, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZB4, (KLON,KLEV))

! MATRIX COEFFICIENTS
temp (REAL(KIND=JPRB), ZA1C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA1CUN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA2C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA3C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA4C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA5C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA1N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA1NUN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA2N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA3N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA4N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA5N, (KLON,KLEV))

temp (REAL(KIND=JPRB), ZQLI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZQICE, (KLON,KLEV))    

temp (REAL(KIND=JPRB), ZFRTH, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFRTH, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZGEPC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZLEPC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZXEPC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDA4G, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDA4L, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDA4X, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDAMP, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZTDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTRTH, (KLON,0:KLEV))

temp (REAL(KIND=JPRB), ZMU0I, (KLON))
temp (REAL(KIND=JPRB), ZDM0I, (KLON))
temp (REAL(KIND=JPRB), ZDM0I_MIN, (KLON))
temp (REAL(KIND=JPRB), ZDM0I_MAX, (KLON))
temp (REAL(KIND=JPRB), ZRSURF, (KLON))
REAL(KIND=JPRB) :: ZDRB,ZDRS,ZFLUXE&
  &,ZNMNB,ZNMNH,ZNMXB,ZNMXH&
  &,ZII0,ZMU0,ZMU00,&
  &ZWEIGHT,&
  &ZTRB,ZTRS,ZZTRB,ZZTRS&
  &,ZFRB,ZFRS,ZZFRB,ZZFRS

REAL(KIND=JPRB) :: ZCOLAT,ZCOLON,ZSILON
REAL(KIND=JPRB) :: ZTAUC,ZTAUCUN,ZFRSOPS_TRUE
temp (REAL(KIND=JPRB), ZFRSOPS_C, (KLON))
temp (REAL(KIND=JPRB), ZFRSOPS_CUN, (KLON))
REAL(KIND=JPRB) :: ZFRSOPS_UN

temp (REAL(KIND=JPRB), ZPNER0, (KLON,KLEV,KLEV))
temp (REAL(KIND=JPRB), ZPNER1, (KLON,KLEV,KLEV))
temp (REAL(KIND=JPRB), ZFLUXD, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFLUXL, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFLUXR, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFLUXC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZRPROX, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZMIXP, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTAUD, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZTAUL, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZTAU, (KLON,0:KLEV,0:KLEV))

! CLOUD/AEROSOL OPTICAL COEFFICIENTS
temp (REAL(KIND=JPRB), ZBSFSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZBSFSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZBSFTI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZBSFTN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1TI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1TN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2TI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2TN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOASI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOASN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOATI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOATN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODTI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODTN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSAI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSAN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSBI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSBN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2SN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2SI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1SN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1SI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSNUN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSIDIR, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSADIR, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO3SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO4SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1TA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2TA, (KLON,KLEV))

! GASEOUS OPTICAL DEPTHS
temp (REAL(KIND=JPRB), ZDEOSA, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOSI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOSI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTI2, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTI2, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZEOLT, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOXT, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOTI, (KLON,0:KLEV))

temp (LOGICAL, LLMASKS, (KLON))

! LOCAL INTEGER SCALARS
INTEGER(KIND=JPIM) :: IAUCR,ILEV,ICALS,ICALT,ISTEP
INTEGER(KIND=JPIM) :: JAE,JLEV,JLEV1,JLEV2,JLON,JN,JSTEP

! LOCAL LOGICAL SCALARS
LOGICAL :: LLAUTO,LLFDIA,LLIDIA,LLREWS

! LOCAL REAL SCALARS
REAL(KIND=JPRB) :: ZARGLI,ZTRLI,ZEARRT,ZDEBL,ZW,&
  & ZCLOV,ZCNEB,ZCOVSR,ZCOVSR_NEW,ZSIVSR,ZCODT,ZSIDT,&
  & ZEPS1,ZEPSAL,ZIEART,ZSOLLEV,ZEPSNEB,ZSECUR,ZSIGMA,ZUNSCALE,&
  & ZEFFE2,ZTARE2,ZDEL1,ZDEL2,ZFTPP,ZTARSP,ZUSA,ZZEO2TA,ZZEO2SA

!-------------------------------------------------------------------------------

#include "ac_cloud_model2.intfb.h"
#include "acraneb_transs.intfb.h"
#include "acraneb_transt.intfb.h"
#include "acraneb_coefs.intfb.h"
#include "acraneb_coeft.intfb.h"
#include "acraneb_solvs.intfb.h"
#include "acraneb_solvt.intfb.h"
#include "acraneb_solvt3.intfb.h"
#include "abor1.intfb.h"

YLSTACK = YDSTACK

alloc (ZBB)
alloc (ZFDC)
alloc (ZFDN)
alloc (ZFMC)
alloc (ZFMN)
alloc (ZFPC)
alloc (ZFPCUN)
alloc (ZFPN)
alloc (ZFPNUN)
alloc (ZZFDC)
alloc (ZZFDN)
alloc (ZZFMC)
alloc (ZZFMN)
alloc (ZB1)
alloc (ZB2)
alloc (ZB3)
alloc (ZB4)
alloc (ZA1C)
alloc (ZA1CUN)
alloc (ZA2C)
alloc (ZA3C)
alloc (ZA4C)
alloc (ZA5C)
alloc (ZA1N)
alloc (ZA1NUN)
alloc (ZA2N)
alloc (ZA3N)
alloc (ZA4N)
alloc (ZA5N)
alloc (ZQLI)
alloc (ZQICE)
alloc (ZFRTH)
alloc (ZZFRTH)
alloc (ZZZFDC)
alloc (ZZZFDN)
alloc (ZZZFMC)
alloc (ZZZFMN)
alloc (ZGEPC)
alloc (ZLEPC)
alloc (ZXEPC)
alloc (ZDA4G)
alloc (ZDA4L)
alloc (ZDA4X)
alloc (ZDAMP)
alloc (ZTDC)
alloc (ZTDN)
alloc (ZTMC)
alloc (ZTMN)
alloc (ZZTDC)
alloc (ZZTDN)
alloc (ZZTMC)
alloc (ZZTMN)
alloc (ZZZTDC)
alloc (ZZZTDN)
alloc (ZZZTMC)
alloc (ZZZTMN)
alloc (ZZTRTH)
alloc (ZMU0I)
alloc (ZDM0I)
alloc (ZDM0I_MIN)
alloc (ZDM0I_MAX)
alloc (ZRSURF)
alloc (ZFRSOPS_C)
alloc (ZFRSOPS_CUN)
alloc (ZPNER0)
alloc (ZPNER1)
alloc (ZFLUXD)
alloc (ZFLUXL)
alloc (ZFLUXR)
alloc (ZFLUXC)
alloc (ZRPROX)
alloc (ZMIXP)
alloc (ZTAUD)
alloc (ZTAUL)
alloc (ZTAU)
alloc (ZBSFSI)
alloc (ZBSFSN)
alloc (ZBSFTI)
alloc (ZBSFTN)
alloc (ZEO1TI)
alloc (ZEO1TN)
alloc (ZEO2TI)
alloc (ZEO2TN)
alloc (ZEOASI)
alloc (ZEOASN)
alloc (ZEOATI)
alloc (ZEOATN)
alloc (ZEODSI)
alloc (ZEODSN)
alloc (ZEODTI)
alloc (ZEODTN)
alloc (ZUSAI)
alloc (ZUSAN)
alloc (ZUSBI)
alloc (ZUSBN)
alloc (ZEO2SN)
alloc (ZEO2SI)
alloc (ZEO1SN)
alloc (ZEO1SI)
alloc (ZEOSN)
alloc (ZEOSNUN)
alloc (ZEOSI)
alloc (ZEOSIDIR)
alloc (ZUSN)
alloc (ZUSI)
alloc (ZEOSA)
alloc (ZEOSADIR)
alloc (ZEO1SA)
alloc (ZEO2SA)
alloc (ZEO3SA)
alloc (ZEO4SA)
alloc (ZEO1TA)
alloc (ZEO2TA)
alloc (ZDEOSA)
alloc (ZDEOSI)
alloc (ZUEOSI)
alloc (ZDEOTI)
alloc (ZDEOTI2)
alloc (ZUEOTI)
alloc (ZUEOTI2)
alloc (ZEOLT)
alloc (ZEOXT)
alloc (ZEOTI)
alloc (LLMASKS)



JLON = KIDIA

!-------------------------------------------------------------------------------


!     I - CALCUL DES PARAMETRES DERIVES, CONSTANTES DE SECURITE (POUR
!     L'EPAISSEUR EN PRESSION DE LA COUCHE "AU DESSUS DU SOMMET DU
!     MODELE", L'HUMIDITE SPECIFIQUE ET LA CORRECTION POUR EVITER UNE
!     SOLUTION ANALYTIQUE RESONNANTE ENTRE RAYONNEMENT PARALLELE ET
!     RAYONNEMENT DIFFUS) AINSI QU'ARGUMENT LIMITE POUR EVITER
!     "L'UNDERFLOW" DANS LES CALCULS DE TRANSMISSION.

!     I - COMPUTATION OF DERIVED PARAMETERS, SECURITY CONSTANTS (FOR THE
!     PRESSURE THICKNESS OF THE LAYER "ABOVE THE MODEL'S TOP, THE
!     SPECIFIC HUMIDITY AND THE CORRECTION TO AVOID A RESONNANT
!     ANALYTICAL SOLUTION BETWEEN PARALLEL AND SCATTERED RADIATION) AS
!     WELL AS A LIMIT ARGUMENT TO AVOID "UNDERFLOW" IN THE COMPUTATIONS
!     OF TRANSMISSIVITY.
!-------------------------------------------------------------------------------

! SECURITY CONSTANTS.
ZEPSNEB=1.E-12_JPRB
ZSECUR=4._JPRB*RSIGMA*RG*YDML_PHY_MF%YRPHY2%TSPHY
ZEPS1=1.E-03_JPRB
ZEPSAL=1.E-04_JPRB
IF (JPRB == JPRD) THEN
  ZARGLI=-250._JPRB
ELSE
  ZARGLI=-80._JPRB
ENDIF
ZTRLI=EXP(ZARGLI)

!     II - PRELIMINARY CALCULATIONS.
!-------------------------------------------------------------------------------

! What to compute for gases in solar band:
!   ICALS = 0 => transmissions for current sun elevation
!   ICALS = 1 => transmissions for min/max sun elevation, plus
!                interpolated transmissions between min/max values
!   ICALS = 2 => interpolated transmissions between min/max values only
IF (YDML_PHY_MF%YRPHY%NSORAYFR == 1) THEN
  ICALS=0
ELSEIF (MOD(KSTEP,YDML_PHY_MF%YRPHY%NSORAYFR) == 0) THEN
  IF ( KSTEP == YDRIP%NSTOP ) THEN
    ICALS=0
  ELSE
    ICALS=1
  ENDIF
ELSE
  ICALS=2
ENDIF

! What to update for gases in thermal band:
!   ICALT = 0 => nothing
!   ICALT = 1 => transmissions
!   ICALT = 2 => transmissions and non-statistical weights for bracketing
ICALT=0
IF (MOD(KSTEP,YDML_PHY_MF%YRPHY%NTHRAYFR) == 0) THEN
  ICALT=1
  LLAUTO=.FALSE.
ENDIF
IF (YDML_PHY_MF%YRPHY%NRAUTOEV > 0) THEN
  IF (MOD(KSTEP,YDML_PHY_MF%YRPHY%NTHRAYFR*YDML_PHY_MF%YRPHY%NRAUTOEV) == 0.AND. &
   & (KSTEP < YDRIP%NSTOP.OR.YDRIP%NSTOP == 0)) THEN
    ICALT=2
    LLAUTO=.TRUE.
  ENDIF
ENDIF

!     II.1 - SET QL AND QI DEFINITIONS TO THEIR VALUE INSIDE THE CLOUD.
!     COMPUTE AEROSOL OPTICAL PROPERTIES.
!-------------------------------------------------------------------------------

DO JLEV=KTDIA,KLEV
  
    ZQICE(JLON,JLEV)=PQICE(JLON,JLEV)/MAX(ZEPSNEB,PNEB(JLON,JLEV))
    ZQLI (JLON,JLEV)=PQLI (JLON,JLEV)/MAX(ZEPSNEB,PNEB(JLON,JLEV))
  
ENDDO

! ALPHA1, ALPHA2 FOR AEROSOLS.
! daand: add loops
DO JLEV=KTDIA,KLEV
  
		ZEO2TA(JLON,JLEV)=0._JPRB
		ZEO2SA(JLON,JLEV)=0._JPRB
		ZEO1TA(JLON,JLEV)=0._JPRB
		ZEO1SA(JLON,JLEV)=0._JPRB
		ZEOSA(JLON,JLEV)=0._JPRB
		ZEOSADIR(JLON,JLEV)=0._JPRB
	
ENDDO
DO JAE=1,6
  ZUNSCALE=4._JPRB*YDML_PHY_MF%YRPHY3%USAA(JAE)/(3._JPRB+4._JPRB*YDML_PHY_MF%YRPHY3%USAA(JAE))  ! -g
  ZUNSCALE=1._JPRB/(1._JPRB-ZUNSCALE*ZUNSCALE)            ! 1/(1 - g^2)
  DO JLEV=KTDIA,KLEV
    
      ZZEO2TA=2._JPRB*YDML_PHY_MF%YRPHY3%BSFTA(JAE)*YDML_PHY_MF%YRPHY3%EODTA(JAE)*PDAER(JLON,JLEV,JAE)
      ZZEO2SA=2._JPRB*YDML_PHY_MF%YRPHY3%BSFSA(JAE)*YDML_PHY_MF%YRPHY3%EODSA(JAE)*PDAER(JLON,JLEV,JAE)
      ZEO2TA(JLON,JLEV)=ZEO2TA(JLON,JLEV)+ZZEO2TA
      ZEO2SA(JLON,JLEV)=ZEO2SA(JLON,JLEV)+ZZEO2SA
      ZEO1TA(JLON,JLEV)=ZEO1TA(JLON,JLEV)+ZZEO2TA+2._JPRB*YDML_PHY_MF%YRPHY3%EOATA(JAE)&
       & *PDAER(JLON,JLEV,JAE)  
      ZEO1SA(JLON,JLEV)=ZEO1SA(JLON,JLEV)+ZZEO2SA+2._JPRB*YDML_PHY_MF%YRPHY3%EOASA(JAE)&
       & *PDAER(JLON,JLEV,JAE)
      ZEOSA(JLON,JLEV)=ZEOSA(JLON,JLEV)+(YDML_PHY_MF%YRPHY3%EODSA(JAE)+YDML_PHY_MF%YRPHY3%EOASA(JAE))&
       & *PDAER(JLON,JLEV,JAE)
     ZEOSADIR(JLON,JLEV)=ZEOSADIR(JLON,JLEV)+ &
       & (YDML_PHY_MF%YRPHY3%EODSA(JAE)*ZUNSCALE+YDML_PHY_MF%YRPHY3%EOASA(JAE))*PDAER(JLON,JLEV,JAE)
    
  ENDDO
ENDDO

!     II.2 - CALCULS SOLAIRES ET LUNAIRES.
!     LORSQUE LE SOLEIL EST LEVE, ZMU0 ET ZII0 SONT CEUX DU SOLEIL.
!     SI SEULE LA LUNE EST LEVEE, ILS RECOIVENT CEUX DE LA LUNE.

!     II.2 - SOLAR AND LUNAR COMPUTATIONS.
!     IF SUN IS UP, ZMU0 AND ZII0 ARE RELATIVE TO THE SUN.
!     IF MOON IS UP AND SUN IS DOWN, ZMU0 AND ZII0 ARE RELATIVE TO THE MOON.
!-------------------------------------------------------------------------------

! detemine mu_0 and its min/max values within intermittency window
IF ( ICALS == 1 ) THEN
  
    PGMU0_MIN(JLON)=1._JPRB
    PGMU0_MAX(JLON)=0._JPRB
    ZCOLON=COS(PGELAM(JLON))
    ZSILON=SIN(PGELAM(JLON))
    ZCOLAT=SQRT(1.0_JPRB-PGEMU(JLON)**2)
  
  ZCOVSR=YDRIP%RCOVSR
  ZSIVSR=YDRIP%RSIVSR
  ZCODT =COS(2._JPRB*RPI*YDRIP%TSTEP/RDAY)
  ZSIDT =SIN(2._JPRB*RPI*YDRIP%TSTEP/RDAY)
  DO JSTEP=0,MIN(YDML_PHY_MF%YRPHY%NSORAYFR-1,YDRIP%NSTOP-KSTEP)
    
      ! store mu0 in global storage, so that it is not affected by change
      ! in solar declination in the following model steps
      PGMU0(JLON,JSTEP)=MAX( YDRIP%RSIDEC*PGEMU(JLON)   &
       & -YDRIP%RCODEC*ZCOVSR*ZCOLAT*ZCOLON &
       & +YDRIP%RCODEC*ZSIVSR*ZCOLAT*ZSILON &
       & ,0.0_JPRB)
      PGMU0_MIN(JLON)=MIN(PGMU0_MIN(JLON),PGMU0(JLON,JSTEP))
      PGMU0_MAX(JLON)=MAX(PGMU0_MAX(JLON),PGMU0(JLON,JSTEP))
    
    ZCOVSR_NEW=ZCOVSR*ZCODT-ZSIVSR*ZSIDT
    ZSIVSR    =ZSIVSR*ZCODT+ZCOVSR*ZSIDT
    ZCOVSR    =ZCOVSR_NEW
  ENDDO
ENDIF

IF (YDML_PHY_MF%YRPHY%LRAYLU) THEN

  ! get current solar/lunar mu0 and intensity (lunar when sun is down)
  
    ZSOLLEV=0.5_JPRB*(1._JPRB-SIGN(1._JPRB,0._JPRB-PMU0(JLON)))
    ZMU0=ZSOLLEV*PMU0(JLON)+(1._JPRB-ZSOLLEV)*PMU0LU(JLON)
    ZII0=ZSOLLEV*YDML_PHY_MF%YRPHY3%RII0+(1._JPRB-ZSOLLEV)*YDRIP%RIP0LU
  

ELSEIF ( ICALS == 0 ) THEN

  ! get current solar mu0 and intensity
  
    ZMU0=PMU0(JLON)
    ZII0=YDML_PHY_MF%YRPHY3%RII0
  

ELSE

  ! get precalculated mu0 from global storage and current intensity
  ISTEP=MOD(KSTEP,YDML_PHY_MF%YRPHY%NSORAYFR)
  
    ZMU0=PGMU0(JLON,ISTEP)
    ZII0=YDML_PHY_MF%YRPHY3%RII0
  

ENDIF

!     II.3 - CORRECTION TO ROTUNDITY OF EARTH
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 1D

! ZDM0I     : DEMI DE L'INVERSE DU COSINUS MODIFIE DE L'ANGLE ZENITHAL.
!           : HALF INVERSE OF THE MODIFIED COSINE OF THE ZENITH ANGLE.

! compute 1/(2.mu_0')
ZEARRT=YDML_PHY_MF%YRPHY3%EARRT*(YDML_PHY_MF%YRPHY3%EARRT+2._JPRB)
ZIEART=1._JPRB/YDML_PHY_MF%YRPHY3%EARRT

  ZDM0I(JLON)=0.5_JPRB*(SQRT(ZMU0*ZMU0+ &
   & ZEARRT)-ZMU0)*ZIEART


!     II.4 - CALCUL DES LIMITES JOUR/NUIT.

!     II.4 - COMPUTATION OF DAY/NIGHT LIMITS.
!-------------------------------------------------------------------------------

! compute day/night limits
IF (.NOT.YDML_PHY_MF%YRPHY%LRAYPL.OR.(YDML_PHY_MF%YRPHY%NPHYREP /= 0 .AND. YDML_PHY_MF%YRPHY%NPHYREP /= -4)) THEN
  ! daand: in my config, NPHYREP=1, so this case is always selected!
	
  ! PAS DE CALCUL DE "PLAGES" SOLAIRES.
  ! NO "DAYLIGHT" INTERVALS COMPUTATION.
  
	  LLMASKS(JLON)=.TRUE.
	

ELSE

  IF ( ICALS == 0 ) THEN
    
      ZMU00=ZMU0
    
  ELSE
    
      ZMU00=PGMU0_MAX(JLON)
    
  ENDIF
	
	
	  LLMASKS(JLON) = ZMU00 > 0._JPRB
	

ENDIF

!     III - CALCUL DES EPAISSEURS OPTIQUES GASEUSES. LES CALCULS
!     DESCENDANTS SONT SYMETRIQUES ENTRE SOLAIRE ET THERMIQUE (AU TERME
!     D'ALLONGEMENT PRES) MAIS LES CALCULS MONTANTS (DIFFUS TOUS LES
!     DEUX) SONT POUR UN FLUX SOLAIRE REFLECHI ET UN FLUX THERMIQUE
!     CORRESPONDANT A L'ECHANGE AVEC LA SURFACE.

!     III - COMPUTATION OF GASEOUS OPTICAL DEPTHS. THE DESCENDING
!     CALCULATIONS ARE SYMETRICAL BETWEEN SOLAR AND THERMAL (EXCEPT FOR
!     THE DIFFUSIVITY FACTOR) BUT THE ASCENDING CALCULATIONS (DIFFUSE IN
!     BOTH CASES) ARE FOR A REFLECTED SOLAR FLUX AND FOR A THERMAL FLUX
!     CORRESPONDING TO THE EXCHANGE WITH THE SURFACE.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 2D (1:KLEV)

! ZDEOSI    : EPAISSEUR OPTIQUE "GAZ" DESCENDANTE SOLAIRE.
!           : GASEOUS OPTICAL DEPTH SOLAR DESCENDING.
! ZUEOSI    : EPAISSEUR OPTIQUE "GAZ" MONTANTE SOLAIRE.
!           : GASEOUS OPTICAL DEPTH SOLAR ASCENDING.

!     III.1 - SOLAR GASEOUS OPTICAL DEPTHS
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 1D

! IIDIA     : TABLEAU DES INDICES DE DEBUTS DE "PLAGES" SOLAIRES.
!           : ARRAY OF INDICES OF BEGINNING OF "DAYLIGHT" INTERVALS.
! IFDIA     : TABLEAU DES INDICES DE FINS DE "PLAGES" SOLAIRES.
!           : ARRAY OF INDICES OF END OF "DAYLIGHT" INTERVALS.

! determine min/max 1/(2.mu_0')
IF ( ICALS > 0 ) THEN
	
		!IF ( LLMASKS(JLON) ) THEN
      ZDM0I_MIN(JLON)=0.5_JPRB*(SQRT(PGMU0_MAX(JLON)*PGMU0_MAX(JLON)+ &
       & ZEARRT)-PGMU0_MAX(JLON))*ZIEART-ZEPS1
      ZDM0I_MAX(JLON)=0.5_JPRB*(SQRT(PGMU0_MIN(JLON)*PGMU0_MIN(JLON)+ &
       & ZEARRT)-PGMU0_MIN(JLON))*ZIEART+ZEPS1
    !ENDIF
  
ENDIF

! compute optical depths
IF ( ICALS == 0 ) THEN

  ! compute current optical depths
  CALL ACRANEB_TRANSS(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
   & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,ZDM0I,&
   & ZDEOSI,ZUEOSI, YLSTACK)

ELSEIF ( ICALS == 1 ) THEN

  ! compute log of optical depths for min 1/(2.mu_0') alias max sun elevation
  CALL ACRANEB_TRANSS(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
   & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,ZDM0I_MIN,&
   & ZDEOSI,ZUEOSI, YLSTACK)
  DO JLEV=KTDIA-1,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        PGDEOSI(JLON,JLEV,1)=LOG(MAX(ZDEOSI(JLON,JLEV),ZTRLI))
        PGUEOSI(JLON,JLEV,1)=LOG(MAX(ZUEOSI(JLON,JLEV),ZTRLI))
      !ENDIF
    
  ENDDO

  ! compute log of optical depths for max 1/(2.mu_0') alias min sun elevation
  CALL ACRANEB_TRANSS(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
   & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,ZDM0I_MAX,&
   & ZDEOSI,ZUEOSI, YLSTACK)
  DO JLEV=KTDIA-1,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        PGDEOSI(JLON,JLEV,2)=LOG(MAX(ZDEOSI(JLON,JLEV),ZTRLI))
        PGUEOSI(JLON,JLEV,2)=LOG(MAX(ZUEOSI(JLON,JLEV),ZTRLI))
      !ENDIF
    
  ENDDO

ENDIF

! interpolate between min/max optical depths
IF ( ICALS > 0 ) THEN

  ! precompute interpolation weights
	
		!IF ( LLMASKS(JLON) ) THEN
      ZWEIGHT=LOG(ZDM0I    (JLON)/ZDM0I_MIN(JLON))/ &
       &            LOG(ZDM0I_MAX(JLON)/ZDM0I_MIN(JLON))
    !ENDIF
  

  ! interpolation with respect to 1/(2.mu_0') in log-log scale
  DO JLEV=KTDIA-1,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        ZDEOSI(JLON,JLEV)=EXP(PGDEOSI(JLON,JLEV,1)+ZWEIGHT* &
         & (PGDEOSI(JLON,JLEV,2)-PGDEOSI(JLON,JLEV,1)))
        ZUEOSI(JLON,JLEV)=EXP(PGUEOSI(JLON,JLEV,1)+ZWEIGHT* &
         & (PGUEOSI(JLON,JLEV,2)-PGUEOSI(JLON,JLEV,1)))
      !ENDIF
    
  ENDDO

ENDIF

!     III.1 - THERMAL GASEOUS OPTICAL DEPTHS
!-------------------------------------------------------------------------------

IF ( ICALT > 0 ) THEN

  CALL ACRANEB_TRANST(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,&
   & LLAUTO,PAPRS,PAPRSF,PDELP,PR,PT,PTS,PQ,PQCO2,PQO3,&
   & ZDEOTI,ZDEOTI2,ZUEOTI,ZUEOTI2,&
   & ZEOLT,ZEOXT,ZPNER0,ZPNER1,ZRPROX,ZRSURF, YLSTACK)

  IF ( YDML_PHY_MF%YRPHY%NTHRAYFR /= 1 ) THEN

    ! full timestep within intermittency => store necessary arrays
    
      PGRSURF(JLON)=ZRSURF(JLON)
    
    DO JLEV=KTDIA,KLEV
      
        PGEOXT(JLON,JLEV)=ZEOXT(JLON,JLEV)
      
      IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
        
          PGEOLT(JLON,JLEV)=ZEOLT(JLON,JLEV)
        
      ENDIF
    ENDDO
    DO JLEV=KTDIA-1,KLEV
      
        PGDEOTI (JLON,JLEV)=ZDEOTI (JLON,JLEV)   
        PGDEOTI2(JLON,JLEV)=ZDEOTI2(JLON,JLEV)   
        PGUEOTI (JLON,JLEV)=ZUEOTI (JLON,JLEV)   
        PGUEOTI2(JLON,JLEV)=ZUEOTI2(JLON,JLEV)   
      
      IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
        
          PGRPROX(JLON,JLEV)=ZRPROX(JLON,JLEV)
        
      ENDIF
    ENDDO

  ENDIF

ELSE

  ! partial timestep within intermittency => read necessary arrays
  
    ZRSURF(JLON)=PGRSURF(JLON)
  
  DO JLEV=KTDIA,KLEV
    
      ZEOXT(JLON,JLEV)=PGEOXT(JLON,JLEV)
    
    IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
      
        ZEOLT(JLON,JLEV)=PGEOLT(JLON,JLEV)
      
    ENDIF
  ENDDO
  DO JLEV=KTDIA-1,KLEV
    
      ZDEOTI (JLON,JLEV)=PGDEOTI (JLON,JLEV)
      ZDEOTI2(JLON,JLEV)=PGDEOTI2(JLON,JLEV)
      ZUEOTI (JLON,JLEV)=PGUEOTI (JLON,JLEV)
      ZUEOTI2(JLON,JLEV)=PGUEOTI2(JLON,JLEV)
    
    IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
      
        ZRPROX (JLON,JLEV)=PGRPROX (JLON,JLEV)
      
    ENDIF
  ENDDO

ENDIF

! compute minimum optical depth
DO JLEV=KTDIA-1,KLEV
  
    ZEOTI(JLON,JLEV)=MIN(ZUEOTI(JLON,JLEV),ZDEOTI(JLON,JLEV))
  
ENDDO

! compute total solar descending optical depths

	!IF ( LLMASKS(JLON) ) THEN
    ZDEOSA(JLON,KTDIA-1)=ZDM0I(JLON)*ZDEOSI(JLON,KTDIA-1)
  !ENDIF

DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZDEOSA(JLON,JLEV)=ZDEOSA(JLON,JLEV-1)+ZDM0I(JLON)*ZDEOSI(JLON,JLEV)
    !ENDIF
  
ENDDO

! call cloud model to compute saturated cloud optical properties
CALL AC_CLOUD_MODEL2( YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
 & KIDIA    ,KFDIA    ,KLON     ,KTDIA    ,KLEV     ,&
 & LLMASKS    ,PDELP    ,&
 & PNEB     ,ZQICE    ,ZQLI     ,PR       ,PAPRSF   ,&
 & PT       ,ZDEOSA   ,ZBSFSI   ,ZBSFSN   ,ZBSFTI   ,&
 & ZBSFTN   ,ZEOASI   ,ZEOASN   ,ZEOATI   ,ZEOATN   ,&
 & ZEODSI   ,ZEODSN   ,ZEODTI   ,ZEODTN   ,ZEOSIDIR ,&
 & ZEOSNUN  ,ZUSAI    ,ZUSAN    ,ZUSBI    ,ZUSBN, YLSTACK     &
 & )

! ALPHA1, ALPHA2 FOR ICE/LIQUID (THERMAL BAND).
DO JLEV=KTDIA,KLEV
  
    ZEO2TI(JLON,JLEV)=2._JPRB*ZBSFTI(JLON,JLEV)*ZEODTI(JLON,JLEV)
    ZEO2TN(JLON,JLEV)=2._JPRB*ZBSFTN(JLON,JLEV)*ZEODTN(JLON,JLEV)
    ZEO1TI(JLON,JLEV)=ZEO2TI(JLON,JLEV)+2._JPRB*ZEOATI(JLON,JLEV)
    ZEO1TN(JLON,JLEV)=ZEO2TN(JLON,JLEV)+2._JPRB*ZEOATN(JLON,JLEV)
  
ENDDO

!     IV - CALCUL DES ELEMENTS DE GEOMETRIE NUAGEUSE. LES TERMES ZB
!     SERVENT DE STOCKAGE TEMPORAIRE POUR LES "ALPHA", "BETA", "GAMMA"
!     ET "DELTA" POUR LES CALCULS DE RECOUVREMENT MAXIMUM OU (POUR ZB1
!     SEUL) POUR LE COMPLEMENT A UN DE LA NEBULOSITE DANS LE CAS DE
!     RECOUVREMENT AU HASARD.

!     IV - COMPUTATION OF THE CLOUD GEOMETRY ELEMENTS. THE TERMS ZB ARE
!     USED AS TEMPORARY STORAGE FOR THE "ALPHA", "BETA", "GAMMA" AND
!     "DELTA" COEFFICIENTS OF THE MAXIMUM OVERLAP CALCULATIONS OR (IN
!     THAT CASE ZB1 ALONE) FOR THE COMPLEMENT TO ONE OF CLOUDINESS IN
!     THE RANDOM OVERLAP CALCULATIONS.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 2D (1:KLEV)

! ZB1       : PREMIER STOCKAGE INTERMEDIAIRE DE GEOMETRIE NUAGEUSE.
!           : FIRST INTERMEDIATE STORAGE FOR CLOUD GEOMETRY.
! ZB2       : SECOND STOCKAGE INTERMEDIAIRE DE GEOMETRIE NUAGEUSE.
!           : SECOND INTERMEDIATE STORAGE FOR CLOUD GEOMETRY.
! ZB3       : TROISIEME STOCKAGE INTERMEDIAIRE DE GEOMETRIE NUAGEUSE.
!           : THIRD INTERMEDIATE STORAGE FOR CLOUD GEOMETRY.
! ZB4       : QUATRIEME STOCKAGE INTERMEDIAIRE DE GEOMETRIE NUAGEUSE.
!           : FOURTH INTERMEDIATE STORAGE FOR CLOUD GEOMETRY.

!     CALCUL OPTIONNEL (EN VERSION RECOUVREMENT MAXIMUM DES PARTIES
!     NUAGEUSES ADJACENTES) DES COEFFICIENTS MATRICIELS DE TRANSFERT
!     ENTRE PARTIES CLAIRES ET COUVERTES DE DEUX COUCHES. DANS LE CAS
!     CONTRAIRE (RECOUVREMENT ALEATOIRE DE TOUTES LES FRACTIONS
!     NUAGEUSES) ON NE CALCULE QUE LE COMPLEMENT A UN DE LA NEBULOSITE.

!     OPTIONAL CALCULATION (IN MAXIMUM OVERLAP VERSION FOR THE ADJACENT
!     CLOUDY PARTS) OF THE MATRIX COEFFICIENT FOR TRANSFER BETWEEN CLEAR
!     AND CLOUDY PARTS OF BOTH LAYERS. IN THE OPPOSITE CASE (RANDOM
!     OVERLAP OF ALL CLOUD FRACTIONS) ONE COMPUTES ONLY THE COMPLEMENT
!     TO ONE OF CLOUDINESS.

! - TEMPORAIRE(S) 1D

! ZNMXB     : NEBULOSITE MAXIMUM ADJACENTE "VERS LE BAS".
!           : MAXIMUM ADJACENT CLOUDINESS "DOWNWARDS".
! ZNMNB     : NEBULOSITE MINIMUM ADJACENTE "VERS LE BAS".
!           : MINIMUM ADJACENT CLOUDINESS "DOWNWARDS".
! ZNMXH     : NEBULOSITE MAXIMUM ADJACENTE "VERS LE HAUT".
!           : MAXIMUM ADJACENT CLOUDINESS "UPWARDS".
! ZNMNH     : NEBULOSITE MINIMUM ADJACENTE "VERS LE HAUT".
!           : MINIMUM ADJACENT CLOUDINESS "UPWARDS".

IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN

  ! MAXIMUM-RANDOM OVERLAPS

  
    ZB1(JLON,KTDIA)=1._JPRB-PNEB(JLON,KTDIA)
    ZB3(JLON,KTDIA)=1._JPRB
    ZNMXB=MAX(PNEB(JLON,KTDIA),PNEB(JLON,KTDIA+1))
    ZNMNB=MIN(PNEB(JLON,KTDIA),PNEB(JLON,KTDIA+1))
    ZB1(JLON,KTDIA+1)=(1._JPRB-ZNMXB)/(1._JPRB-PNEB(JLON,KTDIA))
    ZB3(JLON,KTDIA+1)=ZNMNB/PNEB(JLON,KTDIA)
  

!cdir unroll=8
  DO JLEV=KTDIA+1,KLEV-1
    
      ZNMXH=ZNMXB
      ZNMNH=ZNMNB
    
    
      ZNMXB=MAX(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1))
      ZNMNB=MIN(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1))
      ZB1(JLON,JLEV+1)=(1._JPRB-ZNMXB)*(1._JPRB/(1._JPRB-PNEB(JLON,JLEV)))
      ZB2(JLON,JLEV-1)=(1._JPRB-ZNMXH)*(1._JPRB/(1._JPRB-PNEB(JLON,JLEV)))
      ZB3(JLON,JLEV+1)=ZNMNB*(1._JPRB/PNEB(JLON,JLEV))
      ZB4(JLON,JLEV-1)=ZNMNH*(1._JPRB/PNEB(JLON,JLEV))
    
  ENDDO

  
    ZNMXH=ZNMXB
    ZNMNH=ZNMNB
    ZB2(JLON,KLEV-1)=(1._JPRB-ZNMXH)/(1._JPRB-PNEB(JLON,KLEV))
    ZB4(JLON,KLEV-1)=ZNMNH/PNEB(JLON,KLEV)
    ZB2(JLON,KLEV)=1._JPRB
    ZB4(JLON,KLEV)=1._JPRB
  

  ! relax maximum-random overlaps towards random overlaps according to
  ! Shonk decorrelation length
  IF (YDML_PHY_MF%YRPHY%LRNUEXP) THEN
    DO JLEV=KTDIA+1,KLEV
      
        ZCLOV=EXP((PAPRSF(JLON,JLEV-1)-PAPRSF(JLON,JLEV))/PDECRD(JLON))
        ZCNEB=1._JPRB-PNEB(JLON,JLEV)
        ZB1(JLON,JLEV)=ZCNEB          +ZCLOV*(ZB1(JLON,JLEV)-ZCNEB          )
        ZB3(JLON,JLEV)=PNEB(JLON,JLEV)+ZCLOV*(ZB3(JLON,JLEV)-PNEB(JLON,JLEV))
      
    ENDDO
    DO JLEV=KTDIA,KLEV-1
      
        ZCLOV=EXP((PAPRSF(JLON,JLEV)-PAPRSF(JLON,JLEV+1))/PDECRD(JLON))
        ZCNEB=1._JPRB-PNEB(JLON,JLEV)
        ZB2(JLON,JLEV)=ZCNEB          +ZCLOV*(ZB2(JLON,JLEV)-ZCNEB          )
        ZB4(JLON,JLEV)=PNEB(JLON,JLEV)+ZCLOV*(ZB4(JLON,JLEV)-PNEB(JLON,JLEV))
      
    ENDDO
  ENDIF

ELSE

  ! RANDOM OVERLAPS

  DO JLEV=KTDIA,KLEV
    
      ZB1(JLON,JLEV)=1._JPRB-PNEB(JLON,JLEV)
    
  ENDDO

ENDIF ! LRNUMX

!     V - NER PREPARATIONS.
!-------------------------------------------------------------------------------

!     V.1 - CALCUL DES FLUX DE CORPS NOIR AU SOMMET DES COUCHES SUPPOSEES
!     ISOTHERMES ET DE L'EMISSIVITE EFFECTIVE DE SURFACE. LES FLUX DE
!     CORPS NOIR SONT DANS UN TABLEAU (0:KLEV) CAR LE SOL EST CONSIDERE
!     COMME UNE COUCHE D'EPAISSEUR OPTIQUE INFINIE.

!     V.1 - COMPUTATION OF THE BLACK-BODY FLUXES AT THE TOP OF LAYERS
!     (ASSUMED ISOTHERMAL) AND OF THE EFFECTIVE SURFACE EMISSIVITY.
!     BLACK-BODY FLUXES ARE IN A (0:KLEV) ARRAY SINCE THE SOIL IS
!     CONSIDERED TO BE A LAYER OF INFINITE OPTICAL DEPTH.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 2D (0:KLEV)

! ZBB       : FLUX DE CORPS NOIR AU SOMMET DES COUCHES.
!           : BLACK-BODY FLUX AT THE TOP OF THE LAYERS.

!     FLUX DE CORPS NOIR.
!     BLACK-BODY FLUX.

DO JLEV=KTDIA,KLEV
  
    ZBB(JLON,JLEV-1)=RSIGMA*(PT(JLON,JLEV)*PT(JLON,JLEV))&
     & *(PT(JLON,JLEV)*PT(JLON,JLEV))

!     ATMOSPHERIC-ONLY DAMPING COEFFICIENTS FOR THE TIME-SECURE HANDLING
!     OF FLUX DIVERGENCES.

    ZDAMP(JLON,JLEV)=ZSECUR*PT(JLON,JLEV)*(PT(JLON,JLEV)&
     & *PT(JLON,JLEV))/(PDELP(JLON,JLEV)*PCP(JLON,JLEV))
  
ENDDO


  ZBB(JLON,KLEV)=RSIGMA*(PTS(JLON)*PTS(JLON))*(PTS(JLON)*PTS(JLON))


!     V.2 - COMPUTATION OF EBL FLUXES AND BRACKETING WEIGHTS.
!-------------------------------------------------------------------------------

! computation of clearsky EBL_min/max fluxes for bracketing
IF ( (YDML_PHY_MF%YRPHY%NRAUTOEV == 0.AND.ICALT == 1).OR.ICALT == 2 ) THEN

  ! distant and local layer transmissions
  DO JLEV=KTDIA,KLEV
    
      ZTAUD(JLON,JLEV)=EXP(MAX(-ZEOTI(JLON,JLEV),ZARGLI))
      ZTAUL(JLON,JLEV)=EXP(MAX(-ZEOXT(JLON,JLEV),ZARGLI))
    
  ENDDO

  ! distant 'grey' transmissions for each pair of levels (multiplicative)
  DO JLEV1=KTDIA-1,KLEV      ! initial half level
    
      ZTAU(JLON,JLEV1,JLEV1)=1._JPRB
    
    DO JLEV2=JLEV1+1,KLEV    ! final half level
      
        ZTAU(JLON,JLEV1,JLEV2)=ZTAU(JLON,JLEV1,JLEV2-1)*ZTAUD(JLON,JLEV2)
      
    ENDDO
  ENDDO

  ! EBL flux for 'grey' minimum optical depths
  ! (zfluxd positive downwards, D - distant)
  ! daand: add explicit loops
	DO JLEV=0,KLEV
	  
		  ZFLUXD(JLON,JLEV)=0._JPRB
		
	ENDDO
  DO JLEV1=KTDIA,KLEV        ! exchanging layer 1
    IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
      ILEV=JLEV1+2           ! exclude exchange between adjacent layers
    ELSE
      ILEV=JLEV1+1           ! include exchange between adjacent layers
    ENDIF
    DO JLEV2=ILEV,KLEV       ! exchanging layer 2
      
        ZFLUXE=(ZBB(JLON,JLEV2-1)-ZBB(JLON,JLEV1-1))*      &
         & (ZTAU(JLON,JLEV1,JLEV2  )-ZTAU(JLON,JLEV1-1,JLEV2  )- &
         &  ZTAU(JLON,JLEV1,JLEV2-1)+ZTAU(JLON,JLEV1-1,JLEV2-1))
      
      DO JLEV=JLEV1,JLEV2-1  ! half levels between layers 1 and 2
        
          ZFLUXD(JLON,JLEV)=ZFLUXD(JLON,JLEV)+ZFLUXE
        
      ENDDO
    ENDDO
  ENDDO

  ! local 'grey' transmissions for each pair of levels (multiplicative)
  DO JLEV1=KTDIA-1,KLEV      ! initial half level
    
      ZTAU(JLON,JLEV1,JLEV1)=1._JPRB
    
    DO JLEV2=JLEV1+1,KLEV    ! final half level
      
        ZTAU(JLON,JLEV1,JLEV2)=ZTAU(JLON,JLEV1,JLEV2-1)*ZTAUL(JLON,JLEV2)
      
    ENDDO
  ENDDO
 
  ! EBL flux for 'grey' maximum optical depths
  ! (ZFLUXL positive downwards, L - local)
  ! daand: add explicit loops
	DO JLEV=0,KLEV
	  
		  ZFLUXL(JLON,JLEV)=0._JPRB
		
	ENDDO
  DO JLEV1=KTDIA,KLEV        ! exchanging layer 1
    IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
      ILEV=JLEV1+2           ! exclude exchange between adjacent layers
    ELSE
      ILEV=JLEV1+1           ! include exchange between adjacent layers
    ENDIF
    DO JLEV2=ILEV,KLEV       ! exchanging layer 2
      
        ZFLUXE=(ZBB(JLON,JLEV2-1)-ZBB(JLON,JLEV1-1))*      &
         & (ZTAU(JLON,JLEV1,JLEV2  )-ZTAU(JLON,JLEV1-1,JLEV2  )- &
         &  ZTAU(JLON,JLEV1,JLEV2-1)+ZTAU(JLON,JLEV1-1,JLEV2-1))
      
      DO JLEV=JLEV1,JLEV2-1  ! half levels between layers 1 and 2
        
          ZFLUXL(JLON,JLEV)=ZFLUXL(JLON,JLEV)+ZFLUXE
        
      ENDDO
    ENDDO
  ENDDO

ENDIF

! computation of statistically fitted / true clearsky EBL flux
IF ( YDML_PHY_MF%YRPHY%NRAUTOEV == 0.AND.ICALT == 1 ) THEN

  ! statistically fitted EBL flux
  
    ZFLUXR(JLON,KTDIA-1)=0._JPRB
    ZFLUXR(JLON,KLEV   )=0._JPRB
  
  DO JLEV=KTDIA,KLEV-1
    
      ZSIGMA=PAPRS(JLON,JLEV)/PAPRS(JLON,KLEV)
      ZFLUXR(JLON,JLEV)= &
       & (YDML_PHY_MF%YRPHY3%FSM_AA(0)+ZSIGMA*(YDML_PHY_MF%YRPHY3%FSM_AA(1)+ZSIGMA*YDML_PHY_MF%YRPHY3%FSM_AA(2)))*ZFLUXD(JLON,JLEV)+ &
       & (YDML_PHY_MF%YRPHY3%FSM_BB(0)+ZSIGMA*(YDML_PHY_MF%YRPHY3%FSM_BB(1)+ZSIGMA*YDML_PHY_MF%YRPHY3%FSM_BB(2)))*ZFLUXL(JLON,JLEV)
    
  ENDDO

ELSEIF ( ICALT == 2 ) THEN

  ! true EBL flux
  ! (ZFLUXR positive downwards, R - real)
  ! daand: add explicit loops
	DO JLEV=0,KLEV
	  
		  ZFLUXR(JLON,JLEV)=0._JPRB
		
	ENDDO
  DO JLEV1=KTDIA,KLEV        ! exchanging layer 1
    IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
      ILEV=JLEV1+2           ! exclude exchange between adjacent layers
    ELSE
      ILEV=JLEV1+1           ! include exchange between adjacent layers
    ENDIF
    DO JLEV2=ILEV,KLEV       ! exchanging layer 2
      
        ZFLUXE=                                                      &
         & (ZBB(JLON,JLEV2-1)-ZBB(JLON,JLEV1-1))*ZPNER0(JLON,JLEV1,JLEV2)+ &
         & 4._JPRB*(PT(JLON,JLEV2)/YDML_PHY_MF%YRPHY3%RTL-1._JPRB)*ZBB(JLON,JLEV2-1)*         &
         & (ZPNER1(JLON,JLEV1,JLEV2)-ZPNER0(JLON,JLEV1,JLEV2))-            &
         & 4._JPRB*(PT(JLON,JLEV1)/YDML_PHY_MF%YRPHY3%RTL-1._JPRB)*ZBB(JLON,JLEV1-1)*         &
         & (ZPNER1(JLON,JLEV1,JLEV2)-ZPNER0(JLON,JLEV1,JLEV2))
      
      DO JLEV=JLEV1,JLEV2-1  ! half levels between layers 1 and 2
        
          ZFLUXR(JLON,JLEV)=ZFLUXR(JLON,JLEV)+ZFLUXE
        
      ENDDO
    ENDDO
  ENDDO

ENDIF

! computation of bracketing weights (with smoothing) and offset
IF ( (YDML_PHY_MF%YRPHY%NRAUTOEV == 0.AND.ICALT == 1).OR.ICALT == 2 ) THEN

  DO JLEV=KTDIA-1,KLEV
    
      ZDEBL=ZFLUXL(JLON,JLEV)-ZFLUXD(JLON,JLEV)
      ZW   =YDML_PHY_MF%YRPHY3%RMIXD*YDML_PHY_MF%YRPHY3%RMIXD/(YDML_PHY_MF%YRPHY3%RMIXD*YDML_PHY_MF%YRPHY3%RMIXD+ZDEBL*ZDEBL)
      ZMIXP(JLON,JLEV)=MAX(0._JPRB,MIN(1._JPRB,                    &
       & ((1._JPRB-ZW)*ZFLUXR(JLON,JLEV)+                          &
       & ZW*(ZFLUXD(JLON,JLEV)+YDML_PHY_MF%YRPHY3%RMIXP0*(ZFLUXL(JLON,JLEV)-          &
       & ZFLUXD(JLON,JLEV)))-ZFLUXD(JLON,JLEV))/                   &
       & SIGN(MAX(ABS(ZFLUXL(JLON,JLEV)-ZFLUXD(JLON,JLEV)),ZEPS1), &
       & ZFLUXL(JLON,JLEV)-ZFLUXD(JLON,JLEV))))
      ZFLUXC(JLON,JLEV)=ZFLUXR(JLON,JLEV)-(ZFLUXD(JLON,JLEV)+ &
       & ZMIXP(JLON,JLEV)*(ZFLUXL(JLON,JLEV)-ZFLUXD(JLON,JLEV)))
    
  ENDDO

ENDIF

! check if intermittency for bracketing weights is on
IF (YDML_PHY_MF%YRPHY%NTHRAYFR /= 1.OR.YDML_PHY_MF%YRPHY%NRAUTOEV > 1) THEN

  ! store/read bracketing weights
  IF ( (YDML_PHY_MF%YRPHY%NRAUTOEV == 0.AND.ICALT == 1).OR.ICALT == 2 ) THEN

    ! timestep with update of bracketing weights => store PGMIXP, PGFLUXC
    DO JLEV=KTDIA-1,KLEV
      
        PGMIXP (JLON,JLEV)=ZMIXP (JLON,JLEV)
        PGFLUXC(JLON,JLEV)=ZFLUXC(JLON,JLEV)
      
    ENDDO

  ELSE

    ! timestep without update of bracketing weights => read PGMIXP, PGFLUXC
    DO JLEV=KTDIA-1,KLEV
      
        ZMIXP (JLON,JLEV)=PGMIXP (JLON,JLEV)
        ZFLUXC(JLON,JLEV)=PGFLUXC(JLON,JLEV)
      
    ENDDO

  ENDIF

ENDIF

!     VI-VIII - CALCUL DES COMPLEMENTS A UN DES FLUX THERMIQUES EN ATMOSPHERE
!     ISOTHERME NORMALISEE PAR ELIMINATION/SUBSTITUTION PUIS UTILISATION
!     DES RESULTATS POUR LES CALCULS DE FLUX. DANS TOUTE LA SUITE LES
!     QUANTITES ZF (OU ZZF DANS LE CAS THERMIQUE AVEC TEMPERATURES
!     REELLES) DESIGNERONT LES SECONDS MEMBRES DES SYSTEMES LINEAIRES
!     "ADDING METHOD" ET LES QUANTITES ZTU LES ELEMENTS SURDIAGONAUX A
!     UTILISER DANS LA PARTIE SUBSTITUTION DE LA SOLUTION. IL FAUT
!     EGALEMENT SIGNALER QUE DANS LE CAS "RECOUVREMENT AU HASARD" LE
!     SUFFIXE "C" (CLAIR) REPRESENTE TOUTE LA COUCHE ET QUE LE SUFFIXE
!     "N" (NUAGEUX) REPRESENTE DES TABLEAUX INUTILISES.

!     VI-VIII - COMPUTATION OF THE COMPLEMENTS TO ONE OF THE THERMAL FLUXES
!     IN A NORMALIZED ISOTHERMAL ATMOSPHERE VIA ELIMINATION/BACK-
!     SUBSTITUTION AND USE OF THE RESULT TO COMPUTE FLUXES. IN ALL THE
!     FOLLOWING THE QUANTITIES ZF (OR ZZF IN THE THERMAL CASE WITH
!     REAL TEMPERATURES) WILL INDICATE THE RIGHT HAND SIDES OF THE
!     "ADDING METHOD" LINEAR SYSTEMS AND THE QUANTITIES ZTU THE
!     SUPERDIAGONAL ELEMENTS TO BE USED IN THE BACK-SUBSTITUTION PART
!     OF THE ALGORITHM. ONE MUST ALSO MENTION THAT IN THE "RANDOM
!     OVERLAP" CASE THE SUFFIX "C" (CLEAR) REPRESENTS THE WHOLE LAYER
!     AND THAT THE SUFFIX "N" (NEBULOUS) REPRESENTS UNUSED ARRAYS.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 2D (0:KLEV)

! ZFPC      : TERME "MEMBRE DE DROITE" POUR FLUX PARALLELE CLAIR.
!           : RIGHT HAND SIDE TERM FOR CLEAR-SKY PARALLEL FLUX.
! ZFPN      : TERME "MEMBRE DE DROITE" POUR FLUX PARALLELE NUAGEUX.
!           : RIGHT HAND SIDE TERM FOR CLOUDY PARALLEL FLUX.
! ZFDC      : TERME "MEMBRE DE DROITE" POUR FLUX DIFFUS "DOWN" CLAIR.
!           : RIGHT HAND SIDE TERM FOR CLEAR-SKY DOWNWARDS DIFF. FLUX.
! ZFDN      : TERME "MEMBRE DE DROITE" POUR FLUX DIFFUS "DOWN" NUAGEUX.
!           : RIGHT HAND SIDE TERM FOR CLOUDY DOWNWARDS DIFF. FLUX.
! ZFMC      : TERME "MEMBRE DE DROITE" POUR FLUX DIFFUS "UP" CLAIR.
!           : RIGHT HAND SIDE TERM FOR CLEAR-SKY UPWARDS DIFF. FLUX.
! ZFMN      : TERME "MEMBRE DE DROITE" POUR FLUX DIFFUS "UP" NUAGEUX.
!           : RIGHT HAND SIDE TERM FOR CLOUDY UPWARDS DIFF. FLUX.
! ZZFDC     : AUTRE TERME "M.D.D." POUR FLUX DIFFUS "DOWN" CLAIR.
!           : OTHER "R.H.S." TERM FOR CLEAR-SKY DOWNWARDS DIFF. FLUX.
! ZZFDN     : AUTRE TERME "M.D.D." POUR FLUX DIFFUS "DOWN" NUAGEUX.
!           : OTHER "R.H.S." TERM FOR CLOUDY DOWNWARDS DIFF. FLUX.
! ZZFMC     : AUTRE TERME "M.D.D." POUR FLUX DIFFUS "UP" CLAIR.
!           : OTHER "R.H.S." TERM FOR CLEAR-SKY UPWARDS DIFF. FLUX.
! ZZFMN     : AUTRE TERME "M.D.D." POUR FLUX DIFFUS "UP" NUAGEUX.
!           : OTHER "R.H.S." TERM FOR CLOUDY UPWARDS DIFF. FLUX.

!     VI - CALCUL DES FLUX AVEC L'HYPOTHESE "COOLING TO SPACE".

!     VI - FLUX COMPUTATION IN THE COOLING TO SPACE CASE.
!-------------------------------------------------------------------------------

!     VI.1 - COMPUTE MATRIX COEFICIENTS OF ADDING METHOD LINEAR SYSTEM
!     FOR "DECENDING" GASEOUS OPTICAL DEPTH.
!-------------------------------------------------------------------------------

CALL ACRANEB_COEFT(KIDIA,KFDIA,KLON,KTDIA,0_JPIM,KLEV,&
 & PDELP,ZDEOTI2,ZEO1TI,ZEO2TI,ZEO1TN,ZEO2TN,ZQICE,&
 & ZQLI,ZEO1TA,ZEO2TA,ZA4C,ZA5C,ZA4N,ZA5N, YLSTACK)

!     VI.2 - PREPARE RIGHT-HAND SIDE OF ADDING METHOD LINEAR SYSTEM
!     FOR IDEALIZED PROFILE A.
!-------------------------------------------------------------------------------


  ZFDC(JLON,KTDIA-1)=EXP(MAX(-ZDEOTI2(JLON,KTDIA-1),ZARGLI))
  ZFMC(JLON,KTDIA-1)=0._JPRB
  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZFDN(JLON,KTDIA-1)=0._JPRB
    ZFMN(JLON,KTDIA-1)=0._JPRB
  ENDIF


DO JLEV=KTDIA,KLEV
  
    ZFMC(JLON,JLEV)=0._JPRB
    ZFDC(JLON,JLEV)=0._JPRB
    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZFDN(JLON,JLEV)=0._JPRB
      ZFMN(JLON,JLEV)=0._JPRB
    ENDIF
  
ENDDO

!     VI.3 - SOLVE THE ADDING METHOD LINEAR SYSTEM BY GAUSSIAN
!     ELIMINATION BACK-SUBSTITUTION.
!-------------------------------------------------------------------------------

CALL ACRANEB_SOLVT(YDML_PHY_MF%YRPHY,KIDIA,KFDIA,KLON,KTDIA,KLEV,&
 & PEMIS,PNEB,ZB1,ZB2,ZB3,ZB4,ZA4C,ZA5C,ZA4N,ZA5N,&
 & ZFDC,ZFMC,ZFDN,ZFMN, YLSTACK)

!     VI.4 - COMPUTE THE FINAL FLUXES FOR THE CTS CASE.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 1D

! ZTRS      : FLUX "Z" NET AU SOMMET D'UNE COUCHE.
!           : "Z" NET FLUX AT THE TOP OF A LAYER.
! ZTRB      : FLUX "Z" NET A LA BASE D'UNE COUCHE.
!           : "Z" NET FLUX AT THE BOTTOM OF A LAYER.


  ZTRS=ZFDC(JLON,KLEV)-ZFMC(JLON,KLEV)

  ! CALCULS OPTIONNELS SUIVANT LA GEOMETRIE NUAGEUSE CHOISIE.
  ! OPTIONAL COMPUTATIONS ACCORDING TO THE CHOSEN GEOMETRY.
  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZTRS=ZTRS+ZFDN(JLON,KLEV)-ZFMN(JLON,KLEV)
  ENDIF

  PFRTH(JLON,KLEV)=-ZBB(JLON,KLEV)*MIN(1._JPRB,ZTRS*ZRSURF(JLON))


!cdir unroll=8
DO JLEV=KLEV,KTDIA,-1

  
    ZTRB=ZTRS
  

  
    ZTRS=ZFDC(JLON,JLEV-1)-ZFMC(JLON,JLEV-1)

!     CALCULS OPTIONNELS SUIVANT LA GEOMETRIE NUAGEUSE CHOISIE.
!     OPTIONAL COMPUTATIONS ACCORDING TO THE CHOSEN GEOMETRY.

    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZTRS=ZTRS+ZFDN(JLON,JLEV-1)-ZFMN(JLON,JLEV-1)
    ENDIF

!     FINAL COMPUTATION INCLUDING THE CTS SECURISATION FOR THE TIME-STEP.

    PFRTH(JLON,JLEV-1)=PFRTH(JLON,JLEV)+ZBB(JLON,JLEV-1)&
     & *(ZTRB-ZTRS)/(1._JPRB-(ZTRB-ZTRS)&
     & *ZDAMP(JLON,JLEV))
  

ENDDO

!     VII - CALCUL DES FLUX AVEC L'HYPOTHESE "EXCHANGE WITH SURFACE".

!     VII - FLUX COMPUTATION IN THE EXCHANGE WITH SURFACE (EWS) CASE.
!-------------------------------------------------------------------------------


!     VII.1 - COMPUTE MATRIX COEFICIENTS OF ADDING METHOD LINEAR SYSTEM
!     USING "ASCENDING" GASEOUS OPTICAL DEPTH.
!-------------------------------------------------------------------------------

CALL ACRANEB_COEFT(KIDIA,KFDIA,KLON,KTDIA,0_JPIM,KLEV,&
 & PDELP,ZUEOTI2,ZEO1TI,ZEO2TI,ZEO1TN,ZEO2TN,ZQICE,&
 & ZQLI,ZEO1TA,ZEO2TA,ZA4C,ZA5C,ZA4N,ZA5N, YLSTACK)

!     VII.2 - PREPARE RIGHT-HAND SIDE OF ADDING METHOD LINEAR SYSTEM
!     FOR IDEALIZED PROFILE B.
!-------------------------------------------------------------------------------

IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN

  
    ZFDC(JLON,KTDIA-1)=0._JPRB
    ZFMC(JLON,KTDIA-1)=0._JPRB
    ZFDC(JLON,KTDIA)=0._JPRB
    ZFMC(JLON,KLEV-1)=-ZA4C(JLON,KLEV)
    ZFDC(JLON,KLEV)=-ZA5C(JLON,KLEV)
    ZFMC(JLON,KLEV)=1._JPRB-PEMIS(JLON)

    ZFMC(JLON,KLEV-1)=ZFMC(JLON,KLEV-1)*(1._JPRB-PNEB(JLON,KLEV))
    ZFDC(JLON,KLEV)=ZFDC(JLON,KLEV)*(1._JPRB-PNEB(JLON,KLEV))
    ZFMC(JLON,KLEV)=ZFMC(JLON,KLEV)*(1._JPRB-PNEB(JLON,KLEV))
    ZFDN(JLON,KTDIA-1)=0._JPRB
    ZFMN(JLON,KTDIA-1)=0._JPRB
    ZFDN(JLON,KTDIA)=0._JPRB
    ZFMN(JLON,KLEV-1)=-ZA4N(JLON,KLEV)*PNEB(JLON,KLEV)
    ZFDN(JLON,KLEV)=-ZA5N(JLON,KLEV)*PNEB(JLON,KLEV)
    ZFMN(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*PNEB(JLON,KLEV)
  

  DO JLEV=KTDIA+1,KLEV-1
    
      ZFDC(JLON,JLEV)=0._JPRB
      ZFMC(JLON,JLEV-1)=0._JPRB
      ZFDN(JLON,JLEV)=0._JPRB
      ZFMN(JLON,JLEV-1)=0._JPRB
    
  ENDDO

ELSE

  
    ZFDC(JLON,KTDIA-1)=0._JPRB
    ZFMC(JLON,KTDIA-1)=0._JPRB
    ZFDC(JLON,KTDIA  )=0._JPRB
    ZFMC(JLON,KLEV-1 )=-(ZA4C(JLON,KLEV)*ZB1(JLON,KLEV)+ &
     &                   ZA4N(JLON,KLEV)*PNEB(JLON,KLEV)) 
    ZFDC(JLON,KLEV   )=-(ZA5C(JLON,KLEV)*ZB1(JLON,KLEV)+ &
     &                   ZA5N(JLON,KLEV)*PNEB(JLON,KLEV))
    ZFMC(JLON,KLEV   )=1._JPRB-PEMIS(JLON)
  

  DO JLEV=KTDIA+1,KLEV-1
    
      ZFDC(JLON,JLEV)=0._JPRB
      ZFMC(JLON,JLEV-1)=0._JPRB
    
  ENDDO

ENDIF ! LRNUMX

!     VII.3 - SOLVE THE ADDING METHOD LINEAR SYSTEM BY GAUSSIAN
!     ELIMINATION BACK-SUBSTITUTION.
!-------------------------------------------------------------------------------

CALL ACRANEB_SOLVT(YDML_PHY_MF%YRPHY,KIDIA,KFDIA,KLON,KTDIA,KLEV,&
 & PEMIS,PNEB,ZB1,ZB2,ZB3,ZB4,ZA4C,ZA5C,ZA4N,ZA5N,&
 & ZFDC,ZFMC,ZFDN,ZFMN, YLSTACK)

!     VII.4 - COMPUTE THE FINAL FLUXES FOR EWS CASE.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 1D

! ZTRS      : FLUX "Z" NET AU SOMMET D'UNE COUCHE.
!           : "Z" NET FLUX AT THE TOP OF A LAYER.
! ZTRB      : FLUX "Z" NET A LA BASE D'UNE COUCHE.
!           : "Z" NET FLUX AT THE BOTTOM OF A LAYER.


  ZTRB=ZFDC(JLON,KTDIA-1)-ZFMC(JLON,KTDIA-1)

  ! CALCULS OPTIONNELS SUIVANT LA GEOMETRIE NUAGEUSE CHOISIE.
  ! OPTIONAL COMPUTATIONS ACCORDING TO THE CHOSEN GEOMETRY.
  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZTRB=ZTRB+ZFDN(JLON,KTDIA-1)-ZFMN(JLON,KTDIA-1)
  ENDIF

  ZFRTH(JLON,KTDIA-1)=0._JPRB


!cdir unroll=8
DO JLEV=KTDIA,KLEV
  LLREWS=(JLEV==KLEV)
  
    ZTRS=ZTRB
  
  
    ZTRB=ZFDC(JLON,JLEV)-ZFMC(JLON,JLEV)

    ! CALCULS OPTIONNELS SUIVANT LA GEOMETRIE NUAGEUSE CHOISIE.
    ! OPTIONAL COMPUTATIONS ACCORDING TO THE CHOSEN GEOMETRY.
    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZTRB=ZTRB+ZFDN(JLON,JLEV)-ZFMN(JLON,JLEV)
    ENDIF

    IF (LLREWS) THEN
      ZTRB=ZTRB+1._JPRB
    ENDIF

    ! FINAL COMPUTATION INCLUDING THE EWS SECURISATION FOR THE TIME-STEP.
    ZFRTH(JLON,JLEV)=ZFRTH(JLON,JLEV-1)+(ZBB(JLON,KLEV)&
     & -ZBB(JLON,JLEV-1))*(ZTRS-ZTRB)&
     & /(1._JPRB-(ZTRS-ZTRB)*ZDAMP(JLON,JLEV))
  
ENDDO

!     VIII - CALCUL DES COMPLEMENTS AU FLUX DU CORPS NOIR DES FLUX
!     THERMIQUES 'ANTI-SURESTIMATION' POUR LE PROFIL DE TEMPERATURE REEL
!     ET LE PROFIL ISOTHERME NORMALISE AFIN D'OBTENIR PAR DIFFERENCE LES
!     TERMES D'ECHANGE QUI, AJOUTES AUX RESULTATS "COOLING TO SPACE",
!     DONNERONT LE CHAMP DE RAYONNEMENT THERMIQUE FINAL.

!     VIII - COMPUTATION OF THE COMPLEMENTS TO THE BLACK-BODY FLUXES OF
!     "ANTI-SURESTIMATION" THERMAL FLUXES FOR THE REAL TEMPERATURE
!     PROFILE AND FOR THE NORMALIZED ISOTHERMAL PROFILE IN ORDER TO
!     OBTAIN BY DIFFERENCIATION THE EXCHANGE TERMS THAT, ADDED TO THE
!     COOLING TO SPACE AND EXCHANGE WITH SURFACE RESULTS, WILL GIVE
!     THE FINAL THERMAL RADIATIVE FLUXES' ARRAY.
!-------------------------------------------------------------------------------


!     VIII.1 - COMPUTE LINEAR SYSTEM COEFICIENTS FOR ADDING METHOD.
!-------------------------------------------------------------------------------

CALL ACRANEB_COEFT(KIDIA,KFDIA,KLON,KTDIA,0_JPIM,KLEV,&
 & PDELP,ZEOTI,ZEO1TI,ZEO2TI,ZEO1TN,ZEO2TN,ZQICE,&
 & ZQLI,ZEO1TA,ZEO2TA,ZA4C,ZA5C,ZA4N,ZA5N, YLSTACK)

! GLOBAL STORAGE FOR THE 'PROXIMITY' COMPUTATION.
IF (YDML_PHY_MF%YRPHY%LRPROX) THEN
  DO JLEV=KTDIA,KLEV
    
      ZGEPC(JLON,JLEV)=(1._JPRB-PNEB(JLON,JLEV))*ZA4C(JLON,JLEV)&
       & +PNEB(JLON,JLEV)*ZA4N(JLON,JLEV)
      ZDA4G(JLON,JLEV)=ZA4C(JLON,JLEV)-ZA4N(JLON,JLEV)
    
  ENDDO
ENDIF

!     VIII.2 - PREPARE RIGHT-HAND SIDES OF ADDING METHOD LINEAR SYSTEM
!     FOR NORMALIZED ISOTHERMAL PROFILES A AND B AND THE REAL
!     TEMPERATURE PROFILE.
!-----------------------------------------------------------------------


! RIGHT-HAND SIDE FOR PROFILE A.


  ZTDC(JLON,KTDIA-1)=EXP(MAX(-ZEOTI(JLON,KTDIA-1),ZARGLI))
  ZTMC(JLON,KTDIA-1)=0._JPRB
  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZTDN(JLON,KTDIA-1)=0._JPRB
    ZTMN(JLON,KTDIA-1)=0._JPRB
  ENDIF


DO JLEV=KTDIA,KLEV
  
    ZTMC(JLON,JLEV)=0._JPRB
    ZTDC(JLON,JLEV)=0._JPRB
    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZTDN(JLON,JLEV)=0._JPRB
      ZTMN(JLON,JLEV)=0._JPRB
    ENDIF
  
ENDDO

! RIGHT-HAND SIDE FOR PROFILE B.

IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN

  
    ZZZTDC(JLON,KTDIA-1)=0._JPRB
    ZZZTMC(JLON,KTDIA-1)=0._JPRB
    ZZZTDC(JLON,KTDIA)=0._JPRB
    ZZZTMC(JLON,KLEV-1)=-ZA4C(JLON,KLEV)
    ZZZTDC(JLON,KLEV)=-ZA5C(JLON,KLEV)
    ZZZTMC(JLON,KLEV)=1._JPRB-PEMIS(JLON)
    ZZZTMC(JLON,KLEV-1)=ZZZTMC(JLON,KLEV-1)*(1._JPRB-PNEB(JLON,KLEV))
    ZZZTDC(JLON,KLEV)=ZZZTDC(JLON,KLEV)*(1._JPRB-PNEB(JLON,KLEV))
    ZZZTMC(JLON,KLEV)=ZZZTMC(JLON,KLEV)*(1._JPRB-PNEB(JLON,KLEV))
    ZZZTDN(JLON,KTDIA-1)=0._JPRB
    ZZZTMN(JLON,KTDIA-1)=0._JPRB
    ZZZTDN(JLON,KTDIA)=0._JPRB
    ZZZTMN(JLON,KLEV-1)=-ZA4N(JLON,KLEV)*PNEB(JLON,KLEV)
    ZZZTDN(JLON,KLEV)=-ZA5N(JLON,KLEV)*PNEB(JLON,KLEV)
    ZZZTMN(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*PNEB(JLON,KLEV)
  

  DO JLEV=KTDIA+1,KLEV-1
    
      ZZZTDC(JLON,JLEV)=0._JPRB
      ZZZTMC(JLON,JLEV-1)=0._JPRB
      ZZZTDN(JLON,JLEV)=0._JPRB
      ZZZTMN(JLON,JLEV-1)=0._JPRB
    
  ENDDO

ELSE

  
    ZZZTDC(JLON,KTDIA-1)=0._JPRB
    ZZZTMC(JLON,KTDIA-1)=0._JPRB
    ZZZTDC(JLON,KTDIA)=0._JPRB
    ZZZTMC(JLON,KLEV-1)=-(ZA4C(JLON,KLEV)*ZB1(JLON,KLEV)+ZA4N(JLON,KLEV)&
     & *PNEB(JLON,KLEV)) 
    ZZZTDC(JLON,KLEV)=-(ZA5C(JLON,KLEV)*ZB1(JLON,KLEV)+ZA5N(JLON,KLEV)&
     & *PNEB(JLON,KLEV))
    ZZZTMC(JLON,KLEV)=1._JPRB-PEMIS(JLON)
  

  DO JLEV=KTDIA+1,KLEV-1
    
      ZZZTDC(JLON,JLEV)=0._JPRB
      ZZZTMC(JLON,JLEV-1)=0._JPRB
    
  ENDDO

ENDIF ! LRNUMX

! RIGHT-HAND SIDE FOR A REAL TEMPERATURE PROFILE.

IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN

  
    ZZTDC(JLON,KTDIA-1)=ZBB(JLON,KTDIA-1)*ZTDC(JLON,KTDIA-1)
    ZZTMC(JLON,KTDIA-1)=ZA4C(JLON,KTDIA)*(ZBB(JLON,KTDIA-1)&
     & -ZBB(JLON,KTDIA)) 
    ZZTDC(JLON,KTDIA)=ZA5C(JLON,KTDIA)*(ZBB(JLON,KTDIA-1)&
     & -ZBB(JLON,KTDIA))
    ZZTMC(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*(ZBB(JLON,KLEV)&
     & -ZBB(JLON,KLEV-1))
    ZZTMC(JLON,KTDIA-1)=(1._JPRB-PNEB(JLON,KTDIA))*ZZTMC(JLON,KTDIA-1)
    ZZTDC(JLON,KTDIA)=(1._JPRB-PNEB(JLON,KTDIA))*ZZTDC(JLON,KTDIA)
    ZZTMC(JLON,KLEV)=(1._JPRB-PNEB(JLON,KLEV))*ZZTMC(JLON,KLEV)
    ZZTDN(JLON,KTDIA-1)=0._JPRB
    ZZTMN(JLON,KTDIA-1)=ZA4N(JLON,KTDIA)*PNEB(JLON,KTDIA)&
     & *(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA))
    ZZTDN(JLON,KTDIA)=ZA5N(JLON,KTDIA)*PNEB(JLON,KTDIA)&
     & *(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA))
    ZZTMN(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*PNEB(JLON,KLEV)&
     & *(ZBB(JLON,KLEV)-ZBB(JLON,KLEV-1))
  

  DO JLEV=KTDIA+1,KLEV
    
      ZZTMC(JLON,JLEV-1)=ZA4C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV))+ZA5C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV-2))
      ZZTDC(JLON,JLEV)=ZA5C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV))+ZA4C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV-2))
      ZZTMC(JLON,JLEV-1)=(1._JPRB-PNEB(JLON,JLEV))*ZZTMC(JLON,JLEV-1)
      ZZTDC(JLON,JLEV)=(1._JPRB-PNEB(JLON,JLEV))*ZZTDC(JLON,JLEV)
      ZZTMN(JLON,JLEV-1)=PNEB(JLON,JLEV)*(ZA4N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+ZA5N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV-2))) 
      ZZTDN(JLON,JLEV)=PNEB(JLON,JLEV)*(ZA5N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+ZA4N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV-2)))
   
  ENDDO

ELSE

  
    ZZTDC(JLON,KTDIA-1)=ZBB(JLON,KTDIA-1)*ZTDC(JLON,KTDIA-1)
    ZZTMC(JLON,KTDIA-1)=(ZA4C(JLON,KTDIA)*ZB1(JLON,KTDIA)+ZA4N(JLON,KTDIA)&
     & *PNEB(JLON,KTDIA))*(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA)) 
    ZZTDC(JLON,KTDIA)=(ZA5C(JLON,KTDIA)*ZB1(JLON,KTDIA)+ZA5N(JLON,KTDIA)&
     & *PNEB(JLON,KTDIA))*(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA))
    ZZTMC(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*(ZBB(JLON,KLEV)-ZBB(JLON,KLEV-1))
  

  DO JLEV=KTDIA+1,KLEV
    
    ZZTMC(JLON,JLEV-1)=(ZA4C(JLON,JLEV)*ZB1(JLON,JLEV)+ZA4N(JLON,JLEV)&
     & *PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+(ZA5C(JLON,JLEV)&
     & *ZB1(JLON,JLEV)+ZA5N(JLON,JLEV)*PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)&
     & -ZBB(JLON,JLEV-2))
    ZZTDC(JLON,JLEV)=(ZA5C(JLON,JLEV)*ZB1(JLON,JLEV)+ZA5N(JLON,JLEV)&
     & *PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+(ZA4C(JLON,JLEV)&
     & *ZB1(JLON,JLEV)+ZA4N(JLON,JLEV)*PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)&
     & -ZBB(JLON,JLEV-2))
   
  ENDDO

ENDIF ! LRNUMX


!     VIII.3 - SOLVE THE ADDING METHOD LINEAR SYSTEM BY GAUSSIAN
!     ELIMINATION BACK-SUBSTITUTION.
!-------------------------------------------------------------------------------

CALL ACRANEB_SOLVT3(YDML_PHY_MF%YRPHY,KIDIA,KFDIA,KLON,KTDIA,KLEV,&
 & PEMIS,PNEB,ZB1,ZB2,ZB3,ZB4,ZA4C,ZA5C,ZA4N,ZA5N,&
 & ZTDC,ZTMC,ZTDN,ZTMN,ZZTDC,ZZTMC,ZZTDN,ZZTMN,&
 & ZZZTDC,ZZZTMC,ZZZTDN,ZZZTMN, YLSTACK)

! DUPLICATION OF THE LOOP WITH THREE RIGHT-HAND SIDES
! WHEN REPLACING THE EBL MINIMUM VALUES FOR OPTICAL
! DEPTHS BY THE MAXIMUM (LOCAL) ONES.

! GLOBAL STORAGE FOR THE 'PROXIMITY' COMPUTATION
IF (YDML_PHY_MF%YRPHY%LRPROX) THEN

  ! COMPUTE MATRIX COEFICIENTS USING LOCAL GAS. OPTICAL DEPTH ZEOLT

  CALL ACRANEB_COEFT(KIDIA,KFDIA,KLON,KTDIA,1_JPIM,KLEV,&
   & PDELP,ZEOLT,ZEO1TI,ZEO2TI,ZEO1TN,ZEO2TN,ZQICE,&
   & ZQLI,ZEO1TA,ZEO2TA,ZA4C,ZA5C,ZA4N,ZA5N, YLSTACK)

  DO JLEV=KTDIA,KLEV
    
      ZLEPC(JLON,JLEV)=(1._JPRB-PNEB(JLON,JLEV))*ZA4C(JLON,JLEV)&
       & +PNEB(JLON,JLEV)*ZA4N(JLON,JLEV)
      ZDA4L(JLON,JLEV)=ZA4C(JLON,JLEV)-ZA4N(JLON,JLEV)
    
  ENDDO

ENDIF

! COMPUTE MATRIX COEFICIENTS USING MAXIMUM GAS. OPTICAL DEPTH ZEOXT

CALL ACRANEB_COEFT(KIDIA,KFDIA,KLON,KTDIA,1_JPIM,KLEV,&
 & PDELP,ZEOXT,ZEO1TI,ZEO2TI,ZEO1TN,ZEO2TN,ZQICE,&
 & ZQLI,ZEO1TA,ZEO2TA,ZA4C,ZA5C,ZA4N,ZA5N, YLSTACK)

IF ( YDML_PHY_MF%YRPHY%LRPROX ) THEN
  DO JLEV=KTDIA,KLEV
    
      ZXEPC(JLON,JLEV)=(1._JPRB-PNEB(JLON,JLEV))*ZA4C(JLON,JLEV)&
       & +PNEB(JLON,JLEV)*ZA4N(JLON,JLEV)
      ZDA4X(JLON,JLEV)=ZA4C(JLON,JLEV)-ZA4N(JLON,JLEV)
    
  ENDDO
ELSEIF ( YDML_PHY_MF%YRPHY%LRTPP ) THEN
  
    ZLEPC(JLON,KLEV)=(1._JPRB-PNEB(JLON,KLEV))*ZA4C(JLON,KLEV)&
     & +PNEB(JLON,KLEV)*ZA4N(JLON,KLEV)
  
ENDIF

! RHS FOR PROFILE A


  ZFDC(JLON,KTDIA-1)=EXP(MAX(-ZDEOTI(JLON,KTDIA-1),ZARGLI))
  ZFMC(JLON,KTDIA-1)=0._JPRB
  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZFDN(JLON,KTDIA-1)=0._JPRB
    ZFMN(JLON,KTDIA-1)=0._JPRB
  ENDIF


DO JLEV=KTDIA,KLEV
  
    ZFMC(JLON,JLEV)=0._JPRB
    ZFDC(JLON,JLEV)=0._JPRB
    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZFDN(JLON,JLEV)=0._JPRB
      ZFMN(JLON,JLEV)=0._JPRB
    ENDIF
  
ENDDO

! RHS FOR PROFILE C

IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN

  
    ZZFDC(JLON,KTDIA-1)=ZBB(JLON,KTDIA-1)*ZFDC(JLON,KTDIA-1)
    ZZFMC(JLON,KTDIA-1)=ZA4C(JLON,KTDIA)*(ZBB(JLON,KTDIA-1)&
     & -ZBB(JLON,KTDIA)) 
    ZZFDC(JLON,KTDIA)=ZA5C(JLON,KTDIA)*(ZBB(JLON,KTDIA-1)&
     & -ZBB(JLON,KTDIA))
    ZZFMC(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*(ZBB(JLON,KLEV)&
     & -ZBB(JLON,KLEV-1))
    ZZFMC(JLON,KTDIA-1)=(1._JPRB-PNEB(JLON,KTDIA))*ZZFMC(JLON,KTDIA-1)
    ZZFDC(JLON,KTDIA)=(1._JPRB-PNEB(JLON,KTDIA))*ZZFDC(JLON,KTDIA)
    ZZFMC(JLON,KLEV)=(1._JPRB-PNEB(JLON,KLEV))*ZZFMC(JLON,KLEV)
    ZZFDN(JLON,KTDIA-1)=0._JPRB
    ZZFMN(JLON,KTDIA-1)=ZA4N(JLON,KTDIA)*PNEB(JLON,KTDIA)&
     & *(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA))
    ZZFDN(JLON,KTDIA)=ZA5N(JLON,KTDIA)*PNEB(JLON,KTDIA)&
     & *(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA))
    ZZFMN(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*PNEB(JLON,KLEV)&
     & *(ZBB(JLON,KLEV)-ZBB(JLON,KLEV-1))
  

  DO JLEV=KTDIA+1,KLEV
    
      ZZFMC(JLON,JLEV-1)=ZA4C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV))+ZA5C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV-2))
      ZZFDC(JLON,JLEV)=ZA5C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV))+ZA4C(JLON,JLEV)*(ZBB(JLON,JLEV-1)&
       & -ZBB(JLON,JLEV-2))
      ZZFMC(JLON,JLEV-1)=(1._JPRB-PNEB(JLON,JLEV))*ZZFMC(JLON,JLEV-1)
      ZZFDC(JLON,JLEV)=(1._JPRB-PNEB(JLON,JLEV))*ZZFDC(JLON,JLEV)
      ZZFMN(JLON,JLEV-1)=PNEB(JLON,JLEV)*(ZA4N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+ZA5N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV-2))) 
      ZZFDN(JLON,JLEV)=PNEB(JLON,JLEV)*(ZA5N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+ZA4N(JLON,JLEV)&
       & *(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV-2)))
    
  ENDDO

ELSE

  
    ZZFDC(JLON,KTDIA-1)=ZBB(JLON,KTDIA-1)*ZFDC(JLON,KTDIA-1)
    ZZFMC(JLON,KTDIA-1)=(ZA4C(JLON,KTDIA)*ZB1(JLON,KTDIA)+ZA4N(JLON,KTDIA)&
     & *PNEB(JLON,KTDIA))*(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA)) 
    ZZFDC(JLON,KTDIA)=(ZA5C(JLON,KTDIA)*ZB1(JLON,KTDIA)+ZA5N(JLON,KTDIA)&
     & *PNEB(JLON,KTDIA))*(ZBB(JLON,KTDIA-1)-ZBB(JLON,KTDIA))
    ZZFMC(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*(ZBB(JLON,KLEV)-ZBB(JLON,KLEV-1))
  

  DO JLEV=KTDIA+1,KLEV
    
    ZZFMC(JLON,JLEV-1)=(ZA4C(JLON,JLEV)*ZB1(JLON,JLEV)+ZA4N(JLON,JLEV)&
     & *PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+(ZA5C(JLON,JLEV)&
     & *ZB1(JLON,JLEV)+ZA5N(JLON,JLEV)*PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)&
     & -ZBB(JLON,JLEV-2))
    ZZFDC(JLON,JLEV)=(ZA5C(JLON,JLEV)*ZB1(JLON,JLEV)+ZA5N(JLON,JLEV)&
     & *PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))+(ZA4C(JLON,JLEV)&
     & *ZB1(JLON,JLEV)+ZA4N(JLON,JLEV)*PNEB(JLON,JLEV))*(ZBB(JLON,JLEV-1)&
     & -ZBB(JLON,JLEV-2))
   
  ENDDO

ENDIF ! LRNUMX

! RHS FOR PROFILE B

IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN

  
    ZZZFDC(JLON,KTDIA-1)=0._JPRB
    ZZZFMC(JLON,KTDIA-1)=0._JPRB
    ZZZFDC(JLON,KTDIA)=0._JPRB
    ZZZFMC(JLON,KLEV-1)=-ZA4C(JLON,KLEV)
    ZZZFDC(JLON,KLEV)=-ZA5C(JLON,KLEV)
    ZZZFMC(JLON,KLEV)=1._JPRB-PEMIS(JLON)

    ZZZFMC(JLON,KLEV-1)=ZZZFMC(JLON,KLEV-1)*(1._JPRB-PNEB(JLON,KLEV))
    ZZZFDC(JLON,KLEV)=ZZZFDC(JLON,KLEV)*(1._JPRB-PNEB(JLON,KLEV))
    ZZZFMC(JLON,KLEV)=ZZZFMC(JLON,KLEV)*(1._JPRB-PNEB(JLON,KLEV))
    ZZZFDN(JLON,KTDIA-1)=0._JPRB
    ZZZFMN(JLON,KTDIA-1)=0._JPRB
    ZZZFDN(JLON,KTDIA)=0._JPRB
    ZZZFMN(JLON,KLEV-1)=-ZA4N(JLON,KLEV)*PNEB(JLON,KLEV)
    ZZZFDN(JLON,KLEV)=-ZA5N(JLON,KLEV)*PNEB(JLON,KLEV)
    ZZZFMN(JLON,KLEV)=(1._JPRB-PEMIS(JLON))*PNEB(JLON,KLEV)
  

  DO JLEV=KTDIA+1,KLEV-1
    
      ZZZFDC(JLON,JLEV)=0._JPRB
      ZZZFMC(JLON,JLEV-1)=0._JPRB
      ZZZFDN(JLON,JLEV)=0._JPRB
      ZZZFMN(JLON,JLEV-1)=0._JPRB
    
  ENDDO

ELSE

  
    ZZZFDC(JLON,KTDIA-1)=0._JPRB
    ZZZFMC(JLON,KTDIA-1)=0._JPRB
    ZZZFDC(JLON,KTDIA)=0._JPRB
    ZZZFMC(JLON,KLEV-1)=-(ZA4C(JLON,KLEV)*ZB1(JLON,KLEV)+ZA4N(JLON,KLEV)&
     & *PNEB(JLON,KLEV)) 
    ZZZFDC(JLON,KLEV)=-(ZA5C(JLON,KLEV)*ZB1(JLON,KLEV)+ZA5N(JLON,KLEV)&
     & *PNEB(JLON,KLEV))
    ZZZFMC(JLON,KLEV)=1._JPRB-PEMIS(JLON)
  

  DO JLEV=KTDIA+1,KLEV-1
    
      ZZZFDC(JLON,JLEV)=0._JPRB
      ZZZFMC(JLON,JLEV-1)=0._JPRB
    
  ENDDO

ENDIF ! LRNUMX

! SOLVE ADDING SYSTEM FOR PROFILES A, B, C

CALL ACRANEB_SOLVT3(YDML_PHY_MF%YRPHY,KIDIA,KFDIA,KLON,KTDIA,KLEV,&
 & PEMIS,PNEB,ZB1,ZB2,ZB3,ZB4,ZA4C,ZA5C,ZA4N,ZA5N,&
 & ZFDC,ZFMC,ZFDN,ZFMN,ZZFDC,ZZFMC,ZZFDN,ZZFMN,&
 & ZZZFDC,ZZZFMC,ZZZFDN,ZZZFMN, YLSTACK)

!     VIII.4 - CALCUL DES FLUX DEFINITIFS.

!     VIII.4 - COMPUTATION OF THE FINAL FLUXES.
!-------------------------------------------------------------------------------


  ZTRB=ZZZTDC(JLON,KTDIA-1)-ZZZTMC(JLON,KTDIA-1)
  ZFRB=ZZZFDC(JLON,KTDIA-1)-ZZZFMC(JLON,KTDIA-1)

  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZTRB=ZTRB+ZZZTDN(JLON,KTDIA-1)-ZZZTMN(JLON,KTDIA-1)
    ZFRB=ZFRB+ZZZFDN(JLON,KTDIA-1)-ZZZFMN(JLON,KTDIA-1)
  ENDIF

  ZZTRTH(JLON,KTDIA-1)=0._JPRB
  ZZFRTH(JLON,KTDIA-1)=0._JPRB


!cdir unroll=8
DO JLEV=KTDIA,KLEV
  LLREWS=(JLEV==KLEV)

  
    ZTRS=ZTRB
    ZFRS=ZFRB
  

  
    ZTRB=ZZZTDC(JLON,JLEV)-ZZZTMC(JLON,JLEV)
    ZFRB=ZZZFDC(JLON,JLEV)-ZZZFMC(JLON,JLEV)

    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZTRB=ZTRB+ZZZTDN(JLON,JLEV)-ZZZTMN(JLON,JLEV)
      ZFRB=ZFRB+ZZZFDN(JLON,JLEV)-ZZZFMN(JLON,JLEV)
    ENDIF

    IF (LLREWS) THEN
      ZTRB=ZTRB+1._JPRB
      ZFRB=ZFRB+1._JPRB
    ENDIF

    ZZTRTH(JLON,JLEV)=ZZTRTH(JLON,JLEV-1)+(ZBB(JLON,KLEV)&
     & -ZBB(JLON,JLEV-1))*(ZTRS-ZTRB)
    ZZFRTH(JLON,JLEV)=ZZFRTH(JLON,JLEV-1)+(ZBB(JLON,KLEV)&
     & -ZBB(JLON,JLEV-1))*(ZFRS-ZFRB)
    ZFRTH(JLON,JLEV)=ZFRTH(JLON,JLEV)-(ZZTRTH(JLON,JLEV)&
     & +ZMIXP(JLON,JLEV)*(ZZFRTH(JLON,JLEV)-ZZTRTH(JLON,JLEV)))
  

ENDDO

! - TEMPORAIRE(S) 1D

! ZZTRS     : FLUX "ZZ" NET AU SOMMET D'UNE COUCHE.
!           : "ZZ" NET FLUX AT THE TOP OF THE LAYER.
! ZDRS      : FLUX NET AU SOMMET "COOLING TO SPACE".
!           : NET TOP FLUX FOR COOLING TO SPACE.
! ZZTRB     : FLUX "ZZ" NET A LA BASE D'UNE COUCHE.
!           : "ZZ" NET FLUX AT THE BOTTOM OF THE LAYER.
! ZDRB      : FLUX NET A LA BASE "COOLING TO SPACE".
!           : NET BOTTOM FLUX FOR COOLING TO SPACE.


  ZTRS=ZTDC(JLON,KLEV)-ZTMC(JLON,KLEV)
  ZFRS=ZFDC(JLON,KLEV)-ZFMC(JLON,KLEV)
  ZZTRS=ZZTDC(JLON,KLEV)-ZZTMC(JLON,KLEV)+ZBB(JLON,KLEV)&
   & -ZBB(JLON,KLEV-1)
  ZZFRS=ZZFDC(JLON,KLEV)-ZZFMC(JLON,KLEV)+ZBB(JLON,KLEV)&
   & -ZBB(JLON,KLEV-1)

  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZTRS=ZTRS+ZTDN(JLON,KLEV)-ZTMN(JLON,KLEV)
    ZFRS=ZFRS+ZFDN(JLON,KLEV)-ZFMN(JLON,KLEV)
    ZZTRS=ZZTRS+ZZTDN(JLON,KLEV)-ZZTMN(JLON,KLEV)
    ZZFRS=ZZFRS+ZZFDN(JLON,KLEV)-ZZFMN(JLON,KLEV)
  ENDIF

  ZDRS=PFRTH(JLON,KLEV)
  ZZTRTH(JLON,KLEV)=ZDRS+ZBB(JLON,KLEV)*ZTRS-ZZTRS
  ZZFRTH(JLON,KLEV)=ZDRS+ZBB(JLON,KLEV)*ZFRS-ZZFRS
  PFRTH(JLON,KLEV)=ZZTRTH(JLON,KLEV)+ZMIXP(JLON,KLEV)&
   & *(ZZFRTH(JLON,KLEV)-ZZTRTH(JLON,KLEV))


!cdir unroll=8
DO JLEV=KLEV,KTDIA+1,-1

  
    ZTRB=ZTRS
    ZFRB=ZFRS
    ZZTRB=ZZTRS
    ZZFRB=ZZFRS
    ZDRB=ZDRS
  

  
    ZTRS=ZTDC(JLON,JLEV-1)-ZTMC(JLON,JLEV-1)
    ZFRS=ZFDC(JLON,JLEV-1)-ZFMC(JLON,JLEV-1)
    ZZTRS=ZZTDC(JLON,JLEV-1)-ZZTMC(JLON,JLEV-1)&
     & +ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV-2)
    ZZFRS=ZZFDC(JLON,JLEV-1)-ZZFMC(JLON,JLEV-1)&
     & +ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV-2)

    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZTRS=ZTRS+ZTDN(JLON,JLEV-1)-ZTMN(JLON,JLEV-1)
      ZFRS=ZFRS+ZFDN(JLON,JLEV-1)-ZFMN(JLON,JLEV-1)
      ZZTRS=ZZTRS+ZZTDN(JLON,JLEV-1)-ZZTMN(JLON,JLEV-1)
      ZZFRS=ZZFRS+ZZFDN(JLON,JLEV-1)-ZZFMN(JLON,JLEV-1)
    ENDIF

    ZDRS=PFRTH(JLON,JLEV-1)
    ZZTRTH(JLON,JLEV-1)=ZZTRTH(JLON,JLEV)+ZDRS-ZDRB&
     & -ZBB(JLON,JLEV-1)*(ZTRB-ZTRS)&
     & +ZZTRB-ZZTRS
    ZZFRTH(JLON,JLEV-1)=ZZFRTH(JLON,JLEV)+ZDRS-ZDRB&
     & -ZBB(JLON,JLEV-1)*(ZFRB-ZFRS)&
     & +ZZFRB-ZZFRS
    PFRTH(JLON,JLEV-1)=ZZTRTH(JLON,JLEV-1)+ZMIXP(JLON,JLEV-1)&
     & *(ZZFRTH(JLON,JLEV-1)-ZZTRTH(JLON,JLEV-1))
  

ENDDO


  ZTRB=ZTRS
  ZFRB=ZFRS
  ZZTRB=ZZTRS
  ZZFRB=ZZFRS
  ZDRB=ZDRS



  ZTRS=ZTDC(JLON,KTDIA-1)-ZTMC(JLON,KTDIA-1)
  ZFRS=ZFDC(JLON,KTDIA-1)-ZFMC(JLON,KTDIA-1)
  ZZTRS=ZZTDC(JLON,KTDIA-1)-ZZTMC(JLON,KTDIA-1)
  ZZFRS=ZZFDC(JLON,KTDIA-1)-ZZFMC(JLON,KTDIA-1)

  IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
    ZTRS=ZTRS+ZTDN(JLON,KTDIA-1)-ZTMN(JLON,KTDIA-1)
    ZFRS=ZFRS+ZFDN(JLON,KTDIA-1)-ZFMN(JLON,KTDIA-1)
    ZZTRS=ZZTRS+ZZTDN(JLON,KTDIA-1)-ZZTMN(JLON,KTDIA-1)
    ZZFRS=ZZFRS+ZZFDN(JLON,KTDIA-1)-ZZFMN(JLON,KTDIA-1)
  ENDIF

  ZDRS=PFRTH(JLON,KTDIA-1)
  ZZTRTH(JLON,KTDIA-1)=ZZTRTH(JLON,KTDIA)+ZDRS-ZDRB&
   & -ZBB(JLON,KTDIA-1)*(ZTRB-ZTRS)&
   & +ZZTRB-ZZTRS
  ZZFRTH(JLON,KTDIA-1)=ZZFRTH(JLON,KTDIA)+ZDRS-ZDRB&
   & -ZBB(JLON,KTDIA-1)*(ZFRB-ZFRS)&
   & +ZZFRB-ZZFRS
  PFRTH(JLON,KTDIA-1)=ZZTRTH(JLON,KTDIA-1)+ZMIXP(JLON,KTDIA-1)&
   & *(ZZFRTH(JLON,KTDIA-1)-ZZTRTH(JLON,KTDIA-1))


DO JLEV=KTDIA-1,KLEV
  
    PFRTH(JLON,JLEV)=PFRTH(JLON,JLEV)+ZFRTH(JLON,JLEV)
  
ENDDO

! 'PROXIMITY CORRECTION' WITH THE INTERPOLATED EFFECT AS BASIS
! AND THE TIME-SECURED MAXIMUM EXCHANGE SITUATION AS TARGET.
IF (YDML_PHY_MF%YRPHY%LRPROX) THEN

!cdir unroll=8
  DO JLEV=KTDIA,KLEV-1
    
      ZDEL1=-LOG(MAX(ZTRLI,ZLEPC(JLON,JLEV)))
      ZDEL2=-LOG(MAX(ZTRLI,ZLEPC(JLON,JLEV+1)))
      ZFTPP=1._JPRB
      IF (YDML_PHY_MF%YRPHY%LRTPP) THEN
        ZFTPP=MAX(0._JPRB,1._JPRB-((2._JPRB-(1._JPRB-ZLEPC(JLON,JLEV))&
         & *(1._JPRB+2._JPRB/ZDEL1))+(2._JPRB-(1._JPRB-ZLEPC(JLON,JLEV+1))&
         & *(1._JPRB+2._JPRB/ZDEL2)))/((1._JPRB-ZLEPC(JLON,JLEV))+(1._JPRB&
         & -ZLEPC(JLON,JLEV+1))))
      ENDIF
      ZTARE2=(1._JPRB-ZXEPC(JLON,JLEV))*(1._JPRB-ZXEPC(JLON,JLEV+1))
      ZEFFE2=(1._JPRB-ZGEPC(JLON,JLEV))*(1._JPRB-ZGEPC(JLON,JLEV+1))
      ZTARSP=1._JPRB-ZLEPC(JLON,JLEV)-ZLEPC(JLON,JLEV+1)+       &
       & ZRPROX(JLON,JLEV)*ZLEPC(JLON,JLEV)*ZLEPC(JLON,JLEV+1)
      ZTARSP=MAX(0._JPRB,MIN(1._JPRB,ZTARSP))
      IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
        ZTARE2=ZTARE2&
         & +MIN(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1))*(1._JPRB&
         & -MAX(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1)))&
         & *(ZDA4X(JLON,JLEV)*ZDA4X(JLON,JLEV+1))
        ZEFFE2=ZEFFE2&
         & +MIN(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1))*(1._JPRB&
         & -MAX(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1)))&
         & *(ZDA4G(JLON,JLEV)*ZDA4G(JLON,JLEV+1))
        ZTARSP=MIN(1._JPRB,ZTARSP&
         & +MIN(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1))*(1._JPRB&
         & -MAX(PNEB(JLON,JLEV),PNEB(JLON,JLEV+1)))&
         & *(ZDA4L(JLON,JLEV)*ZDA4L(JLON,JLEV+1))*ZRPROX(JLON,JLEV))
      ENDIF
      ZEFFE2=ZEFFE2*(1._JPRB-ZMIXP(JLON,JLEV))+ZTARE2*ZMIXP(JLON,JLEV)
      ZTARE2=ZTARSP
      ZTARE2=ZTARE2*ZFTPP
      ZTARE2=ZTARE2/(1._JPRB+(ZDAMP(JLON,JLEV)+ZDAMP(JLON,JLEV+1))*ZTARE2)
      PFRTH(JLON,JLEV)=PFRTH(JLON,JLEV)&
       & +(ZBB(JLON,JLEV-1)-ZBB(JLON,JLEV))*(ZTARE2-ZEFFE2)
    
  ENDDO

ENDIF ! LRPROX

! LRTPP correction for exchange between lowest model level and surface
IF (YDML_PHY_MF%YRPHY%LRTPP) THEN
  
    ZDEL1=-LOG(MAX(ZTRLI,ZLEPC(JLON,KLEV)))
    ZEFFE2=(1._JPRB-ZLEPC(JLON,KLEV))*PEMIS(JLON)
    ZTARE2=2._JPRB*PEMIS(JLON)*((1._JPRB-ZLEPC(JLON,KLEV))&
     & *(1._JPRB+1._JPRB/ZDEL1)-1._JPRB)
    PFRTH(JLON,KLEV)=PFRTH(JLON,KLEV)&
     & +(ZBB(JLON,KLEV-1)-ZBB(JLON,KLEV))*(ZTARE2-ZEFFE2)&
     & /(1._JPRB+ZDAMP(JLON,KLEV)*ZEFFE2)
  
ENDIF

DO JLEV=KTDIA-1,KLEV
  
    PFRTH(JLON,JLEV)=PFRTH(JLON,JLEV)+ZFLUXC(JLON,JLEV)
  
ENDDO


  PFRTHDS(JLON)=ZBB(JLON,KLEV)+PFRTH(JLON,KLEV)/PEMIS(JLON)


!     IX - SOLAR COMPUTATIONS.
!-------------------------------------------------------------------------------

!     IX.1 - CALCUL DES TABLEAUX DEPENDANTS DE L'ANGLE SOLAIRE.

!     IX.1 - COMPUTATION OF THE SOLAR ANGLE DEPENDENT ARRAYS.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 1D

! ZMU0I     : INVERSE DU COSINUS MODIFIE DE L'ANGLE ZENITHAL.
!           : INVERSE OF THE MODIFIED COSINE OF THE ZENITH ANGLE.
! ZUSA      : "UPSCATTERED FRACTION" POUR LES AEROSOLS.
!           : AEROSOLS' UPSCATTERED FRACTION.


	!IF ( LLMASKS(JLON) ) THEN
    ZMU0I(JLON)=2._JPRB*ZDM0I(JLON)
  !ENDIF


DO JLEV=1,KLEV
  
	  ZEO3SA(JLON,JLEV)=0._JPRB
    ZEO4SA(JLON,JLEV)=0._JPRB
	
ENDDO
DO JAE=1,6
  DO JLEV=KTDIA,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        ZUSA=(0.5_JPRB+YDML_PHY_MF%YRPHY3%USAA(JAE)*ZMU0)/(1._JPRB+YDML_PHY_MF%YRPHY3%USBA*ZMU0)
        ZEO3SA(JLON,JLEV)=ZEO3SA(JLON,JLEV)+YDML_PHY_MF%YRPHY3%EODSA(JAE)*PDAER(JLON,JLEV,JAE)&
         & *ZUSA
        ZEO4SA(JLON,JLEV)=ZEO4SA(JLON,JLEV)+YDML_PHY_MF%YRPHY3%EODSA(JAE)*PDAER(JLON,JLEV,JAE)&
         & *(1._JPRB-ZUSA)
      !ENDIF
    
  ENDDO
ENDDO

!     IX.2 - COMPUTE SOLAR OPTICAL PROPERTIES.
!-------------------------------------------------------------------------------

DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZEO2SN(JLON,JLEV)=2._JPRB*ZBSFSN(JLON,JLEV)*ZEODSN(JLON,JLEV)
      ZEO2SI(JLON,JLEV)=2._JPRB*ZBSFSI(JLON,JLEV)*ZEODSI(JLON,JLEV)
      ZEO1SN(JLON,JLEV)=ZEO2SN(JLON,JLEV)+2._JPRB*ZEOASN(JLON,JLEV)
      ZEO1SI(JLON,JLEV)=ZEO2SI(JLON,JLEV)+2._JPRB*ZEOASI(JLON,JLEV)
      ZEOSN(JLON,JLEV)=ZEODSN(JLON,JLEV)+ZEOASN(JLON,JLEV)
      ZEOSI(JLON,JLEV)=ZEODSI(JLON,JLEV)+ZEOASI(JLON,JLEV)
      ZUSN(JLON,JLEV)=(0.5_JPRB+ZUSAN(JLON,JLEV)*ZMU0)&
       & /(1._JPRB+ZUSBN(JLON,JLEV)*ZMU0)
      ZUSI(JLON,JLEV)=(0.5_JPRB+ZUSAI(JLON,JLEV)*ZMU0)&
       & /(1._JPRB+ZUSBI(JLON,JLEV)*ZMU0)
    !ENDIF
  
ENDDO

CALL ACRANEB_COEFS(YDML_PHY_MF%YRPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
 & PAPRSF,PDELP,ZQICE,ZQLI,ZDEOSI,ZEODSI,ZEODSN,&
 & ZEOSI,ZEOSIDIR,ZEOSN,ZEOSNUN,&
 & ZEO1SI,ZEO1SN,ZEO2SI,ZEO2SN,ZUEOSI,ZUSI,ZUSN,&
 & ZEOSA,ZEOSADIR,ZEO1SA,ZEO2SA,ZEO3SA,ZEO4SA,ZMU0I,&
 & ZA1C,ZA1CUN,ZA2C,ZA3C,ZA4C,ZA5C,ZA1N,ZA1NUN,ZA2N,ZA3N,ZA4N,ZA5N, YLSTACK)

! atmospheric transmissions for clearsky direct fluxes at surface

	ZTAUC  =1._JPRB
	ZTAUCUN=1._JPRB

DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZTAUC  =ZTAUC  *ZA1C  (JLON,JLEV)  ! delta-scaled
      ZTAUCUN=ZTAUCUN*ZA1CUN(JLON,JLEV)  ! delta-unscaled
    !ENDIF
  
ENDDO

! set surface fluxes to zero for security of the night-time case

  PFRSODS     (JLON)=0._JPRB
  PFRSOPS     (JLON)=0._JPRB
  ZFRSOPS_C   (JLON)=0._JPRB
  ZFRSOPS_CUN (JLON)=0._JPRB
  ZFRSOPS_TRUE=0._JPRB
  ZFRSOPS_UN  =0._JPRB


!     IX.3 - INCOMING SOLAR FLUXES AT UPPER BOUNDARY, CLEARSKY DIRECT
!            FLUXES AT SURFACE FOR LRTRUEBBC OPTION.
!-------------------------------------------------------------------------------


	!IF ( LLMASKS(JLON) ) THEN
    ZFPC  (JLON,KTDIA-1)=ZII0*ZMU0*EXP(MAX(-0.5_JPRB&
     & *ZMU0I(JLON)*ZDEOSI(JLON,KTDIA-1),ZARGLI))
    ZFPCUN(JLON,KTDIA-1)=ZFPC(JLON,KTDIA-1)
    ZFDC  (JLON,KTDIA-1)=0._JPRB
    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZFPN  (JLON,KTDIA-1)=0._JPRB
      ZFPNUN(JLON,KTDIA-1)=0._JPRB
      ZFDN  (JLON,KTDIA-1)=0._JPRB
    ENDIF
    ZFRSOPS_C  (JLON)=ZFPC(JLON,KTDIA-1)*ZTAUC  
    ZFRSOPS_CUN(JLON)=ZFPC(JLON,KTDIA-1)*ZTAUCUN
  !ENDIF


!     IX.4 - SOLVE DELTA-TWO STREAM ADDING SYSTEM FOR SOLAR FLUXES.
!-------------------------------------------------------------------------------

CALL ACRANEB_SOLVS(YDML_PHY_MF%YRPHY,KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
 & PALB,PALBDIR,PCLCT,ZFRSOPS_C,ZFRSOPS_CUN,&
 & PNEB,ZB1,ZB2,ZB3,ZB4,&
 & ZA1C,ZA1CUN,ZA2C,ZA3C,ZA4C,ZA5C,ZA1N,ZA1NUN,ZA2N,ZA3N,ZA4N,ZA5N,&
 & ZFDC,ZFMC,ZFPC,ZFPCUN,ZFDN,ZFMN,ZFPN,ZFPNUN, YLSTACK)

DO JLEV=KTDIA-1,KLEV

  ! MISE A ZERO DES FLUX EN SECURITE POUR LE CAS NOCTURNE.
  ! SETTING FLUXES TO ZERO FOR SECURITY IN THE NIGHT-TIME CASE.
  
    PFRSO(JLON,JLEV)=0._JPRB
  

  ! CALCUL EFFECTIF.
  ! ACTUAL COMPUTATION.
	
		!IF ( LLMASKS(JLON) ) THEN
      PFRSO(JLON,JLEV)=ZFPC(JLON,JLEV)+ZFDC(JLON,JLEV)-ZFMC(JLON,JLEV)
      IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
        PFRSO(JLON,JLEV)=PFRSO(JLON,JLEV)+ZFPN(JLON,JLEV)&
         & +ZFDN(JLON,JLEV)-ZFMN(JLON,JLEV)
      ENDIF
    !ENDIF
  

ENDDO

!     IX.5 - DIAGN. FLUX SOLAIRE PARALLELE ET DIFFUS VERS LE BAS EN SURFACE.
!
!     IX.5 - DIAGNOSE DIFFUSE AND DIRECT DOWNWARD FLUXES AT THE SURFACE.
!-------------------------------------------------------------------------------

! direct solar fluxes (unscaled, clearsky scaled/unscaled and true)

	!IF ( LLMASKS(JLON) ) THEN
    ZFRSOPS_UN=ZFPCUN(JLON,KLEV)
    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      ZFRSOPS_UN=ZFRSOPS_UN+ZFPNUN(JLON,KLEV)
    ENDIF
    ZFRSOPS_TRUE=ZFRSOPS_UN+(1._JPRB-PCLCT(JLON))* &
     & (ZFRSOPS_C(JLON)-ZFRSOPS_CUN(JLON))
  !ENDIF


! direct and downward diffuse fluxes

	!IF ( LLMASKS(JLON) ) THEN
    PFRSODS(JLON)=ZFDC(JLON,KLEV)
    PFRSOPS(JLON)=ZFPC(JLON,KLEV)
    IF (YDML_PHY_MF%YRPHY%LRNUMX) THEN
      PFRSODS(JLON)=PFRSODS(JLON)+ZFDN(JLON,KLEV)
      PFRSOPS(JLON)=PFRSOPS(JLON)+ZFPN(JLON,KLEV)
    ENDIF
  !ENDIF

IF ( YDML_PHY_MF%YRPHY%LRTRUEDIR ) THEN
	
		!IF ( LLMASKS(JLON) ) THEN
      PFRSODS(JLON)=PFRSODS(JLON)+PFRSOPS(JLON)-ZFRSOPS_TRUE
      PFRSOPS(JLON)=                            ZFRSOPS_TRUE
    !ENDIF
  
ENDIF

! no flux divergence above KTDIA
DO JLEV=0,KTDIA-2
  
    PFRSO(JLON,JLEV)=PFRSO(JLON,KTDIA-1)
    PFRTH(JLON,JLEV)=PFRTH(JLON,KTDIA-1)
  
ENDDO

! update sunshine duration

  IF ( ZFRSOPS_C(JLON) > YDERDI%RSUNDUR*PMU0(JLON) ) THEN
    PSDUR(JLON)=PSDUR(JLON)+(1._JPRB-PCLCT(JLON))*YDRIP%TSTEP
  ENDIF
  IF ( ZFRSOPS_UN-(1._JPRB-PCLCT(JLON))*ZFRSOPS_CUN(JLON) > &
   & YDERDI%RSUNDUR*PMU0(JLON)*PCLCT(JLON) ) THEN
    PSDUR(JLON)=PSDUR(JLON)+PCLCT(JLON)*YDRIP%TSTEP
  ENDIF


!     IX.6 - CALCUL DU FLUX LUNAIRE DESCENDANT EN SURFACE.
!     ATTENTION: ON VEUT LE SEUL FLUX LUNAIRE,
!     AUSSI CE FLUX EST MIS A ZERO LA OU LE SOLEIL EST LEVE.

!     IX.6 - COMPUTE LUNAR DOWNWARD FLUX AT SURFACE.
!     WARNING: ONE COMPUTES THE MERE LUNAR FLUX,
!     SO THIS FLUX IS PUT TO ZERO WHERE THE SUN IS UP.
!-------------------------------------------------------------------------------

IF (YDML_PHY_MF%YRPHY%LRAYLU) THEN

  
    PFRSOLU(JLON)=1._JPRB/(MAX(ZEPSAL,1._JPRB-PALB(JLON)))*PFRSO(JLON,KLEV)&
     & *0.5_JPRB*(1._JPRB+SIGN(1._JPRB,0._JPRB-PMU0(JLON)))
  

ELSE

  
    PFRSOLU(JLON)=0._JPRB
  

ENDIF

!-------------------------------------------------------------------------------

END SUBROUTINE ACRANEB2

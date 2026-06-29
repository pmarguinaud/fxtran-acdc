#ifdef RS6K
@PROCESS NOCHECK
#endif
!option! -O nomove
SUBROUTINE LARMES(YDGEOMETRY, YDGEOMVARS, YDCST, YDRIP, YDML_DYN, YDCPG_OPTS, YDCPG_BNDS, YDSL, &
& YDCPG_SL1, YDCPG_SL2, YDMASK_SL2, YDA_KVSEPC, YDA_KVSEPL, PSCO, PLEV, PCCO, PUF, PVF, &
& PWF, KL0, KLH0, KLEV, PLSCAW, PRSCAW)  

!$ACDC pointerparallel    


!----compiled for Cray with -hcontiguous----

!**** *LARMES - semi-LAgrangian scheme:
!                Research of the origin point on the Sphere.

!     Purpose.
!     --------

!      The computation of the location of the interpolation point of
!     the lagrangian trajectory is performed by an iterative
!     method described by Robert and adapted to the sphere by M. Rochas.
!     Trajectories are great circles.
!     Finally we find the departure (origin) point "O".

!**   Interface.
!     ----------
!        *CALL* *LARMES(...)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST      - first element of arrays where computations are performed.
!          KEND    - depth of work.
!          YDSL     - SL_STRUCT definition
!          KSTABUF  - for a latitude IGL, KSTABUF(IGL) is the
!                     address of the element corresponding to
!                     (ILON=1,IGL) in the NPROMA arrays.
!          PLSDEPI  - (Number of points by latitude) / (2 * PI) .
!          KIBL     - index into YRCSGEOM/YRGSGEOM instances in YDGEOMETRY

!        INPUT/OUTPUT:
!          KVSEPC   - vertical separation (used in S/L adjoint, cubic interp.)
!          KVSEPL   - vertical separation (used in S/L adjoint, linear interp.)

!        OUTPUT:
!          PSCO     - information about geographic position of interpol. point.
!          PLEV     - vertical coordinate of the interpolation point.
!          PCCO     - information about comput. space position of interpol. point.
!          PUF      - U-comp of wind necessary to
!                     find the position of the origin point,
!                     in a local repere linked to computational sphere.
!          PVF      - V-comp of wind necessary to
!                     find the position of the origin point,
!                     in a local repere linked to computational sphere.
!          PWF      - interpolated etadot used to find the vertical displacement.
!          KL0      - index of the four western points
!                     of the 16 points interpolation grid.
!          KLH0     - second value of index of the four western points
!                     of the 16 points interpolation grid if needed.
!          KLEV     - lower level of the vertical interpolation
!                     grid needed for vertical interpolations.
!          PLSCAW   - linear weights (distances) for interpolations.
!          PRSCAW   - non-linear weights for interpolations.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation about semi-Lagrangian scheme.

!     Externals.
!     ----------
!        Calls  LARCINA.
!        Is called by LAPINEA (3D model)

!     Reference.
!     ----------

!     Author.
!     -------
!      K. YESSAD after LAGRMIL and a part of CPLGDY2 written by
!      Maurice IMBARD     METEO FRANCE/EERM/CRMD
!      Original : JUNE 1991

!     Modifications.
!     --------------
!      N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!      30-Jun-2008 J. Masek    Dataflow for new SLHD interpolators.
!      K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!      K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!      K. Yessad (Nov 2009): keep LLO.OR.LELTRA=T only in SL2TL.
!      K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!      F. Vana 21-Feb-2011: update of weights dimensions (hor. turbulence)
!      G.Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!      G.Mozdzynski (Feb 2011): OOPS cleaning, use of derived types TGSGEOM and TCSGEOM
!      K. Yessad (Nov 2011): introduce LRALTVDISP.
!      F. Vana  13-Feb-2014  update of variables for LARCINA
!      T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!      S. Malardel (Nov 2013): COMAD weights for SL interpolations
!      K. Yessad (July 2014): Rename some variables.
!      F. Vana    21-Nov-2017: Options LSLDP_CURV and LHOISLT
!      K. Yessad (Feb 2018): remove deep-layer formulations.
!      F. Vana    July 2018: Optimization
!      F. Vana October 2018: Extended LSLDP_CURV.
!   R. El Khatib 27-02-2019 Use pointer function SC2PRG to avoid bounds violation
! End Modifications
!     ------------------------------------------------------------------

USE MODEL_DYNAMICS_MOD     , ONLY : MODEL_DYNAMICS_TYPE
USE GEOMETRY_MOD           , ONLY : GEOMETRY
USE PARKIND1               , ONLY : JPIM, JPRB
USE YOMHOOK                , ONLY : DR_HOOK, JPHOOK, LHOOK
USE YOMCST                 , ONLY : TCST
USE YOMRIP                 , ONLY : TRIP
USE EINT_MOD               , ONLY : SL_STRUCT
USE FIELD_VARIABLES_MOD    , ONLY : GEOMETRY_VARIABLES
USE CPG_SL1_TYPE_MOD       , ONLY : CPG_SL1F_TYPE
USE CPG_SL2_TYPE_MOD       , ONLY : CPG_SL2_TYPE
USE CPG_SL_MASK_TYPE_MOD   , ONLY : CPG_SL_MASK_TYPE
USE FIELD_ARRAY_MODULE
USE CPG_OPTS_TYPE_MOD      , ONLY : CPG_OPTS_TYPE, CPG_BNDS_TYPE

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)            ,INTENT(IN)     :: YDGEOMETRY
TYPE(GEOMETRY_VARIABLES)  ,INTENT(IN)     :: YDGEOMVARS
TYPE(TCST)                ,INTENT(IN)     :: YDCST
TYPE(MODEL_DYNAMICS_TYPE) ,INTENT(IN)     :: YDML_DYN
TYPE(TRIP)                ,INTENT(IN)     :: YDRIP
TYPE(CPG_OPTS_TYPE)       ,INTENT(IN)     :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)       ,INTENT(IN)     :: YDCPG_BNDS
TYPE(SL_STRUCT)           ,INTENT(IN)     :: YDSL
TYPE(CPG_SL1F_TYPE)       ,INTENT(IN)     :: YDCPG_SL1
TYPE(CPG_SL2_TYPE)        ,INTENT(IN)     :: YDCPG_SL2
TYPE(CPG_SL_MASK_TYPE)    ,INTENT(INOUT)  :: YDMASK_SL2
TYPE(FIELD_2IM_ARRAY)     ,INTENT(INOUT)  :: YDA_KVSEPC
TYPE(FIELD_2IM_ARRAY)     ,INTENT(INOUT)  :: YDA_KVSEPL
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PSCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTSCO%NDIM)
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PCCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTCCO%NDIM)
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PUF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PVF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)           ,INTENT(OUT)    :: PWF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
INTEGER(KIND=JPIM)        ,INTENT(INOUT)  :: KL0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3)
INTEGER(KIND=JPIM)        ,INTENT(INOUT)  :: KLH0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3)
INTEGER(KIND=JPIM)        ,INTENT(INOUT)  :: KLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PLSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTLSCAW%NDIM)
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PRSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTRSCAW%NDIM)

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: IHVI
INTEGER(KIND=JPIM) :: IROT
INTEGER(KIND=JPIM) :: ISTESB(YDGEOMETRY%YRDIM%NPROMA)
INTEGER(KIND=JPIM) :: ISTEST(YDGEOMETRY%YRDIM%NPROMA)
INTEGER(KIND=JPIM) :: ITIP
INTEGER(KIND=JPIM) :: JITER
INTEGER(KIND=JPIM) :: JLEV
INTEGER(KIND=JPIM) :: JROF
INTEGER(KIND=JPIM) :: IL0A (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3)
INTEGER(KIND=JPIM) :: IDEP (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
INTEGER(KIND=JPIM) :: INOWENO (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

LOGICAL :: LLINTV
LOGICAL :: LLO,LSMOOTH
LOGICAL :: LLSLHD, LLSLHDQUAD

REAL(KIND=JPRB) :: ZDTS22, ZDTS62, ZDTSA
REAL(KIND=JPRB) :: ZINT
REAL(KIND=JPRB) :: ZLEVB, ZLEVO, ZLEVT
REAL(KIND=JPRB) :: ZNOR2

REAL(KIND=JPRB) :: ZPU, ZPV
REAL(KIND=JPRB) :: ZSPHSV
REAL(KIND=JPRB) :: Z2COPHI
REAL(KIND=JPRB), PARAMETER :: ZEPS = 1.E-6_JPRB
REAL(KIND=JPRB) :: ZVETAON, ZVETAOX

REAL(KIND=JPRB) :: ZXF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZYF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZZF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZWF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZWFSM(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZWFASM(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTSCO%NDIM)
REAL(KIND=JPRB) :: ZPLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)


! unused arguments in call to LARCINA:
! a) input arrays - will not be used since LSLHD was set to .FALSE.
!    in LAPINEA before call to LARMES/ELARMES => dimensions can be
!    contracted
REAL(KIND=JPRB) :: ZUN_KAPPA(1),ZUN_KAPPAT(1),ZUN_KAPPAM(1),ZUN_KAPPAH(1)

REAL(KIND=JPRB) :: ZVDISP_1,ZZ,ZVDISP_2,ZVDISP,ZPU0,ZPV0
REAL(KIND=JPRB) :: ZWF_2(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG),&
 & ZWO_2(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZHVETAON,ZHVETAOX,ZFN,ZFX,ZEF,ZEO

REAL(KIND=JPRB) :: ZX0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZY0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZZ0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "larcina.intfb.h"
#include "laismoa.intfb.h"
#include "lamaxwind.intfb.h"
#include "latrajout.intfb.h"
#include "mcoords.intfb.h"
#include "rotuv.intfb.h"
#include "rotuvgeo.intfb.h"
#include "cart2local.intfb.h"
#include "local2cart.intfb.h"

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LARMES',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV, YDSTA=>YDGEOMETRY%YRSTA, YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA,     &
& YDDYN=>YDML_DYN%YRDYN,YDDYNA=>YDML_DYN%YRDYNA, YDPTRSLB1=>YDML_DYN%YRPTRSLB1, YDPTRSLB2=>YDML_DYN%YRPTRSLB2,&
& YDSCO=>YDML_DYN%YYTSCO,YDCCO=>YDML_DYN%YYTCCO,GEMU=>YDGEOMVARS%GEMU%P,GSQM2=>YDGEOMVARS%GSQM2%P,&
& GECLO=>YDGEOMVARS%GECLO%P,GESLO=>YDGEOMVARS%GESLO%P,GNORDM=>YDGEOMVARS%GNORDM%P,&
& GNORDL=>YDGEOMVARS%GNORDL%P)

ASSOCIATE(NPROMA=>YDDIM%NPROMA, NFLEN=>YDDIMV%NFLEN, NFLEVG=>YDDIMV%NFLEVG, NFLSA=>YDDIMV%NFLSA, &
& LFINDVSEP=>YDDYN%LFINDVSEP, LSVTSM=>YDDYN%LSVTSM, NITMP=>YDDYN%NITMP, RPRES_SVTSM=>YDDYN%RPRES_SVTSM, &
& VETAON=>YDDYN%VETAON, VETAOX=>YDDYN%VETAOX, LSLDP_CURV=>YDDYN%LSLDP_CURV,  &
& RDTS22=>YDRIP%RDTS22, RDTS62=>YDRIP%RDTS62, RDTSA=>YDRIP%RDTSA, RTDT=>YDRIP%RTDT, STPREH=>YDSTA%STPREH, &
& KST=>YDCPG_BNDS%KIDIA, KEND=>YDCPG_BNDS%KFDIA)

!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS AND TESTS.
!              --------------------------------------

!*       1.1   Miscellaneous preliminary initialisations.

! in practical LLO.OR.LELTRA should now be always T for SL2TL.
IF (YDDYNA%LTWOTL) THEN
  LLO=.NOT.YDDYNA%LELTRA
ELSE
  LLO=.FALSE.
ENDIF

IF(LLO.OR.YDDYNA%LELTRA) THEN
  ZDTS22=RDTS22*4._JPRB
  ZDTSA=RDTSA*2._JPRB
  ZDTS62=RDTS62*4._JPRB
ELSE
  ! this case may occur only for SL3TL scheme.
  IF (YDDYNA%LTWOTL) CALL ABOR1(' LARMES 1.2 ')
  ZDTS22=RDTS22
  ZDTSA=RDTSA
  ZDTS62=RDTS62
ENDIF

! deactivate computation of SLHD weights
LLSLHD     =.FALSE.
LLSLHDQUAD =.FALSE.

LSMOOTH = LSVTSM.AND.(YDCPG_OPTS%NCONF == 1.OR.YDCPG_OPTS%NCONF == 302)

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LARMES {

!*       1.2   Test that wind is not too strong.

CALL LAMAXWIND (YDML_DYN, YDCST, NPROMA, NFLEVG, KST, KEND, YDCPG_SL2%URL, YDCPG_SL2%VRL, &
              & YDGEOMVARS%RCOLON%P, YDGEOMVARS%GEMU%P)

!*       1.3   Computation of weights for smooth interpolation at arrival point.

IF(LSVTSM) THEN
  !*     SMOOTH INTERPOLATION AT ARRIVAL POINT
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      ZSCO(JROF,JLEV,YDSCO%M_SINLA)=YDGEOMVARS%GEMU%P(JROF)-ZEPS
      ZSCO(JROF,JLEV,YDSCO%M_COSCO)=YDGEOMVARS%GSQM2%P(JROF)
      ZSCO(JROF,JLEV,YDSCO%M_SINCO)=ZEPS
      ZSCO(JROF,JLEV,YDSCO%M_COPHI)=1.0_JPRB-ZEPS
      ZPLEV(JROF,JLEV)=YDVETA%VETAF(JLEV)
    ENDDO
  ENDDO
  IHVI=0
  IROT=0
  ITIP=1
  LLINTV=.FALSE.
  CALL LARCINA(YDGEOMETRY,YDCST,YDML_DYN,KST,KEND,YDSL,IHVI,LFINDVSEP,LLSLHD,LLSLHDQUAD,LLINTV,&
   & ITIP,IROT,.FALSE.,&
   & ZSCO,ZPLEV,&
   & ZUN_KAPPA,ZUN_KAPPAT,ZUN_KAPPAM,ZUN_KAPPAH,&
   & YDCPG_SL2%STDDISU,YDCPG_SL2%STDDISV,YDCPG_SL2%STDDISW,&
   & YDCPG_SL1%UR0%P,YDCPG_SL1%VR0%P,YDCPG_SL1%ZR0%P,YDCPG_SL1%WRA%P,&
   & YDMASK_SL2%P,YDA_KVSEPC%P,YDA_KVSEPL%P,PCCO,&
   & PUF,PVF,ZZF,ZWF,ZWFASM,&
   & IL0A,KLH0,KLEV,PLSCAW,PRSCAW,IDEP,INOWENO,&
   & YDGEOMVARS%RCOLON%P,YDGEOMVARS%RSILON%P,YDGEOMVARS%GECLO%P,YDGEOMVARS%GEMU%P,&
   & YDGEOMVARS%GESLO%P,YDGEOMVARS%GSQM2%P)
  CALL LAISMOA(YDSL%NASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,&
   & NFLEN,PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLO),IL0A,PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDVER),YDCPG_SL1%WRA%P,ZWFASM)  
ENDIF

!     ------------------------------------------------------------------

!*       2.    ITERATIONS.
!              -----------


ZLEVT=YDVETA%VETAF(0)
ZLEVB=YDVETA%VETAF(NFLEVG+1)
ZVETAON=(1.0_JPRB-VETAON)*YDVETA%VETAH(0)+VETAON*YDVETA%VETAF(1)
ZVETAOX=(1.0_JPRB-VETAOX)*YDVETA%VETAH(NFLEVG)+VETAOX*YDVETA%VETAF(NFLEVG)
IF (YDDYNA%LRALTVDISP) THEN
  ZHVETAON=0.5_JPRB*ZVETAON
  ZHVETAOX=1.0_JPRB-0.5_JPRB*(1.0_JPRB-ZVETAOX)
ENDIF

DO JITER=1,NITMP

  !*       2.1   DETERMINATION OF THE MEDIUM POINT "M" OR THE ORIGIN POINT "O".

  ! Computation of the norm of the real wind vector.
  ! Computation of the angle (PHI=DT . NORM OF V ON RADIUS)**2
  ! then computation of the coordinates of the medium point "M".
  ! If (LLO=T or LELTRA=T) the origin point "O" is computed
  ! instead of the medium point "M".

  ISTEST(:)=0
  ISTESB(:)=0

  IF (JITER == 1) THEN
    ! * computations on horizontal plans.
    IF (LSLDP_CURV.AND..NOT.YDDYNA%LELTRA) THEN
      ! convert arrival point wind to cartesian space
      CALL LOCAL2CART(NFLEVG,NPROMA,KST,KEND,GEMU,GSQM2,GECLO,GESLO,GNORDM,GNORDL,&
        &YDCPG_SL2%URL,YDCPG_SL2%VRL,ZX0,ZY0,ZZ0,LDROT=.TRUE.)
    ENDIF

    CALL MCOORDS(YDSCO,YDRIP,NFLEVG,NPROMA,KST,KEND,YDCST%RA,GEMU,GSQM2,GNORDM,GNORDL,&
      &YDCPG_SL2%URL,YDCPG_SL2%VRL,PSCO,LLO,LD2TLM=.TRUE.,LDROT=.TRUE.)

    DO JLEV=1,NFLEVG
      ! * computations on a vertical.
      IF (YDDYNA%LRALTVDISP) THEN
        ZFN = YDVETA%VETAF(JLEV)-ZHVETAON
        ZFX = ZHVETAOX-YDVETA%VETAF(JLEV)
        ZEF = 1._JPRB/ZFN+1._JPRB/ZFX
      ENDIF

      DO JROF=KST,KEND
        IF(STPREH(JLEV) > RPRES_SVTSM.OR..NOT.LSMOOTH) THEN
          ZWF(JROF,JLEV)=YDCPG_SL2%WRL(JROF,JLEV)
        ELSE
          ZWF(JROF,JLEV)=ZWFASM(JROF,JLEV)
        ENDIF
        IF (YDDYNA%LRALTVDISP) THEN
          ZVDISP_1=-RTDT*ZWF(JROF,JLEV)
          ZZ=EXP(ZVDISP_1*ZEF)*ZFN/ZFX
          ZVDISP_2=(ZHVETAON+ZHVETAOX*ZZ)/(1.0_JPRB+ZZ)-YDVETA%VETAF(JLEV)
          ZVDISP=SIGN(1.0_JPRB,ZVDISP_1)*MIN(ABS(ZVDISP_1),ABS(ZVDISP_2))
          ZLEVO=YDVETA%VETAF(JLEV)+ZVDISP
        ELSE
          ZLEVO=YDVETA%VETAF(JLEV)-RTDT*ZWF(JROF,JLEV)
        ENDIF
        ISTEST(JROF)=ISTEST(JROF)-MIN(0,MAX(-1,NINT(ZLEVO-ZLEVT-0.5_JPRB)))
        ISTESB(JROF)=ISTESB(JROF)-MIN(0,MAX(-1,NINT(ZLEVB-ZLEVO-0.5_JPRB)))
        ZLEVO=MIN(ZVETAOX,MAX(ZVETAON,ZLEVO))
        IF(LLO.OR.YDDYNA%LELTRA) THEN
          PLEV(JROF,JLEV)=ZLEVO
        ELSE
          PLEV(JROF,JLEV)=0.5_JPRB*(ZLEVO+YDVETA%VETAF(JLEV))
        ENDIF
      ENDDO
    ENDDO

  ELSE

    IF (LSLDP_CURV) THEN
      IF (LLO) THEN
        ! Averaging wind to Midpoint in Cartesian space
        DO JLEV=1,NFLEVG
          !DIR$ IVDEP
          DO JROF=KST,KEND
            ZXF(JROF,JLEV)=0.5_JPRB*(PUF(JROF,JLEV)+ZX0(JROF,JLEV))
            ZYF(JROF,JLEV)=0.5_JPRB*(PVF(JROF,JLEV)+ZY0(JROF,JLEV))
            ZZF(JROF,JLEV)=0.5_JPRB*(ZZF(JROF,JLEV)+ZZ0(JROF,JLEV))
          ENDDO
        ENDDO
      ENDIF

      ! Convert Midpoint wind back to lat,lon at Arrival point
      CALL CART2LOCAL(NFLEVG,NPROMA,KST,KEND,GEMU,GSQM2,GECLO,GESLO,ZXF,ZYF,ZZF,PUF,PVF)
    ELSE
      CALL ROTUV(NFLEVG,NPROMA,KST,KEND,PUF,PVF,PCCO(:,:,YDCCO%M_RQX),&
        &PCCO(:,:,YDCCO%M_RQY),PUF,PVF,LDIR=.FALSE.)

      IF (LLO) THEN
        DO JLEV=1,NFLEVG
          !DIR$ IVDEP
          DO JROF=KST,KEND
            ZPU = PUF(JROF,JLEV)
            ZPV = PVF(JROF,JLEV)
            PUF(JROF,JLEV) = 0.5_JPRB*(ZPU+YDCPG_SL2%URL(JROF,JLEV)*YDGEOMVARS%GNORDM%P(JROF)&
             & -YDCPG_SL2%VRL(JROF,JLEV)*YDGEOMVARS%GNORDL%P(JROF))
            PVF(JROF,JLEV) = 0.5_JPRB*(ZPV+YDCPG_SL2%URL(JROF,JLEV)*YDGEOMVARS%GNORDL%P(JROF)&
             & +YDCPG_SL2%VRL(JROF,JLEV)*YDGEOMVARS%GNORDM%P(JROF))
          ENDDO
        ENDDO
      ENDIF
    ENDIF

    CALL MCOORDS(YDSCO,YDRIP,NFLEVG,NPROMA,KST,KEND,YDCST%RA,GEMU,GSQM2,GNORDM,GNORDL,&
      &PUF,PVF,PSCO,LLO,LD2TLM=.TRUE.)

    DO JLEV=1,NFLEVG
      ! * computations on a vertical.
      IF (YDDYNA%LRALTVDISP) THEN
        ZFN = YDVETA%VETAF(JLEV)-ZHVETAON
        ZFX = ZHVETAOX-YDVETA%VETAF(JLEV)
        ZEF = 1._JPRB/ZFN+1._JPRB/ZFX
      ENDIF

      IF(LLO) THEN
        DO JROF=KST,KEND
          IF(STPREH(JLEV) > RPRES_SVTSM.OR..NOT.LSMOOTH) THEN
            IF (YDDYNA%LRALTVDISP) THEN
              ZWF_2(JROF,JLEV)=ZWF(JROF,JLEV)
              ZWO_2(JROF,JLEV)=YDCPG_SL2%WRL(JROF,JLEV)
            ENDIF
            ZWF(JROF,JLEV)=0.5_JPRB*(ZWF(JROF,JLEV)+YDCPG_SL2%WRL(JROF,JLEV))
          ELSE
            IF (YDDYNA%LRALTVDISP) THEN
              ZWF_2(JROF,JLEV)=ZWFASM(JROF,JLEV)
              ZWO_2(JROF,JLEV)=ZWFSM(JROF,JLEV)
            ENDIF
            ZWF(JROF,JLEV)=0.5_JPRB*(ZWFSM(JROF,JLEV)+ZWFASM(JROF,JLEV))
          ENDIF
        ENDDO
      ENDIF

      IF( YDDYNA%LSLDIA.AND.(JITER==NITMP) ) THEN
        ! trajectory vertical velocity
        PWF(KST:KEND,JLEV)=ZWF(KST:KEND,JLEV)
      ENDIF

      DO JROF=KST,KEND
        IF (YDDYNA%LRALTVDISP) THEN
          ZVDISP_1=-RTDT*ZWF(JROF,JLEV)
          ZEO = 1._JPRB/(PLEV(JROF,JLEV)-ZHVETAON)+1._JPRB/(ZHVETAOX-PLEV(JROF,JLEV))
          IF (LLO) THEN
            ZZ=EXP(-0.5_JPRB*RTDT*ZWF_2(JROF,JLEV)*ZEF-0.5_JPRB*RTDT*ZWO_2(JROF,JLEV)*ZEO)*ZFN/ZFX
          ELSE
            ZZ=EXP(-RTDT*ZWF(JROF,JLEV)*ZEO)*ZFN/ZFX
          ENDIF
          ZVDISP_2=(ZHVETAON+ZHVETAOX*ZZ)/(1.0_JPRB+ZZ)-YDVETA%VETAF(JLEV)
          ZVDISP=SIGN(1.0_JPRB,ZVDISP_1)*MIN(ABS(ZVDISP_1),ABS(ZVDISP_2))
          ZLEVO=YDVETA%VETAF(JLEV)+ZVDISP
        ELSE
          ZLEVO=YDVETA%VETAF(JLEV)-RTDT*ZWF(JROF,JLEV)
        ENDIF
        ISTEST(JROF)=ISTEST(JROF)-MIN(0,MAX(-1,NINT(ZLEVO-ZLEVT-0.5_JPRB)))
        ISTESB(JROF)=ISTESB(JROF)-MIN(0,MAX(-1,NINT(ZLEVB-ZLEVO-0.5_JPRB)))
        ZLEVO=MIN(ZVETAOX,MAX(ZVETAON,ZLEVO))
        IF(LLO.OR.YDDYNA%LELTRA) THEN
          PLEV(JROF,JLEV)=ZLEVO
        ELSE
          PLEV(JROF,JLEV)=0.5_JPRB*(ZLEVO+YDVETA%VETAF(JLEV))
        ENDIF
      ENDDO
    ENDDO

    IF (JITER == NITMP) THEN
      ! * save (puf,pvf) in (puf,pvf) with a rotation.
      CALL ROTUVGEO(NFLEVG,NPROMA,KST,KEND,GNORDM,GNORDL,PUF,PVF,PUF,PVF,LDIR=.FALSE.)
    ENDIF
  ENDIF

  IF (JITER == NITMP) THEN
    CALL LATRAJOUT (YDCST, YDDYNA%LRPRSLTRJ, NPROMA, NFLEVG, KST, KEND, ISTEST, &
                  & ISTESB, ZWF, YDGEOMVARS%RCOLON%P, YDGEOMVARS%GEMU%P)
  ENDIF

  !*       2.2   DETERMINATION OF THE wind AT "M" OR "O".

  IF(JITER /= NITMP) THEN

    ! If (LLO=T or LELTRA=T)
    ! and JITER < NITMP wind is interpolated at
    ! the origin point "O" instead of at the medium point "M".
    ! In the other cases the wind is interpolated at "M".

    IHVI=0
    IF (LSLDP_CURV) THEN
      IROT=0
    ELSE
      IROT=1
    ENDIF
    ITIP=1
    LLINTV=.TRUE.

    CALL LARCINA(YDGEOMETRY,YDCST,YDML_DYN,KST,KEND,YDSL,IHVI,LFINDVSEP,LLSLHD,LLSLHDQUAD,LLINTV,&
     & ITIP,IROT,.FALSE.,&
     & PSCO,PLEV,&
     & ZUN_KAPPA,ZUN_KAPPAT,ZUN_KAPPAM,ZUN_KAPPAH,&
     & YDCPG_SL2%STDDISU,YDCPG_SL2%STDDISV,YDCPG_SL2%STDDISW,&
     & YDCPG_SL1%UR0%P,YDCPG_SL1%VR0%P,YDCPG_SL1%ZR0%P,YDCPG_SL1%WR0%P,&
     & YDMASK_SL2%P,YDA_KVSEPC%P,YDA_KVSEPL%P,PCCO,&
     & PUF,PVF,ZZF,ZWF,ZWFSM,&
     & KL0,KLH0,KLEV,PLSCAW,PRSCAW,IDEP,INOWENO,&
     & YDGEOMVARS%RCOLON%P,YDGEOMVARS%RSILON%P,YDGEOMVARS%GECLO%P,YDGEOMVARS%GEMU%P,YDGEOMVARS%GESLO%P,YDGEOMVARS%GSQM2%P)

  ENDIF

ENDDO

!     ------------------------------------------------------------------

!*       3.    COMPUTATION OF THE ORIGIN POINT "O" COORDINATES.
!              ------------------------------------------------

IF (.NOT.(LLO.OR.YDDYNA%LELTRA)) THEN
  ! this case may occur only for SL3TL scheme.
  IF (YDDYNA%LTWOTL) CALL ABOR1(' LARMES 3 ')

  ZVETAON=(1.0_JPRB-VETAON)*YDVETA%VETAH(0)+VETAON*YDVETA%VETAF(1)
  ZVETAOX=(1.0_JPRB-VETAOX)*YDVETA%VETAH(NFLEVG)+VETAOX*YDVETA%VETAF(NFLEVG)

  DO JLEV=1,NFLEVG
!DIR$ IVDEP
    DO JROF=KST,KEND

      ! computations on horizontal plans.

      Z2COPHI = 2.0_JPRB*PSCO(JROF,JLEV,YDSCO%M_COPHI)
      PSCO(JROF,JLEV,YDSCO%M_SINLA)=Z2COPHI*PSCO(JROF,JLEV,YDSCO%M_SINLA)-&
        &YDGEOMVARS%GEMU%P(JROF)
      PSCO(JROF,JLEV,YDSCO%M_SINCO)=Z2COPHI*PSCO(JROF,JLEV,YDSCO%M_SINCO)
      PSCO(JROF,JLEV,YDSCO%M_COSCO)=Z2COPHI*PSCO(JROF,JLEV,YDSCO%M_COSCO)-&
        &YDGEOMVARS%GSQM2%P(JROF)

      ! computations on a vertical.

      PLEV(JROF,JLEV)=2.0_JPRB*PLEV(JROF,JLEV)-YDVETA%VETAF(JLEV)
      PLEV(JROF,JLEV)=MAX(ZVETAON,MIN(ZVETAOX,PLEV(JROF,JLEV)))

      ! Computation of the angle <0,G>.

      PSCO(JROF,JLEV,YDSCO%M_COPHI)=Z2COPHI*PSCO(JROF,JLEV,YDSCO%M_COPHI)-1.0_JPRB

    ENDDO
  ENDDO

ENDIF

!$ACDC }

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LARMES',1,ZHOOK_HANDLE)

END SUBROUTINE LARMES

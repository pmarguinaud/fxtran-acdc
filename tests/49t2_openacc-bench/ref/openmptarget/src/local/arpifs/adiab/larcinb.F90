#ifdef RS6K
@PROCESS NOCHECK
#endif
SUBROUTINE LARCINB(YDGEOMETRY,YDVARS,&
 ! --- INPUT ----------------------------------------------------------
 & YDLDDH,YDMDDH,YDML_GCONF,YDML_DYN,YDCPG_OPTS,YDCPG_BNDS,KASLB1,KHVI,LDSLPHY,LD2TLFF1,&
 & KL0,KLH0,PLSCAW,PRSCAW,YDCPG_SL1,KNOWENO,&
 ! --- INOUT ----------------------------------------------------------
 & YDCPG_DDH_TND,&
 ! --- OUTPUT --------------------------------------------------------
 & YDCPG_GMVF,YDCPG_GMVF_SI,YDCPG_GMVF_NL,YDCPG_GFLF,PDP,PCF,PCF_SI,PCF_NL,&
 & PUFZ,PVFZ,PUPF,PVPF,PTPF,YDCPG_GFLPF,PDBBCF,PDPHIF,PGWSF)

!$ACDC pointerparallel    


!**** *LARCINB    -  semi-LAgrangian scheme:(Interpolation)

!     Purpose.
!     --------
!       Does the interpolations necessary in the semi-Lagrangian scheme.

!**   Interface.
!     ----------
!        *CALL* *LARCINB(......)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST         - first element where computations are performed.
!          KEND       - depth of work.
!          KASLB1      - horizontal dimension of SL fields
!          KHVI        - 1/0: Cubic Hermite vert. interp. are needed/not needed
!          LDSLPHY     - split physics in semi lagrangian mode.
!          LD2TLFF1    - .T./.F.: Refined treatement of (2*Omega Vec r) at
!                        the origin point when there is t-dt (or t in SL2TL)
!                        physics / Other cases.
!          KL0         - index of the four western points
!                        of the 16 points interpolation grid.
!          KLH0        - second value of index of the four western points
!                        of the 16 points interpolation grid if needed.
!          PLSCAW      - linear weights (distances) for interpolations.
!          PRSCAW      - non-linear weights for interpolations.
!          KIBL        - index into YRCSGEOM/YRGSGEOM instances in YDGEOMETRY
!          KNOWENO     - boundary condition treatment for WENO

!        INOUT:
!          PGMVTNDSL   - cf. GMVTNDSL_DDH in YOMGPDDH
!          PGFLTNDSL   - cf. GFLTNDSL_DDH in YOMGPDDH
!          PGMVTNDSI   - cf. GMVTNDSI_DDH in YOMGPDDH

!        OUTPUT:
!          PGMVF       - Interpolated quantity at O for GMV.
!          PGMVF_SI    - interpolated separate linear terms at O for GMV.
!          PGMVF_NL    - interpolated NL term useful for SETTLS/PC scheme
!          PDP         - Interpolated 2D term at O for continuity equation.
!          PGFLF       - Interpolated quantity at O in the GFL fields.
!          PCF         - Interpolated 3D quantity at O in the cont. eqn.
!          PCF_SI      - Interpolated 3D quantity at O in the cont. eqn (SI).
!          PCF_NL      - Interpolated 3D quantity at O in the cont. eqn (LSETTLS/LPC_CHEAP).
!          PUFZ and PVFZ for SL2TL scheme:
!          PUFZ        - Interpolated U-wind at O for L2TLFF=.T. option.
!          PVFZ        - Interpolated V-wind at O for L2TLFF=.T. option.
!          PUPF        - Interpolated quantity at O in U-wind eqn
!                        for split physics (if LDSLPHY=.T. only).
!          PVPF        - Interpolated quantity at O in V-wind eqn
!                        for split physics (if LDSLPHY=.T. only).
!          PTPF        - Interpolated quantity at O in T eqn
!                        for split physics (if LDSLPHY=.T. only).
!          PGFLPF      - Interpolated quantity at O in 
!                        unified_treatment grid-point fields.
!          PDBBCF      - interpolated "(dGw_s/dt)/(pressure depth)_L" term at O.
!          PDPHIF      - interpolated "(p/m Rd T)_L" term at O.
!          PGWSF       - interpolated "(Gw_s)" term at O(L).

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!        Called by LAPINEB.
!        Calls LAITLI,LAIDDI,
!              LAITRE_GMV,LAITRE_GFL

!     Reference.
!     ----------

!     Author.
!     -------
!      K. YESSAD, after the subroutine LAGINT0 written by Maurice IMBARD
!      Alain CRAPLET and Michel ROCHAS  METEO FRANCE/EERM/CRMD
!      Original : JUNE 1991.

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   30-Jun-2008 J. Masek   Dataflow for new SLHD interpolators.
!   17-Sep-2008 F. Vana  updating arguments for LAITRE_GFL
!   K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!   K. Yessad Nov 2008: interpolation routines: merge QM with not-QM version.
!   F. Vana  15-Oct-2009: option NSPLTHOI
!   K. Yessad Nov 2009: rationalisation of dummy argument interfaces: PGMVF
!   K. Yessad (Nov 2009): prune lpc_old.
!   K. Yessad (Oct 2010): remove useless LFINDVSEP calculations.
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   F. Vana  21-Feb-2011: horiz. turbulence, gp diff to physics (NSPLTHOI=1), N[x]LAG=4 and LTDIABLIN.
!   G. Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!   M. Diamantakis (Jun 2012): extra argument for quasi-monotone mass fixer
!   S. Malardel (Nov 2013): COMAD weights for SL interpolations
!   K. Yessad (July 2014): Rename some variables.
!   F. Vana  13-Feb-2014 Distinguish between momentum and heat variables in SLHD.
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (June 2017): Introduce NHQE model.
!   K. Yessad (June 2017): alternate "cheap" FULL PC.
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   M. Diamantakis (June 2018): Don't interpolate GFL variable advected on Atlas grid 
!   J. Vivoda (July 2018): LSETTLS with LPC_CHEAP.
!   F. Vana (Oct-2018): Better control on SLPHY
!   F. Vana 20-Feb-2019: Vertical quintic interpolation
!   F. Vana   25-Jun-2019: QM version for cont equation
!   F. Vana   11-Jul-2019: Option LRHS_CURV
! End Modifications
!------------------------------------------------------------------------------

USE MODEL_DYNAMICS_MOD     , ONLY : MODEL_DYNAMICS_TYPE
USE MODEL_GENERAL_CONF_MOD , ONLY : MODEL_GENERAL_CONF_TYPE
USE GEOMETRY_MOD           , ONLY : GEOMETRY
USE PARKIND1               , ONLY : JPIM, JPRB
USE YOMHOOK                , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMLDDH                , ONLY : TLDDH
USE YOMMDDH                , ONLY : TMDDH
USE FIELD_VARIABLES_MOD    , ONLY : FIELD_VARIABLES
USE CPG_SL1_TYPE_MOD       , ONLY : CPG_SL1F_TYPE
USE CPG_DDH_TND_TYPE_MOD   , ONLY : CPG_DDH_TND_TYPE
USE CPG_GMV_TYPE_MOD       , ONLY : CPG_GMV_TYPE
USE CPG_GFL_TYPE_MOD       , ONLY : CPG_GFL_TYPE, CPG_GFLSL_TYPE
USE CPG_OPTS_TYPE_MOD      , ONLY : CPG_OPTS_TYPE, CPG_BNDS_TYPE

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)                ,INTENT(IN)     :: YDGEOMETRY
TYPE(FIELD_VARIABLES)         ,INTENT(INOUT)  :: YDVARS
TYPE(TLDDH)                   ,INTENT(IN)     :: YDLDDH
TYPE(TMDDH)                   ,INTENT(IN)     :: YDMDDH
TYPE(MODEL_DYNAMICS_TYPE)     ,INTENT(IN)     :: YDML_DYN
TYPE(MODEL_GENERAL_CONF_TYPE) ,INTENT(IN)     :: YDML_GCONF
INTEGER(KIND=JPIM)            ,INTENT(IN)     :: KHVI 
TYPE(CPG_OPTS_TYPE)           ,INTENT(IN)     :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)           ,INTENT(IN)     :: YDCPG_BNDS
INTEGER(KIND=JPIM)            ,INTENT(IN)     :: KASLB1
LOGICAL                       ,INTENT(IN)     :: LDSLPHY 
LOGICAL                       ,INTENT(IN)     :: LD2TLFF1 
INTEGER(KIND=JPIM)            ,INTENT(IN)     :: KL0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3) 
INTEGER(KIND=JPIM)            ,INTENT(IN)     :: KLH0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PLSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTLSCAW%NDIM)
REAL(KIND=JPRB)               ,INTENT(IN)     :: PRSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTRSCAW%NDIM)
TYPE(CPG_SL1F_TYPE)           ,INTENT(IN)     :: YDCPG_SL1
INTEGER(KIND=JPIM)            ,INTENT(IN)     :: KNOWENO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
TYPE(CPG_DDH_TND_TYPE)        ,INTENT(INOUT)  :: YDCPG_DDH_TND
TYPE(CPG_GMV_TYPE)            ,INTENT(INOUT)  :: YDCPG_GMVF
TYPE(CPG_GMV_TYPE)            ,INTENT(INOUT)  :: YDCPG_GMVF_SI
TYPE(CPG_GMV_TYPE)            ,INTENT(INOUT)  :: YDCPG_GMVF_NL
TYPE(CPG_GFL_TYPE)            ,INTENT(INOUT)  :: YDCPG_GFLF
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PDP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PCF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PCF_SI(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PCF_NL(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PUFZ(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PVFZ(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PUPF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PVPF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PTPF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
TYPE(CPG_GFLSL_TYPE)          ,INTENT(INOUT)  :: YDCPG_GFLPF
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PDBBCF(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PDPHIF(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)               ,INTENT(OUT)    :: PGWSF(YDGEOMETRY%YRDIM%NPROMA) 
!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZGMVF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

! Storage arrays for LRHS_CURV
REAL(KIND=JPRB) :: ZGMVU(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGMVV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGMVZ(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGMVFU(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGMVFV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGMVFZ(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

REAL(KIND=JPRB) :: ZGFLF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

INTEGER(KIND=JPIM) :: JLEV, JROF, JFLD, IP, IPSPL, IFLDN, IFLDX
INTEGER(KIND=JPIM) :: IWK, IWNT, IWKM, IWKH, IWKGFL, IQM,IDLO,ICLO,ICLA
CHARACTER(LEN=12) :: CLINT
LOGICAL :: LLNOSLHD, LLHOI2,LLCOMAD
LOGICAL :: LL1,LL2,LL3,LLMOM,LL_LIN

!     ------------------------------------------------------------------

#include "laitlif.intfb.h"
#include "laiddi.intfb.h"
#include "laidli.intfb.h"
#include "laitre_gmv.intfb.h"
#include "laitre_gfl.intfb.h"
#include "cart2local.intfb.h"

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LARCINB',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDDYN=>YDML_DYN%YRDYN, YGFL=>YDML_GCONF%YGFL, &
& YDTLSCAW=>YDML_DYN%YYTLSCAW,YDTRSCAW=>YDML_DYN%YYTRSCAW, YDDYNA=>YDML_DYN%YRDYNA, YDGEOMVARS => YDVARS%GEOMETRY)

ASSOCIATE(YCOMP=>YGFL%YCOMP, NUMFLDS_SL1=>YGFL%NUMFLDS_SL1, NUMFLDS_SPL=>YGFL%NUMFLDS_SPL, NPROMA=>YDDIM%NPROMA,  &
& NFLEN=>YDDIMV%NFLEN, NFLEVG=>YDDIMV%NFLEVG, NFLSA=>YDDIMV%NFLSA, RRINTOPT=>YDDYN%RRINTOPT, LQMHP=>YDDYN%LQMHP,  &
& LQMHPD=>YDDYN%LQMHPD, LQMHT=>YDDYN%LQMHT, LQMHVD=>YDDYN%LQMHVD, LQMHW=>YDDYN%LQMHW, LQMP=>YDDYN%LQMP,           &
& LQMPD=>YDDYN%LQMPD, LQMT=>YDDYN%LQMT, LQMVD=>YDDYN%LQMVD, LQMW=>YDDYN%LQMW, LRHS_CURV=>YDDYN%LRHS_CURV,         &
& LVWENO_W=>YDDYN%LVWENO_W,LVWENO_T=>YDDYN%LVWENO_T,LVWENO_SP=>YDDYN%LVWENO_SP, LVWENO_SPD=>YDDYN%LVWENO_SPD,     &
& LVWENO_SVD=>YDDYN%LVWENO_SVD, WENO_ALPHA_W=>YDDYN%WENO_ALPHA_W, WENO_ALPHA_T=>YDDYN%WENO_ALPHA_T,               &
& WENO_ALPHA_SP=>YDDYN%WENO_ALPHA_SP, WENO_ALPHA_SPD=>YDDYN%WENO_ALPHA_SPD,WENO_ALPHA_SVD=>YDDYN%WENO_ALPHA_SVD,  &
& NCURRENT_ITER=>YDDYN%NCURRENT_ITER, NSITER=>YDDYN%NSITER, NSLDIMK=>YDDYN%NSLDIMK, NSPDLAG=>YDDYN%NSPDLAG,       &
& NSPLTHOI=>YDDYN%NSPLTHOI, NSVDLAG=>YDDYN%NSVDLAG, NTLAG=>YDDYN%NTLAG, NVLAG=>YDDYN%NVLAG, NWLAG=>YDDYN%NWLAG,   &
& LCOMAD_W=>YDDYNA%LCOMAD_W,LSLHD_W=>YDDYNA%LSLHD_W,LSLHD_T=>YDDYNA%LSLHD_T, LCOMAD_SP=>YDDYNA%LCOMAD_SP,         &
& LCOMAD_SVD=>YDDYNA%LCOMAD_SVD,LSLHD_SVD=>YDDYNA%LSLHD_SVD, LSLHDHEAT=>YDDYN%LSLHDHEAT, LRSIDDH=>YDLDDH%LRSIDDH, &
& LRSLDDH=>YDLDDH%LRSLDDH, KST=>YDCPG_BNDS%KIDIA, KEND=>YDCPG_BNDS%KFDIA, GEMU=>YDGEOMVARS%GEMU%P,                &
& GSQM2=>YDGEOMVARS%GSQM2%P, GECLO=>YDGEOMVARS%GECLO%P,GESLO=>YDGEOMVARS%GESLO%P)

!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS + ALLOCATIONS.
!              ------------------------------------------

! * switch for GMV fields where SLHD is not applied
LLNOSLHD=.FALSE.

! * Pointers for interpolation weights controlling the horizontal part of 3D turbulence
!   IWNT - for those quantities being never subject to 3D turb
!   IWKM - momentum variables
!   IWKH - heat variables (for the moment all scalar variables)
IWNT=1
IWKGFL=1
IF (YDDYNA%L3DTURB) THEN
  IWKM=2
  IWKH=3
ELSE
  IWKM=1
  IWKH=1
ENDIF

IF (YDDYNA%LPC_FULL.AND.YDDYNA%LPC_CHEAP2.AND.(NCURRENT_ITER<NSITER)) THEN
  LL_LIN=.TRUE.
ELSE
  LL_LIN=YDDYNA%NQMGMV == -4
ENDIF

! copies for local procedures
IFLDN = NFLSA
IFLDX = NFLEN
!     ------------------------------------------------------------------

!*       2.    ORIGIN POINT INTERPOLATIONS FOR GMV VARIABLES.
!              ----------------------------------------------

IF (LRHS_CURV) THEN

!$ACDC ABORT {

  ! * U and V equations together
  LLMOM=.TRUE.

  ! --- part of the RHS requiring a "high order" interpolation:
  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
   & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,KL0,PLSCAW,PRSCAW,&
   & YDCPG_SL1%U9%P,ZGMVU)
  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
   & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,KL0,PLSCAW,PRSCAW,&
   & YDCPG_SL1%V9%P,ZGMVV)
  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
   & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,KL0,PLSCAW,PRSCAW,&
   & YDCPG_SL1%Z9%P,ZGMVZ)

  IF(NWLAG >= 3) THEN

    ! --- save U in some cases:
    IF (LD2TLFF1) THEN
      ! Interpolate U at the origin point and use PUFZ
      ! to store the interpolated quantities.
      CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
       & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,KL0,PLSCAW,PRSCAW,&
       & YDCPG_SL1%UR9%P,ZGMVFU)
      CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
       & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,KL0,PLSCAW,PRSCAW,&
       & YDCPG_SL1%VR9%P,ZGMVFV)
      CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
       & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,KL0,PLSCAW,PRSCAW,&
       & YDCPG_SL1%ZR9%P,ZGMVFZ)

      ! Conversion
      CALL CART2LOCAL(NFLEVG,NPROMA,KST,KEND,GEMU,GSQM2,GECLO,GESLO,ZGMVFU,ZGMVFV,ZGMVFZ,&
        &PUFZ,PVFZ)
    ELSE
      ! Save PGMVF(.,.,YGP%MU) in case of refined treatment of
      ! Coriolis term and lagged physics in 2TL scheme:

      ! Conversion
      CALL CART2LOCAL(NFLEVG,NPROMA,KST,KEND,GEMU,GSQM2,GECLO,GESLO,ZGMVU,ZGMVV,ZGMVZ,&
        &PUFZ,PVFZ)
    ENDIF

    ! --- part of the RHS requiring a trilinear interpolation:
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%U0%P,ZGMVU,LDADD=.TRUE.)
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%V0%P,ZGMVV,LDADD=.TRUE.)
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%Z0%P,ZGMVZ,LDADD=.TRUE.)

    IF (YDDYNA%LSETTLS .AND. YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER == 0) THEN
      CALL ABOR1(' LARCINB: LRHS_CURV is not yet coded for LPC_CHEAP')
    ENDIF
  ENDIF

  ! Conversion
  CALL CART2LOCAL(NFLEVG,NPROMA,KST,KEND,GEMU,GSQM2,GECLO,GESLO,ZGMVU,ZGMVV,ZGMVZ,&
    &YDCPG_GMVF%U%P,YDCPG_GMVF%V%P)

  ! --- Split physics requiring a trilinear interpolation:
  IF(LDSLPHY) THEN
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%UP9%P,ZGMVU)
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%VP9%P,ZGMVV)
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%ZP9%P,ZGMVZ)

    ! Conversion
    CALL CART2LOCAL(NFLEVG,NPROMA,KST,KEND,GEMU,GSQM2,GECLO,GESLO,ZGMVU,ZGMVV,ZGMVZ,&
      &PUPF,PVPF)
  ENDIF

!$ACDC }

ELSE  ! LRHS_CURV

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LRHS_CURV_F {

  ! STANDARD SITUATION

  ! * U-Momentum equation:
  LLMOM=.TRUE.

  ! --- part of the RHS requiring a "high order" interpolation:
  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
   & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,&
   & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%U9%P,YDCPG_GMVF%U%P)

  IF (LRSLDDH) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDCPG_DDH_TND%YSL%U%P(JROF,JLEV)=YDCPG_DDH_TND%YSL%U%P(JROF,JLEV)&
        & +YDCPG_GMVF%U%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

  IF ((NSPLTHOI /= 0).AND.(NWLAG < 4)) THEN
    ! --- part of the RHS requiring special "high order" interpolation
    IF (NSPLTHOI == 1) THEN
      LLHOI2=LLNOSLHD
      IWK=NSLDIMK
    ELSE
      LLHOI2=LSLHD_W
      IWK=IWKM
    ENDIF
    CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWK,&
     & LL_LIN,YDDYNA%LCOMAD_W,LLHOI2,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,&
     & KL0,PLSCAW,PRSCAW,YDCPG_SL1%UF9%P,YDCPG_GMVF%U%P,LDADD=.TRUE.)
  ENDIF

  IF(NWLAG >= 3) THEN
    ! --- save U in some cases:
    IF (LD2TLFF1) THEN
      ! Interpolate U at the origin point and use PUFZ
      ! to store the interpolated quantities.
      CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
       & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,&
       & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%UR9%P,PUFZ)
    ELSE
      ! Save PGMVF(.,.,YGP%MU) in case of refined treatment of
      ! Coriolis term and lagged physics in 2TL scheme:
      PUFZ(KST:KEND,1:NFLEVG)=YDCPG_GMVF%U%P(KST:KEND,1:NFLEVG)
    ENDIF

    ! --- part of the RHS requiring a trilinear interpolation:
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%U0%P,YDCPG_GMVF%U%P,LDADD=.TRUE.)

    IF (LRSIDDH) THEN
      ! Linear terms interpolated separately for DDH.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LCOMAD_W,PLSCAW,&
        &YDCPG_SL1%U9_SI%P,YDCPG_DDH_TND%YSI%U%T0,LDADD=.TRUE.)
    ENDIF

    IF (YDDYNA%LSLINL .AND. NCURRENT_ITER == 0) THEN
      ! Linear terms interpolated separately when required.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LCOMAD_W,PLSCAW,&
        &YDCPG_SL1%U9_SI%P,YDCPG_GMVF_SI%U%P)
    ENDIF

    IF (YDDYNA%LSETTLS .AND. YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER == 0) THEN
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LCOMAD_W,PLSCAW,&
        &YDCPG_SL1%U9_NL%P,YDCPG_GMVF_NL%U%P)
    ENDIF
  ENDIF

  ! --- Split physics requiring a trilinear interpolation:
  IF(LDSLPHY) THEN
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%UP9%P,PUPF)
  ENDIF

  ! * V-Momentum equation:
  LLMOM=.TRUE.

  ! --- part of the RHS requiring a "high order" interpolation:
  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
   & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,&
   & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%V9%P,YDCPG_GMVF%V%P)

  IF (LRSLDDH) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDCPG_DDH_TND%YSL%V%P(JROF,JLEV)=YDCPG_DDH_TND%YSL%V%P(JROF,JLEV)&
        & +YDCPG_GMVF%V%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

  IF ((NSPLTHOI /= 0).AND.(NWLAG < 4)) THEN
    ! --- part of the RHS requiring special "high order" interpolation
    IF (NSPLTHOI == 1) THEN
      LLHOI2=LLNOSLHD
      IWK=NSLDIMK
    ELSE
      LLHOI2=LSLHD_W
      IWK=IWKM
    ENDIF
    CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWK,&
     & LL_LIN,YDDYNA%LCOMAD_W,LLHOI2,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,KNOWENO,&
     & KL0,PLSCAW,PRSCAW,YDCPG_SL1%VF9%P,YDCPG_GMVF%V%P,LDADD=.TRUE.)
  ENDIF

  IF(NWLAG >= 3) THEN
    ! --- save V in some cases:
    IF (LD2TLFF1) THEN
      ! Interpolate V at the origin point and use PVFZ
      ! to store the interpolated quantities.
      CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
       & LL_LIN,YDDYNA%LCOMAD_W,YDDYNA%LSLHD_W,LVWENO_W,WENO_ALPHA_W,LLMOM,LQMW,LQMHW,&
       & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%VR9%P,PVFZ)
    ELSE
      ! Save PGMVF(.,.,YGP%MV) in case of refined treatment of
      ! Coriolis term and lagged physics in 2TL scheme:
      PVFZ(KST:KEND,1:NFLEVG)=YDCPG_GMVF%V%P(KST:KEND,1:NFLEVG)
    ENDIF

    ! --- part of the RHS requiring a trilinear interpolation:
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%V0%P,YDCPG_GMVF%V%P,LDADD=.TRUE.)

    IF (LRSIDDH) THEN
      ! Linear terms interpolated separately for DDH.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
        &YDCPG_SL1%V9_SI%P,YDCPG_DDH_TND%YSI%V%T0,LDADD=.TRUE.)
    ENDIF

    IF (YDDYNA%LSLINL .AND. NCURRENT_ITER == 0) THEN
      ! Linear terms interpolated separately when required.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
        &YDCPG_SL1%V9_SI%P,YDCPG_GMVF_SI%V%P)
    ENDIF

    IF (YDDYNA%LSETTLS .AND. YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER == 0) THEN
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
        &YDCPG_SL1%V9_NL%P,YDCPG_GMVF_NL%V%P)
    ENDIF
  ENDIF

  ! --- Split physics requiring a trilinear interpolation:
  IF(LDSLPHY) THEN
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_W,PLSCAW,&
      &YDCPG_SL1%VP9%P,PVPF)
  ENDIF

!$ACDC }

ENDIF ! LRHS_CURV

! * Temperature equation:

LLMOM=.NOT.LSLHDHEAT

! --- part of the RHS requiring a "high order" interpolation:

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LAITRE_GMV {

CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKH,&
 & LL_LIN,YDDYNA%LCOMAD_T,YDDYNA%LSLHD_T,LVWENO_T,WENO_ALPHA_T,LLMOM,LQMT,LQMHT,KNOWENO,&
 & KL0,PLSCAW,PRSCAW,YDCPG_SL1%T9%P,YDCPG_GMVF%T%P)

IF (LRSLDDH) THEN
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      YDCPG_DDH_TND%YSL%T%P(JROF,JLEV)=YDCPG_DDH_TND%YSL%T%P(JROF,JLEV)&
      & +YDCPG_GMVF%T%P(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

IF ((NSPLTHOI /= 0).AND.(NTLAG < 4)) THEN
  ! --- part of the RHS requiring special "high order" interpolation
  IF (NSPLTHOI == 1) THEN
    LLHOI2=LLNOSLHD
    IWK=NSLDIMK
  ELSE
    LLHOI2=LSLHD_T
    IWK=IWKH
  ENDIF
  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWK,&
   & LL_LIN,YDDYNA%LCOMAD_T,LLHOI2,LVWENO_T,WENO_ALPHA_T,LLMOM,LQMT,LQMHT,KNOWENO,&
   & KL0,PLSCAW,PRSCAW,YDCPG_SL1%TF9%P,YDCPG_GMVF%T%P,LDADD=.TRUE.)
ENDIF

!$ACDC }

IF(NTLAG >= 3) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=NTLAG_3 {

  ! --- part of the RHS requiring a trilinear interpolation:
  CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_T,PLSCAW,&
    &YDCPG_SL1%T0%P,YDCPG_GMVF%T%P,LDADD=.TRUE.)

  IF (LRSIDDH) THEN
    ! Linear terms interpolated separately for DDH.
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_T,PLSCAW,&
      &YDCPG_SL1%T9_SI%P,YDCPG_DDH_TND%YSI%T%T0,LDADD=.TRUE.)
  ENDIF

  IF (YDDYNA%LSLINL .AND. NCURRENT_ITER == 0) THEN
    ! Linear terms interpolated separately when required.
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_T,PLSCAW,&
      &YDCPG_SL1%T9_SI%P,YDCPG_GMVF_SI%T%P)
  ENDIF

  IF (YDDYNA%LSETTLS .AND. YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER == 0) THEN
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_T,PLSCAW,&
      &YDCPG_SL1%T9_NL%P,YDCPG_GMVF_NL%T%P)
  ENDIF

!$ACDC }

ENDIF

! --- Split physics requiring a trilinear interpolation:
IF(LDSLPHY) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LDSLPHY {

  CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_T,PLSCAW,&
    &YDCPG_SL1%TP9%P,PTPF)

!$ACDC }

ENDIF

! * Pressure departure variable equation (NH model):

IF (YDDYNA%LNHDYN) THEN !!! LNHEE

!$ACDC ABORT {

  LLMOM=.TRUE.

  ! --- part of the RHS requiring a "high order" interpolation:
  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWNT,&
   & LL_LIN,YDDYNA%LCOMAD_SPD,YDDYNA%LSLHD_SPD,LVWENO_SPD,WENO_ALPHA_SPD,LLMOM,LQMPD,LQMHPD,&
   & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%PD9%P,YDCPG_GMVF%SPD%P)

  ! diabatic tendency is 0 for the time being in this equation;
  ! some code must be activated here in the future when the non-zero
  ! diabatic tendency will be coded.
  !later IF (LRSLDDH) THEN
  !later   PGMVTNDSL(KST:KEND,1:NFLEVG,MSLDDH_PD)=PGMVTNDSL(KST:KEND,1:NFLEVG,MSLDDH_PD) &
  !later    & +PGMVF(KST:KEND,1:NFLEVG,YGP%MSPD)
  !later ENDIF

  !later IF ((NSPLTHOI /= 0).AND.(NSPDLAG < 4)) THEN
  !later   ...
  !later ENDIF

  !later IF(NSPDLAG >= 3) THEN
  IF(NSPDLAG == 3) THEN

    ! --- part of the RHS requiring a trilinear interpolation:
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SPD,&
      &PLSCAW,YDCPG_SL1%PD0%P,YDCPG_GMVF%SPD%P,LDADD=.TRUE.)

    IF (LRSIDDH) THEN
      ! Linear terms interpolated separately for DDH.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SPD,PLSCAW,&
        &YDCPG_SL1%PD9_SI%P,YDCPG_DDH_TND%YSI%PD%T0,LDADD=.TRUE.)
    ENDIF

    IF (YDDYNA%LSLINL .AND. NCURRENT_ITER == 0) THEN
      ! Linear terms interpolated separately when required.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SPD,&
        &PLSCAW,YDCPG_SL1%PD9_SI%P,YDCPG_GMVF_SI%SPD%P)
    ENDIF

    IF (YDDYNA%LSETTLS .AND. YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER == 0) THEN
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SPD,&
        &PLSCAW,YDCPG_SL1%PD9_NL%P,YDCPG_GMVF_NL%SPD%P)
    ENDIF

  ENDIF

!$ACDC }

ENDIF

! * Vertical divergence equation (NH model):
!   Valid for:
!   - case LGWADV=F.
!   - case (LGWADV,LVFE_GW)=T,T.

IF (YDDYNA%LNHDYN) THEN

!$ACDC ABORT {

  LLMOM=.TRUE.
  LL1=.NOT.YDDYNA%LGWADV
  LL2=YDGEOMETRY%YRVERT_GEOM%YRCVER%LVFE_GW
  LL3=YDDYNA%ND4SYS == 2.AND.NCURRENT_ITER < NSITER

  ! --- part of the RHS requiring a "high order" interpolation:
  IF (LL1.OR.LL2) THEN
    CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWKM,&
     & LL_LIN,YDDYNA%LCOMAD_SVD,YDDYNA%LSLHD_SVD,LVWENO_SVD,WENO_ALPHA_SVD,LLMOM,LQMVD,LQMHVD,&
     & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%VD9%P,YDCPG_GMVF%SVD%P)
  ENDIF

  IF (LRSLDDH) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDCPG_DDH_TND%YSL%VD%P(JROF,JLEV)=YDCPG_DDH_TND%YSL%VD%P(JROF,JLEV)&
        & +YDCPG_GMVF%SVD%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

  IF ((NSPLTHOI /= 0).AND.(NSVDLAG < 4)) THEN
    ! --- part of the RHS requiring special "high order" interpolation
    IF (NSPLTHOI == 1) THEN
      LLHOI2=LLNOSLHD
      IWK=NSLDIMK
    ELSE
      LLHOI2=LSLHD_SVD
      IWK=IWKM
    ENDIF

    CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWK,&
     & LL_LIN,YDDYNA%LCOMAD_SVD,LLHOI2,LVWENO_SVD,WENO_ALPHA_SVD,LLMOM,LQMVD,LQMHVD,&
     & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%VDF9%P,YDCPG_GMVF%SVD%P,LDADD=.TRUE.)
  ENDIF

  ! --- "X" variable requiring a "high order" interpolation,
  !     present only if NVDVAR=4 or 5 (the SLHD interpolation is never
  !     applied to this quantity):
  IF ((LL1.OR.LL3).AND.(YDDYNA%NVDVAR == 4 .OR. YDDYNA%NVDVAR == 5)) THEN
    CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWNT,&
     & LL_LIN,YDDYNA%LCOMAD_SVD,LLNOSLHD,LVWENO_SVD,WENO_ALPHA_SVD,LLMOM,LQMVD,LQMHVD,&
     & KNOWENO,KL0,PLSCAW,PRSCAW,YDCPG_SL1%NHX9%P,YDCPG_GMVF%NHX%P)
    IF (LRSLDDH) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDCPG_DDH_TND%YSL%NHX%P(JROF,JLEV)=YDCPG_DDH_TND%YSL%NHX%P(JROF,JLEV)+YDCPG_GMVF%NHX%P(JROF,JLEV)
        ENDDO
      ENDDO
    ENDIF
  ENDIF

  IF(NSVDLAG >= 3) THEN

    ! --- part of the RHS requiring a trilinear interpolation:
    IF(LL1.OR.LL2) THEN
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SVD,&
        &PLSCAW,YDCPG_SL1%VD0%P,YDCPG_GMVF%SVD%P,LDADD=.TRUE.)

      IF (YDDYNA%LSETTLS .AND. YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER == 0) THEN
        CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SVD,&
          &PLSCAW,YDCPG_SL1%VD9_NL%P,YDCPG_GMVF_NL%SVD%P)
      ENDIF
    ENDIF

    IF (LRSIDDH) THEN
      ! Linear terms interpolated separately when required.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SVD,&
        &PLSCAW,YDCPG_SL1%VD9_SI%P,YDCPG_DDH_TND%YSI%VD%T0,LDADD=.TRUE.)
    ENDIF

    IF (YDDYNA%LSLINL .AND. NCURRENT_ITER == 0) THEN
      ! Linear terms interpolated separately when required.
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SVD,&
        &PLSCAW,YDCPG_SL1%VD9_SI%P,YDCPG_GMVF_SI%SVD%P)
    ENDIF

  ENDIF

!$ACDC }

ENDIF

!     ------------------------------------------------------------------

!*       3.    ORIGIN POINT INTERPOLATIONS FOR GFL VARIABLES.
!              ----------------------------------------------

! * The interpolations are tri-dimensional ones.

DO JFLD=1,SIZE(YDVARS%GFL_PTR)
  IF (YDVARS%GFL_PTR(JFLD)%YCOMP%LADV.AND.(.NOT.YDVARS%GFL_PTR(JFLD)%YCOMP%LMGRID)) THEN
    LLMOM = TRIM(YDVARS%GFL_PTR(JFLD)%YCOMP%CNAME) == 'TKE'.OR..NOT.LSLHDHEAT

    IF (YDDYNA%L3DTURB.AND.YDVARS%GFL_PTR(JFLD)%YCOMP%LHORTURB) THEN
      IWKGFL=IWKH
      IF (TRIM(YDVARS%GFL_PTR(JFLD)%YCOMP%CNAME) == 'TKE' ) IWKGFL=IWKM
    ELSE
      IWKGFL=1
    ENDIF

    IF (YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT(1:3) == "LIN") THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LIN {

      LLCOMAD = YCOMP(JFLD)%CSLINT == "LIN(MAD)"
      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LLCOMAD,&
        &PLSCAW,YDCPG_SL1%GFL(JFLD)%P,YDCPG_GFLF%GFL(JFLD)%P)

!$ACDC }

    ELSEIF (NUMFLDS_SL1 > 0 .AND. NUMFLDS_SPL > 0 .AND. YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT == 'LAITVSPCQM  ') THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LAITVSPCQM {

      CALL LAITRE_GFL(YDGEOMETRY,YGFL,YDML_DYN,KASLB1, &
       & NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,KHVI,IWKGFL,&
       & YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT,YDVARS%GFL_PTR(JFLD)%YCOMP%WENO_ALPHA,LLMOM,KNOWENO,KL0,PLSCAW,PRSCAW,&
       & YDCPG_SL1%GFL(JFLD)%P,YDCPG_GFLF%GFL(JFLD)%P, &
       & PXSPSL=YDCPG_SL1%GFL(JFLD)%P_SP)

!$ACDC }

    ELSE 

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LAITRE_GFL {

      CALL LAITRE_GFL(YDGEOMETRY,YGFL,YDML_DYN,KASLB1, &
       & NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,KHVI,IWKGFL,&
       & YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT,YDVARS%GFL_PTR(JFLD)%YCOMP%WENO_ALPHA,LLMOM,KNOWENO,KL0,PLSCAW,PRSCAW,&
       & YDCPG_SL1%GFL(JFLD)%P,YDCPG_GFLF%GFL(JFLD)%P)

!$ACDC }

    ENDIF

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=RRINTOPT {

    ! Convert modified interpolated quantity back to physical space
    IF (RRINTOPT /= 1._JPRB) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDCPG_GFLF%GFL(JFLD)%P(JROF,JLEV)=YDCPG_GFLF%GFL(JFLD)%P(JROF,JLEV)*RRINTOPT
        ENDDO
      ENDDO
    ENDIF

    IF (LRSLDDH) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDCPG_DDH_TND%YSL%GFL(JFLD)%P(JROF,JLEV)=YDCPG_DDH_TND%YSL%GFL(JFLD)%P(JROF,JLEV)&
          & +YDCPG_GFLF%GFL(JFLD)%P(JROF,JLEV)
        ENDDO
      ENDDO
    ENDIF

!$ACDC }

    IF (.NOT.(NSPLTHOI == 0.OR.YDVARS%GFL_PTR(JFLD)%YCOMP%LTDIABLIN)) THEN
      ! --- part of the RHS requiring special "high order" interpolation
      IF (NSPLTHOI == 1) THEN
        ! Deactivate SLHD in case it is active
        SELECT CASE (YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT)
          CASE ('LAITQM(SLD) ')
            CLINT='LAITQM      '
          CASE ('LAITQMH(SLD)')
            CLINT='LAITQMH     '
          CASE ('LAITRI(SLD) ')
            CLINT='LAITRI      '
          CASE DEFAULT
            CLINT=YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT
        END SELECT
        IWK=NSLDIMK
      ELSE
        CLINT=YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT
        IWK=IWKGFL
      ENDIF

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LTDIABLIN {

      CALL LAITRE_GFL(YDGEOMETRY,YGFL,YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG, &
       & NFLSA,NFLEN,KHVI,IWK,CLINT,YDVARS%GFL_PTR(JFLD)%YCOMP%WENO_ALPHA,LLMOM,KNOWENO,&
       & KL0,PLSCAW,PRSCAW,YDCPG_SL1%GFL(JFLD)%P,ZGFLF,YDCPG_SL1%GFL(JFLD)%P_SPF)

      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDCPG_GFLF%GFL(JFLD)%P(JROF,JLEV)= YDCPG_GFLF%GFL(JFLD)%P(JROF,JLEV) + ZGFLF(JROF,JLEV)
        ENDDO
      ENDDO

!$ACDC }

    ENDIF
    ! --- Split physics requiring a trilinear interpolation:
    !  This time the same but for different purpose (and different physics). 
    !  Unlike in previous case, here the useless computation is not performed.

    IF(YDVARS%GFL_PTR(JFLD)%YCOMP%LTDIABLIN) THEN
      LLCOMAD = YCOMP(JFLD)%CSLINT == 'LIN(MAD)    '.OR.&
       & YCOMP(JFLD)%CSLINT == 'LAITQM(MAD) '.OR.&
       & YCOMP(JFLD)%CSLINT == 'LAITQMH(MAD)'.OR.&
       & YCOMP(JFLD)%CSLINT == 'LAITRI(MAD) '

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LLCOMAD {

      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LLCOMAD,&
        &PLSCAW,YDCPG_SL1%GFL(JFLD)%P_F,YDCPG_GFLF%GFL(JFLD)%P,LDADD=.TRUE.)

!$ACDC }

    ENDIF

  ENDIF ! YCOMP(JFLD)%LADV.AND.(.NOT.YCOMP(JFLD)%LMGRID)

  IF (YDVARS%GFL_PTR(JFLD)%YCOMP%LADV) THEN

    ! --- Split physics requiring a trilinear interpolation:
    IF(YDVARS%GFL_PTR(JFLD)%YCOMP%LPHY.AND.LDSLPHY) THEN
      ! The numbering of GFL fields in PGFLSLP should be consistent with
      !  what is done in GPADDSLPHY and SUSLB.
      ! For the time being fields in interpolation buffer are counted even
      !  if they are useless.
      LLCOMAD=(YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT == 'LIN(MAD)    ') .OR.&
       &      (YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT == 'LAITQM(MAD) ') .OR.&
       &      (YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT == 'LAITQMH(MAD)') .OR.&
       &      (YDVARS%GFL_PTR(JFLD)%YCOMP%CSLINT == 'LAITRI(MAD) ') 

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LLCOMAD_2 {

      CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,LLCOMAD,&
       &PLSCAW,YDCPG_SL1%GFL(JFLD)%P_P,YDCPG_GFLPF%GFL(JFLD)%P)

!$ACDC }

    ENDIF

  ENDIF

ENDDO

!     ------------------------------------------------------------------

!*       4.    ORIGIN POINT INTERPOLATIONS FOR GMVS VARIABLES.
!              -----------------------------------------------

LLMOM=.TRUE.

! * The interpolations are tri-dimensional and 2D ones.
!   Remark: 
!   The available 2D "high order" interpolator is LAIDDI.
!   The SLHD interpolator is never applied to continuity equation.

! * Continuity equation:

! --- 3D part of the RHS requiring a "high order" interpolation:
IF(NVLAG == 2) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=NVLAG_2 {

  CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IWNT,&
   & LL_LIN,LCOMAD_SP,LLNOSLHD,LVWENO_SP,WENO_ALPHA_SP,LLMOM,LQMP,LQMHP,KNOWENO,&
   & KL0,PLSCAW,PRSCAW,YDCPG_SL1%C9%P,PCF)

!$ACDC }

ENDIF

! --- 3D part of the RHS requiring a trilinear interpolation:
IF(NVLAG == 3) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=NVLAG_3 {

  CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SP,PLSCAW,&
    &YDCPG_SL1%C9%P,PCF)
  IF (YDDYNA%LSLINLC2.AND. NCURRENT_ITER == 0) THEN
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SP,PLSCAW,&
      &YDCPG_SL1%C9_SI%P,PCF_SI)
  ENDIF
  IF (YDDYNA%LSETTLS .AND. YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER == 0) THEN
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SP,PLSCAW,&
      &YDCPG_SL1%C9_NL%P,PCF_NL)
  ENDIF

!$ACDC }

ENDIF

! --- 2D part of the RHS requiring a "high order" interpolation:
IF (YDDYNA%NQMGMV == -4) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=NQMGMV {

  IF (YDDYNA%LCOMAD_SP) THEN
    CALL LAIDLI(KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,&
     & PLSCAW(:,:,YDTLSCAW%M_WDLAMAD),PLSCAW(:,:,YDTLSCAW%M_WDLOMAD+1),&
     & KLH0(:,:,1:2),YDCPG_SL1%SP9%P,PDP)
  ELSE
    CALL LAIDLI(KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,&
     & PLSCAW(:,:,YDTLSCAW%M_WDLAT),PLSCAW(:,:,YDTLSCAW%M_WDLO+1),&
     & KLH0(:,:,1:2),YDCPG_SL1%SP9%P,PDP)
  ENDIF

!$ACDC }

ELSE
  IF(LQMP.OR.LQMHP) THEN
    IQM = 1
  ELSE
    IQM = 0
  ENDIF


!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LCOMAD_SP {

  IF (YDDYNA%LCOMAD_SP) THEN
    CALL LAIDDI(KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IQM,&
     & PRSCAW(:,:,YDTRSCAW%M_WCLAMAD(IWNT)),PLSCAW(:,:,YDTLSCAW%M_WDLOMAD),&
     & PRSCAW(:,:,YDTRSCAW%M_WCLOMAD(IWNT)),KLH0,YDCPG_SL1%SP9%P,PDP)
  ELSE
    CALL LAIDDI(KASLB1,NPROMA,KST,KEND,NFLEVG,NFLSA,NFLEN,IQM,&
     & PRSCAW(:,:,YDTRSCAW%M_WCLA(IWNT)),PLSCAW(:,:,YDTLSCAW%M_WDLO),&
     & PRSCAW(:,:,YDTRSCAW%M_WCLO(IWNT)),KLH0,YDCPG_SL1%SP9%P,PDP)
  ENDIF

!$ACDC }

ENDIF

!     ------------------------------------------------------------------

!*       5.    ORIGIN POINT INTERPOLATIONS FOR SOME MISCELLANEOUS VARIABLES.
!              -------------------------------------------------------------

! * The interpolations are tri-dimensional ones (excepted for
!   the horizontal quantity "GWD" which requires a horizontal interpolation).
!   Remark: the available 3D "high order" interpolators are
!   LAITRI, LAITQM and LAITQMH (called via LAITRE_GMV and LAITRE_GFL);
!   the 3D trilinear interpolator is LAITLI;
!   the available 2D "high order" interpolator is LAIDDI.
!   The SLHD interpolator is never applied on these variables.

! * Additional terms required for diagnostic BBC in NH model.
!   - Quantity "GWS" requires a 2D horizontal "high order" interpolation.
!   - If NSVDLAG=2, quantities "DBBC" and "DPHI" requires a "high order"
!     interpolation (SLHD interpolations, vertical Hermite cubic and
!     vertical spline cubic interpolations are not coded for these quantities).
!   - If NSVDLAG=3, quantities "DBBC" and "DPHI" requires a trilinear interpolation.
!   - Note that the calculation which is done for "DBBC" and "DPHI"
!     only provides the interpolated value for the bottom full layer.

IF (YDDYNA%LNHDYN.AND.YDDYNA%LRDBBC) THEN

!$ACDC ABORT {

  IF(LQMVD.OR.LQMHVD) THEN
    IQM = 1
  ELSE
    IQM = 0
  ENDIF

  IF (YDDYNA%LCOMAD_SVD) THEN
    IDLO = YDTLSCAW%M_WDLOMAD
    ICLO = YDTRSCAW%M_WCLOMAD(IWNT)
    ICLA = YDTRSCAW%M_WCLAMAD(IWNT)
  ELSE
    IDLO = YDTLSCAW%M_WDLO
    ICLO = YDTRSCAW%M_WCLO(IWNT)
    ICLA = YDTRSCAW%M_WCLA(IWNT)
  ENDIF

  CALL LAIDDI(KASLB1,NPROMA,KST,KEND,1,NFLSA,NFLEN,IQM,&
   & PRSCAW(:,NFLEVG,ICLA:ICLA+2),PLSCAW(:,NFLEVG,IDLO:IDLO+3),&
   & PRSCAW(:,NFLEVG,ICLO:ICLO+5),KLH0(:,NFLEVG,:),YDCPG_SL1%GWS9%P,PGWSF)

  IF (NSVDLAG == 2) THEN
    CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,1,NFLSA,NFLEN,IWNT,&
     & LL_LIN,YDDYNA%LCOMAD_SVD,LLNOSLHD,LVWENO_SVD,WENO_ALPHA_SVD,LLMOM,LQMVD,LQMHVD,KNOWENO,KL0(:,NFLEVG,0:3),&
     & PLSCAW(:,NFLEVG,1:YDTLSCAW%NDIM),PRSCAW(:,NFLEVG,1:YDTRSCAW%NDIM),&
     & YDCPG_SL1%DBBC9%P,PDBBCF)
    CALL LAITRE_GMV(YDML_DYN,KASLB1,NPROMA,KST,KEND,1,NFLSA,NFLEN,IWNT,&
     & LL_LIN,YDDYNA%LCOMAD_SVD,LLNOSLHD,LVWENO_SVD,WENO_ALPHA_SVD,LLMOM,LQMVD,LQMHVD,KNOWENO,KL0(:,NFLEVG,0:3),&
     & PLSCAW(:,NFLEVG,1:YDTLSCAW%NDIM),PRSCAW(:,NFLEVG,1:YDTRSCAW%NDIM),&
     & YDCPG_SL1%DPHI9%P,PDPHIF)
  ELSEIF (NSVDLAG >= 3) THEN
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SVD,PLSCAW,&
      &YDCPG_SL1%DBBC9%P,PDBBCF,LD2D=.TRUE.)
    CALL LAITLIF(YDTLSCAW,KASLB1,NPROMA,NFLEVG,KST,KEND,KL0,YDDYNA%LCOMAD_SVD,PLSCAW,&
      &YDCPG_SL1%DPHI9%P,PDPHIF,LD2D=.TRUE.)
  ENDIF

!$ACDC }

ENDIF

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LARCINB',1,ZHOOK_HANDLE)

END SUBROUTINE LARCINB


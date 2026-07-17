SUBROUTINE CPG (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_TND, YDCPG_SL1AUX, YDCPG_SL2, YDCPG_MISC,          &
& YDCPG_GPAR, YDCPG_PHY0, YDCPG_PHY9, YDMF_PHYS, YDHGRAD, YDCPG_DDH, YDCPG_DYN0, YDCPG_DYN9, YDMF_PHYS_SURF, &
& YDVARS, YDXFU, YDCFU, YDMODEL, YDFIELDS, YDA_GFLSLP, YDA_SAVTEND, YDCPG_DDH_TND, YDPGTERM,                 &
& YDA_GFLPC, YDA_GFLPT, YDCPG_SLMISC, YDCPG_SL1, YDA_EXTRA, YDDDH, YDTDDH, YDA_RSAVEDP, YDA_PWRL9,           &
& YDPHYSMWAVE, PGPSDT2D, PGFL, PGMVT1, PGFLT1, PTRAJ_PHYS, PTRAJ_SLAG)

!$ACDC pointerparallel    


!**** *CPG* - Grid point calculations.

!     Purpose.
!     --------
!           Grid point calculations.

!**   Interface.
!     ----------
!        *CALL* *CPG(...)*

!        Explicit arguments :
!        --------------------

!     INPUT:
!     ------
!        LDCONFX      : T if this call to CPG is done only to do some diagnostics like CFU,XFU
!                       (LDCONFX=T <==> former CDCONF='X')
!        LD_DFISTEP      : 'D' -> DFI computations
!        LDFSTEP      : .T. if first step.
!        LDDIAB       : .T. if complete physics is activated.
!        LDSLPHY      : .T. if ECMWF split physics.
!        LDUSEPB1     : .T. if updating PB1
!        PDT          : For a leap-frog scheme (three time level scheme):
!                       'dt' at the first time-step, '2 dt' otherwise.
!                       For a 2TL SL scheme: timestep 'dt'.
!        PDTPHY       : timestep used in the physics.
!        PTE          : 1. or 0. according to different configurations.
!        PBETADT      : BETADT or 0. according to different configurations.
!        YDSL         : SL_STRUCT definition
!        PGFLSLP      : GFL array for use in semi-lagrangian physics
!        PSAVTEND     : tendencies buffer for split ECMWF physics.
!        PGMVTNDHD_DDH: tendencies of horizontal diffusion scheme for GMV.
!        PGFLTNDHD_DDH: tendencies of horizontal diffusion for spectrally treated GFL.
!        PGPSDT2D     : cf. YGPSDT%GP2D in YOMSPSDT (buffer for stochastic physics).

!     INPUT/OUTPUT:
!     -------------
!        PGFL         : unified_treatment grid-point fields at t
!        PGFLPC       : unified_treatment grid-point fields at t (3TL PC only)
!        PGFLPT       : tendency of X variable from phy.
!        PSD_PF       : precip fraction
!        PSD_XP       : precipitation type diagnostic
!        YDPGTERM      : pressure gradient quantities, only used if LPGFSF=T
!        PGMVTNDSI_DDH: tendencies of semi-implicit scheme.
!        PGMU0        : COSINE OF SOLAR ZENITH ANGLE, APPROXIMATE ACTUAL VALUE
!                       linear T_e correction
!                       linear T_e correction
!        YDDDH        : diagnostic superstructure

!     OUTPUT:
!     -------
!        PB1          : "SLBUF1" buffer for interpolations in SL scheme.
!        PGMVT1       : upper air GMV variables buffer.
!        PGFLT1       : GFL variables buffer.
!        PEXTRA       : additional quantity for diagnostics.
!        PGMVTNDSL_DDH: GMV(t+dt,F)-GMV(t or t-dt,O) for DDH

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ARPEGE documentation vol 2 ch 1 and vol 3 ch 6

!     Author.
!     -------
!      Philippe Courtier  *DMN*
!      Original : 90-03-19

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!    4-Mar-2008 Y. Seity : Cleaning IR and WV similated sat pictures
!                            (replaced by Fullpos Calculations)
!   K. Yessad (Sep 2008): prune enhanced diffusion (lfrein).
!   18-Nov-2008 R.Brozkova: OpenMP bugfix
!   K. Yessad (Dec 2008): remove dummy CDLOCK + cleanings
!   18-May-2009 S. Riette : mean cls wind added
!   F. Vana  15-Oct-2009: NSPLTHOI option
!   15-October-2009 Y. Bouteloup : Store radiative cloud water and ice in GFL (YIRAD and YLRAD)
!   K. Yessad (Nov 2009): cleanings, DT/Dt now pre-computed in CPG_GP.
!   K. Yessad (Nov 2009): prune lpc_old.
!   24-Feb-2010 S. Riette : max gust avec NXGSTPERIOD seconds
!   11-Mai-2010 F. Bouyssel : RINDX, RINDY in argument
!   L. Bengtsson-Sedlar & F. Vana  18-Feb-2011 : CA arrays for MF physics
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures + cleanings.
!   F. Vana  21-Feb-2011: horiz. turbulence, N[x]LAG=4, diffus. on phys. tend.
!   G.Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!   G.Mozdzynski (Feb 2011): OOPS cleaning, use of derived type TGSGEOM
!   2011-09-07, J.M. Piriou and E. Bazile: open/close output LFA files for 1D model MUSC (LMUSCLFA).
!   K. Yessad (Fev 2012): various modifications.
!   R. El Khatib 16-Mar-2012 Automatic allocations + cleanings
!   F. Bouttier  Jul 2012: pass stochastic physics pattern ZGPSDT2D
!   M. Ahlgrimm  31-Oct-2011 add rain, snow and PEXTRA to DDH output
!   T. Wilhelmsson 29-Mar-2012 Remove LCPG_SPLIT option and move OpenMP loop to CPG_DRV
!   F. Vana  28-Nov-2013 : Redesigned trajectory handling.
!   M. Diamantakis Dec 2013: add KSETTLOFF - may be used when LSETTLSVF=T
!   M. Ahlgrimm Apr 2014: Add lake variables and precip fraction to DDH output
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   2013-11, J. Masek: Passing intermittency arrays for ACRANEB2.
!   K. Yessad (July 2014): Move some variables.
!   O. Marsden (May 2016): Replace NPROMA9 by NPROMA for dimensions, as NPROMA9 not compatible with some routines called
!   F.Taillefer (June 2016): add MF assim CLS arrays in CPG_DIA
!   K. Yessad (Dec 2016): Prune obsolete options.
!   2017-09, J. Masek: Shifted dimensioning of PGMU0.
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   R. El Khatib 05-Jun-2018 computation of periods moved from cnt4 (OOPS refactoring)
!   M. Diamantakis (June 2018): Extra arguments for multiple (Atlas) grid advection scheme
!   R. Brozkova (Sept 2018) : Dataflow for global normal irradiance and mean
!                             radiant temperature.
!   F. Vana  Oct-2018: More optimal trigger of SLPHY computation.
!   I. Etchevers (Jan 2019) : PSD_XP for precipitation type diagnostic
!   F. Vana 11-Jul-2019: Option LRHS_CURV
!   2019-09, M. Hrastinski: Dataflow for TKE and TTE terms in ALARO DDH (ZFTCNS)
!   I. Polichtchouk (June 2020): code for LPGFSF=T
!   F. Vana 21-Sep-2020: LPGREUSE
!   D. Nemec (July 2021): Add ZQGRAUPEL.
! End Modifications
!-------------------------------------------------------------------------------

USE TYPE_MODEL              , ONLY : MODEL
USE FIELDS_MOD              , ONLY : FIELDS
USE GEOMETRY_MOD            , ONLY : GEOMETRY
USE MF_PHYS_TYPE_MOD        , ONLY : MF_PHYS_TYPE
USE CPG_TYPE_MOD            , ONLY : CPG_DDH_TYPE, CPG_DYN_TYPE, CPG_GPAR_TYPE, CPG_MISC_TYPE, CPG_PHY_TYPE, CPG_TND_TYPE
USE CPG_SL1_TYPE_MOD        , ONLY : CPG_SL1B_TYPE, CPG_SL1F_TYPE
USE CPG_SL2_TYPE_MOD        , ONLY : CPG_SL2_TYPE
USE CPG_OPTS_TYPE_MOD       , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE MF_PHYS_SURFACE_TYPE_MOD, ONLY : MF_PHYS_SURF_TYPE
USE FIELD_VARIABLES_MOD     , ONLY : FIELD_VARIABLES
USE YOMXFU                  , ONLY : TXFU
USE YOMCFU                  , ONLY : TCFU
USE PARKIND1                , ONLY : JPIM, JPRB
USE YOMHOOK                 , ONLY : DR_HOOK, LHOOK, JPHOOK
USE EINT_MOD                , ONLY : SL_STRUCT
USE DDH_MIX                 , ONLY : CLEANDDH, RESET_DDHFLEX, SETDDH, TYP_DDH
USE YOMTRAJ                 , ONLY : LTRAJSAVE, LTRAJSLAG, TRAJ_PHYS_TYPE, TRAJ_SLAG_TYPE
USE YOMINI                  , ONLY : LINITER
USE ALGORITHM_STATE_MOD     , ONLY : GET_NUPTRA
USE CPG_DDH_TND_TYPE_MOD    , ONLY : CPG_DDH_TND_TYPE
USE CPG_SLMISC_TYPE_MOD     , ONLY : CPG_SLMISC_TYPE
USE FIELD_ARRAY_MODULE
USE INTDYN_MOD              , ONLY : TPG_TYPE
USE YOMTDDH                 , ONLY : TTDDH
USE YOMDGRADIENT_TYPE_MOD   , ONLY : GRADIENT_TYPE
USE YOE_PHYS_MWAVE          , ONLY : TEPHYSMWAVE

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)          ,INTENT(IN)     :: YDGEOMETRY
TYPE(CPG_BNDS_TYPE)     ,INTENT(IN)     :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE)     ,INTENT(IN)     :: YDCPG_OPTS
TYPE(CPG_TND_TYPE)      ,INTENT(INOUT)  :: YDCPG_TND
TYPE(CPG_SL1B_TYPE)     ,INTENT(INOUT)  :: YDCPG_SL1AUX
TYPE(CPG_SL2_TYPE)      ,INTENT(INOUT)  :: YDCPG_SL2
TYPE(CPG_MISC_TYPE)     ,INTENT(INOUT)  :: YDCPG_MISC
TYPE(CPG_GPAR_TYPE)     ,INTENT(INOUT)  :: YDCPG_GPAR
TYPE(CPG_PHY_TYPE)      ,INTENT(INOUT)  :: YDCPG_PHY0
TYPE(CPG_PHY_TYPE)      ,INTENT(INOUT)  :: YDCPG_PHY9
TYPE(MF_PHYS_TYPE)      ,INTENT(INOUT)  :: YDMF_PHYS
TYPE(GRADIENT_TYPE)     ,INTENT(INOUT)  :: YDHGRAD
TYPE(CPG_DDH_TYPE)      ,INTENT(INOUT)  :: YDCPG_DDH
TYPE(CPG_DYN_TYPE)      ,INTENT(INOUT)  :: YDCPG_DYN0
TYPE(CPG_DYN_TYPE)      ,INTENT(INOUT)  :: YDCPG_DYN9
TYPE(MF_PHYS_SURF_TYPE) ,INTENT(INOUT)  :: YDMF_PHYS_SURF
TYPE(FIELD_VARIABLES)   ,INTENT(INOUT)  :: YDVARS
TYPE(TXFU)              ,INTENT(INOUT)  :: YDXFU
TYPE(TCFU)              ,INTENT(INOUT)  :: YDCFU
TYPE(MODEL)             ,INTENT(IN)     :: YDMODEL
TYPE(FIELDS)            ,INTENT(INOUT)  :: YDFIELDS
TYPE(FIELD_4RB_ARRAY)   ,INTENT(IN)     :: YDA_GFLSLP
TYPE(FIELD_4RB_ARRAY)   ,INTENT(IN)     :: YDA_SAVTEND
TYPE(CPG_DDH_TND_TYPE)  ,INTENT(INOUT)  :: YDCPG_DDH_TND
TYPE(TPG_TYPE)          ,INTENT(INOUT)  :: YDPGTERM
TYPE(FIELD_4RB_ARRAY)   ,INTENT(INOUT)  :: YDA_GFLPC
TYPE(FIELD_4RB_ARRAY)   ,INTENT(INOUT)  :: YDA_GFLPT
TYPE(CPG_SLMISC_TYPE)   ,INTENT(INOUT)  :: YDCPG_SLMISC
TYPE(CPG_SL1F_TYPE)     ,INTENT(INOUT)  :: YDCPG_SL1
TYPE(FIELD_4RB_ARRAY)   ,INTENT(INOUT)  :: YDA_EXTRA
TYPE(TYP_DDH)           ,INTENT(INOUT)  :: YDDDH
TYPE(TTDDH)             ,INTENT(INOUT)  :: YDTDDH
TYPE(FIELD_4RB_ARRAY)   ,INTENT(INOUT)  :: YDA_RSAVEDP
TYPE(FIELD_3RB_ARRAY)   ,INTENT(INOUT)  :: YDA_PWRL9
TYPE(TEPHYSMWAVE)       ,INTENT(INOUT)  :: YDPHYSMWAVE
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL :: PGPSDT2D(YDGEOMETRY%YRDIM%NPROMA,YDMODEL%YRML_SPPT%YGPSDT(1)%NG2D,YDGEOMETRY%YRDIM%NGPBLKS)
REAL(KIND=JPRB)         ,INTENT(INOUT) ,OPTIONAL :: PGFL(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_GCONF%YGFL%NDIM)
REAL(KIND=JPRB)         ,INTENT(OUT)   ,OPTIONAL :: PGMVT1(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDFIELDS%YRGMV%YT1%NDIM)
REAL(KIND=JPRB)         ,INTENT(OUT)   ,OPTIONAL :: PGFLT1(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_GCONF%YGFL%NDIM1)
TYPE(TRAJ_PHYS_TYPE)    ,INTENT(INOUT) ,OPTIONAL :: PTRAJ_PHYS
TYPE(TRAJ_SLAG_TYPE)    ,INTENT(INOUT) ,OPTIONAL :: PTRAJ_SLAG

INTEGER(KIND=JPIM) :: JLEV
INTEGER(KIND=JPIM) :: JFLD
INTEGER(KIND=JPIM) :: IUPTRA

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "cpdysldia.intfb.h"
#include "cpg_dia_ddh.intfb.h"
#include "cpg_dia_flu.intfb.h"
#include "cpg_dyn_slg.intfb.h"
#include "cpg_dyn_eul.intfb.h"
#include "cpg_dyn_tra.intfb.h"
#include "cpg_gp_tenc.intfb.h"
#include "cpg_gp.intfb.h"
#include "ec_phys_lslphy.intfb.h"
#include "gpiniddh.intfb.h"
#include "mf_phys.intfb.h"
#include "cpg_gp_tndddh_expl.intfb.h"
#include "cpg_end_sfc.intfb.h"
#include "cpg_end_tra.intfb.h"
#include "cpg_end_map.intfb.h"
#include "cpg_end_spr.intfb.h"

IF (LHOOK) CALL DR_HOOK('CPG', 0, ZHOOK_HANDLE)

!     ------------------------------------------------------------------

ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM, YDDIMV=>YDGEOMETRY%YRDIMV, YDDYN=>YDMODEL%YRML_DYN%YRDYN, YDSIMPHL=>YDMODEL%YRML_PHY_MF%YRSIMPHL, &
& YDMDDH=>YDMODEL%YRML_DIAG%YRMDDH, YDECUCONVCA=>YDMODEL%YRML_PHY_EC%YRECUCONVCA, YDDPHY=>YDMODEL%YRML_PHY_G%YRDPHY,                 &
& YDLDDH=>YDMODEL%YRML_DIAG%YRLDDH, YDSLPHY=>YDMODEL%YRML_PHY_G%YRSLPHY, YDPHY=>YDMODEL%YRML_PHY_MF%YRPHY, YDDYNA=>YDMODEL%YRML_DYN%YRDYNA)

ASSOCIATE(  NPROMA=>YDDIM%NPROMA, NFLEVG=>YDDIMV%NFLEVG, NCURRENT_ITER=>YDDYN%NCURRENT_ITER, NSITER=>YDDYN%NSITER,           &
& RCUCONVCA=>YDECUCONVCA%RCUCONVCA, RNLCONVCA=>YDECUCONVCA%RNLCONVCA, LCUCONV_CA=>YDECUCONVCA%LCUCONV_CA,                    &
& LSDDH=>YDLDDH%LSDDH, LFLEXDIA=>YDLDDH%LFLEXDIA, LDDH_OMP=>YDLDDH%LDDH_OMP, LSIMPH=>YDSIMPHL%LSIMPH, LMPHYS=>YDPHY%LMPHYS)


!     ------------------------------------------------------------------

!$ACDC PARALLEL, TARGET=UpdateView/OpenMPMethod/OpenACCMethod, NAME=ZEROPB2 {

CALL YDCPG_SL2%SET (YDCPG_OPTS, YDCPG_BNDS, 0._JPRB)

!$ACDC }

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ISETTLOFF {

YDCPG_SLMISC%ISETTLOFF(:,:) = 0_JPIM

!$ACDC }

!*       3.    READ BUFFERS, COMPUTE AUXILIARY QUANTITIES.
!              -------------------------------------------
  
IF (YDMODEL%YRML_LBC%LTENC) THEN

!$ACDC ABORT {

    CALL CPG_GP_TENC (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDVARS, YDMODEL, YDFIELDS)

!$ACDC }

ENDIF


CALL CPG_GP (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_TND, YDCPG_MISC, YDCPG_GPAR, YDCPG_DYN0,  &
           & YDCPG_DYN9, YDHGRAD, YDMF_PHYS_SURF, YDVARS, YDMODEL, YDCPG_SL2,  YDPGTERM, YDDDH)

IF (YDMODEL%YRML_DIAG%YRLDDH%LRSLDDH.AND.YDMODEL%YRML_DYN%YRDYNA%LSLAG) THEN

!$ACDC ABORT {

  CALL CPG_GP_TNDDDH_EXPL (YDGEOMETRY, YDCPG_OPTS, YDCPG_BNDS, YDCPG_DYN0, YDCPG_DYN9, YDVARS, YDMODEL, YDCPG_DDH_TND%YSL)

!$ACDC }

ENDIF
  
IF (LSDDH) THEN

!$ACDC ABORT {

  IF (NCURRENT_ITER == 0) CALL GPINIDDH(YDGEOMETRY, YDMDDH, YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA, YDCPG_DDH%DDHI, &
                          & YDCPG_MISC%DHSF, YDCPG_DDH%DHCV, YDCPG_MISC%NEB, YDCPG_MISC%CLCT, YDCPG_BNDS%KSTGLO)

  IF (LFLEXDIA.AND.LDDH_OMP) THEN
    CALL SETDDH(YDMDDH, YDDDH, YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA, NPROMA, NFLEVG, NCURRENT_ITER, YDCPG_MISC%DHSF, &
    & YDCPG_DDH%DDHI, YDCPG_DDH%AUX3D, YDCPG_DDH%AUX2D, YDCPG_DDH%AUXSM)
  ENDIF

!$ACDC }

ENDIF
  
IF (YDCPG_OPTS%LUSEPB1) THEN

!$ACDC PARALLEL, TARGET=UpdateView/OpenMPMethod/OpenACCMethod, NAME=ZEROSL1 {

  CALL YDCPG_SL1AUX%SET (YDCPG_OPTS, YDCPG_BNDS, 0._JPRB)

!$ACDC }

ENDIF
  
!*       4.3.2   Call ECMWF split physics.

!     ------------------------------------------------------------------
  
IF (YDCPG_OPTS%LSLPHY.AND.((YDDYNA%LPC_FULL.AND.(NCURRENT_ITER == NSITER)).OR.(.NOT.YDDYNA%LPC_FULL))) THEN

!$ACDC ABORT {

  CALL EC_PHYS_LSLPHY (YDGEOMETRY, YDMODEL%YRML_DYN, YDSLPHY, YDMODEL%YRML_GCONF, YDMODEL%YRML_PHY_MF%YRPHY2, &
  & YDCPG_OPTS, YDCPG_BNDS, YDVARS, YDCPG_SL1AUX, YDA_SAVTEND, YDA_GFLSLP)

!$ACDC }

ENDIF

IF (LCUCONV_CA) THEN

!$ACDC ABORT {

! Quick fix. The arrays should rather be dimensionned with (nproma,ngpblks). REK
  YDMF_PHYS%OUT%CUCONVCA(1:YDCPG_BNDS%KFDIA)=RCUCONVCA(YDCPG_BNDS%KSTGLO:YDCPG_BNDS%KSTGLO+YDCPG_BNDS%KFDIA-1)
  YDMF_PHYS%OUT%NLCONVCA(1:YDCPG_BNDS%KFDIA)=RNLCONVCA(YDCPG_BNDS%KSTGLO:YDCPG_BNDS%KSTGLO+YDCPG_BNDS%KFDIA-1)

!$ACDC }

ENDIF

IF (LMPHYS.OR.LSIMPH) THEN
  CALL MF_PHYS (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_MISC, YDCPG_GPAR, YDCPG_PHY0, YDCPG_PHY9, YDMF_PHYS, &
  & YDHGRAD, YDCPG_DYN0, YDCPG_DYN9, YDMF_PHYS_SURF, YDCPG_SL1AUX, YDCPG_SL2, YDVARS, YDXFU, YDCFU, YDMODEL, YDFIELDS,  &
  & YDPHYSMWAVE, YDA_GFLPT, PGPSDT2D, PGFL, PGMVT1, PGFLT1, PTRAJ_PHYS, YDDDH)
ENDIF


IF (NCURRENT_ITER == 0) THEN

  IF ((YDCPG_OPTS%NSTEP >= 0).AND.YDMODEL%YRML_DIAG%YRLDDH%LSDDH.AND.(.NOT.LINITER).AND.(.NOT.YDCPG_OPTS%LCONFX)) THEN 

!$ACDC ABORT {

      CALL CPG_DIA_DDH (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_TND, YDCPG_MISC, YDCPG_GPAR, YDMF_PHYS%OUT, YDCPG_DYN0, &
                      & YDMF_PHYS_SURF, YDVARS, YDMODEL, YDCPG_DDH%DDHI, YDCPG_SL2%VVEL, YDCPG_DDH_TND%YSI, YDCPG_DDH_TND%YHD, &
                      & YDCPG_DDH%DHCV, YDDDH, YDTDDH, YDCPG_MISC%FTCNS)     

!$ACDC }

  ENDIF

  CALL CPG_DIA_FLU (YDCPG_BNDS, YDCPG_OPTS, YDCPG_MISC, YDMF_PHYS%OUT, YDCPG_DYN0, YDMF_PHYS_SURF, YDVARS, YDCFU, YDXFU, YDMODEL)

  IF (YDCPG_OPTS%LCONFX.AND.YDMODEL%YRML_DIAG%YRLDDH%LFLEXDIA.AND.(.NOT.YDMODEL%YRML_DIAG%YRLDDH%LDDH_OMP)) THEN

!$ACDC ABORT {

    CALL RESET_DDHFLEX

!$ACDC }

  ENDIF

ENDIF



IF (YDDYNA%LSLDIA .AND. (NCURRENT_ITER == NSITER)) THEN

!$ACDC ABORT {

  CALL CPDYSLDIA(YDMODEL%YRCST, YDVARS, YDCPG_DYN0, YDCPG_OPTS, YDCPG_BNDS, YDGEOMETRY, YDDPHY, YDMODEL%YRML_GCONF, YDA_EXTRA)

!$ACDC }

ENDIF
  
!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LSMTSOL {

IF (YDCPG_OPTS%YRSURF_OPTS%YSD_VF%YLSM%LSET) THEN
  YDCPG_MISC%LSM(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=YDMF_PHYS_SURF%GSD_VF%PLSM(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)
ELSE
  YDCPG_MISC%LSM(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=0.0_JPRB
ENDIF
IF (YDCPG_OPTS%YRSURF_OPTS%YSP_RR%YT%LSET) THEN
  YDCPG_MISC%TSOL(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=YDMF_PHYS_SURF%GSP_RR%PT_T0(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)
ELSE
  YDCPG_MISC%TSOL(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=YDVARS%T%T0(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA,NFLEVG)
ENDIF

!$ACDC }

IF (YDMODEL%YRML_DYN%YRDYNA%LSLAG) THEN
  CALL CPG_DYN_SLG (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_TND, YDCPG_MISC, YDCPG_DYN0, YDCPG_DYN9, &
                  & YDVARS, YDCPG_SL1AUX, YDCPG_SL2, YDMODEL, YDCPG_SLMISC, YDCPG_DDH_TND%YSI, YDA_PWRL9)

ELSE

!$ACDC ABORT {

  CALL CPG_DYN_EUL (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_TND, YDCPG_MISC, YDCPG_DYN0, &
                  & YDCPG_DYN9, YDVARS, YDMODEL,YDCPG_SL2, YDA_GFLPC)

!$ACDC }

ENDIF

IF (LTRAJSAVE .AND. LTRAJSLAG) THEN

!$ACDC ABORT {

  CALL CPG_DYN_TRA (YDGEOMETRY, YDCPG_BNDS, YDCPG_DYN9, YDVARS, YDMODEL, YDCPG_SL1AUX, &
                  & YDCPG_SL2, PTRAJ_SLAG, YDA_RSAVEDP, YDA_PWRL9)

!$ACDC }

ENDIF
  
IF (YDCPG_OPTS%LUSEPB1) THEN

!$ACDC PARALLEL, TARGET=UpdateView/OpenMPMethod/OpenACCMethod, NAME=SHUFFLESL1 {

  CALL YDCPG_SL1AUX%SHUFFLE (YDMODEL%YRML_DYN%YRSL, YDCPG_OPTS, YDCPG_BNDS, YDCPG_SL1)

!$ACDC }

ENDIF
  

IUPTRA = GET_NUPTRA ()
CALL CPG_END_SFC (YDMODEL, YDCPG_OPTS, YDCPG_BNDS, YDMF_PHYS_SURF, IUPTRA)

IF (YDMODEL%YRML_PHY_MF%YRSIMPHL%LTRAJPS) THEN  

!$ACDC ABORT {

  CALL CPG_END_TRA (YDMF_PHYS_SURF, YDCPG_MISC, YDCPG_BNDS, PTRAJ_PHYS)

!$ACDC }

ENDIF

CALL CPG_END_MAP (YDGEOMETRY, YDMODEL, YDVARS, YDCPG_BNDS, YDCPG_OPTS)

IF (YDMODEL%YRML_LBC%LTENC) THEN

!$ACDC ABORT {

  CALL CPG_END_SPR (YDVARS, YDDYNA, YDCPG_BNDS, YDCPG_OPTS)

!$ACDC }

ENDIF
  
IF (LSDDH.AND.LFLEXDIA.AND.LDDH_OMP) THEN

!$ACDC ABORT {

  CALL CLEANDDH(YDTDDH, YDDDH, YDCPG_BNDS%KSTGLO, NCURRENT_ITER)

!$ACDC }

ENDIF

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('CPG', 1, ZHOOK_HANDLE)

END SUBROUTINE CPG


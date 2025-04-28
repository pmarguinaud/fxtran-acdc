SUBROUTINE MF_PHYS (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_MISC, YDCPG_GPAR, YDCPG_PHY0, YDCPG_PHY9, YDMF_PHYS, &
 & YDHGRAD, YDCPG_DYN0, YDCPG_DYN9, YDMF_PHYS_SURF, YDCPG_SL1, YDCPG_SL2, YDVARS, YDXFU, YDCFU, YDMODEL, YDFIELDS, &
 & YDPHYSMWAVE, YDA_GFLPT, PGPSDT2D, PGFL, PGMVT1, PGFLT1, PTRAJ_PHYS, YDDDH)

!**** *MF_PHYS* METEO-FRANCE PHYSICS.

!     Purpose.
!     --------
!         Call METEO-FRANCE physics and physical tendencies.

!**   Interface.
!     ----------
!        *CALL* *MF_PHYS(...)*

!        Explicit arguments :
!        --------------------

!     INPUT:
!     ------

!     INPUT/OUTPUT:
!     -------------

!     OUTPUT:

!        Implicit arguments :
!        --------------------

!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------
!       2000-12-04: F. Bouyssel & J.M. Piriou

!     Modifications.
!     --------------
!       04-Mar-2009 A.Alias : call CPTEND/INITAPLPAR modified to add
!                         Humidity Mesopheric flux (ZFRMQ).
!                     and IVCLIA removed and call to CPNUDG modified as
!                         Nuding mask is now in SD_VF group
!                         call HL_APLPAR modified to add PFCQNG for acdifus
!                         call APL_AROME modified to add Sulfate/Volcano aerosols for radaer
!       K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!       2009-10-15 Y. Bouteloup : Store radiative cloud water and ice in GFL (YIRAD and YLRAD)
!       F. Vana   15-Oct-2009 : NSPLTHOI option
!       K.Yessad (Feb 2010): use YM_RADTC and RFORADTC
!       2010-03-26 Y. Bouteloup : Store radiative cloud water and ice in GFL (AROME case)
!       2010-04-26 Y. Bouteloup : Only one call to cputqy, cputqys and cputqy_arome
!            This need the use of ZTENDGFL as argument of cptend, cptend_new and apl_arome.
!       2010-05-11 F. Bouyssel : Use of PINDX, PINDY
!       2010-05-28 C. Geijo    : Fix error in IPTR array element referencing 
!       2010-06-21 O.Riviere/F. Bouyssel : Fix to have Ts evolving in Fa files with Surfex
!       Dec 2010 A.Alias   : ZMU0N added to call CPPHINP/APLPAR/APL_AROME/HL_APLPAR
!                            CALL to CPNUDG with or with LMSE (A.Voldoire)
!       K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!       L. Bengtsson-Sedlar & F. Vana 18-Feb-2011 : CA scheme for convection
!       F. Vana   22-Feb-2011 : 3D turbulence
!       2011-02-01 M. Mokhtari: Add LMDUST and PEXTT9 and PEXTT0 IN APLPAR
!                             (treatment of the desert aerosols) 
!       2011-03 A.Alias  : new argument to  for sunshine hours YSD_VD%YSUND 
!                      CPNUDG if LMSE=.T. or LMSE=.F. (bugfix)
!                      debug ozone GFL (IPO3) (D. St-Martin)
!                      Humidity Mesopheric flux (ZFRMQ) added in CPTEND_NEW
!       F.Bouyssel (26-03-2011): Fix to have Snow in hist file with surfex
!       2011-06: M. Jerczynski - some cleaning to meet norms
!       E. Bazile 2011-08-26 : Output for MUSC 1D with LFA files with WRITEPHYSIO
!         used previously for extracting profiles from 3D (now also available for AROME).
!       K. Yessad (Dec 2011): use YDOROG, YDGSGEOM and YDCSGEOM.
!       2011-11-21 JF Gueremy : dry convective adjustment (LAJUCV)
!       F. Vana  26-Jan-2012 : historic Qs for TOM's BBC.
!       F.Bouttier Jul 2012: stochastic physics for AROME
!       Z. SASSI  : 07-Mar-2013   INITIALIZING THE WEIGHT VECTORS PDHSF(NPROMA)
!       [DISTRIBUTION OF HORIZONTAL MEANS WEIGHTS]
!       F. Vana  28-Nov-2013 : Redesigned trajectory handling
!       T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!       2013-11, D. Degrauwe: Flexible interface CPTEND_FLEX.
!       2013-11, J. Masek: Passing intermittency arrays for ACRANEB2.
!       K. Yessad (July 2014): Move some variables.
!       2016-04, J. Masek: Passing sunshine duration to APL_AROME.
!       2016-09, M. Mokhtari & A. Ambar: replacement of ZEXT and ZEZDIAG by PGFL
!                                        in aplpar.F90 argument.
!       2016-10, P. Marguinaud : Port to single precision
!       K. Yessad (Dec 2016): Prune obsolete options.
!       K. Yessad (June 2017): Introduce NHQE model.
!       2017-09, J. Masek: Shifted dimensioning of PGMU0.
!       K. Yessad (Feb 2018): remove deep-layer formulations.
!       K. Yessad (Apr 2018): introduce key L_RDRY_VD (ensure consistent definition of "dver" everywhere).
!       2018-09, F. Duruisseau: add rconv and sconv in gfl for bayrad
!       2018-09, R. Brozkova: Passing of diagnostic hail, global normal
!         irradiance and mean radiant temperature from APLPAR.
!       2018-09, D. St-Martin : add NOGWD inputs in aplpar
!       2018-09, M. Michou : add ARPEGE-Climat chemistry call in aplpar  
!   R. El Khatib 27-02-2019 Use pointer function SC2PRG to avoid bounds violation
!       2019-05, I. Etchevers : add visibilities and precipitation type
!   R. El Khatib 27-02-2019 memory bandwidth savings.
!   R. El Khatib 30-Oct-2018 IMAXDRAFT
!       2019-09, M. Hrastinski: Dataflow for TKE and TTE terms in ALARO DDH (PFTCNS).
!       2019-09, J. Masek: Modified call to APL_AROME (added argument NFRRC).
!       2019-12, Y. Bouteloup: Introduction of ZTENDU and ZTENDV for computation of ZDEC in cputqy
!                diferent from PTENDU and PTENDV in the case of the use of Tiedtke scheme to avoid double counting
!       2020-12, U. Andrae : Introduce SPP for HARMONIE-AROME
!       2021-01, R. Brozkova: ALARO graupel fix.
!        R. El Khatib 18-Jul-2022 LAPL_ARPEGE in YRPHY
! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1                   , ONLY : JPRB
USE YOMHOOK                    , ONLY : LHOOK, DR_HOOK, JPHOOK

USE GEOMETRY_MOD               , ONLY : GEOMETRY
USE CPG_OPTS_TYPE_MOD          , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE MF_PHYS_TYPE_MOD           , ONLY : MF_PHYS_TYPE
USE CPG_TYPE_MOD               , ONLY : CPG_DYN_TYPE, CPG_GPAR_TYPE, CPG_MISC_TYPE, CPG_PHY_TYPE
USE CPG_SL1_TYPE_MOD           , ONLY : CPG_SL1B_TYPE
USE CPG_SL2_TYPE_MOD           , ONLY : CPG_SL2_TYPE
USE MF_PHYS_SURFACE_TYPE_MOD   , ONLY : MF_PHYS_SURF_TYPE
USE FIELD_VARIABLES_MOD        , ONLY : FIELD_VARIABLES
USE TYPE_MODEL                 , ONLY : MODEL
USE FIELDS_MOD                 , ONLY : FIELDS
USE YOMTRAJ                    , ONLY : TRAJ_PHYS_TYPE
USE DDH_MIX                    , ONLY : TYP_DDH
USE MF_PHYS_BASE_STATE_TYPE_MOD, ONLY : MF_PHYS_BASE_STATE_TYPE
USE MF_PHYS_NEXT_STATE_TYPE_MOD, ONLY : MF_PHYS_NEXT_STATE_TYPE

USE YOMXFU                     , ONLY : TXFU
USE YOMCFU                     , ONLY : TCFU
USE FIELD_ARRAY_MODULE
USE YOMDGRADIENT_TYPE_MOD      , ONLY : GRADIENT_TYPE
USE YOE_PHYS_MWAVE             , ONLY : TEPHYSMWAVE

IMPLICIT NONE

TYPE(GEOMETRY)          ,INTENT(IN)               :: YDGEOMETRY
TYPE(CPG_BNDS_TYPE)     ,INTENT(IN)               :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE)     ,INTENT(IN)               :: YDCPG_OPTS
TYPE(CPG_MISC_TYPE)     ,INTENT(INOUT)            :: YDCPG_MISC
TYPE(CPG_GPAR_TYPE)     ,INTENT(INOUT)            :: YDCPG_GPAR
TYPE(CPG_PHY_TYPE)      ,INTENT(INOUT)            :: YDCPG_PHY0
TYPE(CPG_PHY_TYPE)      ,INTENT(INOUT)            :: YDCPG_PHY9
TYPE(MF_PHYS_TYPE)      ,INTENT(INOUT)            :: YDMF_PHYS
TYPE(GRADIENT_TYPE)     ,INTENT(IN)               :: YDHGRAD
TYPE(CPG_DYN_TYPE)      ,INTENT(IN)               :: YDCPG_DYN0
TYPE(CPG_DYN_TYPE)      ,INTENT(IN)               :: YDCPG_DYN9
TYPE(MF_PHYS_SURF_TYPE) ,INTENT(INOUT)            :: YDMF_PHYS_SURF
TYPE(CPG_SL1B_TYPE)     ,INTENT(INOUT)            :: YDCPG_SL1
TYPE(CPG_SL2_TYPE)      ,INTENT(INOUT)            :: YDCPG_SL2
TYPE(FIELD_VARIABLES)   ,INTENT(INOUT)            :: YDVARS
TYPE(TXFU)              ,INTENT(INOUT)            :: YDXFU
TYPE(TCFU)              ,INTENT(INOUT)            :: YDCFU
TYPE(MODEL)             ,INTENT(IN)               :: YDMODEL
TYPE(FIELDS)            ,INTENT(IN)               :: YDFIELDS
TYPE(TEPHYSMWAVE)       ,INTENT(INOUT)            :: YDPHYSMWAVE
TYPE(FIELD_4RB_ARRAY)   ,INTENT(INOUT) ,OPTIONAL  :: YDA_GFLPT
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PGPSDT2D(YDCPG_OPTS%KLON,YDMODEL%YRML_SPPT%YGPSDT(1)%NG2D,YDMODEL%YRML_SPPT%N2D)
REAL(KIND=JPRB)         ,INTENT(INOUT) ,OPTIONAL  :: PGFL(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_GCONF%YGFL%NDIM)
REAL(KIND=JPRB)         ,INTENT(INOUT) ,OPTIONAL  :: PGMVT1(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDFIELDS%YRGMV%YT1%NDIM)
REAL(KIND=JPRB)         ,INTENT(INOUT) ,OPTIONAL  :: PGFLT1(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_GCONF%YGFL%NDIM1)
TYPE(TRAJ_PHYS_TYPE)    ,INTENT(INOUT) ,OPTIONAL  :: PTRAJ_PHYS
TYPE(TYP_DDH)           ,INTENT(INOUT) ,OPTIONAL  :: YDDDH

#include "apl_arome.intfb.h"
#include "aplpar.intfb.h"
#include "apl_alaro.intfb.h"
#include "apl_arpege.intfb.h"
#include "aplsim.intfb.h"
#include "mf_phys_prep.intfb.h"
#include "mf_phys_init.intfb.h"
#include "mf_phys_nhqe_part1.intfb.h"
#include "mf_phys_nhqe_part2.intfb.h"
#include "cpg_pt_ulp_expl.intfb.h"


REAL (KIND=JPRB) :: ZNHQ_TT0L (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)
REAL (KIND=JPRB) :: ZNHQ_TT9  (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)
REAL (KIND=JPRB) :: ZNHQ_TT0  (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)
REAL (KIND=JPRB) :: ZNHQ_TT0M (YDCPG_OPTS%KLON, 1:YDCPG_OPTS%KFLEVG)

CHARACTER(LEN=1024) :: CL_APL_ALARO
LOGICAL :: LL_APL_ALARO

TYPE (MF_PHYS_BASE_STATE_TYPE) :: YLMF_PHYS_BASE_STATE
TYPE (MF_PHYS_NEXT_STATE_TYPE) :: YLMF_PHYS_NEXT_STATE

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE, ZHOOK_HANDLE_1

LOGICAL :: LLGET

IF (LHOOK) CALL DR_HOOK('MF_PHYS',0,ZHOOK_HANDLE)

!=SERIAL
CALL GETENV ('APL_ALARO', CL_APL_ALARO)
LL_APL_ALARO = (CL_APL_ALARO /= '') .AND. (CL_APL_ALARO /= '0')
!=END SERIAL

!$ACDC SERIAL {
IF (LHOOK) CALL DR_HOOK('MF_PHYS:MF_PHYS_NEXT_STATE%INIT',0,ZHOOK_HANDLE_1)
CALL YLMF_PHYS_NEXT_STATE%INIT (YDCPG_SL1, YDGEOMETRY, YDVARS, YDMODEL)
IF (LHOOK) CALL DR_HOOK('MF_PHYS:MF_PHYS_NEXT_STATE%INIT',1,ZHOOK_HANDLE_1)

LLGET = YDMODEL%YRML_DYN%YRDYN%NCURRENT_ITER > 0

!$ACDC }


IF (.NOT. LLGET) THEN  
  
!$ACDC SERIAL {
  IF (LHOOK) CALL DR_HOOK('MF_PHYS:MF_PHYS_BASE_STATE%INIT',0,ZHOOK_HANDLE_1)
  CALL YLMF_PHYS_BASE_STATE%INIT (YDCPG_DYN0, YDCPG_DYN9, YDCPG_PHY0, YDCPG_PHY9, YDVARS, &
  & YDMF_PHYS_SURF, PGFL=PGFL, YDMODEL=YDMODEL)
  IF (LHOOK) CALL DR_HOOK('MF_PHYS:MF_PHYS_BASE_STATE%INIT',1,ZHOOK_HANDLE_1)
!$ACDC }

  !*       4.4.1   Call MF unlagged physics (predictor only if PC scheme).
  
  CALL MF_PHYS_PREP (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDMODEL%YRML_PHY_MF%YRARPHY, YDMODEL%YRML_DYN%YRDYNA, YDCPG_DYN0, &
  & YDCPG_DYN9, YDCPG_PHY0, YDCPG_PHY9, YDVARS)
  
  CALL MF_PHYS_INIT (YDCPG_BNDS, YDCPG_OPTS, YDMF_PHYS, YDCPG_MISC)
  
  ! In the NHQE model, APLPAR enters with Tt and grad(Tt), where Tt = T * exp(-(R/cp) log(pre/prehyd)).
  ! But calculations of APLPAR must use T and grad(T).
  ! So we do a conversion Tt -> T.
  IF (YDMODEL%YRML_DYN%YRDYNA%LNHQE .AND. YDMODEL%YRML_DYN%YRDYNA%LNHQE_SOLVER_QE) THEN

!$ACDC ABORT {

    CALL MF_PHYS_NHQE_PART1 (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, ZNHQ_TT0, ZNHQ_TT0L, ZNHQ_TT0M, &
    & ZNHQ_TT9, YDVARS, YDMODEL)

!$ACDC }

  ENDIF
  
  
  IF (YDMODEL%YRML_PHY_MF%YRSIMPHL%LSIMPH) THEN
!$ACDC ABORT {
    CALL APLSIM(YLMF_PHYS_BASE_STATE, YLMF_PHYS_NEXT_STATE, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, &
    & YDCPG_MISC, YDCPG_PHY0, YDMF_PHYS, YDCPG_DYN0, YDMF_PHYS_SURF, YDVARS, YDMODEL, &
    & PTRAJ_PHYS)
!$ACDC }
  ELSEIF (YDMODEL%YRML_PHY_MF%YRARPHY%LMPA) THEN
!$ACDC ABORT {
    CALL APL_AROME(YDMODEL%YRCST, YLMF_PHYS_BASE_STATE, YLMF_PHYS_NEXT_STATE, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS,        &
    & YDCPG_MISC, YDCPG_GPAR, YDCPG_PHY0, YDMF_PHYS, YDHGRAD, YDCPG_DYN0, &
    & YDMF_PHYS_SURF, YDCPG_SL1, YDVARS, YDFIELDS%YRGMV, YDFIELDS%YRSURF, YDCFU, YDXFU, &
    & YDMODEL, PGFL, PGPSDT2D, PGFLT1, YDDDH)
!$ACDC }
  ELSEIF (YDMODEL%YRML_PHY_MF%YRPHY%LAPL_ARPEGE) THEN
  
    CALL APL_ARPEGE (YLMF_PHYS_BASE_STATE, YLMF_PHYS_NEXT_STATE, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS,  &
    & YDCPG_MISC, YDCPG_GPAR, YDCPG_PHY0, YDMF_PHYS, YDCPG_DYN0, YDMF_PHYS_SURF, YDCPG_SL2, YDVARS,   &
    & YDCFU, YDMODEL, YDDDH, YDPHYSMWAVE)

  ELSEIF ( LL_APL_ALARO ) THEN

!$ACDC ABORT {

    CALL APL_ALARO(YDMODEL%YRCST, YLMF_PHYS_BASE_STATE, YLMF_PHYS_NEXT_STATE, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS,       &
    & YDCPG_MISC, YDCPG_GPAR, YDCPG_PHY0, YDMF_PHYS, YDCPG_DYN0, YDMF_PHYS_SURF, YDCPG_SL1, YDCPG_SL2, &
    & YDVARS, YDFIELDS%YRGMV, YDFIELDS%YRSURF, YDFIELDS%YRCFU, YDFIELDS%YRXFU, YDMODEL, &
    & PGFL, PGMVT1, PGFLT1, YDDDH)

!$ACDC }

  ELSE
!$ACDC ABORT {
    CALL APLPAR (YDMODEL%YRCST, YLMF_PHYS_BASE_STATE, YLMF_PHYS_NEXT_STATE, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS,       &
    & YDCPG_MISC, YDCPG_GPAR, YDCPG_PHY0, YDMF_PHYS, YDCPG_DYN0, YDMF_PHYS_SURF, YDCPG_SL1, YDCPG_SL2, &
    & YDVARS, YDFIELDS%YRGMV, YDFIELDS%YRSURF, YDCFU, YDXFU, YDMODEL, &
    & PGFL, PGMVT1, PGFLT1, PTRAJ_PHYS, YDDDH, YDPHYSMWAVE)
!$ACDC }
  ENDIF
  
  ! Restore Tt and grad(Tt) for NHQE model.
  IF (YDMODEL%YRML_DYN%YRDYNA%LNHQE) THEN
!$ACDC ABORT {
    CALL MF_PHYS_NHQE_PART2 (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, ZNHQ_TT0, ZNHQ_TT0L, ZNHQ_TT0M, &
  & ZNHQ_TT9, YDVARS, YDMODEL)
!$ACDC }
  ENDIF

ENDIF

!*       4.4.2   Store phy. tends.
    
IF (YDMODEL%YRML_DYN%YRDYNA%LPC_FULL.AND.(.NOT.YDMODEL%YRML_DYN%YRDYNA%LPC_CHEAP).AND.YDMODEL%YRML_DYN%YRDYNA%LSLAG) THEN

!$ACDC ABORT {

    
! * currently valid only for SL2TL advection scheme.
!   Remarks KY:
!   - PC scheme with Eulerian scheme: physics is passed differently
!     from predictor to corrector step, and no call of CPG_PT_ULP is done.
!   - LPC_CHEAP: physics is passed differently from predictor to
!     corrector step (in LAPINEB), and no call of CPG_PT_ULP is done.
!   - pressure departure variable: its diabatic tendency is currently
!     assumed to be zero and it is ignored.

  CALL CPG_PT_ULP_EXPL(YDMODEL, YDGEOMETRY, YLMF_PHYS_NEXT_STATE, YDCPG_SL1, YDVARS, YDCPG_BNDS, LLGET, YDA_GFLPT)

!$ACDC }

ENDIF

  
IF (LHOOK) CALL DR_HOOK('MF_PHYS',1,ZHOOK_HANDLE)

END SUBROUTINE MF_PHYS


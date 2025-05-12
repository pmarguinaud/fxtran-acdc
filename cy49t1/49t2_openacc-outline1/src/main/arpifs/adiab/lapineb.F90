SUBROUTINE LAPINEB(YDMODEL, YDGEOMETRY, YDCPG_OPTS, YDCPG_BNDS, YDSL, LDSLPHY, YDCPG_GMV9, &
& YDCPG_GMV9_SI, YDCPG_GMV9_NL, YDCPG_GFL9, YDA_KNOWENO, YDA_KL0, YDA_KLH0, YDA_PLSCAW, YDA_PRSCAW, &
& YDA_KL0H, YDA_PLSCAWH, YDA_PRSCAWH, YDA_PUF, YDA_PVF, YDCPG_SL1, YDA_PSCO, YDA_PCCO, YDCPG_SL2,  &
& YDVARS, YDA_PGFLPC, YDCPG_DDH_TND, YDA_PUP9, YDA_PVP9,  YDA_PTP9, YDCPG_GFLP9)

!$ACDC outline1    


!**** *LAPINEB* - semi-LAgrangian scheme(Interpolation):
!                Interface subroutine for interpolations and lagrangian
!                trends. (Programme INterface d'Ensemble).

!     Purpose.
!     --------
!       Grid point calculations in dynamics.

!       Does the interpolations at the origin point O of the SL-trajectory.
!       Then adds the interpolated quantities of the RHS of equations to
!       yield a provisional value for X(t+dt) for each prognostic variable X.

!       Abbreviation "vwv" stands for "vertical wind variable".

!**   Interface.
!     ----------
!        *CALL* *LAPINEB(.....)

!        Explicit arguments :
!        --------------------
!        INPUT:
!          KFLDSLB2    - second dimension of PB2 (must be >= NFLDSLB2)
!          KST         - first element where computations are performed.
!          KEND       - depth of work.
!          YDSL        - SL_STRUCT definition
!          KSTGLO      - global offset into nproma buffer.
!          LDSLPHY     - split physics in semi lagrangian mode.
!          KIBL        - index into YRCSGEOM/YRGSGEOM instances in YDGEOMETRY
!          KL0         - index of the four western points
!                        of the 16 points interpolation grid.
!          KLH0        - second value of index of the four western points
!                        of the 16 points interpolation grid if needed.
!          PLSCAW      - linear weights (distances) for interpolations.
!          PRSCAW      - non-linear weights for interpolations.
!          KL0H        - half-level KL0
!          PLSCAWH     - linear weights (distances) for interpolations at half levels.
!          PRSCAWH     - non-linear weights for interpolations at half levels.
!          PUF,PVF     - U-comp and V-comp of wind at the medium point necessary
!                        to find the positions of the medium and origin points,
!                        in a local repere linked to computational sphere.
!                        Used in this routine for l2tlff=T.
!          PQ          - moisture at "t".

!        INPUT/OUTPUT:
!          PSCO        - information about geographic position of interpol. point.
!          PCCO        - information about comput. space position of interpol. point.
!          PB2         - "SLBUF2" buffer.
!          PGFLPC      - to store PC scheme quantities for GFL.
!          PGMVTNDSL   - cf. GMVTNDSL_DDH in YOMGPDDH
!          PGFLTNDSL   - cf. GFLTNDSL_DDH in YOMGPDDH
!          PGMVTNDSI   - cf. GMVTNDSI_DDH in YOMGPDDH

!        OUTPUT:
!          PUP9        - Interpolated quantity at O in U-wind eqn
!                        for split physics (if LDSLPHY=.T. only).
!          PVP9        - Interpolated quantity at O in V-wind eqn
!                        for split physics (if LDSLPHY=.T. only).
!          PTP9        - Interpolated quantity at O in T eqn
!                        for split physics (if LDSLPHY=.T. only).
!          PGFLP9      - Interpolated quantity at O in unified_treatment
!                        grid-point fields
!                        for split physics (if LDSLPHY=.T. only).

!        Implicit arguments :
!        --------------------

!     Method.
!     -------

!     Externals.
!     ----------
!      Called by CALL_SL.

!     Reference.
!     ----------
!        ARPEGE documentation about semi-Lagrangian scheme.

!     Author.
!     -------
!      K. YESSAD after the subroutine CPLGDY2 written by
!      Maurice IMBARD   METEO FRANCE/EERM/CRMD
!      Original : JUNE 1991.

! Modifications
! -------------
!   K. Yessad Nov 2009: use ZGMV9 and ZGMV9_SI.
!   K. Yessad (Nov 2009): prune lpc_old.
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   G. Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!   K. Yessad (Dec 2011): Use GPHPRE.
!   M. Diamantakis (Jun 2012): extra argument for quasi-monotone mass fixer
!   G. Mozdzynski (May 2012): further cleaning
!   P. Marguinaud (Jan 2013): Move initialisation of MGPGMV* in lapineb_setup.F90
!   M. Diamantakis (Feb 2013): Remove PCUB due to simplification of mass fixers
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   K. Yessad (July 2014): Rename some variables.
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (March 2017): simplify level numbering in interpolator.
!   K. Yessad (June 2017): Introduce NHQE model.
!   J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH.
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   K. Yessad (Apr 2018): introduce key L_RDRY_VD (ensure consistent definition of "dver" everywhere).
!   M. Diamantakis (June 2018): Don't update GFL variable advected on Atlas grid
!   J. Vivoda (July 2018): LSETTLS with LPC_CHEAP.
!   F. Vana  20-Feb-2019: Vertical quintic interpolation
!   F. Vana  11-Jul-2019: Option LRHS_CURV
!   F. Voitus (Aug 2019): NVDVAR=5.
!   R. El Khatib 02-Jun-2022 Optimization
! End Modifications
!     ------------------------------------------------------------------

USE TYPE_MODEL             , ONLY : MODEL
USE GEOMETRY_MOD           , ONLY : GEOMETRY
USE YOMGMV                 , ONLY : TGMV
USE PARKIND1               , ONLY : JPIM, JPRB
USE YOMCST                 , ONLY : TCST
USE YOMHOOK                , ONLY : DR_HOOK, JPHOOK, LHOOK
USE EINT_MOD               , ONLY : SL_STRUCT
USE FIELD_VARIABLES_MOD    , ONLY : FIELD_VARIABLES
USE CPG_SL1_TYPE_MOD       , ONLY : CPG_SL1F_TYPE
USE CPG_SL2_TYPE_MOD       , ONLY : CPG_SL2_TYPE
USE CPG_DDH_TND_TYPE_MOD   , ONLY : CPG_DDH_TND_TYPE
USE CPG_GMV_TYPE_MOD       , ONLY : CPG_GMV_TYPE
USE CPG_GFL_TYPE_MOD       , ONLY : CPG_GFL_TYPE, CPG_GFLSL_TYPE
USE CPG_OPTS_TYPE_MOD      , ONLY : CPG_OPTS_TYPE, CPG_BNDS_TYPE
USE FIELD_ARRAY_MODULE     , ONLY : FIELD_3IM_ARRAY, FIELD_4IM_ARRAY, FIELD_3RB_ARRAY, FIELD_4RB_ARRAY

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(MODEL)           ,INTENT(IN)     :: YDMODEL
TYPE(GEOMETRY)        ,INTENT(IN)     :: YDGEOMETRY
TYPE(CPG_OPTS_TYPE)   ,INTENT(IN)     :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)   ,INTENT(IN)     :: YDCPG_BNDS
TYPE(SL_STRUCT)       ,INTENT(IN)     :: YDSL
LOGICAL               ,INTENT(IN)     :: LDSLPHY
TYPE(CPG_GMV_TYPE)    ,INTENT(INOUT)  :: YDCPG_GMV9
TYPE(CPG_GMV_TYPE)    ,INTENT(INOUT)  :: YDCPG_GMV9_SI
TYPE(CPG_GMV_TYPE)    ,INTENT(INOUT)  :: YDCPG_GMV9_NL
TYPE(CPG_GFL_TYPE)    ,INTENT(INOUT)  :: YDCPG_GFL9
TYPE(FIELD_3IM_ARRAY) ,INTENT(IN)     :: YDA_KNOWENO  
TYPE(FIELD_4IM_ARRAY) ,INTENT(IN)     :: YDA_KL0      
TYPE(FIELD_4IM_ARRAY) ,INTENT(IN)     :: YDA_KLH0     
TYPE(FIELD_4RB_ARRAY) ,INTENT(IN)     :: YDA_PLSCAW   
TYPE(FIELD_4RB_ARRAY) ,INTENT(IN)     :: YDA_PRSCAW   
TYPE(FIELD_4IM_ARRAY) ,INTENT(IN)     :: YDA_KL0H     
TYPE(FIELD_4RB_ARRAY) ,INTENT(IN)     :: YDA_PLSCAWH  
TYPE(FIELD_4RB_ARRAY) ,INTENT(IN)     :: YDA_PRSCAWH  
TYPE(FIELD_3RB_ARRAY) ,INTENT(IN)     :: YDA_PUF      
TYPE(FIELD_3RB_ARRAY) ,INTENT(IN)     :: YDA_PVF      
TYPE(CPG_SL1F_TYPE)   ,INTENT(IN)     :: YDCPG_SL1
TYPE(FIELD_4RB_ARRAY) ,INTENT(IN)     :: YDA_PSCO     
TYPE(FIELD_4RB_ARRAY) ,INTENT(IN)     :: YDA_PCCO     
TYPE(CPG_SL2_TYPE)    ,INTENT(IN)     :: YDCPG_SL2
TYPE(FIELD_VARIABLES) ,INTENT(INOUT)  :: YDVARS
TYPE(FIELD_4RB_ARRAY) ,INTENT(IN)     :: YDA_PGFLPC   
TYPE(CPG_DDH_TND_TYPE),INTENT(INOUT)  :: YDCPG_DDH_TND
TYPE(FIELD_3RB_ARRAY) ,INTENT(IN)     :: YDA_PUP9     
TYPE(FIELD_3RB_ARRAY) ,INTENT(IN)     :: YDA_PVP9     
TYPE(FIELD_3RB_ARRAY) ,INTENT(IN)     :: YDA_PTP9     
TYPE(CPG_GFLSL_TYPE)  ,INTENT(INOUT)  :: YDCPG_GFLP9
!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: IHVIO, IVDVAR
INTEGER(KIND=JPIM) :: JLEV,  JROF,  JGFL, JFLD

LOGICAL :: LL2TLFFO
LOGICAL :: LLUPDATE_NHX
LOGICAL :: LLDER
LOGICAL :: LLSLINLC2
LOGICAL :: LLNHEE

! * interpolated quantities arrays.
REAL(KIND=JPRB) :: ZUT1(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG),&
 & ZVT1(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZUZ9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG),&
 & ZVZ9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSPT1(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG+1),&
 & ZSPTO(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZDP9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZC9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZC9_SI(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZC9_NL(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
! * interpolated quantities for physics tendencies:
REAL(KIND=JPRB) :: ZUP9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZVP9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

! * grad(orog) used only for NH model with lgwadv=true or lrdbbc=true:
REAL(KIND=JPRB) :: ZOROGL_SR(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZOROGM_SR(YDGEOMETRY%YRDIM%NPROMA)
! * surface wind when lgwadv=true or lrdbbc=true (NH):
REAL(KIND=JPRB) :: ZUST1(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZVST1(YDGEOMETRY%YRDIM%NPROMA)
! * used only for NH model with lgwadv=true:
REAL(KIND=JPRB) :: ZVWVT1(YDGEOMETRY%YRDIM%NPROMA,1:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZVWVT0(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZVWVT1S(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSVDT1(YDGEOMETRY%YRDIM%NPROMA,1:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZNHYT0(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG)
! * interpolated quantities arrays for diagnostic BBC in NH:
REAL(KIND=JPRB)  :: ZDBBC9(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)  :: ZDPHI9(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)  :: ZGWS9(YDGEOMETRY%YRDIM%NPROMA)
! * other quantities (declared in alphabetical order).
REAL(KIND=JPRB) :: ZGMDTX(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZGMDTY(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZINEZV(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZP2(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG),ZQ2(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZVWEI(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) ! weight for vertical integrations.
! * versions of RT
REAL(KIND=JPRB) :: ZR1(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)  ! Moist R at t+dt
REAL(KIND=JPRB) :: ZRT1(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) ! Rd * T(t+dt) or R * T(t+dt)
REAL(KIND=JPRB) :: ZP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZQ(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

REAL(KIND=JPRB) :: ZCOPHI
REAL(KIND=JPRB) :: ZDEPI
REAL(KIND=JPRB) :: ZDGUN
REAL(KIND=JPRB) :: ZDGUX
REAL(KIND=JPRB) :: ZDLUN
REAL(KIND=JPRB) :: ZDLUX
REAL(KIND=JPRB) :: ZDSTRET
REAL(KIND=JPRB) :: ZPHI
REAL(KIND=JPRB) :: ZPP
REAL(KIND=JPRB) :: ZPP1
REAL(KIND=JPRB) :: ZPP9
REAL(KIND=JPRB) :: ZQQ
REAL(KIND=JPRB) :: ZQQ1
REAL(KIND=JPRB) :: ZQQ9
REAL(KIND=JPRB) :: ZSINX
REAL(KIND=JPRB) :: ZTXO
REAL(KIND=JPRB) :: ZTYO
REAL(KIND=JPRB) :: ZCMSLP
REAL(KIND=JPRB) :: ZTAUD_NL

!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "elarche.intfb.h"
#include "gnhgw2svd.intfb.h"
#include "gpuvs.intfb.h"
#include "gprcp_expl.intfb.h"
#include "lacone.intfb.h"
#include "larche.intfb.h"
#include "larcinb.intfb.h"
#include "larcinhb.intfb.h"
#include "verdisint.intfb.h"
#include "mcoords.intfb.h"
#include "rotuv.intfb.h"
#include "rotuvgeo.intfb.h"

!     ------------------------------------------------------------------

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('LAPINEB',0,ZHOOK_HANDLE)

ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDGEM=>YDGEOMETRY%YRGEM, YDVAB=>YDGEOMETRY%YRVERT_GEOM%YRVAB,  &
& YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA, YDVFE=>YDGEOMETRY%YRVERT_GEOM%YRVFE, YDEGSL=>YDGEOMETRY%YREGSL,                   &
& YDEGEO=>YDGEOMETRY%YREGEO, YDDYN=>YDMODEL%YRML_DYN%YRDYN,YDDYNA=>YDMODEL%YRML_DYN%YRDYNA,                                &
& YDCVER=>YDGEOMETRY%YRVERT_GEOM%YRCVER, YDMDDH=>YDMODEL%YRML_DIAG%YRMDDH, YDRIP=>YDMODEL%YRML_GCONF%YRRIP,                &
& YGFL=>YDMODEL%YRML_GCONF%YGFL, YDEPHY=>YDMODEL%YRML_PHY_EC%YREPHY, YDPHY=>YDMODEL%YRML_PHY_MF%YRPHY,                     &
& YDTCCO=>YDMODEL%YRML_DYN%YYTCCO, YDTSCO=>YDMODEL%YRML_DYN%YYTSCO, YDGEOMVARS => YDVARS%GEOMETRY, YDCST=>YDMODEL%YRCST)

ASSOCIATE(NUMFLDS=>YGFL%NUMFLDS, YCOMP=>YGFL%YCOMP, YCVGQ=>YGFL%YCVGQ, NPROMA=>YDDIM%NPROMA, NFLEVG=>YDDIMV%NFLEVG,   &
& L2TLFF=>YDDYN%L2TLFF, LADVF=>YDDYN%LADVF,  NCURRENT_ITER=>YDDYN%NCURRENT_ITER, NSITER=>YDDYN%NSITER,                &
& NVLAG=>YDDYN%NVLAG, RCMSLP0=>YDDYN%RCMSLP0, RW2TLFF=>YDDYN%RW2TLFF, LRHS_CURV=>YDDYN%LRHS_CURV,                     &
& EDELX=>YDEGEO%EDELX, EDELY=>YDEGEO%EDELY, LAGPHY=>YDEPHY%LAGPHY, LEPHYS=>YDEPHY%LEPHYS, NSTTYP=>YDGEM%NSTTYP,       &
& RC2M1=>YDGEM%RC2M1, RC2P1=>YDGEM%RC2P1, RLOCEN=>YDGEM%RLOCEN, RMUCEN=>YDGEM%RMUCEN, RSTRET=>YDGEM%RSTRET,           &
& RTDT=>YDRIP%RTDT, LMPHYS=>YDPHY%LMPHYS, KST=>YDCPG_BNDS%KIDIA, KEND=>YDCPG_BNDS%KFDIA, GEMU=>YDGEOMVARS%GEMU%P,     &
& GSQM2=>YDGEOMVARS%GSQM2%P, GNORDM=>YDGEOMVARS%GNORDM%P,GNORDL=>YDGEOMVARS%GNORDL%P)

!     ------------------------------------------------------------------

!*       1.    PRELIMINARY ALLOCATIONS AND INITIALISATIONS:
!              -------------------------------------------

! ky: if LGWADV.AND.ND4SYS==2.AND.NCURRENT_ITER==NSITER, PGMVT1(.,YT1%MNHX)
!     is updated in CPGLAG with a diagnosed value of NHX, so no need to do here
LLUPDATE_NHX=YDDYNA%LNHX.AND.(YDDYNA%NVDVAR == 4 .OR. YDDYNA%NVDVAR == 5) &
            & .AND.(.NOT.YDDYNA%LGWADV.OR.(YDDYNA%ND4SYS==2.AND.NCURRENT_ITER<NSITER))

ZDEPI=2.0_JPRB*YDCST%RPI

ZDSTRET=2.0_JPRB*RSTRET

DO JFLD = 1, SIZE (YDCPG_GMV9%GMV)

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ZERO_GMV9 {

  YDCPG_GMV9%GMV (JFLD)%P=0.0_JPRB

!$ACDC }

ENDDO

IF(YDDYNA%LSLINL) THEN
  DO JFLD = 1, SIZE (YDCPG_GMV9_SI%GMV)

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ZERO_GMV9_SI {

    YDCPG_GMV9_SI%GMV (JFLD)%P=0.0_JPRB

!$ACDC }

  ENDDO
ENDIF

IF(YDDYNA%LPC_CHEAP.AND.YDDYNA%LSETTLS) THEN
  DO JFLD = 1, SIZE (YDCPG_GMV9_NL%GMV)

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ZERO_GMV9_SL {

    YDCPG_GMV9_NL%GMV (JFLD)%P=0.0_JPRB

!$ACDC }

  ENDDO
ENDIF

IF( LDSLPHY ) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ZERO_UV9 {

  ZUP9(:,:)=0.0_JPRB
  ZVP9(:,:)=0.0_JPRB

!$ACDC }

ENDIF


!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ROTUVGEO_1 {

! "rotate" qx/qy with "compass" (gnordm/gnordl)
CALL ROTUVGEO(NFLEVG,NPROMA,KST,KEND,GNORDM,GNORDL,&
  &YDA_PCCO%P(:,:,YDTCCO%M_RQX),YDA_PCCO%P(:,:,YDTCCO%M_RQY),ZP,ZQ,LDIR=.TRUE.)

!$ACDC }


IHVIO=0
DO JGFL=1,NUMFLDS
  IF(YCOMP(JGFL)%CSLINT == 'LAIHVT      ' .OR.&
     & YCOMP(JGFL)%CSLINT == 'LAIHVTQM    ' .OR.&
     & YCOMP(JGFL)%CSLINT == 'LAIHVTQMH   ' ) THEN
    IHVIO=1
  ENDIF
ENDDO

LL2TLFFO=L2TLFF.AND.(LMPHYS.OR.LEPHYS).AND.(.NOT.LAGPHY)

IF(YDCPG_OPTS%LRPLANE) THEN
  ZDLUN=REAL(YDSL%NDLUNG,JPRB)
  ZDLUX=REAL(YDSL%NDLUXG,JPRB)
  ZDGUN=REAL(YDSL%NDGUNG,JPRB)
  ZDGUX=REAL(YDSL%NDGUXG,JPRB)

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LRPLANE {

  DO JROF = KST, KEND
    ZGMDTX (JROF)=0.5_JPRB*YDGEOMVARS%GM%P(JROF)*RTDT/EDELX
    ZGMDTY (JROF)=0.5_JPRB*YDGEOMVARS%GM%P(JROF)*RTDT/EDELY
    ZINEZV(JROF)=MAX(0.0_JPRB,SIGN(1.0_JPRB,(YDGEOMVARS%RINDX%P(JROF)-ZDLUN)*(&
     & ZDLUX-YDGEOMVARS%RINDX%P(JROF))))*&
     & MAX(0.0_JPRB,SIGN(1.0_JPRB,(YDGEOMVARS%RINDY%P(JROF)-ZDGUN)*(ZDGUX-YDGEOMVARS%RINDY%P(JROF))))
  ENDDO
  ! * Retrieve the origin point coordinates from LAPINEA
  !   for "2 Omega vectorial a k" recalculation.
  YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLON)=YDA_PSCO%P(KST:KEND,1:NFLEVG,YDTSCO%M_SINLA)
  YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLAT)=YDA_PSCO%P(KST:KEND,1:NFLEVG,YDTSCO%M_COPHI)

!$ACDC }

ENDIF


!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ZVWEI {

! * [vertical weight]_l = [delta B]_l
DO JLEV=1,NFLEVG
  ZVWEI(KST:KEND,JLEV)=YDVAB%VDELB(JLEV)
ENDDO

!$ACDC }


!     ------------------------------------------------------------------

!*       2.    INTERPOLATIONS AT THE ORIGIN POINT AND UPDATE EQNS RHS
!              ------------------------------------------------------

!        2.1   INTERPOLATIONS AND/OR GET/SAVE INTERPOLATED VALUES.

!        2.1.1 INTERPOLATIONS.

IF ( .NOT. (YDDYNA%LPC_CHEAP.AND.(NCURRENT_ITER > 0)) ) THEN

  ! * Full level quantities:

  CALL LARCINB(YDGEOMETRY,YDVARS,&
   ! --- INPUT ---------------------------------------------------------
   & YDMODEL%YRML_DIAG%YRLDDH,YDMDDH,YDMODEL%YRML_GCONF,YDMODEL%YRML_DYN,&
   & YDCPG_OPTS,YDCPG_BNDS,YDSL%NASLB1,IHVIO,LDSLPHY,LL2TLFFO,&
   & YDA_KL0%P,YDA_KLH0%P,YDA_PLSCAW%P,YDA_PRSCAW%P,YDCPG_SL1,YDA_KNOWENO%P,&
   ! --- INOUT ---------------------------------------------------------
   & YDCPG_DDH_TND, &
   ! --- OUTPUT --------------------------------------------------------
   & YDCPG_GMV9,YDCPG_GMV9_SI,YDCPG_GMV9_NL,YDCPG_GFL9,ZDP9,ZC9,ZC9_SI,ZC9_NL,&
   & ZUZ9,ZVZ9,ZUP9,ZVP9,YDA_PTP9%P,YDCPG_GFLP9,ZDBBC9,ZDPHI9,ZGWS9)

  ! * Half level quantities:
  IF (YDDYNA%LNHDYN.AND.YDDYNA%LGWADV.AND.(.NOT.YDCVER%LVFE_GW)) THEN

!$ACDC ABORT {

    ! * Remarks:
    !   - LARCINHB is called only:
    !     - in a finite difference vertical discretisation;
    !     - in a VFE discretisation if (gw) is at half levels and if the
    !       Laplacian term is discretised with FD.
    !     For the other cases of VFE, (gw) is at full levels so the ad-hoc
    !      interpolation should be done under LARCINB.
    CALL LARCINHB(YDGEOMETRY,YDMODEL%YRML_DYN,YDCPG_OPTS,YDCPG_BNDS,YDSL%NASLB1,YDA_KL0H%P,YDA_PLSCAWH%P,YDA_PRSCAWH%P, &
     & YDCPG_SL1,YDCPG_GMV9%SVD%P,YDCPG_GMV9_NL%SVD%P)

!$ACDC }

  ENDIF

ENDIF

!        2.1.2 SAVE INTERPOLATED VALUES IN GPPCBUF AND ZGPPC.

IF ( YDDYNA%LPC_CHEAP.AND.(NCURRENT_ITER == 0) ) THEN

!$ACDC ABORT {

  ! initialization with zero for [U,V] (useful ???)
  YDVARS%PCF_U%PC_PH = 0._JPRB
  YDVARS%PCF_V%PC_PH = 0._JPRB

  ! U,V:
  IF (LRHS_CURV) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDVARS%PCF_U%PC_PH(JROF,JLEV)=YDCPG_GMV9%U%P(JROF,JLEV)
        YDVARS%PCF_V%PC_PH(JROF,JLEV)=YDCPG_GMV9%V%P(JROF,JLEV)
        IF( LDSLPHY ) THEN
          YDVARS%PCF_UP%PC_PH(JROF,JLEV)=ZUP9(JROF,JLEV)
          YDVARS%PCF_VP%PC_PH(JROF,JLEV)=ZVP9(JROF,JLEV)
        ENDIF
      ENDDO
    ENDDO
  ELSE
    ! - includes rotation matrix.
    ! - if LADVF=T, does not yet include [2 Omega wedge vec(r)] at O
    !   (additional incrementation to be done in part 2.3.2).
    CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP,ZQ,&
      &YDVARS%PCF_U%PC_PH,YDVARS%PCF_V%PC_PH,LDIR=.FALSE.)
    IF( LDSLPHY ) THEN
      CALL ROTUV(NFLEVG,NPROMA,KST,KEND,ZUP9,ZVP9,ZP,ZQ,&
        &YDVARS%PCF_U%PC_PH,YDVARS%PCF_V%PC_PH,LDIR=.FALSE.)
    ENDIF
  ENDIF

  ! T:

  DO JLEV = 1, NFLEVG
    DO JROF = KST, KEND
      YDVARS%PCF_T%PC_PH(JROF,JLEV)=YDCPG_GMV9%T%P(JROF,JLEV)
    ENDDO
  ENDDO
  IF( LDSLPHY ) THEN
    DO JLEV = 1, NFLEVG
      DO JROF = KST, KEND
        YDVARS%PCF_TP%PC_PH(JROF,JLEV)=YDA_PTP9%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

  ! NH variables:
  IF( YDDYNA%LNHDYN ) THEN
    !!!! LNHEE
    YDVARS%PCF_SPD%PC_PH(KST:KEND,1:NFLEVG)=YDCPG_GMV9%SPD%P(KST:KEND,1:NFLEVG)
    !---------------------------------------------------------------------------------
    YDVARS%PCF_SVD%PC_PH(KST:KEND,1:NFLEVG)=YDCPG_GMV9%SVD%P(KST:KEND,1:NFLEVG)
    IF (LLUPDATE_NHX)THEN
      YDVARS%PCF_NHX%PC_PH(KST:KEND,1:NFLEVG)=YDCPG_GMV9%NHX%P(KST:KEND,1:NFLEVG)
    ENDIF
    IF( YDDYNA%LRDBBC ) THEN
      YDVARS%PCF_BBC%PC_PH(KST:KEND)=ZDBBC9(KST:KEND)
      YDVARS%PCF_DPHI%PC_PH(KST:KEND)=ZDPHI9(KST:KEND)
      YDVARS%PCF_GWS%PC_PH(KST:KEND)=ZGWS9(KST:KEND)
    ENDIF
  ENDIF

  ! Continuity equation:
  DO JLEV = 1, NFLEVG
    DO JROF = KST, KEND
      YDVARS%PCF_SP%PC_PH(JROF,JLEV)=ZDP9(JROF,JLEV)
      YDVARS%PCF_CP%PC_PH(JROF,JLEV)=ZC9(JROF,JLEV)
    ENDDO
  ENDDO

  ! GFL:
  IF( LDSLPHY ) THEN
    ! ky: this one must be saved in GFL(PC), not in ZGPPC (change to do later).
    DO JFLD=1,SIZE(YDVARS%GFL_PTR)
      DO JLEV = 1, NFLEVG
        DO JROF = KST, KEND
          YDVARS%GFL_PTR(JFLD)%PC_PH(JROF,JLEV)=&
           & YDCPG_GFLP9%GFL(JFLD)%P(JROF,JLEV)
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  DO JFLD=1,SIZE(YDVARS%GFL_PTR)
    IF ((YDVARS%GFL_PTR(JFLD)%YCOMP%LADV).AND.(YDVARS%GFL_PTR(JFLD)%YCOMP%LPC)) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDA_PGFLPC%P(JROF,JLEV,YDVARS%GFL_PTR(JFLD)%YCOMP%MPPC)=YDCPG_GFL9%GFL(JFLD)%P(JROF,JLEV)
        ENDDO
      ENDDO
    ENDIF
  ENDDO

!$ACDC }

ENDIF

!        2.1.3 GET INTERPOLATED VALUES FROM GPPCBUF AND ZGPPC.

IF ( YDDYNA%LPC_CHEAP.AND.(NCURRENT_ITER > 0) ) THEN

!$ACDC ABORT {

  ! U,V:
  ! - includes rotation matrix.
  ! - if LADVF=T, includes 2 Omega wedge vec(r) at O.
  YDCPG_GMV9%U%P(KST:KEND,1:NFLEVG)=YDVARS%PCF_U%PC_PH(KST:KEND,1:NFLEVG)
  YDCPG_GMV9%V%P(KST:KEND,1:NFLEVG)=YDVARS%PCF_V%PC_PH(KST:KEND,1:NFLEVG)
  IF( LDSLPHY ) THEN
    ZUP9(KST:KEND,1:NFLEVG)=YDVARS%PCF_UP%PC_PH(KST:KEND,1:NFLEVG)
    ZVP9(KST:KEND,1:NFLEVG)=YDVARS%PCF_VP%PC_PH(KST:KEND,1:NFLEVG)
  ENDIF

  ! T:
  YDCPG_GMV9%T%P(KST:KEND,1:NFLEVG)=YDVARS%PCF_T%PC_PH(KST:KEND,1:NFLEVG)
  IF( LDSLPHY ) THEN
    YDA_PTP9%P(KST:KEND,1:NFLEVG)=YDVARS%PCF_TP%PC_PH(KST:KEND,1:NFLEVG)
  ENDIF

  ! NH variables:
  IF( YDDYNA%LNHDYN ) THEN
    !!!!! LNHEE
    YDCPG_GMV9%SPD%P(KST:KEND,1:NFLEVG)=YDVARS%PCF_SPD%PC_PH(KST:KEND,1:NFLEVG)
    !-------------------------------------------------------------------------------- 
    YDCPG_GMV9%SVD%P(KST:KEND,1:NFLEVG)=YDVARS%PCF_SVD%PC_PH(KST:KEND,1:NFLEVG)
    IF (LLUPDATE_NHX)THEN
      YDCPG_GMV9%NHX%P(KST:KEND,1:NFLEVG)=YDVARS%PCF_NHX%PC_PH(KST:KEND,1:NFLEVG)
    ENDIF
    IF( YDDYNA%LRDBBC ) THEN
      ZDBBC9(KST:KEND)=YDVARS%PCF_BBC%PC_PH(KST:KEND)
      ZDPHI9(KST:KEND)=YDVARS%PCF_DPHI%PC_PH(KST:KEND)
      ZGWS9(KST:KEND)=YDVARS%PCF_GWS%PC_PH(KST:KEND)
    ENDIF
  ENDIF

  ! Continuity equation:
  ZDP9(KST:KEND,1:NFLEVG)=YDVARS%PCF_SP%PC_PH(KST:KEND,1:NFLEVG)
  ZC9(KST:KEND,1:NFLEVG)=YDVARS%PCF_CP%PC_PH(KST:KEND,1:NFLEVG)

  ! GFL:
  IF( LDSLPHY ) THEN
    DO JFLD=1,SIZE(YDVARS%GFL_PTR)
      DO JLEV = 1, NFLEVG
        DO JROF = KST, KEND
          YDCPG_GFLP9%GFL(JFLD)%P(JROF,JLEV)=&
           & YDVARS%GFL_PTR(JFLD)%PC_PH(JROF,JLEV)
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  DO JFLD=1,SIZE(YDVARS%GFL_PTR)
    IF ((YDVARS%GFL_PTR(JFLD)%YCOMP%LADV).AND.(YDVARS%GFL_PTR(JFLD)%YCOMP%LPC)) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDCPG_GFL9%GFL(JFLD)%P(JROF,JLEV)=YDA_PGFLPC%P(JROF,JLEV,YDVARS%GFL_PTR(JFLD)%YCOMP%MPPC)
        ENDDO
      ENDDO
    ENDIF
  ENDDO

!$ACDC }

ENDIF

!*       2.2   ADD QUANTITIES INTERPOLATED AT THE ORIGIN POINT
!              TO ARRAYS P(X)T1 FOR CONTINUITY EQUATION.

!        2.2.0 ADD NL PART SPECIFIC TO SETTLS.

IF (YDDYNA%LPC_CHEAP.AND.YDDYNA%LSETTLS.AND.(NCURRENT_ITER==0)) THEN

!$ACDC ABORT {

  DO JLEV = 1, NFLEVG
    DO JROF = KST, KEND
      ZC9(JROF,JLEV)=ZC9(JROF,JLEV)+ZC9_NL(JROF,JLEV)
    ENDDO
  ENDDO

!$ACDC }

ENDIF

!        2.2.1 ADD INTERPOLATED TERMS.

IF (NVLAG == 2) THEN

  IF(YDCVER%LVERTFE) THEN
    CALL ABOR1(' LAPINEB 2.2.1: LVERTFE=.T. NOT CODED WITH NVLAG == 2')
  ELSE

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=NVLAG2 {

    DO JLEV=1,NFLEVG
      DO JROF = KST, KEND
        YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+&
         & ZVWEI(JROF,JLEV)*(ZDP9(JROF,JLEV)+ZC9(JROF,JLEV))
      ENDDO
    ENDDO

!$ACDC }

  ENDIF

ELSEIF (NVLAG == 3) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=NVLAG3 {

  IF(YDCVER%LVERTFE) THEN
    DO JLEV=1,NFLEVG
      DO JROF = KST, KEND
        ZSPT1(JROF,JLEV)=ZVWEI(JROF,JLEV)*YDVETA%VFE_RDETAH(JLEV)&
         & *(ZDP9(JROF,JLEV)+ZC9(JROF,JLEV))  
      ENDDO
    ENDDO
    ZSPT1(KST:KEND,0)=0.0_JPRB
    ZSPT1(KST:KEND,NFLEVG+1)=0.0_JPRB
    CALL VERDISINT(YDVFE,YDCVER,'ITOP','11',NPROMA,KST,KEND,NFLEVG,ZSPT1,POUTS=ZSPTO)
    YDVARS%SP%T1(KST:KEND)=YDVARS%SP%T1(KST:KEND)+ZSPTO(KST:KEND)
  ELSE
    DO JLEV=1,NFLEVG
      YDVARS%SP%T1(KST:KEND)=YDVARS%SP%T1(KST:KEND)+&
       & ZVWEI(KST:KEND,JLEV)*(ZDP9(KST:KEND,JLEV)+ZC9(KST:KEND,JLEV))  
    ENDDO
  ENDIF

  LLSLINLC2=YDDYNA%LSLINLC2.AND.YDDYNA%NGWADVSI==1.AND.(NCURRENT_ITER == 0)
  IF (LLSLINLC2.AND. .NOT.YDCVER%LVERTFE) THEN
    ! Add the linear term, FD vertically integrated, to PB2.
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDCPG_SL2%SPSI(JROF)=YDCPG_SL2%SPSI(JROF)+ZVWEI(JROF,JLEV)*ZC9_SI(JROF,JLEV)
      ENDDO
    ENDDO
  ELSEIF (LLSLINLC2.AND.YDCVER%LVERTFE) THEN
    ! Add the linear term, VFE vertically integrated, to PB2.
    DO JLEV=1,NFLEVG
      DO JROF = KST, KEND
        ZSPT1(JROF,JLEV)=ZVWEI(JROF,JLEV)*YDVETA%VFE_RDETAH(JLEV)*ZC9_SI(JROF,JLEV)
      ENDDO
    ENDDO
    ZSPT1(KST:KEND,0)=0.0_JPRB
    ZSPT1(KST:KEND,NFLEVG+1)=0.0_JPRB
    CALL VERDISINT(YDVFE,YDCVER,'ITOP','11',NPROMA,KST,KEND,NFLEVG,ZSPT1,POUTS=ZSPTO)
    DO JROF = KST, KEND
      YDCPG_SL2%SPSI(JROF)=YDCPG_SL2%SPSI(JROF)+ZSPTO(JROF)
    ENDDO
  ENDIF

!$ACDC }

ENDIF

!*       2.3   ADD QUANTITIES INTERPOLATED AT THE ORIGIN
!              POINTS TO ARRAYS P(X)T1 FOR EQUATIONS OTHER THAN
!              CONTINUITY EQUATION.

!        2.3.1a ADD NL PART SPECIFIC TO SETTLS.

IF (YDDYNA%LPC_CHEAP.AND.YDDYNA%LSETTLS.AND.(NCURRENT_ITER==0)) THEN

!$ACDC ABORT {

  DO JFLD = 1, SIZE (YDCPG_GMV9%GMV)
    DO JLEV = 1, NFLEVG
      DO JROF = KST, KEND
        YDCPG_GMV9%GMV (JFLD)%P(JROF,JLEV)=YDCPG_GMV9%GMV (JFLD)%P(JROF,JLEV) &
         & +YDCPG_GMV9_NL%GMV (JFLD)%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDDO

!$ACDC }

ENDIF

!        2.3.1b ADD GENERAL CONTRIBUTIONS IN P[X]T1.

! * Momentum equation.
IF( (YDDYNA%LPC_CHEAP.AND.(NCURRENT_ITER > 0)) .OR. LRHS_CURV ) THEN

!$ACDC ABORT {

  ! Rotation (p,q) has already been taken into account in (ZU..9,ZV..9).
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      YDVARS%U%T1(JROF,JLEV)=YDVARS%U%T1(JROF,JLEV)+YDCPG_GMV9%U%P(JROF,JLEV)
      YDVARS%V%T1(JROF,JLEV)=YDVARS%V%T1(JROF,JLEV)+YDCPG_GMV9%V%P(JROF,JLEV)
    ENDDO
  ENDDO
  IF( LDSLPHY ) THEN
    YDA_PUP9%P(KST:KEND,1:NFLEVG)=ZUP9(KST:KEND,1:NFLEVG)
    YDA_PVP9%P(KST:KEND,1:NFLEVG)=ZVP9(KST:KEND,1:NFLEVG)
  ENDIF

!$ACDC }

ELSE

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ROTUV {

  CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP,ZQ,&
    &YDVARS%U%T1,YDVARS%V%T1,LDIR=.FALSE.,LDADD=.TRUE.)
  IF( LDSLPHY ) THEN
    CALL ROTUV(NFLEVG,NPROMA,KST,KEND,ZUP9,ZVP9,ZP,ZQ,YDA_PUP9%P,YDA_PVP9%P,LDIR=.FALSE.)
  ENDIF

  IF (YDDYNA%LSLINL.AND.YDDYNA%NGWADVSI==1.AND.NCURRENT_ITER==0) THEN
    CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9_SI%U%P,YDCPG_GMV9_SI%V%P,ZP,ZQ,&
      &YDCPG_SL2%USI,YDCPG_SL2%VSI,LDIR=.FALSE.,LDADD=.TRUE.)
  ENDIF

!$ACDC }

ENDIF


!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=TEMP_EQ {

! * Temperature equation.
DO JLEV = 1, NFLEVG
  DO JROF = KST, KEND
    YDVARS%T%T1(JROF,JLEV)=YDVARS%T%T1(JROF,JLEV)+YDCPG_GMV9%T%P(JROF,JLEV)
  ENDDO
ENDDO
IF (YDDYNA%LSLINL.AND.YDDYNA%NGWADVSI==1.AND.NCURRENT_ITER==0) THEN
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      YDCPG_SL2%TSI(JROF,JLEV)=YDCPG_SL2%TSI(JROF,JLEV)+YDCPG_GMV9_SI%T%P(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

!$ACDC }

! * GFL variables equations.

DO JFLD = 1, SIZE (YDVARS%GFL_PTR)
  IF (YDDYNA%LSLDIA .AND. (YDVARS%GFL_PTR(JFLD)%YCOMP%CNAME == 'DPDETA0         ') .AND. YDVARS%GFL_PTR(JFLD)%YCOMP%LGP) THEN
    ! SL dynamics diagnostics
    ZCMSLP=RCMSLP0/(YDCST%RD*YDCPG_OPTS%RTSUR)

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=DPDETA0 {

    IF (YDCVER%LVERTFE) THEN
      DO JLEV=1,NFLEVG
        !DIR$ IVDEP
        DO JROF=KST,KEND
          YDVARS%GFL_PTR(JFLD)%T1(JROF,JLEV)=YDVAB%VDELA(JLEV)&
           & + EXP(ZDP9(JROF,JLEV)-ZCMSLP*YDGEOMVARS%OROG%P(JROF))*YDVAB%VDELB(JLEV)
        ENDDO
      ENDDO
    ELSE
      DO JLEV=1,NFLEVG
        !DIR$ IVDEP
        DO JROF=KST,KEND
          YDVARS%GFL_PTR(JFLD)%T1(JROF,JLEV)=(YDVAB%VAH(JLEV)-YDVAB%VAH(JLEV-1))&
           & + EXP(ZDP9(JROF,JLEV)-ZCMSLP*YDGEOMVARS%OROG%P(JROF))*YDVAB%VDELB(JLEV)
        ENDDO
      ENDDO
    ENDIF

!$ACDC }

  ENDIF

  IF (YDVARS%GFL_PTR(JFLD)%YCOMP%LADV) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=CVGQ {

    IF ((YDVARS%GFL_PTR(JFLD)%YCOMP%CNAME == 'CVGQ            ') .AND. YDVARS%GFL_PTR(JFLD)%YCOMP%LGP) THEN
      ! Computation of SL Moisture Convergence for MF physics
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDVARS%GFL_PTR(JFLD)%T1(JROF,JLEV)=&
          & YDCPG_GFL9%GFL(JFLD)%P(JROF,JLEV) - YDVARS%Q%T0(JROF,JLEV)
        ENDDO
      ENDDO
    ELSEIF (.NOT.YDVARS%GFL_PTR(JFLD)%YCOMP%LMGRID) THEN   
      DO JLEV = 1, NFLEVG
        DO JROF = KST, KEND
          YDVARS%GFL_PTR(JFLD)%T1(JROF,JLEV)=&
           & YDVARS%GFL_PTR(JFLD)%T1(JROF,JLEV)+&
           & YDCPG_GFL9%GFL(JFLD)%P(JROF,JLEV)  
        ENDDO
      ENDDO
    ENDIF

!$ACDC }

  ENDIF
ENDDO

! * Computation of spectral Moisture Convergence for French physics
IF (YCVGQ%LSP) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=YCVGQ {

  YDVARS%CVGQ%T1(KST:KEND,1:NFLEVG)=YDVARS%Q%T1(KST:KEND,1:NFLEVG)

!$ACDC }

ENDIF   

! * Pressure departure variable equation. 
IF(YDDYNA%LNHDYN) THEN !!!! LNHEE

!$ACDC ABORT {

  DO JLEV = 1, NFLEVG
    DO JROF = KST, KEND
      YDVARS%SPD%T1(JROF,JLEV)=YDVARS%SPD%T1(JROF,JLEV)&
       & +YDCPG_GMV9%SPD%P(JROF,JLEV)  
    ENDDO
  ENDDO

  IF (YDDYNA%LSLINL.AND.YDDYNA%NGWADVSI==1.AND.NCURRENT_ITER==0) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDCPG_SL2%PDSI(JROF,JLEV)=YDCPG_SL2%PDSI(JROF,JLEV)&
         & +YDCPG_GMV9_SI%SPD%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

!$ACDC }

ENDIF

! * Vertical divergence variable equation.
IF(YDDYNA%LNHDYN) THEN

!$ACDC ABORT {

  ! PGMVT1(.,.,YT1%MSVD): half levels lbar=0 to nflevg-1
  !  if LGWADV.AND.(.NOT.LVFE_GW); full levels otherwise.

  DO JLEV = 1, NFLEVG
    DO JROF = KST, KEND
      YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)&
       & +YDCPG_GMV9%SVD%P(JROF,JLEV)  
    ENDDO
  ENDDO

  IF (LLUPDATE_NHX) THEN
    DO JLEV = 1, NFLEVG
      DO JROF = KST, KEND
        YDVARS%NHX%T1(JROF,JLEV)=YDVARS%NHX%T1(JROF,JLEV)&
         & +YDCPG_GMV9%NHX%P(JROF,JLEV)  
      ENDDO
    ENDDO
  ENDIF
  IF (YDDYNA%LSLINL.AND.YDDYNA%NGWADVSI==1.AND.NCURRENT_ITER==0) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDCPG_SL2%VDSI(JROF,JLEV)=YDCPG_SL2%VDSI(JROF,JLEV)&
         & +YDCPG_GMV9_SI%SVD%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

!$ACDC }

ENDIF

!        2.3.1d COMPUTE "moist R" and appropriate (RT) at t+dt.

! ZRT1 contains appropriate value of (RT), consistent with definition of "dver".
IF (YDDYNA%L_RDRY_VD) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=L_RDRY_VD {

  DO JLEV = 1, NFLEVG
    DO JROF = KST, KEND
      ZRT1(JROF,JLEV)=YDCST%RD*YDVARS%T%T1(JROF,JLEV)
    ENDDO
  ENDDO

!$ACDC }

ELSE

!$ACDC ABORT {

  CALL GPRCP_EXPL (YDCST, YDCPG_BNDS, YDCPG_OPTS, PR=ZR1, YDVARS=YDVARS, KGFLTYP=1)
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      ZRT1(JROF,JLEV)=ZR1(JROF,JLEV)*YDVARS%T%T1(JROF,JLEV)
    ENDDO
  ENDDO

!$ACDC }

ENDIF

!        2.3.2 SPECIAL TREATMENT OF CORIOLIS TERM:

! For the corrector step of LPC_CHEAP nothing has to be done here
!  because term [2 Omega wedge vec(r)] at O has already been taken
!  into account in the calculations done in part 2.1.3.

IF (LADVF.AND..NOT.(YDDYNA%LPC_CHEAP.AND.NCURRENT_ITER > 0)) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LACONE {

  ! * Analytical computation of "2 Omega wedge a k" at "O":
  IF (YDCPG_OPTS%LRPLANE) THEN
    ! * calculation of LACONE done already in ELARCHE -> results are in rlon/rlat
    YDCPG_GMV9%U%P(KST:KEND,1:NFLEVG)=YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLON)
    YDCPG_GMV9%V%P(KST:KEND,1:NFLEVG)=YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLAT)
  ELSE
    CALL LACONE(NPROMA,KST,KEND,NFLEVG,NSTTYP,YDCPG_OPTS%N2DINI,YDDYN%YRSWE,&
     & YDCST%ROMEGA,ZDSTRET,RC2M1,RC2P1,RMUCEN,YDCST%RA,YDCST%RPI,&
     & YDA_PCCO%P(:,:,YDTCCO%M_RLON),YDA_PCCO%P(:,:,YDTCCO%M_RLAT),&
     & YDCPG_GMV9%U%P,YDCPG_GMV9%V%P)
  ENDIF

  ! * Add the content of "pu9,pv9" to "(U,V)(t+dt)":
  IF (LRHS_CURV) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDVARS%U%T1(JROF,JLEV)=YDVARS%U%T1(JROF,JLEV)+YDCPG_GMV9%U%P(JROF,JLEV)
        YDVARS%V%T1(JROF,JLEV)=YDVARS%V%T1(JROF,JLEV)+YDCPG_GMV9%V%P(JROF,JLEV)
      ENDDO
    ENDDO
  ELSE
    CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP,ZQ,&
      &YDVARS%U%T1,YDVARS%V%T1,LDIR=.FALSE.,LDADD=.TRUE.)
  ENDIF

  IF (.NOT.L2TLFF.AND. YDDYNA%LPC_CHEAP.AND.(NCURRENT_ITER == 0) ) THEN
    ! ZGPPC(wind) must be incremented with term [2 Omega wedge vec(r)] at O.
    !  If L2TLFF=T, the incrementation is not done here but in part 2.3.4
    !  in order to take account of the recomputed version of O.
    IF (LRHS_CURV) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDVARS%PCF_U%PC_PH(JROF,JLEV)=YDVARS%PCF_U%PC_PH(JROF,JLEV)+YDCPG_GMV9%U%P(JROF,JLEV)
          YDVARS%PCF_V%PC_PH(JROF,JLEV)=YDVARS%PCF_V%PC_PH(JROF,JLEV)+YDCPG_GMV9%V%P(JROF,JLEV)
        ENDDO
      ENDDO
    ELSE
      CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP,ZQ,&
        &YDVARS%PCF_U%PC_PH,YDVARS%PCF_V%PC_PH,LDIR=.FALSE.,LDADD=.TRUE.)
    ENDIF
  ENDIF

!$ACDC }

ENDIF

!        2.3.4 SPECIAL TREATMENT OF CORIOLIS TERM:
!              CASE "L2TLFF=T".

! * Refined calculation of the origin point.
!   Refined treatment of Coriolis term in 2TL scheme (or in 3TL SLI scheme).
!   Works for the following configurations:
!    - SL3TL or SL2TL.
!    - LADVF=.T.
!    - NWLAG=3.
!   If LPC_CHEAP=T recalculation of O is done in the predictor step only.

!  This piece of code still requires rotational matrix even when LRHS_CURV=true

IF ( L2TLFF .AND. .NOT.(YDDYNA%LPC_CHEAP.AND.(NCURRENT_ITER > 0)) ) THEN
  IF (YDCPG_OPTS%LRPLANE) THEN

!$ACDC ABORT {

    ! * compute new origin point:
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ! * new estimate of trajectory wind:
        !   at this stage (pgmvt1(U,V)) contains wind;
        !   (puf,pvf) and (zuz9,zvz9) contain wind.
        ZUT1(JROF,JLEV)=(1.0_JPRB-RW2TLFF)*YDA_PUF%P(JROF,JLEV)&
         & +RW2TLFF*0.5_JPRB*(YDVARS%U%T1(JROF,JLEV) + ZUZ9(JROF,JLEV))
        ZVT1(JROF,JLEV)=(1.0_JPRB-RW2TLFF)*YDA_PVF%P(JROF,JLEV)&
         & +RW2TLFF*0.5_JPRB*(YDVARS%V%T1(JROF,JLEV) + ZVZ9(JROF,JLEV))

        ! * compute the new origin point ON:
        ZTXO=YDGEOMVARS%RINDX%P(JROF)-2.0_JPRB*ZUT1(JROF,JLEV)*ZGMDTX(JROF)
        ZTYO=YDGEOMVARS%RINDY%P(JROF)-2.0_JPRB*ZVT1(JROF,JLEV)*ZGMDTY(JROF)
        YDA_PCCO%P(JROF,JLEV,YDTCCO%M_RLON)=ZTXO*ZINEZV(JROF)+YDGEOMVARS%RINDX%P(JROF)*(1.0_JPRB-ZINEZV(JROF))
        YDA_PCCO%P(JROF,JLEV,YDTCCO%M_RLAT)=ZTYO*ZINEZV(JROF)+YDGEOMVARS%RINDY%P(JROF)*(1.0_JPRB-ZINEZV(JROF))
        ZTXO=MIN(MAX(ZTXO,ZDLUN),ZDLUX)
        ZTYO=MIN(MAX(ZTYO,ZDGUN),ZDGUX)
        YDA_PSCO%P(JROF,JLEV,YDTSCO%M_COSCO)=ZTXO*ZINEZV(JROF)+YDGEOMVARS%RINDX%P(JROF)*(1.0_JPRB-ZINEZV(JROF))
        YDA_PSCO%P(JROF,JLEV,YDTSCO%M_SINCO)=ZTYO*ZINEZV(JROF)+YDGEOMVARS%RINDY%P(JROF)*(1.0_JPRB-ZINEZV(JROF))
      ENDDO
    ENDDO

    CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP,ZQ,&
     &YDVARS%U%T1,YDVARS%V%T1,LDIR=.FALSE.,LDADD=.TRUE.,LDSUB=.TRUE.)

    ! * compute rotation matrix and Coriolis term "2 Omega wedge a k"
    !   at the new origin point ON.
    CALL ELARCHE(YDCST,YDMODEL%YRML_DYN,NPROMA,KST,KEND,NFLEVG,YDSL,YDEGSL,YDEGEO,YDA_PSCO%P,YDA_PCCO%P,&
               &YDGEOMVARS%GECLO%P,YDGEOMVARS%GEMU%P,YDGEOMVARS%GESLO%P,YDGEOMVARS%GSQM2%P)
    YDCPG_GMV9%U%P(KST:KEND,1:NFLEVG)=YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLON)
    YDCPG_GMV9%V%P(KST:KEND,1:NFLEVG)=YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLAT)

!$ACDC }

  ELSE

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LARCHE {

    ! * compute new origin point:
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        IF (LRHS_CURV) THEN
          ZUT1(JROF,JLEV)=RW2TLFF*0.5_JPRB*(YDVARS%U%T1(JROF,JLEV)+ZUZ9(JROF,JLEV))&
           & +(1.0_JPRB-RW2TLFF)*YDCPG_GMV9%U%P(JROF,JLEV)
          ZVT1(JROF,JLEV)=RW2TLFF*0.5_JPRB*(YDVARS%V%T1(JROF,JLEV)+ZVZ9(JROF,JLEV))&
           & +(1.0_JPRB-RW2TLFF)*YDCPG_GMV9%V%P(JROF,JLEV)
        ELSE
          ZPP9=YDA_PCCO%P(JROF,JLEV,YDTCCO%M_RQX)
          ZQQ9=YDA_PCCO%P(JROF,JLEV,YDTCCO%M_RQY)
          ZPP1=YDGEOMVARS%GNORDM%T0(JROF)
          ZQQ1=-YDGEOMVARS%GNORDL%T0(JROF)
          ! * new estimate of trajectory wind:
          !   at this stage (pgmvt1(U,V)) contains wind;
          !   (puf,pvf) and (zuz9,zvz9) contain wind.
          ZUT1(JROF,JLEV)=RW2TLFF*0.5_JPRB*(&
           & ZPP1*YDVARS%U%T1(JROF,JLEV)+ZQQ1*YDVARS%V%T1(JROF,JLEV)&
           & +ZPP9*ZUZ9(JROF,JLEV)+ZQQ9*ZVZ9(JROF,JLEV))&
           & +(1.0_JPRB-RW2TLFF)*(ZPP1*YDCPG_GMV9%U%P(JROF,JLEV)+ZQQ1*YDCPG_GMV9%V%P(JROF,JLEV))
          ZVT1(JROF,JLEV)=RW2TLFF*0.5_JPRB*(&
           & -ZQQ1*YDVARS%U%T1(JROF,JLEV)+ZPP1*YDVARS%V%T1(JROF,JLEV)&
           & -ZQQ9*ZUZ9(JROF,JLEV)+ZPP9*ZVZ9(JROF,JLEV))&
           & +(1.0_JPRB-RW2TLFF)*(-ZQQ1*YDCPG_GMV9%U%P(JROF,JLEV)+ZPP1*YDCPG_GMV9%V%P(JROF,JLEV))
        ENDIF
      ENDDO
    ENDDO

    ! * compute the new origin point ON
    CALL MCOORDS(YDTSCO,YDRIP,NFLEVG,NPROMA,KST,KEND,YDCST%RA,GEMU,GSQM2,GNORDM,GNORDL,&
      &ZUT1,ZVT1,YDA_PSCO%P,YDDYNA%LTWOTL,LD2TLM=.FALSE.)

    ! * subtract old estimate of advected Coriolis term:
    IF (LRHS_CURV) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDVARS%U%T1(JROF,JLEV)=YDVARS%U%T1(JROF,JLEV)-YDCPG_GMV9%U%P(JROF,JLEV)
          YDVARS%V%T1(JROF,JLEV)=YDVARS%V%T1(JROF,JLEV)-YDCPG_GMV9%V%P(JROF,JLEV)
        ENDDO
      ENDDO
    ELSE
      CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP,ZQ,&
       &YDVARS%U%T1,YDVARS%V%T1,LDIR=.FALSE.,LDSUB=.TRUE.)
    ENDIF

    ! * compute rotation matrix and Coriolis term "2 Omega wedge a k"
    !   at the new origin point ON.
    CALL LARCHE(YDMODEL%YRML_DYN%YYTCCO,YDMODEL%YRML_DYN%YYTSCO,NPROMA,KST,KEND,NFLEVG,&
     & NSTTYP,ZDSTRET,RC2M1,RC2P1,YDCST%RPI,ZDEPI,RLOCEN,RMUCEN,YDA_PSCO%P,1,YDA_PCCO%P,&
     & YDGEOMVARS%RCOLON%P,YDGEOMVARS%RSILON%P,YDGEOMVARS%GECLO%P,YDGEOMVARS%GEMU%P,&
     & YDGEOMVARS%GESLO%P,YDGEOMVARS%GSQM2%P)

    CALL LACONE(NPROMA,KST,KEND,NFLEVG,NSTTYP,YDCPG_OPTS%N2DINI,YDDYN%YRSWE,&
     & YDCST%ROMEGA,ZDSTRET,RC2M1,RC2P1,RMUCEN,YDCST%RA,YDCST%RPI,&
     & YDA_PCCO%P(:,:,YDTCCO%M_RLON),YDA_PCCO%P(:,:,YDTCCO%M_RLAT),&
     & YDCPG_GMV9%U%P,YDCPG_GMV9%V%P)  

!$ACDC }

  ENDIF

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ROTUVGEO_2 {

  ! update (p,q) with new coord (ie rqx/rqy)
  CALL ROTUVGEO(NFLEVG,NPROMA,KST,KEND,GNORDM,GNORDL,&
    &YDA_PCCO%P(:,:,YDTCCO%M_RQX),YDA_PCCO%P(:,:,YDTCCO%M_RQY),ZP2,ZQ2,LDIR=.TRUE.)

  ! * add new estimate of advected Coriolis term:
  CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP2,ZQ2,&
    &YDVARS%U%T1,YDVARS%V%T1,LDIR=.FALSE.,LDADD=.TRUE.)

  IF ( YDDYNA%LPC_CHEAP.AND.(NCURRENT_ITER == 0) ) THEN
    ! ZGPPC(wind) must be incremented with term [2 Omega wedge vec(r)] at O.
    CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9%U%P,YDCPG_GMV9%V%P,ZP2,ZQ2,&
      &YDVARS%PCF_U%PC_PH,YDVARS%PCF_V%PC_PH,LDIR=.FALSE.,LDADD=.TRUE.)
  ENDIF

!$ACDC }

ENDIF

!        2.3.5 CONVERT "gw" TOWARDS "d-hat" + diagnostic BBC in NH.

IF(YDDYNA%LGWADV.OR.YDDYNA%LRDBBC) THEN

!$ACDC ABORT {

  ! * allocate arrays for storing the surface wind, and compute them.
  LLDER=.FALSE.
  CALL GPUVS(NFLEVG,NPROMA,KST,KEND,LLDER,&
   & YDVARS%U%T1,YDVARS%V%T1,ZUST1,ZVST1)

  ! * allocate arrays for storing grad(orog)=M*vnabla'(orog).
  ZOROGL_SR(KST:KEND)=YDGEOMVARS%GM%P(KST:KEND)*YDGEOMVARS%OROGL%P(KST:KEND)
  ZOROGM_SR(KST:KEND)=YDGEOMVARS%GM%P(KST:KEND)*YDGEOMVARS%OROGM%P(KST:KEND)

!$ACDC }

ENDIF

IF(YDDYNA%LGWADV) THEN

!$ACDC ABORT {

  ! * Convert explicit guess of (gw) towards d3.
  !   (for d4, X-term is added in CPGLAG via the SI term).
  IF (YDDYNA%NVDVAR==4 .OR. YDDYNA%NVDVAR==5) THEN
    IVDVAR=3
  ELSE
    IVDVAR=YDDYNA%NVDVAR
  ENDIF

  IF (YDDYNA%LNHHY) THEN
    ZTAUD_NL=YDDYNA%RNHHY_TAUD_NL
  ELSE
    ZTAUD_NL=1.0_JPRB
  ENDIF

  !* LNHEE and other fully compressible Blending systems 
  LLNHEE = YDDYNA%LNHEE &
   & .OR. (YDDYNA%LNHHY .AND. .NOT.YDDYNA%LNHHY_SOLVER_HY) &
   & .OR. (YDDYNA%LNHQE .AND. .NOT.YDDYNA%LNHQE_SOLVER_QE)

  ! * Update ZVWVT1:
  !   "gw(surf) is assumed to be equal to vecV(l=L)*gradorog
  IF (YDCVER%LVERTFE.AND.YDCVER%LVFE_GW) THEN
    ZVWVT1(KST:KEND,1:NFLEVG)=YDVARS%SVD%T1(KST:KEND,1:NFLEVG)
    ZVWVT1S(KST:KEND)=ZUST1(KST:KEND)*ZOROGL_SR(KST:KEND)&
     & +ZVST1(KST:KEND)*ZOROGM_SR(KST:KEND)

    ! * RDERI is not the exact inverse of RINTE, so it is not advised at all
    !   to apply RDERI to (gw)(t+dt): the choice done here is to apply
    !   RDERI to ((gw)(t+dt)-(gw)(t)), and to use quantities saved in buffer
    !   SLBUF2 ((gw)(t), (gw_surf)(t) and (-g dw)(t)).
    CALL GNHGW2SVD(YDGEOMETRY,YDCST,YDCPG_OPTS%TOPPRES,KST,KEND,YDDYNA%LNHEE,YDDYNA%NPDVAR,IVDVAR,&
     & YDVARS%SP%T1,ZRT1,YDVARS%SPD%T1,ZSVDT1,&
     & PGWF=ZVWVT1,PGWS=ZVWVT1S,PGWRF=YDCPG_SL2%GWF,&
     & PGWRS=YDCPG_SL2%GWS,PGDWR=YDCPG_SL2%GDW)
  ELSE
  
    ZVWVT0(KST:KEND,0:NFLEVG-1)=YDVARS%SVD%T1(KST:KEND,1:NFLEVG)
    ZVWVT0(KST:KEND,NFLEVG)=ZUST1(KST:KEND)*ZOROGL_SR(KST:KEND)&
     & +ZVST1(KST:KEND)*ZOROGM_SR(KST:KEND)

    IF (YDDYN%LGWFRIC) THEN  
      CALL GNHGW2SVD(YDGEOMETRY,YDCST,YDCPG_OPTS%TOPPRES,KST,KEND,LLNHEE,YDDYNA%NPDVAR,IVDVAR,&
       & YDVARS%SP%T1,ZRT1,YDVARS%SPD%T1,ZSVDT1,&
       & PGWH=ZVWVT0,PGWDAMP=YDDYN%RGWFRIC,PTAUD_NL=ZTAUD_NL)
    ELSE
      CALL GNHGW2SVD(YDGEOMETRY,YDCST,YDCPG_OPTS%TOPPRES,KST,KEND,LLNHEE,YDDYNA%NPDVAR,IVDVAR,&
       & YDVARS%SP%T1,ZRT1,YDVARS%SPD%T1,ZSVDT1,&
       & PGWH=ZVWVT0,PTAUD_NL=ZTAUD_NL)
    ENDIF
  ENDIF

  ! * Transfer ZSVDT1 into PGMVT1(.,.,YT1%MSVD).
  YDVARS%SVD%T1(KST:KEND,1:NFLEVG)=ZSVDT1(KST:KEND,1:NFLEVG)

!$ACDC }

ENDIF

IF (YDDYNA%LRDBBC) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LRDBBC {

  ! * Diagnostic BBC in NH:
  DO JROF=KST,KEND 
    YDVARS%SVD%T1(JROF,NFLEVG) = YDVARS%SVD%T1(JROF,NFLEVG)&
     & - 0.5_JPRB*(ZDPHI9(JROF)+YDCPG_SL2%DPHI1(JROF))&
     & * (ZUST1(JROF)*ZOROGL_SR(JROF)+ZVST1(JROF)*ZOROGM_SR(JROF)&
     & - ZGWS9(JROF))-(ZDBBC9(JROF)+YDCPG_SL2%DBBC1(JROF))
  ENDDO

!$ACDC }

ENDIF

!        2.3.6 Add linear contributions interpolated at O if NGWADVSI=2
!              (some specific cases of NH + LGWADV only).

! In this case the linear contributions interpolated at O are added to
! the equations RHS after the conversion (gw) -> d4, but before the
! lagged physics and the specific treatment of term NHX done in CPGLAG.
! This is done only at the predictor step (only case where the terms added are non-zero).
! Not coded for the obsolescent option NVLAG=2.

IF (YDDYNA%LNHDYN.AND.YDDYNA%LGWADV.AND.YDDYNA%NGWADVSI==2.AND.NCURRENT_ITER==0) THEN

!$ACDC ABORT {

  ! * Continuity equation:
  IF (NVLAG==3.AND.YDDYNA%LSLINLC2) THEN
    IF (YDCVER%LVERTFE) THEN
      ! Add the linear term, FE vertically integrated, to PB2.
      DO JLEV=1,NFLEVG
        ZSPT1(KST:KEND,JLEV)=ZVWEI(KST:KEND,JLEV)*YDVETA%VFE_RDETAH(JLEV)*ZC9_SI(KST:KEND,JLEV)
      ENDDO
      ZSPT1(KST:KEND,0)=0.0_JPRB
      ZSPT1(KST:KEND,NFLEVG+1)=0.0_JPRB
      CALL VERDISINT(YDVFE,YDCVER,'ITOP','11',NPROMA,KST,KEND,NFLEVG,ZSPT1,POUTS=ZSPTO)
      YDVARS%SP%T1(KST:KEND)=YDVARS%SP%T1(KST:KEND)+ZSPTO(KST:KEND)
    ELSE
      ! Add the linear term, FD vertically integrated, to PB2.
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+ZVWEI(JROF,JLEV)*ZC9_SI(JROF,JLEV)
        ENDDO
      ENDDO
    ENDIF
  ENDIF

  ! * Momentum equation (should use the versions of ZPP,ZQQ not modified by L2TLFF):
  IF (YDDYNA%LSLINL) THEN
    CALL ROTUV(NFLEVG,NPROMA,KST,KEND,YDCPG_GMV9_SI%U%P,YDCPG_GMV9_SI%V%P,ZP,ZQ,&
      &YDVARS%U%T1,YDVARS%V%T1,LDIR=.FALSE.,LDADD=.TRUE.)
  ENDIF

  ! * Temperature and vertical divergence equation:
  IF (YDDYNA%LSLINL) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDVARS%T%T1(JROF,JLEV)=YDVARS%T%T1(JROF,JLEV)+YDCPG_GMV9_SI%T%P(JROF,JLEV)
        YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+YDCPG_GMV9_SI%SVD%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

  ! * Pressure departure equation for NHEE and NHHY or NHQE Blended models:
  IF (YDDYNA%LSLINL.AND.YDDYNA%LNHDYN) THEN
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        YDVARS%SPD%T1(JROF,JLEV)=YDVARS%SPD%T1(JROF,JLEV)+YDCPG_GMV9_SI%SPD%P(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

!$ACDC }

ENDIF

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LAPINEB',1,ZHOOK_HANDLE)

END SUBROUTINE LAPINEB


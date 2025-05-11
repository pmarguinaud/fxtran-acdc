SUBROUTINE LATTEX(YDCST,YDGEOMETRY,YDVARS,YDCPG_TND,YDCPG_SL1,YDCPG_SL2,YDCPG_BNDS,YDCPG_OPTS,&
 ! --- INPUT --------------------------------------------------
 & YDLDDH,YDMDDH,YDML_GCONF,YDML_DYN,PDTS2,PBT,PBDT,PESGP,PESGM,&
 & POROGL,POROGM,PHIF0,&
 & PGAGT0L,PGAGT0M,PTOD0,PSPDS0,PLPD0,&
 & PGAGT9L,PGAGT9M,PTOD9,PSPDS9,PLPD9,&
 & PRDELP,PEVEL,&
 & PNHXT0,PGWT0,PGWFT0,PNHXT9,PGWT9,PMIXNL,&
 ! --- INPUT/OUTPUT -------------------------------------------
 & YDCPG_TNDSI_DDH)

!$ACDC pointerparallel    


!**** *LATTEX*   Semi-Lagrangian scheme.
!                Computation of the t and t-dt useful quantities at grid-points.
!                 Equations for tri-dimensional variables.
!                Abbreviation "vwv" stands for "vertical wind variable".

!     Purpose.
!     --------
!        * This subroutine computes the equation quantities to be
!          interpolated at each grid-point of the colocation grid
!          (Gauss grid at all levels).
!          The terms considered here are the explicit terms and
!          the explicit part of the semi-implicit scheme (terms
!          previously computed in LASSIE or LANHSI).
!          Equations considered here are equations for tri-dimensional
!          variables: momentum, temperature, NH variables, GFL.
!        * Remark 1: when an alternate averaging is used for linear terms
!          in the 2TL SL scheme, the first timestep is treated differently
!          (first order uncentering), no uncentering is applied to the
!          total term ("cursive A") and the term saved in PGMV(1,1,YT9%M[X]NL)
!          is [ (Delta t/2) ("cursive A" - (1 + xidt) beta "cursive B") ]
!          instead of [ (Delta t/2) ("cursive A" - beta "cursive B") ].
!        * Remark 2: for lsettls=true, uncentering is applied to
!          the 'stable' extrapolation if vesl > 0 to avoid instability
!          in the momentum equation.
!        * Remark 3: for lgwadv=true in the NH models, variable
!          "gw" is advected instead of vertical divergence; it is advected at
!          half levels if lvfe_gw=f, full levels if lvfe_gw=t.
!          That means that PLPD0,PLPD9,PATND(.,.,YYTTND%M_TNDGW),PGWT0,PGWT9 contain:
!          - half level values 0 to nflevg-1 if LVFE_GW=F.
!          - full level values 1 to nflevg if LVFE_GW=T.
!        * Remark 4: for PC schemes:
!          - this routine is called for nsiter=0.
!          - this routine is called for the predictor of lpc_full.
!          - this routine is called for the corrector of lpc_full.

!**   Interface.
!     ----------
!        *CALL* *LATTEX(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST         - first element of work.
!          KPROF       - depth of work.
!          PDTS2       - 0.5*time step for the first time-integration step of
!                        a leap-frog scheme or all time-integration steps of
!                        a two-time level scheme; time step for the following
!                        time-integration steps of a leap-frog scheme.
!          PBT         - PDTS2*BETADT (BETADT is in YOMDYN).
!          PBDT        - PBT if semi-implicit scheme with unreduced
!                        divergence, PBT*(c**2/GM**2) if semi-implicit
!                        scheme with reduced divergence.
!          PESGP       - (1 + uncentering factor).
!          PESGM       - (1 - uncentering factor).
!          KIBL        - index into YDGSGEOM instance in YDGEOMETRY
!          POROGL      - zonal component of "grad(surf orography)"
!          POROGM      - meridian component of "grad(surf orography)"
!          PHIF0       - geop. height at full levels
!          PGAGT0L     - semi-implicit term at time t for U-wind equation.
!          PGAGT0M     - semi-implicit term at time t for V-wind equation.
!          PTOD0       - semi-implicit term at time t for temperature equation.
!          PSPDS0      - semi-implicit term at time t for press dep equation.
!          PLPD0       - semi-implicit term at time t for vert div equation.
!          PGAGT9L     - semi-implicit term at time t-dt for U-wind equation.
!          PGAGT9M     - semi-implicit term at time t-dt for V-wind equation.
!          PTOD9       - semi-implicit term at time t-dt for temperature eqn.
!          PSPDS9      - semi-implicit term at time t-dt for press dep equation.
!          PLPD9       - semi-implicit term at time t-dt for vert div equation.
!          PRDELP      - "1/(pressure depth of layers)" at t.
!          PEVEL       - "etadot (d prehyd/d eta)" at half levels at t.
!          PATND       - adiabatic Lagrangian tendencies.
!          PNHXT0      - "X" at full levels at t, diagnosed in CPG_GP.
!          PGWT0       - "gw" at t (LGWADV=T only; layers: see in CPG_GP).
!          PGWFT0      - "gw" at full levels at t (LGWADV=T only; layers: see in CPG_GP).
!          PNHXT9      - "X" at full levels at t-dt, diagnosed in CPG_GP.
!          PGWT9       - "gw" at t-dt (LGWADV=T only; layers: see in CPG_GP).
!          PGFL        - unified_treatment grid-point fields
!          PMIXNL      - extrapolation control variable for mixed NESC/SETTLS scheme

!        INPUT/OUTPUT:
!          PGMV        - GMV variables at t-dt and t.
!          PGMVT1      - GMV variables at t+dt.
!          PGMVTNDSI   - GMV: tendency due to linear terms (for DDH).
!          PB1         - "SLBUF1" buffer for interpolations.

!        OUTPUT:
!          PB2         - "SLBUF2" buffer.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!           none
!           Called by LACDYN.

!     Reference.
!     ----------
!             Arpege documentation about semi-Lagrangian scheme.

!     Author.
!     -------
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after old part 3.2 of LACDYN. 
!      Original : AUGUST 1995.

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!   P. Bechtold+A. Untch 26-10-2008: add LEGWWMS switch for non-orogr. GWD
!   F. Vana  15-Oct-2009: option NSPLTHOI
!   K. Yessad (Nov 2009): cleanings, DT/Dt now pre-computed in CPG_GP.
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   F. Vana  22-Feb-2011: diff of phys. tendencies and LTDIABLIN attribute
!   K. Yessad (Dec 2011): various contributions.
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   K. Yessad (Oct 2013): allow NESC without ICI-PC scheme.
!   K. Yessad (July 2014): Rename some variables, move some variables.
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (June 2017): Introduce NHQE model.
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   J. Vivoda (July 2018): mixed NESC/SETTLS scheme.
!   R. El Khatib 27-02-2019 Use pointer function SC2PRG to avoid bounds violation
!   F. Vana  11-Jul-2019: Option LRHS_CURV
!   I. Polichtchouk Jul-2021: Introduce LSACC model
! End Modifications
!------------------------------------------------------------------------------

USE MODEL_DYNAMICS_MOD    , ONLY : MODEL_DYNAMICS_TYPE
USE MODEL_GENERAL_CONF_MOD, ONLY : MODEL_GENERAL_CONF_TYPE
USE GEOMETRY_MOD          , ONLY : GEOMETRY
USE PARKIND1              , ONLY : JPIM, JPRB
USE YOMHOOK               , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST                , ONLY : TCST
USE YOMMDDH               , ONLY : TMDDH
USE YOMLDDH               , ONLY : TLDDH
USE FIELD_VARIABLES_MOD   , ONLY : FIELD_VARIABLES
USE CPG_OPTS_TYPE_MOD     , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE CPG_TYPE_MOD          , ONLY : CPG_TND_TYPE
USE CPG_SL1_TYPE_MOD      , ONLY : CPG_SL1B_TYPE
USE CPG_SL2_TYPE_MOD      , ONLY : CPG_SL2_TYPE
USE CPG_DDH_TND_TYPE_MOD  , ONLY : CPG_TNDSI_DDH_TYPE

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)                    ,INTENT(IN)     :: YDCST
TYPE(GEOMETRY)                ,INTENT(IN)     :: YDGEOMETRY
TYPE(FIELD_VARIABLES)         ,INTENT(INOUT)  :: YDVARS
TYPE(CPG_TND_TYPE)            ,INTENT(INOUT)  :: YDCPG_TND
TYPE(CPG_SL1B_TYPE)           ,INTENT(INOUT)  :: YDCPG_SL1
TYPE(CPG_SL2_TYPE)            ,INTENT(INOUT)  :: YDCPG_SL2
TYPE(CPG_BNDS_TYPE)           ,INTENT(IN)     :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE)           ,INTENT(IN)     :: YDCPG_OPTS
TYPE(TLDDH)                   ,INTENT(IN)     :: YDLDDH
TYPE(TMDDH)                   ,INTENT(IN)     :: YDMDDH
TYPE(MODEL_GENERAL_CONF_TYPE) ,INTENT(IN)     :: YDML_GCONF
TYPE(MODEL_DYNAMICS_TYPE)     ,INTENT(IN)     :: YDML_DYN
REAL(KIND=JPRB)               ,INTENT(IN)     :: PDTS2 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PBT 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PBDT(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PESGP 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PESGM 
REAL(KIND=JPRB)               ,INTENT(IN)     :: POROGL(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: POROGM(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PHIF0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               ,INTENT(IN)     :: PGAGT0L(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PGAGT0M(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PTOD0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PSPDS0(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PLPD0(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PGAGT9L(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PGAGT9M(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PTOD9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PSPDS9(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PLPD9(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PRDELP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PEVEL(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PNHXT0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PGWT0(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PGWFT0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               ,INTENT(IN)     :: PNHXT9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PGWT9(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)               ,INTENT(IN)     :: PMIXNL(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
TYPE(CPG_TNDSI_DDH_TYPE)      ,INTENT(INOUT)  :: YDCPG_TNDSI_DDH
!     ------------------------------------------------------------------
REAL(KIND=JPRB)               :: ZWT0(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)               :: ZUSI9(YDGEOMETRY%YRDIM%NPROMA,MERGE (0,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YRDYNA%LTWOTL))
REAL(KIND=JPRB)               :: ZVSI9(YDGEOMETRY%YRDIM%NPROMA,MERGE (0,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YRDYNA%LTWOTL))
REAL(KIND=JPRB)               :: ZTSI9(YDGEOMETRY%YRDIM%NPROMA,MERGE (0,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YRDYNA%LTWOTL))
REAL(KIND=JPRB)               :: ZSPDSI9(YDGEOMETRY%YRDIM%NPROMNH,MERGE (0,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YRDYNA%LTWOTL))
REAL(KIND=JPRB)               :: ZVWVSI9(YDGEOMETRY%YRDIM%NPROMNH,MERGE (0,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YRDYNA%LTWOTL))
REAL(KIND=JPRB)               :: ZMOY1U(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMOY1V(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMOY1T(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMOY1SPD(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMOY1VWV(YDGEOMETRY%YRDIM%NPROMNH,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZUSELESS(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
REAL(KIND=JPRB)               :: ZUCOMP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

INTEGER(KIND=JPIM) :: IPX
INTEGER(KIND=JPIM) :: JLEV, JGFL, JROF

LOGICAL :: LLSETTLSW, LLCT, LLCTC
LOGICAL :: LLTDIABLIN

!     -------------------------------------------------------

REAL(KIND=JPRB) :: ZCMSLP, ZRTWORGRA, ZRGOM

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "lattex_dnt.intfb.h"
#include "lattex_tnt.intfb.h"
#include "lattex_expl_2tl.intfb.h"
#include "lattex_expl_3tl.intfb.h"
#include "lattex_expl_vspltrans.intfb.h"
#include "lattex_rintopt.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LATTEX',0,ZHOOK_HANDLE)

ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,&
& YDDYN=>YDML_DYN%YRDYN, YDRIP=>YDML_GCONF%YRRIP, YDDYNA=>YDML_DYN%YRDYNA)

ASSOCIATE(NPROMA=>YDDIM%NPROMA, NPROMNH=>YDDIM%NPROMNH, NFLEVG=>YDDIMV%NFLEVG, LADVF=>YDDYN%LADVF,                &
& NCURRENT_ITER=>YDDYN%NCURRENT_ITER, NSPDLAG=>YDDYN%NSPDLAG, NSVDLAG=>YDDYN%NSVDLAG, NTLAG=>YDDYN%NTLAG,         &
& NWLAG=>YDDYN%NWLAG, RCMSLP0=>YDDYN%RCMSLP0, RCORDIF=>YDDYN%RCORDIF, RCORDIH=>YDDYN%RCORDIH, NFOST=>YDRIP%NFOST, &
& LRSIDDH=>YDLDDH%LRSIDDH, RINTOPT=>YDDYN%RINTOPT, LRHS_CURV=>YDDYN%LRHS_CURV, NSPLTHOI=>YDDYN%NSPLTHOI)

ASSOCIATE(KLON => YDCPG_OPTS%KLON, KST => YDCPG_BNDS%KIDIA, KEND => YDCPG_BNDS%KFDIA, KFLEVG => YDCPG_OPTS%KFLEVG)

!     ------------------------------------------------------------------

!*      1.  PRELIMINARY INITIALISATIONS.
!       --------------------------------

!       1.2  Scalar initialisations:

LLCT =YDDYNA%LPC_FULL .AND. NCURRENT_ITER > 0  ! corrector step
LLCTC=YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER > 0


ZCMSLP=RCMSLP0/(YDCST%RD*YDCPG_OPTS%RTSUR)

!       1.3  reset to zero ddh arrays and pointers

IF (LRSIDDH) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LRSIDDH {


  YDCPG_TNDSI_DDH%U%T0(KST:KEND,1:NFLEVG)=0.0_JPRB
  YDCPG_TNDSI_DDH%V%T0(KST:KEND,1:NFLEVG)=0.0_JPRB
  YDCPG_TNDSI_DDH%T%T0(KST:KEND,1:NFLEVG)=0.0_JPRB
  DO JLEV=1,NFLEVG
    YDCPG_SL1%U9_SI%P(KST:KEND,JLEV)=0.0_JPRB
    YDCPG_SL1%V9_SI%P(KST:KEND,JLEV)=0.0_JPRB
    YDCPG_SL1%T9_SI%P(KST:KEND,JLEV)=0.0_JPRB
  ENDDO
  IF (YDDYNA%LNHDYN) THEN !! ?????? LNHDYN or LNHEE?
    YDCPG_TNDSI_DDH%PD%T0(KST:KEND,1:NFLEVG)=0.0_JPRB
    YDCPG_TNDSI_DDH%VD%T0(KST:KEND,1:NFLEVG)=0.0_JPRB
  ENDIF
  !IF (YDDYNA%LNHEE) THEN
  !  DO JLEV=1,NFLEVG
  !    YDCPG_SL1%PD9_SI%P(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA,JLEV)=0.0_JPRB
  !  ENDDO
  !ENDIF
  IF (YDDYNA%LNHDYN) THEN
    DO JLEV=1,NFLEVG
      YDCPG_SL1%PD9_SI%P(KST:KEND,JLEV)=0.0_JPRB
      YDCPG_SL1%VD9_SI%P(KST:KEND,JLEV)=0.0_JPRB
    ENDDO
  ENDIF

!$ACDC }

ENDIF

!     ------------------------------------------------------------------

!*      2.  TREATMENT OF GMV VARIABLES.
!       -------------------------------

!*       2.1   Momentum equation.

! * LSETTLS is replaced by LLSETTLSW=.FALSE. for wind-eqn if VESL>0 because
!   stable extrapolation deteriorates scores without improving stability.

IF (PESGP > PESGM) THEN
  LLSETTLSW=.FALSE.
ELSE
  LLSETTLSW=YDDYNA%LSETTLS
ENDIF

ZRTWORGRA=2._JPRB/(YDCST%RG*YDCST%RA)
ZRGOM=2._JPRB*YDCST%ROMEGA/YDCST%RG

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=MAIN {


DO JLEV=1,NFLEVG

  DO JROF=KST,KEND
    YDCPG_SL2%USI(JROF,JLEV)=PBT*PGAGT0L(JROF,JLEV)
    YDCPG_SL2%VSI(JROF,JLEV)=PBT*PGAGT0M(JROF,JLEV)
  ENDDO

  ! * Add pressure gradient term + Rayleigh friction in the wind equation.
  DO JROF=KST,KEND
    ZMOY1U(JROF,JLEV)=PDTS2*YDCPG_TND%TNDU_NOC(JROF,JLEV)
    ZMOY1V(JROF,JLEV)=PDTS2*YDCPG_TND%TNDV_NOC(JROF,JLEV)
  ENDDO

  ! * Add "- 2 Omega vec V" explicit contribution when required.
  ! suitabley modified for the shallow-atmosphere complete-coriolis formulation
  IF (.NOT.LADVF) THEN
    IF (YDDYNA%LSACC) THEN
      DO JROF=KST,KEND
        ZMOY1U(JROF,JLEV)=ZMOY1U(JROF,JLEV)&
         & +PDTS2*YDVARS%GEOMETRY%RCORI%T0(JROF)*YDVARS%V%T0(JROF,JLEV)&
         & +PDTS2*ZRTWORGRA*YDVARS%GEOMETRY%RCORI%T0(JROF)*YDVARS%V%T0(JROF,JLEV)*PHIF0(JROF,JLEV)&
         & -PDTS2*ZRGOM*PGWFT0(JROF,JLEV)*(1._JPRB-YDVARS%GEOMETRY%GEMU%T0(JROF)*YDVARS%GEOMETRY%GEMU%T0(JROF)) 
        ZMOY1V(JROF,JLEV)=ZMOY1V(JROF,JLEV)&
         & -PDTS2*YDVARS%GEOMETRY%RCORI%T0(JROF)*YDVARS%U%T0(JROF,JLEV)&
         & -PDTS2*ZRTWORGRA*YDVARS%GEOMETRY%RCORI%T0(JROF)*YDVARS%U%T0(JROF,JLEV)*PHIF0(JROF,JLEV)
      ENDDO
    ELSE
      DO JROF=KST,KEND
        ZMOY1U(JROF,JLEV)=ZMOY1U(JROF,JLEV)+PDTS2*YDVARS%GEOMETRY%RCORI%P(JROF)*YDVARS%V%T0(JROF,JLEV)
        ZMOY1V(JROF,JLEV)=ZMOY1V(JROF,JLEV)-PDTS2*YDVARS%GEOMETRY%RCORI%P(JROF)*YDVARS%U%T0(JROF,JLEV)
      ENDDO
    ENDIF
  ENDIF
ENDDO

IF (YDDYNA%LTWOTL) THEN  

  CALL LATTEX_DNT(YDCPG_OPTS%NSTEP, YDGEOMETRY,YDLDDH,YDRIP,YDDYN,YDDYNA,KST,KEND,LLSETTLSW,NWLAG,PESGP,PESGM,    &
   & YDVARS%U%T0,YDVARS%U%T9,ZMOY1U,PMIXNL,YDCPG_SL2%USI,YDVARS%UNL%T9,YDVARS%U%T1,YDCPG_SL1%U0%P,YDCPG_SL1%U9%P, &
   & YDCPG_SL1%UF9%P,YDVARS%CUNL%T9,YDCPG_TNDSI_DDH%U%T0,YDCPG_TNDSI_DDH%U%T9,YDCPG_SL1%U9_SI%P,YDCPG_SL1%U9_NL%P)
  
  CALL LATTEX_DNT(YDCPG_OPTS%NSTEP, YDGEOMETRY,YDLDDH,YDRIP,YDDYN,YDDYNA,KST,KEND,LLSETTLSW,NWLAG,PESGP,PESGM,   &
   & YDVARS%V%T0,YDVARS%V%T9,ZMOY1V,PMIXNL,YDCPG_SL2%VSI,YDVARS%VNL%T9,YDVARS%V%T1,YDCPG_SL1%V0%P,YDCPG_SL1%V9%P,&
   & YDCPG_SL1%VF9%P,YDVARS%CVNL%T9,YDCPG_TNDSI_DDH%V%T0,YDCPG_TNDSI_DDH%V%T9,YDCPG_SL1%V9_SI%P,YDCPG_SL1%V9_NL%P)
  
ELSE
  
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      ZUSI9(JROF,JLEV)=PBT*PGAGT9L(JROF,JLEV)
      ZVSI9(JROF,JLEV)=PBT*PGAGT9M(JROF,JLEV)
    ENDDO
  ENDDO
  
  CALL LATTEX_TNT(YDGEOMETRY,YDLDDH,YDDYN,KST,KEND,NWLAG,PESGP,PESGM,YDVARS%U%T9,ZMOY1U,ZUSI9,YDCPG_SL2%USI,&
   & YDVARS%U%T1,YDCPG_SL1%U0%P,YDCPG_SL1%U9%P,YDCPG_SL1%UF9%P,YDCPG_TNDSI_DDH%U%T0,YDCPG_SL1%U9_SI%P)

  CALL LATTEX_TNT(YDGEOMETRY,YDLDDH,YDDYN,KST,KEND,NWLAG,PESGP,PESGM,YDVARS%V%T9,ZMOY1V,ZVSI9,YDCPG_SL2%VSI,&
   & YDVARS%V%T1,YDCPG_SL1%V0%P,YDCPG_SL1%V9%P,YDCPG_SL1%VF9%P,YDCPG_TNDSI_DDH%V%T0,YDCPG_SL1%V9_SI%P)
  
ENDIF

IF(LADVF) THEN
  DO JLEV=1,NFLEVG
    !dir$ ivdep
    DO JROF=KST,KEND
      YDVARS%U%T1(JROF,JLEV)=YDVARS%U%T1(JROF,JLEV)-YDVARS%GEOMETRY%GOMVRL%P(JROF)
      YDVARS%V%T1(JROF,JLEV)=YDVARS%V%T1(JROF,JLEV)-YDVARS%GEOMETRY%GOMVRM%P(JROF)
    ENDDO
  ENDDO
ENDIF

!*       2.2   Temperature equation.

DO JLEV=1,NFLEVG

  DO JROF=KST,KEND
    YDCPG_SL2%TSI(JROF,JLEV)=PBDT(JROF)*PTOD0(JROF,JLEV)
  ENDDO

  ! * compute ZMOY1T
  IF(YDGEOMETRY%YRVERT_GEOM%YRCVER%LVERTFE) THEN
    DO JROF=KST,KEND
      ZWT0(JROF)=PEVEL(JROF,JLEV)*PRDELP(JROF,JLEV)
    ENDDO
  ELSE
    DO JROF=KST,KEND
      ZWT0(JROF)=0.5_JPRB*(PEVEL(JROF,JLEV)+PEVEL(JROF,JLEV-1))*PRDELP(JROF,JLEV)
    ENDDO
  ENDIF
  DO JROF=KST,KEND
    ZMOY1T(JROF,JLEV)=&
     & PDTS2*ZCMSLP*RCORDIF(JLEV)*YDVARS%U%T0(JROF,JLEV)*POROGL(JROF)&
     & +PDTS2*ZCMSLP*RCORDIF(JLEV)*YDVARS%V%T0(JROF,JLEV)*POROGM(JROF)
  ENDDO
  DO JROF=KST,KEND
    ZMOY1T(JROF,JLEV)=ZMOY1T(JROF,JLEV) +PDTS2*YDCPG_TND%TNDT(JROF,JLEV)&
     & +PDTS2*ZCMSLP*(RCORDIH(JLEV)-RCORDIH(JLEV-1))*ZWT0(JROF)*YDVARS%GEOMETRY%OROG%P(JROF)
  ENDDO

ENDDO

IF (YDDYNA%LTWOTL) THEN
  
  CALL LATTEX_DNT(YDCPG_OPTS%NSTEP,YDGEOMETRY,YDLDDH,YDRIP,YDDYN,YDDYNA,KST,KEND,YDDYNA%LSETTLS,NTLAG,PESGP,PESGM,&
   & YDVARS%T%T0,YDVARS%T%T9,ZMOY1T,PMIXNL,YDCPG_SL2%TSI,YDVARS%TNL%T9,YDVARS%T%T1,YDCPG_SL1%T0%P,YDCPG_SL1%T9%P, &
   & YDCPG_SL1%TF9%P,YDVARS%CTNL%T9,YDCPG_TNDSI_DDH%T%T0,YDCPG_TNDSI_DDH%T%T9,YDCPG_SL1%T9_SI%P,YDCPG_SL1%T9_NL%P)
  
ELSE
  
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      ZTSI9(JROF,JLEV)=PBDT(JROF)*PTOD9(JROF,JLEV)
    ENDDO
  ENDDO
  
  CALL LATTEX_TNT(YDGEOMETRY,YDLDDH,YDDYN,KST,KEND,NTLAG,PESGP,PESGM,YDVARS%T%T9,ZMOY1T,ZTSI9,YDCPG_SL2%TSI,&
   & YDVARS%T%T1,YDCPG_SL1%T0%P,YDCPG_SL1%T9%P,YDCPG_SL1%TF9%P,YDCPG_TNDSI_DDH%T%T0,YDCPG_SL1%T9_SI%P)
  
ENDIF

DO JLEV=1,NFLEVG
  !dir$ ivdep
  DO JROF=KST,KEND
    YDVARS%T%T1(JROF,JLEV)=YDVARS%T%T1(JROF,JLEV)-RCORDIF(JLEV)*ZCMSLP*YDVARS%GEOMETRY%OROG%P(JROF)
  ENDDO
  IF (.NOT.LLCTC) THEN
    !dir$ ivdep
    DO JROF=KST,KEND
      YDCPG_SL1%T9%P(JROF,JLEV)=YDCPG_SL1%T9%P(JROF,JLEV)+RCORDIF(JLEV)*ZCMSLP*YDVARS%GEOMETRY%OROG%P(JROF)
    ENDDO
  ENDIF
ENDDO

!*       2.3   Anhydrostatic variables equations: "pressure departure".
!* !!! LNHEE
IF (YDDYNA%LNHDYN) THEN

  DO JLEV=1,NFLEVG

    DO JROF=KST,KEND
      YDCPG_SL2%PDSI(JROF,JLEV)=PBDT(JROF)*PSPDS0(JROF,JLEV)
      ZMOY1SPD(JROF,JLEV)=PDTS2*YDCPG_TND%TNDPD(JROF,JLEV)
    ENDDO

  ENDDO

  IF (YDDYNA%LTWOTL) THEN
  
    CALL LATTEX_DNT(YDCPG_OPTS%NSTEP,YDGEOMETRY,YDLDDH,YDRIP,YDDYN,YDDYNA,     &
     & KST,KEND,YDDYNA%LSETTLS,NSPDLAG,PESGP,PESGM,YDVARS%SPD%T0,YDVARS%SPD%T9,&
     & ZMOY1SPD,PMIXNL,YDCPG_SL2%PDSI,YDVARS%SPDNL%T9,YDVARS%SPD%T1,           &
     & YDCPG_SL1%PD0%P,YDCPG_SL1%PD9%P,ZUSELESS,YDVARS%CSPDNL%T9,              &
     & YDCPG_TNDSI_DDH%PD%T0,YDCPG_TNDSI_DDH%PD%T9,YDCPG_SL1%PD9_SI%P,         &
     & YDCPG_SL1%PD9_NL%P)

  ELSE
  
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ZSPDSI9(JROF,JLEV)=PBDT(JROF)*PSPDS9(JROF,JLEV)
      ENDDO
    ENDDO
  
    CALL LATTEX_TNT(YDGEOMETRY,YDLDDH,YDDYN,KST,KEND,      &
     & NSPDLAG,PESGP,PESGM,YDVARS%SPD%T9, ZMOY1SPD,ZSPDSI9,&
     & YDCPG_SL2%PDSI,YDVARS%SPD%T1,YDCPG_SL1%PD0%P,       &
     & YDCPG_SL1%PD9%P, ZUSELESS,YDCPG_TNDSI_DDH%PD%T0,    &
     & YDCPG_SL1%PD9_SI%P)

  ENDIF

ENDIF

!*       2.4a  Anhydrostatic variables equations: "vertical divergence".
IF (YDDYNA%LNHDYN.AND.(.NOT.YDDYNA%LGWADV)) THEN

  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      YDCPG_SL2%VDSI(JROF,JLEV)=PBT*PLPD0(JROF,JLEV)
      ZMOY1VWV(JROF,JLEV)=PDTS2*YDCPG_TND%TNDVD(JROF,JLEV)
    ENDDO
  ENDDO

  IF (YDDYNA%LTWOTL) THEN

    CALL LATTEX_DNT(YDCPG_OPTS%NSTEP, YDGEOMETRY,YDLDDH,YDRIP,YDDYN,YDDYNA,&
     & KST,KEND,YDDYNA%LSETTLS,NSVDLAG,PESGP,PESGM,YDVARS%SVD%T0,          &
     & YDVARS%SVD%T9,ZMOY1VWV,PMIXNL,YDCPG_SL2%VDSI,YDVARS%VWVNL%T9,       &
     & YDVARS%SVD%T1,YDCPG_SL1%VD0%P,YDCPG_SL1%VD9%P,YDCPG_SL1%VDF9%P,     &
     & YDVARS%CVWVNL%T9,YDCPG_TNDSI_DDH%VD%T0,YDCPG_TNDSI_DDH%VD%T9,       &
     & YDCPG_SL1%VD9_SI%P,YDCPG_SL1%VD9_NL%P)

    IF ((YDDYNA%NVDVAR==4 .OR. YDDYNA%NVDVAR==5) .AND. YDDYNA%ND4SYS==1) THEN

      IF (.NOT.LLCT) THEN
        ! * predictor step for LPC_FULL or normal SI step.
        IF(YDDYNA%LNESC.OR.(YDCPG_OPTS%NSTEP <= NFOST)) THEN
          ! * LNESC=T predictor or NSTEP<=0.
          DO JLEV=1,NFLEVG
            !dir$ ivdep
            DO JROF=KST,KEND
              YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+PNHXT0(JROF,JLEV)
              YDVARS%NHX%T1(JROF,JLEV)=PNHXT0(JROF,JLEV)
              YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)-PNHXT0(JROF,JLEV)
              YDCPG_SL1%NHX9%P(JROF,JLEV)=0.0_JPRB
            ENDDO
          ENDDO
        ELSEIF (.NOT.YDDYNA%LNESC.AND.(YDCPG_OPTS%NSTEP > NFOST).AND.YDDYNA%LSETTLS) THEN
          ! * LSETTLS=T predictor if NSTEP>0.
          DO JLEV=1,NFLEVG
            !dir$ ivdep
            DO JROF=KST,KEND
              YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+PNHXT0(JROF,JLEV)
              YDVARS%NHX%T1(JROF,JLEV)=PNHXT0(JROF,JLEV)
              YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)-YDVARS%NHX%T9(JROF,JLEV)
              YDCPG_SL1%NHX9%P(JROF,JLEV)=PNHXT0(JROF,JLEV)-YDVARS%NHX%T9(JROF,JLEV)
            ENDDO
          ENDDO
        ELSE
          ! * predictor LNESC=F, NSTEP>0, LSETTLS=F.
          DO JLEV=1,NFLEVG
            !dir$ ivdep
            DO JROF=KST,KEND
              YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+1.5_JPRB*PNHXT0(JROF,JLEV)-0.5_JPRB*YDVARS%NHX%T9(JROF,JLEV)
              YDVARS%NHX%T1(JROF,JLEV)=1.5_JPRB*PNHXT0(JROF,JLEV)-0.5_JPRB*YDVARS%NHX%T9(JROF,JLEV)
              YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)-0.5_JPRB*PNHXT0(JROF,JLEV)-0.5_JPRB*YDVARS%NHX%T9(JROF,JLEV)
              YDCPG_SL1%NHX9%P(JROF,JLEV)=0.5_JPRB*(PNHXT0(JROF,JLEV)-YDVARS%NHX%T9(JROF,JLEV))
            ENDDO
          ENDDO
        ENDIF

        !dir$ ivdep
        YDVARS%NHX%T9(KST:KEND,1:NFLEVG)=PNHXT0(KST:KEND,1:NFLEVG)
      ELSE
        ! * corrector step for LPC_FULL.
        DO JLEV=1,NFLEVG
          !dir$ ivdep
          DO JROF=KST,KEND
            YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+PNHXT0(JROF,JLEV)
            YDVARS%NHX%T1(JROF,JLEV)=PNHXT0(JROF,JLEV)
          ENDDO
          IF (.NOT.LLCTC) THEN
            DO JROF=KST,KEND
              YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)-YDVARS%NHX%T9(JROF,JLEV)
              YDCPG_SL1%NHX9%P(JROF,JLEV)=0.0_JPRB
            ENDDO
          ENDIF
        ENDDO
      ENDIF

    ELSEIF ((YDDYNA%NVDVAR==4 .OR. YDDYNA%NVDVAR==5) .AND. YDDYNA%ND4SYS==2) THEN

      IF (.NOT.LLCT) THEN
        DO JLEV=1,NFLEVG
          !dir$ ivdep
          DO JROF=KST,KEND
            YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+PNHXT0(JROF,JLEV)
            YDVARS%NHX%T1(JROF,JLEV)=PNHXT0(JROF,JLEV)
            YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)-PNHXT0(JROF,JLEV)
            YDCPG_SL1%NHX9%P(JROF,JLEV)=0.0_JPRB
          ENDDO
        ENDDO
        YDVARS%NHX%T9(KST:KEND,1:NFLEVG)=PNHXT0(KST:KEND,1:NFLEVG)
      ELSE
        ! * corrector step for LPC_FULL (in this case PNHXT9=PGMV(.,.,YT9%MNHX))
        DO JLEV=1,NFLEVG
          !dir$ ivdep
          DO JROF=KST,KEND
            YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV) +YDVARS%NHX%T9(JROF,JLEV)
            YDVARS%NHX%T1(JROF,JLEV)=PNHXT0(JROF,JLEV)
          ENDDO
          IF (.NOT.LLCTC) THEN
            DO JROF=KST,KEND
              YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)-YDVARS%NHX%T9(JROF,JLEV)
              YDCPG_SL1%NHX9%P(JROF,JLEV)=0.0_JPRB
            ENDDO
          ENDIF
        ENDDO
      ENDIF

    ENDIF

  ELSE
   
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ZVWVSI9(JROF,JLEV)=PBT*PLPD9(JROF,JLEV)
      ENDDO
    ENDDO
  
    CALL LATTEX_TNT(YDGEOMETRY,YDLDDH,YDDYN,KST,KEND,NSVDLAG,PESGP,PESGM,&
     & YDVARS%SVD%T9,ZMOY1VWV,ZVWVSI9,YDCPG_SL2%VDSI,YDVARS%SVD%T1,      &
     & YDCPG_SL1%VD0%P,YDCPG_SL1%VD9%P,YDCPG_SL1%VDF9%P,                 &
     & YDCPG_TNDSI_DDH%VD%T0,YDCPG_SL1%VD9_SI%P)

    ! LPC_FULL not yet coded, only the predictor is provided.
    IF ((YDDYNA%NVDVAR==4 .OR. YDDYNA%NVDVAR==5) .AND. YDDYNA%ND4SYS==1) THEN
      DO JLEV=1,NFLEVG
        !dir$ ivdep
        DO JROF=KST,KEND
          YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+PNHXT0(JROF,JLEV)
          YDVARS%NHX%T1(JROF,JLEV)=PNHXT0(JROF,JLEV)
          YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)+PNHXT0(JROF,JLEV)-2.0_JPRB*PNHXT9(JROF,JLEV)
          YDCPG_SL1%NHX9%P(JROF,JLEV)=0.0_JPRB 
        ENDDO
      ENDDO
    ELSEIF ((YDDYNA%NVDVAR==4 .OR. YDDYNA%NVDVAR==5) .AND. YDDYNA%ND4SYS==2) THEN
      DO JLEV=1,NFLEVG
        !dir$ ivdep
        DO JROF=KST,KEND
          YDVARS%SVD%T1(JROF,JLEV)=YDVARS%SVD%T1(JROF,JLEV)+PNHXT9(JROF,JLEV)
          YDVARS%NHX%T1(JROF,JLEV)=PNHXT0(JROF,JLEV)
          YDCPG_SL1%VD9%P(JROF,JLEV)=YDCPG_SL1%VD9%P(JROF,JLEV)-PNHXT9(JROF,JLEV)
          YDCPG_SL1%NHX9%P(JROF,JLEV)=0.0_JPRB 
        ENDDO
      ENDDO
    ENDIF

  ENDIF

ENDIF

!*       2.4b  Anhydrostatic variables equations: "gw".

IF (YDDYNA%LNHDYN.AND.YDDYNA%LGWADV) THEN

  ! To avoid the addition of an extra global 3D array, PB2(.,MSLB2VDSI)
  ! initially holds the SI term for the gw equation. At the end of grid-point
  ! calculations it will be converted to the SI term for the "dver" equation.
  ! The SI stage then follows exactly as usual for the "dver" equation.

  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      YDCPG_SL2%VDSI(JROF,JLEV)=PBT*PLPD0(JROF,JLEV)
      ZMOY1VWV(JROF,JLEV)=PDTS2*YDCPG_TND%TNDGW(JROF,JLEV)
    ENDDO
  ENDDO

  IF (YDDYNA%LTWOTL) THEN

    ! store "gw(t)" into buffer for corrector step
    IF( YDDYNA%LPC_FULL.AND.NCURRENT_ITER==0)THEN
      YDVARS%GW%T9(KST:KEND,1:NFLEVG) = PGWT0(KST:KEND,1:NFLEVG)
    ENDIF

    CALL LATTEX_DNT(YDCPG_OPTS%NSTEP,YDGEOMETRY,YDLDDH,YDRIP,YDDYN,YDDYNA,&
     & KST,KEND,YDDYNA%LSETTLS,NSVDLAG,PESGP,PESGM,PGWT0,YDVARS%GW%T9,    &
     & ZMOY1VWV,PMIXNL,YDCPG_SL2%VDSI,YDVARS%VWVNL%T9,YDVARS%SVD%T1,      &
     & YDCPG_SL1%VD0%P,YDCPG_SL1%VD9%P,YDCPG_SL1%VDF9%P,YDVARS%CVWVNL%T9, &
     & YDCPG_TNDSI_DDH%VD%T0,YDCPG_TNDSI_DDH%VD%T9,YDCPG_SL1%VD9_SI%P,    &
     & YDCPG_SL1%VD9_NL%P)
   
    IF ((YDDYNA%NVDVAR==4 .OR. YDDYNA%NVDVAR==5) .AND. YDDYNA%ND4SYS==2) THEN
      IF (.NOT.LLCT) THEN
        DO JLEV=1,NFLEVG
          YDCPG_SL1%NHX9%P(KST:KEND,JLEV)=PNHXT0(KST:KEND,JLEV)
        ENDDO

        ! ky: this saving does not seem to be necessary for ND4SYS=1,
        !     but this must be checked.
        YDVARS%NHX%T9(KST:KEND,1:NFLEVG)=PNHXT0(KST:KEND,1:NFLEVG)
      ELSE
        DO JLEV=1,NFLEVG
          YDCPG_SL1%NHX9%P(KST:KEND,JLEV)=YDVARS%NHX%T9(KST:KEND,JLEV)
        ENDDO
      ENDIF
    ENDIF

  ELSE

    ! This piece of code has not been validated
    !  LGWADV with SL3TL => ABOR1 in the setup, missing interpolations.

    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ZVWVSI9(JROF,JLEV)=PBT*PLPD9(JROF,JLEV)
      ENDDO
    ENDDO

    CALL LATTEX_TNT(YDGEOMETRY,YDLDDH,YDDYN,KST,KEND,NSVDLAG,PESGP,PESGM,PGWT9,ZMOY1VWV,&
     & ZVWVSI9,YDCPG_SL2%VDSI,YDVARS%SVD%T1,YDCPG_SL1%VD0%P,YDCPG_SL1%VD9%P,            &
     & YDCPG_SL1%VDF9%P,YDCPG_TNDSI_DDH%VD%T0,YDCPG_SL1%VD9_SI%P)

    ! LPC_FULL not yet coded, only the predictor is provided.
    IF ((YDDYNA%NVDVAR==4 .OR. YDDYNA%NVDVAR==5) .AND. YDDYNA%ND4SYS==2) THEN
      DO JLEV=1,NFLEVG
        YDCPG_SL1%NHX9%P(KST:KEND,JLEV)=PNHXT9(KST:KEND,JLEV)
      ENDDO
    ENDIF

  ENDIF

ENDIF

!$ACDC }

!     ------------------------------------------------------------------

!*      3.  TREATMENT OF GFL VARIABLES.
!       --------------------------------

IF (YDDYNA%LTWOTL) THEN
  CALL LATTEX_EXPL_2TL (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDML_DYN, YDCPG_SL1, YDVARS, LLCT, LLCTC)
ELSE

!$ACDC ABORT {

  CALL LATTEX_EXPL_3TL (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDML_DYN, YDCPG_SL1, YDVARS)

!$ACDC }

ENDIF

! Optimize dynamic rande for interpolation
IF (RINTOPT /= 1._JPRB) THEN

!$ACDC ABORT {

  CALL LATTEX_RINTOPT (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDCPG_SL1, RINTOPT)

!$ACDC }

ENDIF

! * Transform the fields to be interpolated with cubic spline in the
!   vertical to "B-spline space".

IF (.NOT.LLCTC) THEN
  CALL LATTEX_EXPL_VSPLTRANS (YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDML_DYN, YDCPG_SL1)
ENDIF

! Conversions to Cartesian system for wind variables
IF (LRHS_CURV) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LRHS_CURV {

  DO JLEV=1,NFLEVG
    ZUCOMP(KST:KEND,JLEV)=YDCPG_SL1%U9%P(KST:KEND,JLEV)
    YDCPG_SL1%Z9%P(KST:KEND,JLEV)=YDCPG_SL1%V9%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GSQM2%P(KST:KEND)
    YDCPG_SL1%U9%P(KST:KEND,JLEV)=-YDCPG_SL1%U9%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GESLO%P(KST:KEND) &
     &             -YDCPG_SL1%V9%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GECLO%P(KST:KEND)*YDVARS%GEOMETRY%GEMU%P(KST:KEND)
    YDCPG_SL1%V9%P(KST:KEND,JLEV)= ZUCOMP(KST:KEND,JLEV)*YDVARS%GEOMETRY%GECLO%P(KST:KEND) &
     &             -YDCPG_SL1%V9%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GESLO%P(KST:KEND)*YDVARS%GEOMETRY%GEMU%P(KST:KEND)
  ENDDO
  IF ((NWLAG >= 3) .OR. (NSPLTHOI /= 0)) THEN
    DO JLEV=1,NFLEVG
      ZUCOMP(KST:KEND,JLEV)=YDCPG_SL1%U0%P(KST:KEND,JLEV)
      YDCPG_SL1%Z0%P(KST:KEND,JLEV)=YDCPG_SL1%V0%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GSQM2%P(KST:KEND)
      YDCPG_SL1%U0%P(KST:KEND,JLEV)=-YDCPG_SL1%U0%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GESLO%P(KST:KEND) &
       &             -YDCPG_SL1%V0%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GECLO%P(KST:KEND)*YDVARS%GEOMETRY%GEMU%P(KST:KEND)
      YDCPG_SL1%V0%P(KST:KEND,JLEV)= ZUCOMP(KST:KEND,JLEV)*YDVARS%GEOMETRY%GECLO%P(KST:KEND) &
       &             -YDCPG_SL1%V0%P(KST:KEND,JLEV)*YDVARS%GEOMETRY%GESLO%P(KST:KEND)*YDVARS%GEOMETRY%GEMU%P(KST:KEND)
    ENDDO
  ENDIF
  ! perhaps the MSLB1[u/v]F9 and MSLB1[u/v]9_SI should be treated the same way here...

!$ACDC }

ENDIF

END ASSOCIATE
END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LATTEX',1,ZHOOK_HANDLE)

END SUBROUTINE LATTEX


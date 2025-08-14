!OCL  NOEVAL
SUBROUTINE LAVENT(YDCST, YDGEOMETRY, YDCPG_OPTS, YDCPG_BNDS, YDVARS, YDCPG_TND, YDCPG_SL1, YDCPG_SL2, &
& YDRIP, YDML_DYN, YDPARAR, KSETTLOFF, LD2TLFF1, PDTS2, PRDELP, PEVEL, PWRL9, PALPHA)

!$ACDC outline1    


!**** *LAVENT*   Semi-Lagrangian scheme.
!                Computation of wind components necessary 
!                to find the SL trajectory.

!     Purpose.
!     --------
!          This subroutine computes the wind components at t
!          at each grid-point of the colocation grid (Gauss
!          grid and poles at all levels). These quantities will
!          be used in routine (E)LARMES to find the semi-Lagrangian trajectory.

!**   Interface.
!     ----------
!        *CALL* *LAVENT(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST       - first element of work.
!          KPROF     - depth of work.
!          KIBL      - index into YDGSGEOM instance in YDGEOMETRY (NPROMA chunks counter)
!          LD2TLFF1  - .T./.F.: Refined treatement of (2*Omega Vec r) at
!                      the origin point when there is t-dt (or t in SL2TL)
!                      physics / Other cases.
!          PDTS2     - 0.5*(time step) for the first time-integration step of
!                      a leap-frog scheme or all time-integration steps of
!                      a two-time level scheme; time step for the following
!                      time-integration steps of a leap-frog scheme.
!          PRDELP    - "1/(pressure depth of layers)" at t.
!          PEVEL     - "etadot (d prehyd/d eta)" at t.
!          PATND     - adiabatic Lagrangian tendencies.

!        INPUT/OUTPUT:
!          PGMV      - GMV variables at t-dt and t.
!          PB1       - "SLBUF1" buffer for interpolations.

!        OUTPUT:
!          PB2       - "SLBUF2" buffer.
!          KSETTLOFF - counter for SETTLSTF=T (# of points new scheme activated at each lev)
!          PWRL9     - vertical velocity at t - dt. (to be eventually stored in trajectory)
!          PALPHA    - weighting function switching the 2nd order scheme to 1st order

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
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after old loops 3202 and 3212 of LACDYN.
!      Original : AUGUST 1995.

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad Aug 2008: rationalisation of dummy argument interfaces + optimis.
!   F. Vana  15-Oct-2009: NSPLTHOI option
!   K. Yessad Nov 2009: rm case (lsettlst,lpc_nesct)=(f,f) for sl2tl
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   F. Vana  22-Feb-2011: NWLAG=4
!   K. Yessad (Nov 2011): use PATND for RHS of wind equation.
!   K. Yessad (Oct 2013): allow NESCT or SETTLST for predictor of LPC_CHEAP.
!   K. Yessad (Oct 2013): use NESCV or SETTLSV for vertical displacement.
!   K. Yessad (July 2014): Rename some variables, move some variables.
!   M. Diamantakis (Feb 2014): code for LSETTLSVF=T (SETTLST filter)
!   F. Vana and M. Diamantakis (Aug 2016): regularization of LSETTLSVF=T
!   F. Vana and J. Masek   21-Nov-2017: Option LSLDP_CURV
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   F. Vana July 2018: RK4 scheme for trajectory research.
!   F. Vana 11-Jul-2019: Option LRHS_CURV
!   F. Vana 11-Sep-2020: SLHD interacts with LSETTLSVF
!   H Petithomme (Dec 2020): turn ZUCOMP to scalar
!   M. Diamantakis Feb 2021: Option LSLDP_XYZ
! End Modifications
!------------------------------------------------------------------------------

USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE
USE FIELD_VARIABLES_MOD, ONLY : FIELD_VARIABLES
USE CPG_TYPE_MOD       , ONLY : CPG_TND_TYPE
USE CPG_SL1_TYPE_MOD   , ONLY : CPG_SL1B_TYPE
USE CPG_SL2_TYPE_MOD   , ONLY : CPG_SL2_TYPE
USE GEOMETRY_MOD       , ONLY : GEOMETRY
USE PARKIND1           , ONLY : JPIM, JPRB
USE YOMHOOK            , ONLY : DR_HOOK, JPHOOK, LHOOK
USE YOMCST             , ONLY : TCST
USE CPG_OPTS_TYPE_MOD  , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE YOMRIP             , ONLY : TRIP
USE YOMPARAR           , ONLY : TPARAR
!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)                ,INTENT(IN)     :: YDCST
TYPE(GEOMETRY)            ,INTENT(IN)     :: YDGEOMETRY
TYPE(CPG_OPTS_TYPE)       ,INTENT(IN)     :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)       ,INTENT(IN)     :: YDCPG_BNDS
TYPE(FIELD_VARIABLES)     ,INTENT(INOUT)  :: YDVARS
TYPE(CPG_TND_TYPE)        ,INTENT(INOUT)  :: YDCPG_TND
TYPE(CPG_SL1B_TYPE)       ,INTENT(INOUT)  :: YDCPG_SL1
TYPE(CPG_SL2_TYPE)        ,INTENT(INOUT)  :: YDCPG_SL2
TYPE(TRIP)                ,INTENT(IN)     :: YDRIP
TYPE(MODEL_DYNAMICS_TYPE) ,INTENT(IN)     :: YDML_DYN
TYPE(TPARAR)              ,INTENT(IN)     :: YDPARAR
INTEGER(KIND=JPIM)        ,INTENT(OUT)    :: KSETTLOFF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
LOGICAL                   ,INTENT(IN)     :: LD2TLFF1 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PDTS2 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PRDELP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)           ,INTENT(IN)     :: PEVEL(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)           ,INTENT(OUT)    :: PWRL9(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)           ,INTENT(OUT)    :: PALPHA(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
!     ------------------------------------------------------------------
INTEGER(KIND=JPIM) ::JROF, JLEV
REAL(KIND=JPRB) :: ZWRL0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZUSA, ZALPHA,ZUCOMP,ZZU,ZZV
LOGICAL :: LLCT, LLSETTLST, LLNESCT, LLSETTLSV, LLNESCV, LLWSAVE, LLCOND

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "local2cartgeov.intfb.h"

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAVENT',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIMV=>YDGEOMETRY%YRDIMV,YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA, YDDYN=>YDML_DYN%YRDYN, &
& YDDYNA=>YDML_DYN%YRDYNA, YDGEOV=>YDVARS%GEOMETRY)
ASSOCIATE(NFLEVG=>YDDIMV%NFLEVG, LSETFSTAT=>YDDYN%LSETFSTAT, LSETTLSVF=>YDDYN%LSETTLSVF, LSVTSM=>YDDYN%LSVTSM, &
& RSCALE=>YDDYN%RSCALE,RSCALEOFF=>YDDYN%RSCALEOFF, LSLDP_CURV=>YDDYN%LSLDP_CURV, LSLDP_RK=>YDDYN%LSLDP_RK,     &
& NCURRENT_ITER=>YDDYN%NCURRENT_ITER, NFLEVSF=>YDDYN%NFLEVSF, NSPLTHOI=>YDDYN%NSPLTHOI, NWLAG=>YDDYN%NWLAG,    &
& NFOST=>YDRIP%NFOST, LSQUALL=>YDPARAR%LSQUALL, VSQUALL=>YDPARAR%VSQUALL, LSLDP_XYZ=>YDDYN%LSLDP_XYZ,          &
& LSLDP_SAVE=>YDDYN%LSLDP_SAVE, LSETTLSVF_DIFF=>YDDYN%LSETTLSVF_DIFF, LRHS_CURV=>YDDYN%LRHS_CURV,              &
& GEMU=>YDGEOV%GEMU%P,GSQM2=>YDGEOV%GSQM2%P,GECLO=>YDGEOV%GECLO%P,GESLO=>YDGEOV%GESLO%P)
ASSOCIATE (KLON => YDCPG_OPTS%KLON, KST => YDCPG_BNDS%KIDIA, KEND => YDCPG_BNDS%KFDIA, KFLEVG => YDCPG_OPTS%KFLEVG)

!     ------------------------------------------------------------------

ZUSA=1.0_JPRB/YDCST%RA

LLCT = (YDDYNA%LPC_FULL .AND. NCURRENT_ITER > 0)

LLSETTLST=YDDYNA%LSETTLST.AND.(.NOT.YDDYNA%LELTRA)
LLNESCT=YDDYNA%LNESCT
LLSETTLSV=YDDYNA%LSETTLSV.AND.(.NOT.YDDYNA%LELTRA)
LLNESCV=YDDYNA%LNESCV
LLCOND = (NSPLTHOI /= 0).AND.(NWLAG<4)

LLWSAVE=LSLDP_XYZ.AND.LSLDP_SAVE

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

PALPHA(KST:KEND,1:NFLEVG)=0._JPRB

!     * Computation of "etadot" at full levels.

IF(YDGEOMETRY%YRVERT_GEOM%YRCVER%LVERTFE) THEN
  ! PEVEL is at full levels.
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      ZWRL0(JROF,JLEV)=PEVEL(JROF,JLEV)&
       & *(YDVETA%VETAH(JLEV)-YDVETA%VETAH(JLEV-1))*PRDELP(JROF,JLEV)
    ENDDO
  ENDDO
ELSE

  ! PEVEL is at half levels.
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      ZWRL0(JROF,JLEV)=&
       & 0.5_JPRB*(PEVEL(JROF,JLEV)+PEVEL(JROF,JLEV-1))&
       & *(YDVETA%VETAH(JLEV)-YDVETA%VETAH(JLEV-1))*PRDELP(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

!     * Computation of wind components necessary to find the SL trajectory.

IF(YDDYNA%LTWOTL.AND.LSVTSM) THEN
  DO JLEV=1,NFLEVG
    YDCPG_SL1%WRA%P(KST:KEND,JLEV)=ZWRL0(KST:KEND,JLEV)
  ENDDO
ENDIF


IF (YDDYNA%LTWOTL.AND.YDDYNA%LELTRA) THEN

  ! Use the RHS of momentum equation.
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      IF (LLCOND) THEN
        ZZU = YDCPG_SL1%UF9%P(JROF,JLEV)
        ZZV = YDCPG_SL1%VF9%P(JROF,JLEV)
      ELSE
        ZZU = YDCPG_SL1%U9%P(JROF,JLEV)
        ZZV = YDCPG_SL1%V9%P(JROF,JLEV)
      ENDIF
      YDCPG_SL1%UR0%P(JROF,JLEV)=YDVARS%U%T0(JROF,JLEV)+0.5_JPRB*ZZU+PDTS2*YDCPG_TND%TNDU(JROF,JLEV)
      YDCPG_SL1%VR0%P(JROF,JLEV)=YDVARS%V%T0(JROF,JLEV)+0.5_JPRB*ZZV+PDTS2*YDCPG_TND%TNDV(JROF,JLEV)
    ENDDO
  ENDDO

  DO JLEV=1,NFLEVG
    YDCPG_SL1%WR0%P(KST:KEND,JLEV)=ZWRL0(KST:KEND,JLEV)
    YDCPG_SL2%URL(KST:KEND,JLEV)=YDCPG_SL1%UR0%P(KST:KEND,JLEV)
    YDCPG_SL2%VRL(KST:KEND,JLEV)=YDCPG_SL1%VR0%P(KST:KEND,JLEV)
    YDCPG_SL2%WRL(KST:KEND,JLEV)=YDCPG_SL1%WR0%P(KST:KEND,JLEV)
  ENDDO

  IF (LD2TLFF1) THEN
    DO JLEV=1,NFLEVG
      YDCPG_SL1%UR9%P(KST:KEND,JLEV)=YDVARS%U%T0(KST:KEND,JLEV)
      YDCPG_SL1%VR9%P(KST:KEND,JLEV)=YDVARS%V%T0(KST:KEND,JLEV)
    ENDDO
  ENDIF

ELSEIF (YDDYNA%LTWOTL.AND.(.NOT.YDDYNA%LELTRA)) THEN

  IF (.NOT.LLCT) THEN

    ! * Cases:
    !   NSITER=0
    !   Predictor step of LPC_FULL

    ! -- Horizontal displacement:
    IF ( YDCPG_OPTS%NSTEP <= NFOST .OR. LLNESCT ) THEN
      ! * non-extrapolating scheme X(t+dt) = X(t) 
      DO JLEV=1,NFLEVG
        YDCPG_SL1%UR0%P(KST:KEND,JLEV)=YDVARS%U%T0(KST:KEND,JLEV)
        YDCPG_SL1%VR0%P(KST:KEND,JLEV)=YDVARS%V%T0(KST:KEND,JLEV)
      ENDDO
      DO JLEV=1,NFLEVG
        YDCPG_SL2%URL(KST:KEND,JLEV)=YDCPG_SL1%UR0%P(KST:KEND,JLEV)
        YDCPG_SL2%VRL(KST:KEND,JLEV)=YDCPG_SL1%VR0%P(KST:KEND,JLEV)
      ENDDO
    ELSEIF (LLSETTLST) THEN
      ! * SETTLS-extrapolating schemes X(t+dt) = f( X(t),X(t-dt) )
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          YDCPG_SL1%UR0%P(JROF,JLEV)=2.0_JPRB*YDVARS%U%T0(JROF,JLEV)-YDVARS%U%T9(JROF,JLEV)
          YDCPG_SL1%VR0%P(JROF,JLEV)=2.0_JPRB*YDVARS%V%T0(JROF,JLEV)-YDVARS%V%T9(JROF,JLEV)
        ENDDO
      ENDDO
      DO JLEV=1,NFLEVG
        YDCPG_SL2%URL(KST:KEND,JLEV)=YDVARS%U%T0(KST:KEND,JLEV)
        YDCPG_SL2%VRL(KST:KEND,JLEV)=YDVARS%V%T0(KST:KEND,JLEV)
      ENDDO
    ENDIF

    ! -- Vertical displacement:
    IF ( YDCPG_OPTS%NSTEP <= NFOST .OR. LLNESCV ) THEN
      ! * non-extrapolating scheme X(t+dt) = X(t) 
      DO JLEV=1,NFLEVG
        YDCPG_SL1%WR0%P(KST:KEND,JLEV)=ZWRL0(KST:KEND,JLEV)
        YDCPG_SL2%WRL(KST:KEND,JLEV)=YDCPG_SL1%WR0%P(KST:KEND,JLEV)
      ENDDO
      IF (YDCPG_OPTS%NSTEP == 0) THEN
        IF (LSETTLSVF.OR.LLWSAVE) PWRL9(KST:KEND,1:NFLEVG)=0.0_JPRB
      ELSEIF (LLWSAVE) THEN
        PWRL9(KST:KEND,1:NFLEVG)=YDVARS%EDOT%T9(KST:KEND,1:NFLEVG)
      ENDIF
    ELSEIF (LLSETTLSV) THEN
      ! * SETTLS-extrapolating schemes X(t+dt) = f( X(t),X(t-dt) )
      IF (LSETTLSVF) THEN
        ! detect noisy gridpoints which should not be extrapolated
        DO JLEV=1,NFLEVSF
          KSETTLOFF(:,JLEV)=0
          DO JROF=KST,KEND
            ZALPHA=0.5_JPRB*(1._JPRB - TANH(-RSCALE*(ZWRL0(JROF,JLEV)*YDVARS%EDOT%T9(JROF,JLEV)+RSCALEOFF)))

            YDCPG_SL1%WR0%P(JROF,JLEV)=ZWRL0(JROF,JLEV) + ZALPHA*(ZWRL0(JROF,JLEV)-YDVARS%EDOT%T9(JROF,JLEV))
            IF (LSETTLSVF_DIFF) PALPHA(JROF,JLEV)=1._JPRB-ZALPHA  ! inverse of zalpha for diffusion
            IF (LSETFSTAT) KSETTLOFF(JROF,JLEV)=KSETTLOFF(JROF,JLEV)+NINT(1.0_JPRB-ZALPHA)
          ENDDO
        ENDDO
        DO JLEV=NFLEVSF+1,NFLEVG
          DO JROF=KST,KEND
            YDCPG_SL1%WR0%P(JROF,JLEV)=2.0_JPRB*ZWRL0(JROF,JLEV)-YDVARS%EDOT%T9(JROF,JLEV)
          ENDDO
        ENDDO
        PWRL9(KST:KEND,1:NFLEVG)=YDVARS%EDOT%T9(KST:KEND,1:NFLEVG)
      ELSE
        DO JLEV=1,NFLEVG
          DO JROF=KST,KEND
            YDCPG_SL1%WR0%P(JROF,JLEV)=2.0_JPRB*ZWRL0(JROF,JLEV)-YDVARS%EDOT%T9(JROF,JLEV)
          ENDDO
        ENDDO
!ifdef WITH_MGRIDS
!       IF( YDML_DYN%YR_MGRIDS_ADVECTION%ACTIVE() ) THEN
!         PWRL9(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA,1:NFLEVG)=YDVARS%EDOT%T9(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA,1:NFLEVG)
!       ENDIF
!endif
      ENDIF
      DO JLEV = 1, NFLEVG
        YDCPG_SL2%WRL(KST:KEND,JLEV)=ZWRL0(KST:KEND,JLEV)
      ENDDO
    ENDIF
    YDVARS%EDOT%T9(KST:KEND,1:NFLEVG)=ZWRL0(KST:KEND,1:NFLEVG)

  ELSE 

    ! * Corrector step of LPC_FULL:

    DO JLEV=1,NFLEVG
      YDCPG_SL1%UR0%P(KST:KEND,JLEV)=YDVARS%U%T9(KST:KEND,JLEV)
      YDCPG_SL1%VR0%P(KST:KEND,JLEV)=YDVARS%V%T9(KST:KEND,JLEV)
    ENDDO
    DO JLEV=1,NFLEVG
      YDCPG_SL1%WR0%P(KST:KEND,JLEV)=YDVARS%EDOT%T9(KST:KEND,JLEV)
    ENDDO

    DO JLEV=1,NFLEVG
      YDCPG_SL2%URL(KST:KEND,JLEV)=YDVARS%U%T0(KST:KEND,JLEV)
      YDCPG_SL2%VRL(KST:KEND,JLEV)=YDVARS%V%T0(KST:KEND,JLEV)
    ENDDO
    DO JLEV=1,NFLEVG
      YDCPG_SL2%WRL(KST:KEND,JLEV)=ZWRL0(KST:KEND,JLEV)
    ENDDO

  ENDIF

  IF (LD2TLFF1) THEN
    DO JLEV=1,NFLEVG
      YDCPG_SL1%UR9%P(KST:KEND,JLEV)=YDVARS%U%T0(KST:KEND,JLEV)
      YDCPG_SL1%VR9%P(KST:KEND,JLEV)=YDVARS%V%T0(KST:KEND,JLEV)
    ENDDO
  ENDIF

ELSE

  ! SL3TL:

  DO JLEV=1,NFLEVG
    YDCPG_SL1%UR0%P(KST:KEND,JLEV)=YDVARS%U%T0(KST:KEND,JLEV)
    YDCPG_SL1%VR0%P(KST:KEND,JLEV)=YDVARS%V%T0(KST:KEND,JLEV)
  ENDDO
  DO JLEV=1,NFLEVG
    YDCPG_SL1%WR0%P(KST:KEND,JLEV)=ZWRL0(KST:KEND,JLEV)
    YDCPG_SL2%URL(KST:KEND,JLEV)=YDCPG_SL1%UR0%P(KST:KEND,JLEV)
    YDCPG_SL2%VRL(KST:KEND,JLEV)=YDCPG_SL1%VR0%P(KST:KEND,JLEV)
    YDCPG_SL2%WRL(KST:KEND,JLEV)=YDCPG_SL1%WR0%P(KST:KEND,JLEV)
  ENDDO

  IF (LD2TLFF1) THEN
    DO JLEV=1,NFLEVG
      YDCPG_SL1%UR9%P(KST:KEND,JLEV)=YDVARS%U%T9(KST:KEND,JLEV)
      YDCPG_SL1%VR9%P(KST:KEND,JLEV)=YDVARS%V%T9(KST:KEND,JLEV)
    ENDDO
  ENDIF

ENDIF

IF (YDCPG_OPTS%LAROME.AND.LSQUALL) THEN
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      YDCPG_SL1%VR0%P(JROF,JLEV)=YDCPG_SL1%VR0%P(JROF,JLEV)+VSQUALL
      YDCPG_SL2%VRL(JROF,JLEV)=YDCPG_SL2%VRL(JROF,JLEV)+VSQUALL
    ENDDO
  ENDDO
ENDIF

! Filling extra buffers for wind interpolation with quantity valid at time t
! Makes sense for the SETTLS option. In the other case it duplicates the MSLB1[x]R0 content.
IF (LSLDP_RK) THEN
  DO JLEV=1,NFLEVG
    YDCPG_SL1%UR00%P(KST:KEND,JLEV)=YDCPG_SL2%URL(KST:KEND,JLEV)
    YDCPG_SL1%VR00%P(KST:KEND,JLEV)=YDCPG_SL2%VRL(KST:KEND,JLEV)
    YDCPG_SL1%WR00%P(KST:KEND,JLEV)=YDCPG_SL2%WRL(KST:KEND,JLEV)
  ENDDO
ENDIF

! Transforming horizontal wind components from lat,lon to 3D cartesian x,y,z coordinates
!    to mitigate the pole singularity.
! NOTE: This transformation has to be of exact match with the reverse transformation in LARCINA
IF (LSLDP_CURV.OR.LSLDP_XYZ) THEN
  CALL LOCAL2CARTGEOV(0,NFLEVG,YDGEOMETRY%YRDIM%NPROMA,KST,KEND,&
    &GEMU,GSQM2,GECLO,GESLO,YDCPG_SL1%UR0%P,YDCPG_SL1%VR0%P,YDCPG_SL1%ZR0%P)

  IF (LSLDP_XYZ) THEN
    CALL LOCAL2CARTGEOV(1,NFLEVG,YDGEOMETRY%YRDIM%NPROMA,KST,KEND,&
      &GEMU,GSQM2,GECLO,GESLO,YDCPG_SL2%URL,YDCPG_SL2%VRL,YDCPG_SL2%ZRL)
  ENDIF

  IF (LSLDP_RK) THEN
    CALL LOCAL2CARTGEOV(0,NFLEVG,YDGEOMETRY%YRDIM%NPROMA,KST,KEND,&
      &GEMU,GSQM2,GECLO,GESLO,YDCPG_SL1%UR00%P,YDCPG_SL1%VR00%P,YDCPG_SL1%ZR00%P)
  ENDIF
ENDIF

IF (LRHS_CURV.AND.LD2TLFF1) THEN
  CALL LOCAL2CARTGEOV(0,NFLEVG,YDGEOMETRY%YRDIM%NPROMA,KST,KEND,&
    &GEMU,GSQM2,GECLO,GESLO,YDCPG_SL1%UR9%P,YDCPG_SL1%VR9%P,YDCPG_SL1%ZR9%P)
ENDIF
!     ------------------------------------------------------------------

!$ACDC }


END ASSOCIATE
END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LAVENT',1,ZHOOK_HANDLE)

END SUBROUTINE LAVENT


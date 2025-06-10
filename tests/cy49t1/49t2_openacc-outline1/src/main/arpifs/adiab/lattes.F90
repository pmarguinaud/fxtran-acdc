SUBROUTINE LATTES(YDCST, YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDVARS, YDCPG_SL1, YDCPG_SL2, YDRIP, &
& YDML_DYN, PDTS2, PBDT, PESGP, PESGM, POROGL, POROGM, PSDIV0, PSDIV9, PSDVBC, PRES0, PMIXNL)

!$ACDC outline1    


!**** *LATTES*   Semi-Lagrangian scheme.
!                Computation of the t and t-dt useful quantities
!                 at grid-points. Equations for bi-dimensional
!                 variables (continuity equation).

!     Purpose.
!     --------
!        * This subroutine computes the equation quantities to be
!          interpolated at each grid-point of the colocation grid.
!          The terms considered here are the explicit terms and
!          the explicit part of the semi-implicit scheme (terms
!          previously computed in LASSIE or LANHSI).
!          Equations considered here are equations for bi-dimensional
!          variables: continuity equation.
!        * Remark 1: when an alternate averaging is used for linear terms
!          in the 2TL SL scheme, the first timestep is treated differently
!          (first order uncentering), no uncentering is applied to the
!          total term ("cursive A") and the term saved in P[X]NLT9
!          is [ (Delta t/2) ("cursive A" - (1 + xidt) beta "cursive B") ]
!          instead of [ (Delta t/2) ("cursive A" - beta "cursive B") ].
!        * Remark 2: for lsettls=true, uncentering is applied to
!          the 'stable' extrapolation if vesl > 0 to avoid instability
!          in the momentum equation.
!        * Remark 3: for PC schemes:
!          - this routine is called for nsiter=0.
!          - this routine is called for the predictor of lpc_full.
!          - this routine is called for the corrector of lpc_full.

!**   Interface.
!     ----------
!        *CALL* *LATTES(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST         - first element of work.
!          KPROF       - depth of work.
!          PDTS2       - 0.5*"pdt", where "pdt" =
!                        time step for the first time-integration step of
!                        a leap-frog scheme or all time-integration steps of
!                        a two-time level scheme; 2*time step for the following
!                        time-integration steps of a leap-frog scheme.
!          PBDT        - zbt if semi-implicit scheme with unreduced
!                        divergence, zbt*(c**2/GM**2) if semi-implicit
!                        scheme with reduced divergence, where zbt
!                        is equal to PDTS2*BETADT (BETADT is in YOMDYN) or zero
!                        according to the value of configuration.
!          PESGP       - (1 + uncentering factor).
!          PESGM       - (1 - uncentering factor).
!          KIBL        - index into YROROG instance in YDGEOMETRY
!          POROGL      - zonal component of "grad(surf orography)"
!          POROGM      - meridian component of "grad(surf orography)"
!          PSDIV0      - SI term at time t for continuity equation (Nu*D).
!          PSDIV9      - SI term at time t-dt for continuity equation (Nu*D).
!          PSDVBC      - vertical integral of divergence computed in "gpcty",
!                        including the "lrubc" and "delta m=1" contributions
!                        of (etadot d prehyd/d eta).
!          PRES0       - hydrostatic pressure at half levels at t.
!          PGMVS       - GMVS variables at t-dt and t.
!          PMIXNL      - extrapolation control variable for mixed NESC/SETTLS scheme.

!        INPUT/OUTPUT:
!          PGMV        - GMV variables at t-dt and t.
!          PGMVT1S     - GMVS variables at t+dt.
!          PB1         - "SLBUF1" buffer for interpolations.
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
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after old parts 3.3 et 3.4 of LACDYN. 
!      Original : AUGUST 1995.

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad (Aug 2008): simplify XIDT treatment with PC + cleanings
!   K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   K. Yessad (Oct 2013): allow NESC without ICI-PC scheme.
!   K. Yessad (July 2014): Move some variables, rename some variables.
!   K. Yessad (Dec 2016): Prune obsolete options.
!   J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   J. Vivoda (July 2018): mixed NESC/SETTLS scheme.
! End Modifications
!------------------------------------------------------------------------------

USE GEOMETRY_MOD       , ONLY : GEOMETRY
USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE

USE PARKIND1           , ONLY : JPIM, JPRB
USE YOMHOOK            , ONLY : DR_HOOK, JPHOOK, LHOOK
USE YOMCST             , ONLY : TCST
USE CPG_OPTS_TYPE_MOD  , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE CPG_SL1_TYPE_MOD   , ONLY : CPG_SL1B_TYPE
USE CPG_SL2_TYPE_MOD   , ONLY : CPG_SL2_TYPE
USE YOMRIP             , ONLY : TRIP
USE FIELD_VARIABLES_MOD, ONLY : FIELD_VARIABLES

!------------------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)                ,INTENT(IN)     :: YDCST
TYPE(GEOMETRY)            ,INTENT(IN)     :: YDGEOMETRY
TYPE(CPG_BNDS_TYPE)       ,INTENT(IN)     :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE)       ,INTENT(IN)     :: YDCPG_OPTS
TYPE(FIELD_VARIABLES)     ,INTENT(INOUT)  :: YDVARS
TYPE(CPG_SL1B_TYPE)       ,INTENT(INOUT)  :: YDCPG_SL1
TYPE(CPG_SL2_TYPE)        ,INTENT(INOUT)  :: YDCPG_SL2
TYPE(TRIP)                ,INTENT(IN)     :: YDRIP
TYPE(MODEL_DYNAMICS_TYPE) ,INTENT(IN)     :: YDML_DYN
REAL(KIND=JPRB)           ,INTENT(IN)     :: PDTS2 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PBDT(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PESGP 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PESGM 
REAL(KIND=JPRB)           ,INTENT(IN)     :: POROGL(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)           ,INTENT(IN)     :: POROGM(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PSDIV0(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PSDIV9(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PSDVBC(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PRES0(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)           ,INTENT(IN)     :: PMIXNL(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
!------------------------------------------------------------------------------
REAL(KIND=JPRB) :: ZMOY1SP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB) :: ZMOYSPNL(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPNLT0(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPNLT1(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPNLT_FE(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG+1)
REAL(KIND=JPRB) :: ZSPTB(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPT1(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZVWEI(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRB) :: ZCMSLP, ZXIGP
REAL(KIND=JPRB) :: ZXIDT0, ZXIDT9
REAL(KIND=JPRB) :: ZNESC, ZSETTLS, ZMIXNL

LOGICAL :: LLCT, LLCTC
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!------------------------------------------------------------------------------

#include "verdisint.intfb.h"

!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LATTES',0,ZHOOK_HANDLE)

ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDVAB=>YDGEOMETRY%YRVERT_GEOM%YRVAB, YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA, &
& YDVFE=>YDGEOMETRY%YRVERT_GEOM%YRVFE, YDDYN=>YDML_DYN%YRDYN, YDDYNA=>YDML_DYN%YRDYNA, YDCVER=> YDGEOMETRY%YRVERT_GEOM%YRCVER)

ASSOCIATE(NPROMA=>YDDIM%NPROMA, NFLEVG=>YDDIMV%NFLEVG, NCURRENT_ITER=>YDDYN%NCURRENT_ITER, NVLAG=>YDDYN%NVLAG, &
& RCMSLP0=>YDDYN%RCMSLP0, XIDT=>YDDYN%XIDT, NFOST=>YDRIP%NFOST)

ASSOCIATE(KLON => YDCPG_OPTS%KLON, KST => YDCPG_BNDS%KIDIA, KEND => YDCPG_BNDS%KFDIA, KFLEVG => YDCPG_OPTS%KFLEVG)

!------------------------------------------------------------------------------

!############################################
! 1. AUXILIARITIES
!############################################

LLCT = YDDYNA%LPC_FULL .AND. NCURRENT_ITER > 0 ! corrector for LPC_FULL
LLCTC = YDDYNA%LPC_CHEAP .AND. NCURRENT_ITER > 0

ZXIDT0=1.0_JPRB+XIDT
ZXIDT9=1.0_JPRB+XIDT
ZXIGP=1.0_JPRB+XIDT

!------------------------------------------------------------------------------

!############################################
! 2. NONLINEAR MODEL
!############################################

ZCMSLP=RCMSLP0/(YDCST%RD*YDCPG_OPTS%RTSUR)

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

DO JROF=KST,KEND
  ZSPTB(JROF)=PSDVBC(JROF,NFLEVG)/PRES0(JROF,NFLEVG)
ENDDO

! * [vertical weight]_l = [delta B]_l
DO JLEV=1,NFLEVG
  ZVWEI(KST:KEND,JLEV)=YDVAB%VDELB(JLEV)
ENDDO

! linear model at time t

DO JROF=KST,KEND
  YDCPG_SL2%SPSI(JROF)=PBDT(JROF)*PSDIV0(JROF)
  ZSPTB(JROF)=-PDTS2*ZSPTB(JROF)
ENDDO

! nonlinear model at time t

DO JLEV=1,NFLEVG
  DO JROF=KST,KEND
    ZMOY1SP(JROF,JLEV)=PDTS2*&
     & (YDVARS%U%T0(JROF,JLEV)*(YDVARS%SP%DL(JROF)+ZCMSLP*POROGL(JROF))&
     & +YDVARS%V%T0(JROF,JLEV)*(YDVARS%SP%DM(JROF)+ZCMSLP*POROGM(JROF)))
  ENDDO
  DO JROF=KST,KEND
    ZMOY1SP(JROF,JLEV)=ZMOY1SP(JROF,JLEV)+ZSPTB(JROF)
  ENDDO
ENDDO

!*       *   Continuity equation.

IF (YDDYNA%LTWOTL) THEN

  IF(.NOT.LLCT)THEN

    !############################################
    ! Predictor for LPC_FULL
    ! or case nsiter=0.
    !############################################

    !---------------------------------------------------------------
    ! 3D buffers 
    !---------------------------------------------------------------
    DO JLEV=1,NFLEVG

      ! nonlinear residual time t
      DO JROF=KST,KEND
        ZSPNLT0(JROF)=ZMOY1SP(JROF,JLEV)+ZXIDT0*YDCPG_SL2%SPSI(JROF)
      ENDDO

      ! Fill PB1(.,MSLB1C9),PGMVT1S(.,YT1%MSP),ZSPNLT_FE:
      IF (NVLAG == 2 .OR. NVLAG == 3) THEN

        IF ( YDCPG_OPTS%NSTEP <= NFOST .OR. YDDYNA%LNESC ) THEN

          DO JROF=KST,KEND
            YDCPG_SL1%C9%P(JROF,JLEV)=YDCPG_SL1%C9%P(JROF,JLEV)+PESGM*ZMOY1SP(JROF,JLEV)
          ENDDO
          IF (YDCVER%LVERTFE) THEN
            DO JROF=KST,KEND
              ZSPNLT_FE(JROF,JLEV)=ZSPNLT0(JROF)*ZVWEI(JROF,JLEV)*YDVETA%VFE_RDETAH(JLEV)*PESGP
            ENDDO
          ELSE
            DO JROF=KST,KEND
              YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+PESGP*ZVWEI(JROF,JLEV)*ZSPNLT0(JROF)
            ENDDO
          ENDIF
 
        ELSEIF (YDDYNA%LSETTLS) THEN

          IF (YDDYNA%LPC_CHEAP) THEN
            DO JROF=KST,KEND
              ZNESC  =PESGM*ZMOY1SP(JROF,JLEV)
              ZSETTLS=ZSPNLT0(JROF)-YDVARS%SPNL%T9(JROF,JLEV)
              ZMIXNL =PMIXNL(JROF,JLEV)
              YDCPG_SL1%C9%P(JROF,JLEV)=YDCPG_SL1%C9%P(JROF,JLEV)+ZNESC
              YDCPG_SL1%C9_NL%P(JROF,JLEV)=ZMIXNL*ZSETTLS
            ENDDO
          ELSE
            DO JROF=KST,KEND
              ZNESC  =PESGM*ZMOY1SP(JROF,JLEV)
              ZSETTLS=ZSPNLT0(JROF)-YDVARS%SPNL%T9(JROF,JLEV)
              ZMIXNL =PMIXNL(JROF,JLEV)
              YDCPG_SL1%C9%P(JROF,JLEV)=YDCPG_SL1%C9%P(JROF,JLEV)+ZNESC+ZMIXNL*ZSETTLS
            ENDDO
          ENDIF
          IF( YDCVER%LVERTFE )THEN
            DO JROF=KST,KEND
              ZSPNLT_FE(JROF,JLEV)=ZSPNLT0(JROF)*PESGP*ZVWEI(JROF,JLEV)*YDVETA%VFE_RDETAH(JLEV)
            ENDDO
          ELSE
            DO JROF=KST,KEND
              YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+PESGP*ZVWEI(JROF,JLEV)*ZSPNLT0(JROF)
            ENDDO
          ENDIF

        ELSE

          ! remaining case: ldsettls=false, lnesc=false.
          DO JROF=KST,KEND
            YDCPG_SL1%C9%P(JROF,JLEV)=YDCPG_SL1%C9%P(JROF,JLEV)+PESGM*ZMOY1SP(JROF,JLEV)&
             & +0.5_JPRB*PESGM*(ZSPNLT0(JROF)-YDVARS%SPNL%T9(JROF,JLEV))
          ENDDO 
          IF (YDCVER%LVERTFE) THEN
            DO JROF=KST,KEND
              ZSPNLT_FE(JROF,JLEV)=(1.5_JPRB*ZSPNLT0(JROF)-0.5_JPRB*YDVARS%SPNL%T9(JROF,JLEV))&
               & *ZVWEI(JROF,JLEV)*YDVETA%VFE_RDETAH(JLEV)*PESGP
            ENDDO
          ELSE
            DO JROF=KST,KEND
              YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+PESGP*ZVWEI(JROF,JLEV)&
               & *(1.5_JPRB*ZSPNLT0(JROF)-0.5_JPRB*YDVARS%SPNL%T9(JROF,JLEV))
            ENDDO
          ENDIF

        ENDIF

      ENDIF

      ! Save quantities for corrector step
      IF( YDDYNA%LPC_FULL )THEN
        ! save nonlinear model at time t
        YDVARS%CSPNL%T9(KST:KEND,JLEV)=ZMOY1SP(KST:KEND,JLEV)
      ENDIF

      IF( .NOT.YDDYNA%LNESC )THEN
        ! save of nonlinear residual at time t
        ! to be used as nonlinear residual at time t-dt next time step
        DO JROF=KST,KEND
          YDVARS%SPNL%T9(JROF,JLEV)=ZMOY1SP(JROF,JLEV)+ZXIDT9*YDCPG_SL2%SPSI(JROF)
        ENDDO
      ENDIF

    ENDDO

    !------------------------------------------------------------------------
    ! 3D additional actions
    !------------------------------------------------------------------------
    IF (YDCVER%LVERTFE) THEN
      IF(NVLAG == 2 .OR. NVLAG == 3)THEN
        ZSPNLT_FE(KST:KEND,0)=0.0_JPRB
        ZSPNLT_FE(KST:KEND,NFLEVG+1)=0.0_JPRB
        CALL VERDISINT(YDVFE,YDCVER,'ITOP','11',NPROMA,KST,KEND,NFLEVG,ZSPNLT_FE,POUTS=ZSPT1)

        DO JROF=KST,KEND
          YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+ZSPT1(JROF)
        ENDDO
      ENDIF
    ENDIF

    !------------------------------------------------------------------------
    ! 2D buffers
    !------------------------------------------------------------------------
    IF(NVLAG == 2 .OR. NVLAG == 3)THEN
      !dir$ ivdep
      DO JROF=KST,KEND
        YDCPG_SL1%SP9%P(JROF)=YDCPG_SL1%SP9%P(JROF)+YDVARS%SP%T0(JROF)+ZCMSLP*YDVARS%GEOMETRY%OROG%T0(JROF)
        YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)-ZCMSLP*YDVARS%GEOMETRY%OROG%T0(JROF)
      ENDDO
    ENDIF

  ELSE

    !############################################
    ! Corrector for LPC_FULL
    !############################################
    !---------------------------------------------------------------
    ! 3D buffers 
    !---------------------------------------------------------------
    DO JLEV=1,NFLEVG

      ! nonlinear residual time t

      DO JROF=KST,KEND
        ZSPNLT1(JROF)=ZMOY1SP(JROF,JLEV)+ZXIDT9*YDCPG_SL2%SPSI(JROF)
      ENDDO

      IF (.NOT.LLCTC) THEN
        DO JROF=KST,KEND
          YDCPG_SL1%C9%P(JROF,JLEV)=YDCPG_SL1%C9%P(JROF,JLEV)+PESGM*YDVARS%CSPNL%T9(JROF,JLEV)
        ENDDO
      ENDIF
      IF (YDCVER%LVERTFE) THEN
        DO JROF=KST,KEND
          ZSPNLT_FE(JROF,JLEV)=ZSPNLT1(JROF)*ZVWEI(JROF,JLEV)*YDVETA%VFE_RDETAH(JLEV)*PESGP
        ENDDO
      ELSE
        DO JROF=KST,KEND
          YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+ZVWEI(JROF,JLEV)*ZSPNLT1(JROF)*PESGP
        ENDDO
      ENDIF

    ENDDO

    !------------------------------------------------------------------------
    ! 3D additional actions
    !------------------------------------------------------------------------
    IF (YDCVER%LVERTFE) THEN
      ZSPNLT_FE(KST:KEND,0)=0.0_JPRB
      ZSPNLT_FE(KST:KEND,NFLEVG+1)=0.0_JPRB
      CALL VERDISINT(YDVFE,YDCVER,'ITOP','11',NPROMA,KST,KEND,NFLEVG,ZSPNLT_FE,POUTS=ZSPT1)

      DO JROF=KST,KEND
        YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+ZSPT1(JROF)
      ENDDO
    ENDIF

    !------------------------------------------------------------------------
    ! 2D buffers
    !------------------------------------------------------------------------
    IF (.NOT.LLCTC) THEN
      !dir$ ivdep
      DO JROF=KST,KEND
        YDCPG_SL1%SP9%P(JROF)=YDCPG_SL1%SP9%P(JROF)+YDVARS%SP%T9(JROF)+ZCMSLP*YDVARS%GEOMETRY%OROG%T0(JROF)
      ENDDO
    ENDIF

    !dir$ ivdep
    DO JROF=KST,KEND
      YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)-ZCMSLP*YDVARS%GEOMETRY%OROG%T0(JROF)
    ENDDO

  ENDIF

  ! add preliminary quantity for LAPGHY physics

  IF( XIDT > 0.0_JPRB )THEN
    DO JROF=KST,KEND
      YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)-ZXIGP*YDCPG_SL2%SPSI(JROF)
      YDCPG_SL2%SPSI(JROF)=ZXIGP*YDCPG_SL2%SPSI(JROF)
    ENDDO
  ELSE
    DO JROF=KST,KEND
      YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)-PESGP*YDCPG_SL2%SPSI(JROF)
      YDCPG_SL2%SPSI(JROF)=PESGP*YDCPG_SL2%SPSI(JROF)
    ENDDO
  ENDIF

ELSE

  IF(NVLAG == 2 .OR. NVLAG == 3)THEN

    IF(YDCVER%LVERTFE) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          ZMOYSPNL(JROF)=ZMOY1SP(JROF,JLEV)+YDCPG_SL2%SPSI(JROF)
          YDCPG_SL1%C9%P(JROF,JLEV)=YDCPG_SL1%C9%P(JROF,JLEV)&
           & +PESGM*ZMOYSPNL(JROF)-PESGM*PBDT(JROF)*PSDIV9(JROF)
          ZSPNLT_FE(JROF,JLEV)=PESGP*ZVWEI(JROF,JLEV)*ZMOYSPNL(JROF)
        ENDDO
      ENDDO
      ZSPNLT_FE(KST:KEND,0)=0.0_JPRB
      ZSPNLT_FE(KST:KEND,NFLEVG+1)=0.0_JPRB
      CALL VERDISINT(YDVFE,YDCVER,'ITOP','11',NPROMA,KST,KEND,NFLEVG,ZSPNLT_FE,POUTS=ZSPT1)

      DO JROF=KST,KEND
        YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+ZSPT1(JROF)
      ENDDO
    ELSE
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND
          ZMOYSPNL(JROF)=ZMOY1SP(JROF,JLEV)+YDCPG_SL2%SPSI(JROF)
          YDCPG_SL1%C9%P(JROF,JLEV)=YDCPG_SL1%C9%P(JROF,JLEV)+PESGM*ZMOYSPNL(JROF)-PESGM*PBDT(JROF)*PSDIV9(JROF)
          YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)+PESGP*ZVWEI(JROF,JLEV)*ZMOYSPNL(JROF)
        ENDDO
      ENDDO
    ENDIF

    !dir$ ivdep
    DO JROF=KST,KEND
      YDCPG_SL1%SP9%P(JROF)=YDCPG_SL1%SP9%P(JROF)+YDVARS%SP%T9(JROF)+ZCMSLP*YDVARS%GEOMETRY%OROG%T0(JROF)
      YDVARS%SP%T1(JROF)=YDVARS%SP%T1(JROF)-ZCMSLP*YDVARS%GEOMETRY%OROG%T0(JROF)-PESGP*YDCPG_SL2%SPSI(JROF)
      YDCPG_SL2%SPSI(JROF)=PESGP*YDCPG_SL2%SPSI(JROF)
    ENDDO

  ENDIF

ENDIF

!$ACDC }

!------------------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LATTES',1,ZHOOK_HANDLE)

END SUBROUTINE LATTES


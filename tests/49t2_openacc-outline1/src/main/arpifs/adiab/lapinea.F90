#ifdef RS6K
@PROCESS NOCHECK
#endif
SUBROUTINE LAPINEA(&
 ! --- INPUT ------------------------------------------------------------------
 & YDMODEL,YDGEOMETRY,YDCPG_OPTS,YDCPG_BNDS,YDSL,YDCPG_SL1,YDCPG_SL2,YDA_PWRL9,&
 ! --- INPUT/OUTPUT -----------------------------------------------------------
 & YDMASK_SL2, &
 & YDA_KVSEPC,YDA_KVSEPL,&
 ! --- OUTPUT -----------------------------------------------------------------
 & YDA_RSAVEDP,YDA_PCCO,YDA_PUF,YDA_PVF,YDA_KL0,YDA_KLH0,YDA_PLSCAW,YDA_PRSCAW,YDA_KL0H,YDA_PLSCAWH,YDA_PRSCAWH,&
 & YDA_PSCO,YDVARS,YDA_KNOWENO)

!$ACDC outline1    


!**** *LAPINEA* - semi-LAgrangian scheme(Trajectory):
!                Interface subroutine for interpolations and lagrangian
!                trends. (Programme INterface d'Ensemble).

!     Purpose.
!     --------
!           Grid point calculations in dynamics.

!           Computes the semi-Lagrangian trajectory. 
!           Computes the weights ready for interpolations at origin point.

!**   Interface.
!     ----------
!        *CALL* *LAPINEA(.....)

!        Explicit arguments :
!        --------------------
!        INPUT:
!          KST       - first element of arrays where computations are performed.
!          KEND     - depth of work.
!          YDSL      - SL_STRUCT definition.
!          KIBL      - index into YRGSGEOM/YRCSGEOM instances in YDGEOMETRY

!        INPUT AND OUTPUT:
!          KVSEPC    - vertical separation (used in S/L adjoint of cubic interp.)
!          KVSEPL    - vertical separation (used in S/L adjoint, linear interp.)

!        OUTPUT:

!          PCCO      - information about comput. space position of interpol. point.
!          PUF,PVF   - U-comp and V-comp of "(a/rs)*wind" necessary to
!                      find the position of the origin point,
!                      in a local repere linked to computational sphere.
!          KL0       - index of the four western points
!                      of the 16 points interpolation grid.
!          KLH0      - second value of index of the four western points
!                      of the 16 points interpolation grid if needed.
!          PLSCAW    - linear weights (distances) for interpolations.
!          PRSCAW    - non-linear weights for interpolations.
!          KL0H      - half-level KL0
!          PLSCAWH   - linear weights (distances) for interpolations at half-levels.
!          PRSCAWH   - non-linear weights for interpolations at half-levels.
!          PSCO      - information about geographic position of interpol. point.
!          KNOWENO   - quintic interpolation stencil indicator

!        Implicit arguments :
!        --------------------

!     Method.
!     -------

!     Externals.
!     ----------
!       See includes below.
!       Called by CALL_SL.

!     Reference.
!     ----------
!        ARPEGE documentation about semi-Lagrangian scheme.

!     Author.
!     -------
!        K. YESSAD after the subroutine CPLGDY2 written by
!        Maurice IMBARD   METEO FRANCE/EERM/CRMD
!        Original : JUNE 1991.

!     Modifications.
!     --------------
!   F.Vana  09-Jan-2007 new argument to LARCINA
!   K. Yessad 07-03-2007: Remove useless (gw)_surf interpol. in NH+LGWADV.
!   F. Vana   28-Aug-2007: removing 3D spline interpolation
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   30-Jun-2008 J. Masek    New dataflow for SLHD scheme.
!   K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!   K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   F. Vana 21-Feb-2011: Extended dimensions of weights for hor. turbulence
!   G. Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!   G.Mozdzynski (Feb 2011): OOPS cleaning, use of derived types TGSGEOM and TCSGEOM
!   G. Mozdzynski (May 2012): further cleaning
!   F. Vana 13-Feb-2014 KAPPAT for heat quantities.
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   S. Malardel (Nov 2013): COMAD weights for SL interpolations
!   K. Yessad (July 2014): Move some variables.
!   K. Yessad (March 2017): simplify level numbering in interpolator.
!   K. Yessad (June 2017): Introduce NHQE model.
!   F. Vana    21-Nov-2017: Options LSLDP_CURV and LHOISLT
!   F. Vana July 2018: RK4 scheme for trajectory research.
!   F. Vana October 2018: Extended LSLDP_CURV.
!   F. Vana 20-Fev-2019: Quintic vertical interpolation
!   R. El Khatib 27-02-2019 Use pointer function SC2PRG to avoid bounds violation
!   F. Vana 11-Jul-2019: Option LRHS_CURV
!   M. Diamantakis Feb 2021: Add cartesian geometry option LSLDP_XYZ for departure point iterations
! End Modifications
!-------------------------------------------------------------------------------

USE TYPE_MODEL             , ONLY : MODEL
USE GEOMETRY_MOD           , ONLY : GEOMETRY
USE PARKIND1               , ONLY : JPIM, JPRB
USE YOMHOOK                , ONLY : LHOOK, DR_HOOK, JPHOOK
USE EINT_MOD               , ONLY : SL_STRUCT
USE FIELD_VARIABLES_MOD    , ONLY : FIELD_VARIABLES
USE CPG_SL1_TYPE_MOD       , ONLY : CPG_SL1F_TYPE
USE CPG_SL2_TYPE_MOD       , ONLY : CPG_SL2_TYPE
USE CPG_SL_MASK_TYPE_MOD   , ONLY : CPG_SL_MASK_TYPE
USE FIELD_ARRAY_MODULE     , ONLY : FIELD_3RB_ARRAY, FIELD_2IM_ARRAY, FIELD_4RB_ARRAY, FIELD_4IM_ARRAY, FIELD_3IM_ARRAY
USE CPG_OPTS_TYPE_MOD      , ONLY : CPG_OPTS_TYPE, CPG_BNDS_TYPE

!    -------------------------------------------------------------

IMPLICIT NONE

TYPE(MODEL)                   ,INTENT(IN)     :: YDMODEL
TYPE(GEOMETRY)                ,INTENT(IN)     :: YDGEOMETRY
TYPE(CPG_OPTS_TYPE)           ,INTENT(IN)     :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)           ,INTENT(IN)     :: YDCPG_BNDS
TYPE(SL_STRUCT)               ,INTENT(IN)     :: YDSL
TYPE(CPG_SL1F_TYPE)           ,INTENT(IN)     :: YDCPG_SL1
TYPE(CPG_SL2_TYPE)            ,INTENT(IN)     :: YDCPG_SL2
TYPE(FIELD_3RB_ARRAY)         ,INTENT(IN)     :: YDA_PWRL9   
TYPE(CPG_SL_MASK_TYPE)        ,INTENT(INOUT)  :: YDMASK_SL2
TYPE(FIELD_2IM_ARRAY)         ,INTENT(INOUT)  :: YDA_KVSEPC
TYPE(FIELD_2IM_ARRAY)         ,INTENT(INOUT)  :: YDA_KVSEPL
TYPE(FIELD_4RB_ARRAY)         ,INTENT(INOUT)  :: YDA_RSAVEDP
TYPE(FIELD_4RB_ARRAY)         ,INTENT(IN)     :: YDA_PCCO    
TYPE(FIELD_3RB_ARRAY)         ,INTENT(IN)     :: YDA_PUF     
TYPE(FIELD_3RB_ARRAY)         ,INTENT(IN)     :: YDA_PVF     
TYPE(FIELD_4IM_ARRAY)         ,INTENT(IN)     :: YDA_KL0     
TYPE(FIELD_4IM_ARRAY)         ,INTENT(IN)     :: YDA_KLH0    
TYPE(FIELD_4RB_ARRAY)         ,INTENT(IN)     :: YDA_PLSCAW  
TYPE(FIELD_4RB_ARRAY)         ,INTENT(IN)     :: YDA_PRSCAW  
TYPE(FIELD_4IM_ARRAY)         ,INTENT(IN)     :: YDA_KL0H    
TYPE(FIELD_4RB_ARRAY)         ,INTENT(IN)     :: YDA_PLSCAWH 
TYPE(FIELD_4RB_ARRAY)         ,INTENT(IN)     :: YDA_PRSCAWH 
TYPE(FIELD_4RB_ARRAY)         ,INTENT(IN)     :: YDA_PSCO    
TYPE(FIELD_VARIABLES)         ,INTENT(INOUT)  :: YDVARS
TYPE(FIELD_3IM_ARRAY)         ,INTENT(IN)     :: YDA_KNOWENO 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: ILEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
INTEGER(KIND=JPIM) :: IGLGLO
INTEGER(KIND=JPIM) :: IHVI
INTEGER(KIND=JPIM) :: IROT
INTEGER(KIND=JPIM) :: ITIP
INTEGER(KIND=JPIM) :: JGL
INTEGER(KIND=JPIM) :: JROF
INTEGER(KIND=JPIM) :: JLEV
INTEGER(KIND=JPIM) :: JGFL
INTEGER(KIND=JPIM) :: IDEP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

INTEGER (KIND=JPIM) :: I_DPDETA0 
INTEGER (KIND=JPIM) :: I_UTRAJ   
INTEGER (KIND=JPIM) :: I_VTRAJ   
INTEGER (KIND=JPIM) :: I_WTRAJ   
INTEGER (KIND=JPIM) :: I_LAMBDA0 
INTEGER (KIND=JPIM) :: I_THETA0  
INTEGER (KIND=JPIM) :: I_ETA0    

LOGICAL :: LLINTV

REAL(KIND=JPRB) :: ZUF0(1), ZURL0(1), ZVF0(1), ZZRL0(1)
REAL(KIND=JPRB) :: ZVRL0(1), ZZF0(1), ZWF0(1), ZWFSM(1), ZWRL0(1)

! SL diagnostics
REAL(KIND=JPRB) :: ZWF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

REAL(KIND=JPRB) :: ZGMDTX(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZGMDTY(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)

!     ------------------------------------------------------------------

#include "elarmes.intfb.h"
#include "larcina.intfb.h"
#include "larcinha.intfb.h"
#include "larmes.intfb.h"
#include "larmes_rk.intfb.h"
#include "larmes_xyz.intfb.h"
#include "abor1.intfb.h"

!     ------------------------------------------------------------------

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('LAPINEA',0,ZHOOK_HANDLE)

ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,  YDEGEO=>YDGEOMETRY%YREGEO, YDDYN=>YDMODEL%YRML_DYN%YRDYN,&
& YDDYNA=>YDMODEL%YRML_DYN%YRDYNA, YDPTRSLB1=>YDMODEL%YRML_DYN%YRPTRSLB1, YDPTRSLB2=>YDMODEL%YRML_DYN%YRPTRSLB2,       &
& YDRIP=>YDMODEL%YRML_GCONF%YRRIP, YGFL=>YDMODEL%YRML_GCONF%YGFL, YDTCCO=>YDMODEL%YRML_DYN%YYTCCO,                     &
& YDTSCO=>YDMODEL%YRML_DYN%YYTSCO, YDGEOMVARS=>YDVARS%GEOMETRY, YDCST=>YDMODEL%YRCST, YDML_DYN=>YDMODEL%YRML_DYN)

ASSOCIATE(NUMFLDS=>YGFL%NUMFLDS, YCOMP=>YGFL%YCOMP, NFLEVG=>YDDIMV%NFLEVG, LADVF=>YDDYN%LADVF,                  &
& LFINDVSEP=>YDDYN%LFINDVSEP, LSLDP_RK=>YDDYN%LSLDP_RK, LSLDP_XYZ=>YDDYN%LSLDP_XYZ, LRHS_CURV=>YDDYN%LRHS_CURV, &
& NCURRENT_ITER=>YDDYN%NCURRENT_ITER, NSITER=>YDDYN%NSITER, EDELX=>YDEGEO%EDELX, EDELY=>YDEGEO%EDELY,           &
& RTDT=>YDRIP%RTDT, KST=>YDCPG_BNDS%KIDIA, KEND=>YDCPG_BNDS%KFDIA)


I_DPDETA0 = -1
I_UTRAJ   = -1
I_VTRAJ   = -1
I_WTRAJ   = -1
I_LAMBDA0 = -1
I_THETA0  = -1
I_ETA0    = -1

DO JGFL = 1, SIZE (YDVARS%SLDIA)
  IF(TRIM(YDVARS%SLDIA (JGFL)%CNAME) == 'DPDETA0') THEN
    I_DPDETA0 = JGFL
  ELSEIF(TRIM(YDVARS%SLDIA (JGFL)%CNAME) == 'UTRAJ') THEN
    I_UTRAJ = JGFL
  ELSEIF(TRIM(YDVARS%SLDIA (JGFL)%CNAME) == 'VTRAJ') THEN
    I_VTRAJ = JGFL
  ELSEIF(TRIM(YDVARS%SLDIA (JGFL)%CNAME) == 'WTRAJ') THEN
    I_WTRAJ = JGFL
  ELSEIF(TRIM(YDVARS%SLDIA (JGFL)%CNAME) == 'LAMBDA0') THEN
    I_LAMBDA0 = JGFL
  ELSEIF(TRIM(YDVARS%SLDIA (JGFL)%CNAME) == 'THETA0') THEN
    I_THETA0 = JGFL
  ELSEIF(TRIM(YDVARS%SLDIA (JGFL)%CNAME) == 'ETA0') THEN
    I_ETA0 = JGFL
  ENDIF
ENDDO

!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS:
!              ---------------------------

IHVI=0
DO JGFL=1,NUMFLDS
  IF(YCOMP(JGFL)%CSLINT == 'LAIHVT      ' .OR.&
     & YCOMP(JGFL)%CSLINT == 'LAIHVTQM    ' .OR.&
     & YCOMP(JGFL)%CSLINT == 'LAIHVTQMH   ' ) THEN  
    IHVI=1
  ENDIF
ENDDO

IF(YDCPG_OPTS%LRPLANE) THEN

!$ACDC ABORT {

  DO JROF = KST, KEND
    ZGMDTX (JROF)=0.5_JPRB*YDGEOMVARS%GM%P(JROF)*RTDT/EDELX
    ZGMDTY (JROF)=0.5_JPRB*YDGEOMVARS%GM%P(JROF)*RTDT/EDELY
  ENDDO

!$ACDC }

ENDIF

!     ------------------------------------------------------------------

!*       2.    COMPUTATION OF THE SL TRAJECTORY.
!              ---------------------------------

IF (YDCPG_OPTS%LRPLANE) THEN

!$ACDC ABORT {

  IF (LSLDP_RK) THEN
    CALL ABOR1(" LAPINEA : RK4 scheme is not yet available for LAM. ")
    !CALL ELARMES_RK(...)
  ELSEIF (LSLDP_XYZ) THEN
    CALL ABOR1(" LAPINEA : DP scheme in XYZ coordinates is not yet available for LAM. ")
    !CALL ELARMES_XYZ(...)
  ELSE
  CALL ELARMES(YDGEOMETRY,YDVARS%GEOMETRY,YDCST,YDML_DYN,YDRIP,YDCPG_OPTS,YDCPG_BNDS,YDSL,ZGMDTX,ZGMDTY,YDCPG_SL1,YDCPG_SL2,&
   & YDMASK_SL2,YDA_KVSEPC,YDA_KVSEPL,YDA_PSCO%P,ZLEV,YDA_PCCO%P,YDA_PUF%P,YDA_PVF%P,&
   & YDA_KL0%P,YDA_KLH0%P,ILEV,YDA_PLSCAW%P,YDA_PRSCAW%P)  
  ENDIF

!$ACDC }

ELSE

  IF (LSLDP_RK) THEN

!$ACDC ABORT {

    CALL LARMES_RK(YDGEOMETRY,YDVARS%GEOMETRY,YDCST,YDRIP,YDML_DYN,YDCPG_OPTS,YDCPG_BNDS,YDSL,YDCPG_SL1,YDCPG_SL2,&
     & YDMASK_SL2,YDA_KVSEPC,YDA_KVSEPL,YDA_PSCO%P,ZLEV,YDA_PCCO%P,YDA_PUF%P,YDA_PVF%P,ZWF,&
     & YDA_KL0%P,YDA_KLH0%P,ILEV,YDA_PLSCAW%P,YDA_PRSCAW%P)

!$ACDC }

  ELSEIF (LSLDP_XYZ) THEN

!$ACDC ABORT {

    CALL LARMES_XYZ(YDGEOMETRY,YDVARS%GEOMETRY,YDCST,YDRIP,YDML_DYN,YDCPG_OPTS,YDCPG_BNDS,&
      & YDSL,YDCPG_SL1,YDCPG_SL2,YDA_PWRL9%P,&
      & YDMASK_SL2,YDA_KVSEPC,YDA_KVSEPL,YDA_RSAVEDP%P,YDA_PSCO%P,ZLEV,YDA_PCCO%P,YDA_PUF%P,YDA_PVF%P,ZWF,&
      & YDA_KL0%P,YDA_KLH0%P,ILEV)

!$ACDC }

  ELSE
    CALL LARMES(YDGEOMETRY, YDVARS%GEOMETRY, YDCST, YDRIP, YDML_DYN, YDCPG_OPTS, YDCPG_BNDS, YDSL,  &
    & YDCPG_SL1, YDCPG_SL2, YDMASK_SL2, YDA_KVSEPC, YDA_KVSEPL, YDA_PSCO%P, ZLEV, YDA_PCCO%P, &
    & YDA_PUF%P, YDA_PVF%P, ZWF, YDA_KL0%P, YDA_KLH0%P, ILEV, YDA_PLSCAW%P, YDA_PRSCAW%P)
  ENDIF

  ! diagnostics for semi-Lagrangian scheme
  IF( YDDYNA%LSLDIA .AND. ( YDDYNA%LPC_CHEAP.OR.(NCURRENT_ITER == NSITER) )) THEN

!$ACDC ABORT {

    IF (I_UTRAJ > 0) THEN
      YDVARS%SLDIA(I_UTRAJ)%P(KST:KEND,1:NFLEVG)=YDA_PUF%P(KST:KEND,1:NFLEVG)
    ENDIF
      
    IF (I_VTRAJ > 0) THEN
      YDVARS%SLDIA(I_VTRAJ)%P(KST:KEND,1:NFLEVG)=YDA_PVF%P(KST:KEND,1:NFLEVG)
    ENDIF
      
    IF (I_WTRAJ > 0) THEN
      YDVARS%SLDIA(I_WTRAJ)%P(KST:KEND,1:NFLEVG)=ZWF(KST:KEND,1:NFLEVG)
    ENDIF
      
    IF (I_LAMBDA0 > 0) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND     
          YDVARS%SLDIA(I_LAMBDA0)%P(JROF,JLEV)=SIN(YDA_PCCO%P(JROF,JLEV,YDTCCO%M_RLON))
        ENDDO
      ENDDO
    ENDIF
      
    IF (I_THETA0 > 0) THEN
      DO JLEV=1,NFLEVG
        DO JROF=KST,KEND     
          YDVARS%SLDIA(I_THETA0)%P(JROF,JLEV)= SIN(YDA_PCCO%P(JROF,JLEV,YDTCCO%M_RLAT))
        ENDDO
      ENDDO
    ENDIF
      
    IF (I_ETA0 > 0) THEN
      YDVARS%SLDIA(I_ETA0)%P(KST:KEND,1:NFLEVG)=ZLEV(KST:KEND,1:NFLEVG)
    ENDIF

!$ACDC }

  ENDIF

ENDIF

!     ------------------------------------------------------------------

!*       3.    COMPUTATION OF WEIGHTS AND INDICES ARRAYS FOR INTERPOLATIONS.
!              -------------------------------------------------------------

!*       3.1   Full-level Quantities.
!              ---------------------

IF (LRHS_CURV) THEN
  IROT=0
ELSE
  IROT=1
ENDIF
ITIP=2

LLINTV=.FALSE.

!  Warning: ZURL0,ZVRL0,ZZRL0,ZWRL0,ZUF0,ZVF0,ZZF0,ZWF0,ZWFSM are not
!           used in this call to LARCINA.

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=LARCINA {

CALL LARCINA(YDGEOMETRY, YDCST, YDML_DYN, KST, KEND, YDSL, IHVI, LFINDVSEP, YDDYNA%LSLHD, YDDYNA%LSLHDQUAD, &
& LLINTV, ITIP, IROT, YDCPG_OPTS%LRPLANE, YDA_PSCO%P, ZLEV, YDCPG_SL2%KAPPA, YDCPG_SL2%KAPPAT, &
& YDCPG_SL2%KAPPAM, YDCPG_SL2%KAPPAH, YDCPG_SL2%STDDISU, YDCPG_SL2%STDDISV, YDCPG_SL2%STDDISW, &
& ZURL0, ZVRL0, ZZRL0, ZWRL0, YDMASK_SL2%P, YDA_KVSEPC%P, YDA_KVSEPL%P, YDA_PCCO%P, ZUF0, ZVF0, ZZF0, ZWF0, ZWFSM, &
& YDA_KL0%P, YDA_KLH0%P, ILEV, YDA_PLSCAW%P, YDA_PRSCAW%P, IDEP, YDA_KNOWENO%P, YDGEOMVARS%RCOLON%P, YDGEOMVARS%RSILON%P, YDGEOMVARS%GECLO%P,  &
& YDGEOMVARS%GEMU%P, YDGEOMVARS%GESLO%P, YDGEOMVARS%GSQM2%P)

!$ACDC }

IF (YDCPG_OPTS%LRPLANE) THEN

!$ACDC ABORT {

! * Use PSCO (SINLA and COPHI) to transfer origin point coordinates to LAPINEB
!   for "2 Omega vectorial a k" recalculation.
  IF(LADVF) THEN
    YDA_PSCO%P(KST:KEND,1:NFLEVG,YDTSCO%M_SINLA)=YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLON)
    YDA_PSCO%P(KST:KEND,1:NFLEVG,YDTSCO%M_COPHI)=YDA_PCCO%P(KST:KEND,1:NFLEVG,YDTCCO%M_RLAT)
  ENDIF

!$ACDC }

ENDIF

!*       3.2   Half-level Quantities.
!              ---------------------

IF (YDDYNA%LNHDYN.AND.YDDYNA%LGWADV.AND.(.NOT.YDGEOMETRY%YRVERT_GEOM%YRCVER%LVFE_GW)) THEN

!$ACDC ABORT {

  IROT=0
  ITIP=2

  CALL LARCINHA(YDGEOMETRY, YDCST, YDML_DYN, KST, KEND, YDSL, IHVI, ITIP, IROT, YDCPG_OPTS%LRPLANE, &
  & YDA_PSCO%P, YDA_PCCO%P, ZLEV, YDCPG_SL2%KAPPA, YDCPG_SL2%KAPPAT, YDCPG_SL2%KAPPAM, YDCPG_SL2%KAPPAH, &
  & YDCPG_SL2%STDDISU, YDCPG_SL2%STDDISV, YDCPG_SL2%STDDISW, YDMASK_SL2%P, YDA_KL0H%P, YDA_PLSCAWH%P, YDA_PRSCAWH%P, YDGEOMVARS%RCOLON%P, &
  & YDGEOMVARS%RSILON%P, YDGEOMVARS%GECLO%P, YDGEOMVARS%GEMU%P, YDGEOMVARS%GESLO%P, YDGEOMVARS%GSQM2%P)

!$ACDC }

ENDIF

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LAPINEA',1,ZHOOK_HANDLE)

END SUBROUTINE LAPINEA


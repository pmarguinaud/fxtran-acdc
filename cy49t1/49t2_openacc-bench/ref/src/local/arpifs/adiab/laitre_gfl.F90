SUBROUTINE LAITRE_GFL(YDGEOMETRY,YGFL,YDML_DYN,&
 & KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KHVI,KWK,&
 & CDINT,PWENOALPHA,LDMOM,KNOWENO,KL0,PLSCAW,PRSCAW,PXSL,POUT,PXSPSL)

!$ACDC singlecolumn  --process-pointers


! Purpose :
! -------
!   LAITRE_GFL - semi-Lagrangian scheme: origin point interpolations 
!   for GFL variables.

! Interface :
! ---------
!   INPUT:
!     KASLB1     - horizontal dimension of SLBUF1
!     KPROMA     - horizontal dimension
!     KST     - first element of arrays where computations are performed
!     KEND      - depth of work
!     KFLEV      - vertical dimension
!     KFLDN      - number of the first field
!     KFLDX      - number of the last field
!     KHVI       - 1/0: Cubic Hermite vert. interp. are needed/not needed
!     KWK        - set of horizontal non-linear weights to be used.
!     CDINT      - kind of interpolation used
!     PWENOALPHA - value of ALPHA (p) controlling level of non-oscillatory control
!     LDMOM      - momentum like variable versus heat (scalar) type of variable
!     KNOWENO    - special boundary treatment for WENO
!     KL0        - indices of the four western points of the 16 point
!                  interpolation grid
!     PLSCAW     - linear weights (distances) for interpolations.
!     PRSCAW     - non-linear weights for interpolations.
!     PXSL       - quantity to interpolate at O in U-wind eqn
!     PXSPSL     - spline representation of above
!   OUTPUT:
!     POUT       - interpolated quantity at O

! Externals :
! ---------
!   ABOR1
!   LAIHVT
!   LAITVSPCQM
!   LAITRI

! Method :
! ------
!   See documentation.

! Reference :
! ---------

! Author :
! ------
!   ??-Nov-2004, F. Vana (CHMI) and K. YESSAD (Meteo-France)

! Modifications :
! -------------
!   30-06-2008 J. Masek   Dataflow for new SLHD interpolators.
!   17-Sep-2008 F. Vana   Weights driven interp. for LAITVSPCQM and LAIHVT(..) 
!   K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!   K. Yessad Nov 2008: interpolation routines: merge QM with not-QM version.
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   M. Diamantakis (June 2012): add code for quasi-monotone mass fixer 
!   M. Diamantakis (Dec  2012): add code for additional mass fixers
!   S. Malardel (Nov 2013): COMAD weights for SL interpolations
!   F. Vana   13-Feb-2014  Distinguish SLHD for momentum and heat variables.
!   M. Diamantakis (Feb 2014): - introduce new QM interpol limiter
!                              - make code independent on use of mass fixer type
!   M. Diamantakis (Feb 2016): - introduce new LQML3D interpol limiter
!                              - improve efficiency of LQM3D
!   F. Vana  20-Feb-2019  Quintic vertical interpolation
!   M. Diamantakis & F. Vana May-2020: LQM3D for cubic ans quintic interpolation
! End Modifications
!   -----------------------------------------------------------------------
USE YOM_YGFL          , ONLY : TYPE_GFLD
USE GEOMETRY_MOD      , ONLY : GEOMETRY
USE PARKIND1          , ONLY : JPIM, JPRB
USE YOMHOOK           , ONLY : DR_HOOK, JPHOOK, LHOOK
USE MODEL_DYNAMICS_MOD, ONLY : MODEL_DYNAMICS_TYPE
!   -----------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)            ,INTENT(IN)             :: YDGEOMETRY
TYPE(TYPE_GFLD)           ,INTENT(IN)             :: YGFL
TYPE(MODEL_DYNAMICS_TYPE) ,INTENT(IN)             :: YDML_DYN
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KASLB1
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KPROMA 
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KST 
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KEND 
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KFLEV 
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KFLDN 
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KFLDX 
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KHVI 
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KWK
CHARACTER(LEN=12)         ,INTENT(IN)             :: CDINT
REAL(KIND=JPRB)           ,INTENT(IN)             :: PWENOALPHA
LOGICAL                   ,INTENT(IN)             :: LDMOM
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KNOWENO(KPROMA,KFLEV)
INTEGER(KIND=JPIM)        ,INTENT(IN)             :: KL0(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)           ,INTENT(IN)             :: PLSCAW(KPROMA,KFLEV,YDML_DYN%YYTLSCAW%NDIM)
REAL(KIND=JPRB)           ,INTENT(IN)             :: PRSCAW(KPROMA,KFLEV,YDML_DYN%YYTRSCAW%NDIM)
REAL(KIND=JPRB)           ,INTENT(IN)             :: PXSL(KASLB1,KFLDN:KFLDX) 
REAL(KIND=JPRB)           ,INTENT(OUT)            :: POUT(KPROMA,KFLEV) 
REAL(KIND=JPRB)           ,INTENT(IN)  ,OPTIONAL  :: PXSPSL(KASLB1,KFLDN:KFLDX) 

INTEGER(KIND=JPIM) :: IQM
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!   -----------------------------------------------------------------------

#include "abor1.intfb.h"
#include "laihvt.intfb.h"
#include "laitvspcqm.intfb.h"
#include "laitri.intfb.h"
#include "laitriqm3d.intfb.h"
#include "laqmlimiter.intfb.h"
#include "laitri_weno.intfb.h"

!   -----------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAITRE_GFL',0,ZHOOK_HANDLE)

ASSOCIATE(YDTLSCAW=>YDML_DYN%YYTLSCAW,YDTRSCAW=>YDML_DYN%YYTRSCAW,YDDYN=>YDML_DYN%YRDYN, & 
  &       YDVSLETA=>YDML_DYN%YRSLINT%YRVSLETA,YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA, &
  &       LQM3DCONS=>YGFL%LQM3DCONS)
!   -----------------------------------------------------------------------

! 1. Origin point interpolations for GFL variables
!    ---------------------------------------------

!   idem for LAITRWENO/QM/H (quintic, aka WENO) and LAIHVT/QM/H (Hermite vertical cubic)
!   LAIVSPCQM is QM (vertical cubic splines)
IF (CDINT(1:7) == "LAITQMH".OR.CDINT(1:7) == "LAITQML".OR.CDINT(1:9) == "LAIHVTQMH".OR.&
  CDINT == 'LAITRWENOQMH') THEN
  IQM = 1
ELSE IF (CDINT(1:8) == "LAIHVTQM".OR.CDINT(1:7) == 'LAITQM3'.OR.&
 & CDINT == 'LAITRWENOQM') THEN
  IQM = 2
ELSE IF (CDINT(1:6) == "LAITRI".OR.CDINT(1:6) == "LAIHVT".OR.&
  CDINT == 'LAITRWENO') THEN
  IQM = 0
ELSE IF (CDINT == "LAITRWENOQM3") THEN
  IQM = -1
ELSE IF (CDINT(1:6) == "LAITQM") THEN
  IQM = YDML_DYN%YRDYNA%NQMGFL
ELSE IF (CDINT /= 'LAITVSPCQM  ') THEN
  CALL ABOR1("unknown type of QM limiter for GFL interpolation: "//CDINT)
ENDIF

! 1.1 SLHD interpolations
!     -------------------
! NOTE:
!   The SLHD versions of the LAIHVT and LAITVSPCQM interpolations are currently not coded.

IF (CDINT == 'LAITQM(SLD) '.OR.CDINT == 'LAITQMH(SLD)'.OR.CDINT == 'LAITRI(SLD) '.OR.&
 & CDINT == 'LAITQM3D(SLD') THEN
  ! NOTE: If one gets here, then the abort line can be commented out. 
  !       For the moment the GFL setup doesn't know this possibility.
  IF (CDINT == 'LAITQM3D(SLD') THEN
    CALL ABOR1('LAITRE_GFL: LAITQM3D(SLD) IS NOT YET AVAILABLE')
  ENDIF

  IF (LDMOM) THEN
    ! Lagrange cubic polynomial with SLHD (momentum)
    CALL LAITRI(KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,IQM,YDDYN%CLAITRI_TYPE,&
     & PLSCAW(:,:,YDTLSCAW%M_WDLAT),PRSCAW(:,:,YDTRSCAW%M_WCLASLD(KWK)),&
     & PLSCAW(:,:,YDTLSCAW%M_WDLO),PRSCAW(:,:,YDTRSCAW%M_WCLOSLD(KWK)),&
     & KL0,PRSCAW(:,:,YDTRSCAW%M_WVINTWSLD),PXSL,POUT)
  ELSE
    ! Lagrange cubic polynomial with SLHD (heat)
    CALL LAITRI(KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,IQM,YDDYN%CLAITRI_TYPE,&
     & PLSCAW(:,:,YDTLSCAW%M_WDLAT),PRSCAW(:,:,YDTRSCAW%M_WCLASLT),&
     & PLSCAW(:,:,YDTLSCAW%M_WDLO),PRSCAW(:,:,YDTRSCAW%M_WCLOSLT),&
     & KL0,PRSCAW(:,:,YDTRSCAW%M_WVINTWSLT),PXSL,POUT)
  ENDIF

  IF (CDINT == 'LAITQM3D(SLD') THEN
    CALL LAQMLIMITER(YDVSLETA%VRDETAR,LQM3DCONS,KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,&
     & KL0,PXSL,POUT)
  ENDIF

ELSEIF (CDINT == 'LAITQM(MAD) '.OR.CDINT == 'LAITQMH(MAD)'.OR.CDINT=='LAITRI(MAD) ') THEN

! 1.2 COMAD (cubic) interpolations

  ! Lagrange cubic polynomial with COMAD
  CALL LAITRI(KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,IQM,YDDYN%CLAITRI_TYPE,&
   & PLSCAW(:,:,YDTLSCAW%M_WDLAMAD),PRSCAW(:,:,YDTRSCAW%M_WCLAMAD(KWK)),&
   & PLSCAW(:,:,YDTLSCAW%M_WDLOMAD),PRSCAW(:,:,YDTRSCAW%M_WCLOMAD(KWK)),&
   & KL0,PRSCAW(:,:,YDTRSCAW%M_WVINTWMAD),PXSL,POUT)

! 1.3 non-SLHD non-COMAD interpolations
!     -----------------------

ELSEIF (CDINT(1:6) == 'LAIHVT') THEN

  ! Hermite cubic polynomial on the vertical "HVT"
  CALL LAIHVT(KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,IQM,&
   & PLSCAW(:,:,YDTLSCAW%M_WDLAT),PRSCAW(:,:,YDTRSCAW%M_WCLA(KWK)),&
   & PLSCAW(:,:,YDTLSCAW%M_WDLO),PRSCAW(:,:,YDTRSCAW%M_WCLO(KWK)),&
   & KL0,PRSCAW(:,:,YDTRSCAW%M_WVDERW),PRSCAW(:,:,YDTRSCAW%M_WHVW),PXSL,POUT)

ELSEIF (CDINT == 'LAITVSPCQM  ') THEN
  IF (.NOT.PRESENT(PXSPSL))&
   & CALL ABOR1('LAITRE_GFL: MISSING ARGUMENT PXSPSL FOR INTERPOLATION TYPE LAITVSPCQM')

  ! spline "VSPC", quasi-monotonic
  CALL LAITVSPCQM(YDVSLETA,KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,&
   & PLSCAW(:,:,YDTLSCAW%M_WDLAT),PRSCAW(:,:,YDTRSCAW%M_WCLA(KWK)),&
   & PLSCAW(:,:,YDTLSCAW%M_WDLO),PRSCAW(:,:,YDTRSCAW%M_WCLO(KWK)),&
   & KL0,PRSCAW(:,:,YDTRSCAW%M_WVINTWS),PXSPSL,POUT,PXSL)

ELSEIF (CDINT == 'LAITQM3(MAD) '.OR.CDINT == 'LAITQML(MAD)') THEN

  ! Lagrange cubic polynomial, 3D Bermejo & Staniforth or linear limiter
  CALL LAITRIQM3D(YDVETA,LQM3DCONS,KASLB1,KPROMA,KST,KEND,KFLEV,&
    & KFLDN,KFLDX,IQM,PLSCAW(:,:,YDTLSCAW%M_WDLAMAD),PRSCAW(:,:,YDTRSCAW%M_WCLAMAD(KWK)),&
    & PLSCAW(:,:,YDTLSCAW%M_WDLOMAD),PRSCAW(:,:,YDTRSCAW%M_WCLOMAD(KWK)),&
    & KL0,PRSCAW(:,:,YDTRSCAW%M_WVINTWMAD),PLSCAW(:,:,YDTLSCAW%M_WDVERMAD),PXSL,POUT)

ELSEIF (CDINT == 'LAITQM3D    '.OR.CDINT == 'LAITQML3D   ') THEN

  ! Lagrange cubic polynomial, 3D Bermejo & Staniforth or linear limiter
  CALL LAITRIQM3D(YDVETA,LQM3DCONS,KASLB1,KPROMA,KST,KEND,KFLEV,&
    & KFLDN,KFLDX,IQM,PLSCAW(:,:,YDTLSCAW%M_WDLAT),PRSCAW(:,:,YDTRSCAW%M_WCLA(KWK)),&
    & PLSCAW(:,:,YDTLSCAW%M_WDLO),PRSCAW(:,:,YDTRSCAW%M_WCLO(KWK)),&
    & KL0,PRSCAW(:,:,YDTRSCAW%M_WVINTW),PLSCAW(:,:,YDTLSCAW%M_WDVER),PXSL,POUT)

ELSEIF (CDINT(1:6) == 'LAITQM'.OR.CDINT(1:6) == 'LAITRI') THEN

  ! Lagrange cubic polynomial
  CALL LAITRI(KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,IQM,YDDYN%CLAITRI_TYPE,&
    & PLSCAW(:,:,YDTLSCAW%M_WDLAT),PRSCAW(:,:,YDTRSCAW%M_WCLA(KWK)),&
    & PLSCAW(:,:,YDTLSCAW%M_WDLO),PRSCAW(:,:,YDTRSCAW%M_WCLO(KWK)),&
    & KL0,PRSCAW(:,:,YDTRSCAW%M_WVINTW),PXSL,POUT)

ELSEIF (CDINT(1:9) == 'LAITRWENO') THEN

  ! WENO quintic on the vertical
  CALL LAITRI_WENO(YDDYN,KASLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,IQM,YDDYN%CLAITRI_WENO_TYPE,&
   & PLSCAW(:,:,YDTLSCAW%M_WDLAT),PRSCAW(:,:,YDTRSCAW%M_WCLA(KWK)), &
   & PLSCAW(:,:,YDTLSCAW%M_WDLO) ,PRSCAW(:,:,YDTRSCAW%M_WCLO(KWK)), &
   & KL0,KNOWENO,PRSCAW(:,:,YDTRSCAW%M_CW),PRSCAW(:,:,YDTRSCAW%M_WVINTW), &
   & PXSL,POUT,PWENOALPHA,YDVSLETA%VRDETAR,LQM3DCONS)

ELSE
  CALL ABOR1('LAITRE_GFL: UNKNOWN INTERPOLATION TYPE: '//CDINT)
ENDIF

!   -----------------------------------------------------------------------
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LAITRE_GFL',1,ZHOOK_HANDLE)
END SUBROUTINE LAITRE_GFL


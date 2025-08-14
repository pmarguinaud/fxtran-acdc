SUBROUTINE VERDER(KPROMA,KST,KEND,KLEVIN,KLEVOUT,PDERI,PIN,POUT)

!$ACDC singleblock

!**** *VERDER*   Vertical derivative for VFE.

!     Purpose.
!     --------
!          This subroutine computes the vertical derivative (with respect
!          to eta) of a function given at full model
!          levels using a general scheme developed by means of finite-elements

!**   Interface.
!     ----------
!        *CALL* *VERDER(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KPROMA   - horizontal dimension.
!          KSTART   - first element of work.
!          KPROF    - depth of work.
!          KLEVIN   - number of input model layers
!          KLEVOUT  - number of output model layers
!          PDERI    - derivative operator (first or second order)
!          PIN      - Input field

!        OUTPUT:
!          POUT     - d(PIN)/d(eta) or d2(PIN)/d(eta)2  at each model level

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!           none

!     Reference.

!     Author.
!     -------
!         M. Hortal (ECMWF) (after VERINT)

!     Modifications.
!     --------------
!         P. Smolikova and J. Vivoda (Oct 2013): more flexible version
!      F. Vana  05-Mar-2015  Support for single precision
!      F. Vana  14-jan-2020  Always use double precision regardless the model precision
!      M. Diamantakis Feb 2021: SP fix for NH runs
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPRD, JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE OML_MOD   , ONLY : OML_IN_PARALLEL
#ifdef FXTRAN_ACDC
USE FXTRAN_ACDC_GEMM_MOD, ONLY : FXTRAN_ACDC_GEMM
#endif

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM) ,INTENT(IN)           :: KPROMA
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KLEVIN
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KLEVOUT
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KST
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KEND
REAL(KIND=JPRD)    ,INTENT(IN)           :: PDERI(KLEVOUT,KLEVIN)
REAL(KIND=JPRB)    ,INTENT(IN)  ,TARGET  :: PIN(KPROMA,KLEVIN)
REAL(KIND=JPRB)    ,INTENT(OUT) ,TARGET  :: POUT(KPROMA,KLEVOUT)

!     ------------------------------------------------------------------

LOGICAL, PARAMETER :: LLDOUBLE = JPRB == JPRD

LOGICAL, PARAMETER :: LLVERINT_ON_CPU = .TRUE.

INTEGER(KIND=JPIM) ::  JLEV, JROF, JLEVIN, JLEVOUT

REAL(KIND=JPRD), POINTER :: ZIN (:,:)
REAL(KIND=JPRD), POINTER :: ZOUT(:,:)

REAL(KIND=JPRD), TARGET :: ZIN0 (KPROMA, MERGE (KLEVIN,  0, .NOT. LLDOUBLE))
REAL(KIND=JPRD), TARGET :: ZOUT0(KPROMA, MERGE (KLEVOUT, 0, .NOT. LLDOUBLE))

LOGICAL :: LLOML_IN_PARALLEL
LOGICAL :: LLFXTRAN_ACDC_GEMM

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('VERDER',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

IF (LLVERINT_ON_CPU) THEN
  LLOML_IN_PARALLEL = OML_IN_PARALLEL ()
ELSE
  LLOML_IN_PARALLEL = .TRUE.
ENDIF

IF (LLDOUBLE) THEN
#ifndef PARKIND1_SINGLE
  ZOUT => POUT
  ZIN  => PIN
#endif
ELSE
  ZIN  => ZIN0
  ZOUT => ZOUT0

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JROF) IF (.NOT. LLOML_IN_PARALLEL)
  DO JLEV=1,KLEVIN
    DO JROF=KST,KEND
      ZIN(JROF,JLEV)=REAL(PIN(JROF,JLEV),KIND=JPRD)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

ENDIF

LLFXTRAN_ACDC_GEMM = .FALSE.
#ifdef FXTRAN_ACDC
CALL FXTRAN_ACDC_GEMM (KST, KEND, 'N', 'T', KPROMA, KLEVOUT, KLEVIN, 1.0_JPRD, ZIN, &
                     & KPROMA, PDERI, KLEVOUT, 0.0_JPRD, ZOUT, KPROMA, LLFXTRAN_ACDC_GEMM)
#endif

IF (.NOT. LLFXTRAN_ACDC_GEMM) THEN

!$ACDC ABORT {

  CALL DGEMM('N','T',KEND-KST+1,KLEVOUT,KLEVIN, &
       & 1.0_JPRD,ZIN,KPROMA,PDERI,KLEVOUT,0.0_JPRD,ZOUT,KPROMA)  

!$ACDC }

ENDIF

IF (.NOT. LLDOUBLE) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JROF) IF (.NOT. LLOML_IN_PARALLEL)
  DO JLEV=1,KLEVOUT
    DO JROF=KST,KEND
      POUT(JROF,JLEV)=REAL(ZOUT(JROF,JLEV),JPRB)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

ENDIF

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('VERDER',1,ZHOOK_HANDLE)
END SUBROUTINE VERDER


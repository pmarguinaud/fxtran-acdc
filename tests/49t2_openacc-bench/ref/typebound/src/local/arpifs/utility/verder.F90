SUBROUTINE VERDER(KPROMA,KST,KEND,KLEVIN,KLEVOUT,PDERI,PIN,POUT)

!$ACDC singlecolumn --set-variables LLVERINT_ON_CPU=.FALSE.,LLSIMPLE_DGEMM=.TRUE. --no-check-pointers-dims ZIN,ZOUT  --process-pointers


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

LOGICAL, SAVE :: LLSIMPLE_DGEMM = .FALSE.
LOGICAL, PARAMETER :: LLVERINT_ON_CPU = .TRUE.
CHARACTER(LEN=8), SAVE :: CLENV = ''

INTEGER(KIND=JPIM) ::  JLEV, JROF, JLEVIN, JLEVOUT

REAL(KIND=JPRD), POINTER :: ZIN (:,:)
REAL(KIND=JPRD), POINTER :: ZOUT(:,:)

REAL(KIND=JPRD), TARGET :: ZIN0 (KPROMA, MERGE (KLEVIN,  0, .NOT. LLDOUBLE))
REAL(KIND=JPRD), TARGET :: ZOUT0(KPROMA, MERGE (KLEVOUT, 0, .NOT. LLDOUBLE))

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('VERDER',0,ZHOOK_HANDLE)

!     ------------------------------------------------------------------

IF (LLVERINT_ON_CPU) THEN
  IF (CLENV == '') THEN
    CALL GETENV ('LLSIMPLE_DGEMM', CLENV)
    LLSIMPLE_DGEMM = CLENV == '1'
  ENDIF
  CLENV = "done"
ENDIF

IF (LLDOUBLE) THEN
#ifndef PARKIND1_SINGLE
  ZOUT => POUT
  ZIN  => PIN
#endif
ELSE
  ZIN  => ZIN0
  ZOUT => ZOUT0
  DO JLEV=1,KLEVIN
    DO JROF=KST,KEND
      ZIN(JROF,JLEV)=REAL(PIN(JROF,JLEV),KIND=JPRD)
    ENDDO
  ENDDO
ENDIF

IF (LLSIMPLE_DGEMM) THEN

  POUT = 0._JPRB

  DO JLEVIN = 1, KLEVIN
    DO JLEVOUT = 1, KLEVOUT
      DO JROF = KST, KEND
        POUT (JROF,JLEVOUT) = POUT (JROF,JLEVOUT) + PDERI (JLEVOUT,JLEVIN) * PIN (JROF,JLEVIN)
      ENDDO
    ENDDO
  ENDDO

ELSE

  CALL DGEMM('N','T',KEND-KST+1,KLEVOUT,KLEVIN, &
       & 1.0_JPRD,ZIN,KPROMA,PDERI,KLEVOUT,0.0_JPRD,ZOUT,KPROMA)  

ENDIF

IF (.NOT. LLDOUBLE) THEN
  DO JLEV=1,KLEVOUT
    DO JROF=KST,KEND
      POUT(JROF,JLEV)=REAL(ZOUT(JROF,JLEV),JPRB)
    ENDDO
  ENDDO
ENDIF

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('VERDER',1,ZHOOK_HANDLE)
END SUBROUTINE VERDER


SUBROUTINE VERINT(KPROMA,KST,KEND,KLEVIN,KLEVOUT,PINTE,PIN,POUT,KTYPE,KCHUNK,POUTS,PINS)

!**** *VERINT*   Vertical integral

!     Purpose.
!     --------
!          This subroutine computes the vertical integral (with respect
!          to eta) of a function given at full model
!          levels using a general scheme
!          The integral is given either from the top (KTYPE=0) down
!          (or from the bottom (KTYPE=1) up) to each full model level

!**   Interface.
!     ----------
!        *CALL* *VERINT(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KPROMA    - horizontal dimension.
!          KST       - first element of work.
!          KEND      - depth of work.
!          KLEVIN    - vertical dimension for array PIN.
!          KLEVOUT   - vertical dimension for array POUT.
!          PINTE     - matrix operator used to perform vertical integrals.
!          PIN       - Input field
!          KTYPE     - starting point of the integral (0=top, 1=bottom)
!          KCHUNK    - chunking size (to maintain bit reproducibility)

!        OUTPUT:
!                    eta
!                     _
!                    |
!          POUT :    | PIN deta   at each half model level
!                   _|
!                 KTYPE

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
!         M. Hortal (ECMWF)

!     Modifications.
!     --------------
!        Original : MAY 2000
!        D.SALMOND : APRIL 2002 FORTRAN Matrix multiply replace by BLAS routine
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        Y.Seity : MAY 2009 bf for B-Level parallelisation
!        J.Hague : OCT 2012: Parallelise call to DGEMM if not in parallel region
!        P. Smolikova and J. Vivoda (Oct 2013): new options for VFE-NH
!        F. Vana  05-Mar-2015  Support for single precision
!        F. Vana  14-Jan-2020  Exclusive usage of double precision
!        P. Gillies & F. Vana  22-Jan-2020  Bit reproducible chunking
!        H. Petithomme (September 2020): full rewrite for optimisation and new options
!        R. El Khatib 19-Apr-2023 portability fix and cleaning of this mess :-(
!        H. Petithomme (July 2023): new options POUTS/PINS (ie integral and surface)
!     ------------------------------------------------------------------

USE PARKIND1  , ONLY : JPRD, JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMLUN    , ONLY : NULERR
USE OML_MOD   , ONLY : OML_IN_PARALLEL

IMPLICIT NONE

INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KPROMA,KLEVIN,KLEVOUT,KST,KEND,KTYPE,KCHUNK
REAL(KIND=JPRD)    ,INTENT(IN)                     :: PINTE(KLEVOUT,KLEVIN) 
REAL(KIND=JPRB)    ,INTENT(IN)            ,TARGET  :: PIN(KPROMA,KLEVIN)
REAL(KIND=JPRB)    ,INTENT(OUT)           ,TARGET  :: POUT(KPROMA,KLEVOUT-1)
REAL(KIND=JPRD)    ,INTENT(IN)  ,OPTIONAL ,TARGET  :: POUTS(KPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL          :: PINS(KPROMA)

INTEGER(KIND=JPIM) :: JLEN, JLEV, JROF, JLEVOUT, JLEVIN

LOGICAL, PARAMETER :: LLDOUBLE = JPRB == JPRD

LOGICAL, SAVE :: LLSIMPLE_DGEMM = .FALSE.
LOGICAL, PARAMETER :: LLVERINT_ON_CPU = .TRUE.
CHARACTER(LEN=8), SAVE :: CLENV = ''

REAL(KIND=JPRD), POINTER :: ZIN (:,:)
REAL(KIND=JPRD), POINTER :: ZOUT(:,:)

REAL(KIND=JPRD), TARGET :: ZIN0 (KPROMA, MERGE (KLEVIN,    0, .NOT. LLDOUBLE))
REAL(KIND=JPRD), TARGET :: ZOUT0(KPROMA, MERGE (KLEVOUT-1, 0, .NOT. LLDOUBLE))

LOGICAL :: LLOML_IN_PARALLEL

#include "abor1.intfb.h"

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE_XGEMM
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('VERINT',0,ZHOOK_HANDLE)

IF (LLVERINT_ON_CPU) THEN
  IF (CLENV == '') THEN
    CALL GETENV ('LLSIMPLE_DGEMM', CLENV)
    LLSIMPLE_DGEMM = CLENV == '1'
  ENDIF
  CLENV = "done"
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
  DO JLEV=1,KLEVIN
    DO JROF=KST,KEND
      ZIN(JROF,JLEV)=REAL(PIN(JROF,JLEV),KIND=JPRD)
    ENDDO
  ENDDO
ENDIF

IF (LLSIMPLE_DGEMM) THEN

  ZOUT = 0._JPRD
  DO JLEVIN = 1, KLEVIN
    DO JLEVOUT = 1, KLEVOUT-1
      DO JROF = KST, KEND
        ZOUT (JROF, JLEVOUT) = ZOUT (JROF, JLEVOUT) + PINTE (JLEVOUT, JLEVIN) * ZIN (JROF, JLEVIN)
      ENDDO
    ENDDO
  ENDDO

ELSE

! note: mind the DGEMM looping on KLEVOUT-1 levels, letting old/new computation of integral
  IF (LLOML_IN_PARALLEL) THEN
    IF (LHOOK) CALL DR_HOOK('VERINT_DGEMM_1',0,ZHOOK_HANDLE_XGEMM)
    CALL DGEMM('N','T',KEND-KST+1,KLEVOUT-1,KLEVIN,1.0_JPRD,ZIN(KST,1),KPROMA,PINTE,&
     & KLEVOUT,0.0_JPRD,ZOUT(KST,1),KPROMA)
    IF (LHOOK) CALL DR_HOOK('VERINT_DGEMM_1',1,ZHOOK_HANDLE_XGEMM)

  ELSE
    IF (LHOOK) CALL DR_HOOK('VERINT_DGEMM_2',0,ZHOOK_HANDLE_XGEMM)
!$OMP PARALLEL DO PRIVATE(JROF,JLEN)
    DO JROF=KST,KEND,KCHUNK
      JLEN=MIN(KCHUNK,KEND-JROF+1)
      CALL DGEMM('N','T',JLEN,KLEVOUT-1,KLEVIN,1.0_JPRD,ZIN(JROF,1),KPROMA,PINTE,KLEVOUT,&
       & 0.0_JPRD,ZOUT(JROF,1),KPROMA)
    ENDDO
!$OMP END PARALLEL DO
    IF (LHOOK) CALL DR_HOOK('VERINT_DGEMM_2',1,ZHOOK_HANDLE_XGEMM)
  ENDIF
ENDIF

IF(KTYPE == 1) THEN
  ! note 1: POUTS is the integral, present in POUTS (always present for type 1)
  !  integral is not computed in verint anymore (KLEVOUT-1), but in verints (cf verdisint)
  !  last level substraction should be useless, hence letting POUTS to be IN (not INOUT)
  ! note 2: PINS is the surface component (ie phis or gws)
  !  using PINS suppresses the need for 3D adding of this surface component in user code

  ! warning: dependence on last level in OMP case, last level must be done separately
  ! however, it is not done anymore because it is useless (-> looping on
  ! KLEVOUT-1)
  IF (PRESENT(PINS)) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JROF) IF (.NOT. LLOML_IN_PARALLEL)
    DO JLEV=1,KLEVOUT-1
      DO JROF=KST,KEND
        IF (.NOT. LLDOUBLE) THEN
          POUT(JROF,JLEV)=REAL(ZOUT(JROF,JLEV)-POUTS(JROF),JPRB)+PINS(JROF)
        ELSE
          POUT(JROF,JLEV)=ZOUT(JROF,JLEV)-POUTS(JROF)+PINS(JROF)
        ENDIF
      ENDDO
    ENDDO
!$OMP END PARALLEL DO
  ELSE
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JROF) IF (.NOT. LLOML_IN_PARALLEL)
    DO JLEV=1,KLEVOUT-1
      DO JROF=KST,KEND
       POUT(JROF,JLEV)=ZOUT(JROF,JLEV)-POUTS(JROF)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO
  ENDIF

  ! computing last level is useless
  !POUTS(KST:KEND)=0._JPRB
ELSEIF (KTYPE /= 0) THEN
  WRITE(NULERR,*) ' INVALID KTYPE IN VERINT =',KTYPE
  CALL ABOR1(' VERINT: ABOR1 CALLED')
ELSEIF (PRESENT(PINS)) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JROF) IF (.NOT. LLOML_IN_PARALLEL)
  DO JLEV=1,KLEVOUT-1
    DO JROF=KST,KEND
      POUT(JROF,JLEV) = ZOUT(JROF,JLEV)+PINS(JROF)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
ELSEIF (.NOT. LLDOUBLE) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JROF) IF (.NOT. LLOML_IN_PARALLEL)
  DO JLEV=1,KLEVOUT-1
    DO JROF=KST,KEND
      POUT(JROF,JLEV) = REAL(ZOUT(JROF,JLEV),KIND=JPRB)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
ENDIF

IF (LHOOK) CALL DR_HOOK('VERINT',1,ZHOOK_HANDLE)

END SUBROUTINE VERINT



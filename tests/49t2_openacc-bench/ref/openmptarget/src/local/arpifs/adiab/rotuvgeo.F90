SUBROUTINE ROTUVGEO(KFLEV,KPROMA,KST,KEND,PGNORDM,PGNORDL,PU,PV,PRU,PRV,LDIR,LDADD,LDSUB)

!$ACDC singlecolumn  --process-pointers

! Purpose:
!  rotate U/V wind between geographical North and computation grid
!
! Input:
!  PU/PV: U/V wind components
!  LDIR: switch for direct or back rotation
!  LDADD: option for adding rot(U/V) wind to PRU/PRV wind
!  LDSUB: option for substracting rot(U/V) wind to PRU/PRV wind
!
! Output:
!  PRU/PRV: U/V wind components rotated to/from geographical North
!
! Author:
!  H Petithomme, Sep 2023

USE PARKIND1, ONLY: JPIM, JPRB
USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM) ,INTENT(IN)               :: KFLEV,KPROMA,KST,KEND
LOGICAL            ,INTENT(IN)               :: LDIR
LOGICAL            ,INTENT(IN)    ,OPTIONAL  :: LDADD,LDSUB
REAL(KIND=JPRB)    ,INTENT(IN)               :: PGNORDM(KPROMA),PGNORDL(KPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)               :: PU(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)               :: PV(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(INOUT)            :: PRU(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(INOUT)            :: PRV(KPROMA,KFLEV)

INTEGER(KIND=JPIM) :: JROF,JLEV
LOGICAL :: LLADD,LLSUB
REAL(KIND=JPRB) :: ZU,ZV,ZGNORDM(KPROMA),ZGNORDL(KPROMA)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "rotate.func.h"

IF (LHOOK) CALL DR_HOOK("ROTUVGEO",0,ZHOOK_HANDLE)

! note: LDSUB has precedence over LDADD (both true means substraction)
! adding is U/V+rot(U/V), substracting is U/V+rot(-U/-V), other is 0+rot(U/V)
LLADD = .FALSE.
IF (PRESENT(LDADD)) LLADD = LDADD
LLSUB = .FALSE.
IF (PRESENT(LDSUB)) LLSUB = LDSUB

IF (LLSUB) THEN
  ZGNORDM(:) = -PGNORDM(:)
  ZGNORDL(:) = -PGNORDL(:)
ELSE
  ZGNORDM(:) = PGNORDM(:)
  ZGNORDL(:) = PGNORDL(:)
ENDIF

LLADD = LLADD.OR.LLSUB

! warning: ZU/ZV are ALWAYS needed to protect against aliasing (U/V and RU/RV)
! do not merge add and "not add" functions because PRU/PRV would have to be zeroed
! (ie 0*PRU/PRV doesn't account for NaN)
IF (LDIR) THEN
  IF (LLADD) THEN
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZU = PU(JROF,JLEV)
        ZV = PV(JROF,JLEV)
        PRU(JROF,JLEV) = FROTUADD(PRU(JROF,JLEV),ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
        PRV(JROF,JLEV) = FROTVADD(PRV(JROF,JLEV),ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
      END DO
    END DO
  ELSE
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZU = PU(JROF,JLEV)
        ZV = PV(JROF,JLEV)
        PRU(JROF,JLEV) = FROTU(ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
        PRV(JROF,JLEV) = FROTV(ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
      END DO
    END DO
  ENDIF
ELSE
  IF (LLADD) THEN
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZU = PU(JROF,JLEV)
        ZV = PV(JROF,JLEV)
        PRU(JROF,JLEV) = FUNROTUADD(PRU(JROF,JLEV),ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
        PRV(JROF,JLEV) = FUNROTVADD(PRV(JROF,JLEV),ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
      END DO
    END DO
  ELSE
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZU = PU(JROF,JLEV)
        ZV = PV(JROF,JLEV)
        PRU(JROF,JLEV) = FUNROTU(ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
        PRV(JROF,JLEV) = FUNROTV(ZU,ZV,ZGNORDM(JROF),ZGNORDL(JROF))
      END DO
    END DO
  ENDIF
ENDIF

IF (LHOOK) CALL DR_HOOK("ROTUVGEO",1,ZHOOK_HANDLE)
END SUBROUTINE ROTUVGEO


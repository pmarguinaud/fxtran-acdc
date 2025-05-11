!option! -O extendreorder
SUBROUTINE LAITRI_SCALAR(KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,&
 & PDLAT,PCLA,PDLO,PCLO,KL0,PVINTW,PXSL,PXF)

!$ACDC singlecolumn --dummy  --process-pointers


USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK, JPHOOK

!------------------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KSLB1 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KPROMA 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KFLEV 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KFLDN 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KFLDX 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KST 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KEND 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KQM
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KL0(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PDLO(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PCLO(KPROMA,KFLEV,3,2)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PVINTW(KPROMA,KFLEV,3)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PXSL(KSLB1*(KFLDX-KFLDN+1))
REAL(KIND=JPRB)    ,INTENT(OUT)                    :: PXF(KPROMA,KFLEV)

!------------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV,JROF,IQM,INX,IL1,IL2,IL3
LOGICAL :: LQMH,LQMV,LQM0
REAL(KIND=JPRB) :: Z0(KPROMA), Z1(KPROMA), Z2(KPROMA), Z3(KPROMA)
REAL(KIND=JPRB) :: ZXF(KPROMA,KFLEV)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "linearlev.intfb.h"
#include "cubiclev.intfb.h"
#include "cubiclev2.intfb.h"
#include "lainterpol.func.h"

!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAITRI_SCALAR',0,ZHOOK_HANDLE)

! LQM options:
! - LQMV: px values bound to 2 central pxsl values in vertical interp
! - LQMH: px values bound to 2 central pxsl values in horizontal (lat/long) interps
! - LQM0: px values bound to 0 at lowest, after all 3 unlimited interps

! note: KQM can be negative (different meaning, further options)
IQM = ABS(KQM)
LQMH = IQM == 1.OR.IQM == 2
LQMV = IQM == 2.OR.IQM == 3
LQM0 = IQM >= 4

! offsets for getting values on interpolation stencil
IL1=1+KSLB1
IL2=IL1+KSLB1
IL3=IL2+KSLB1

INX = KSLB1*(KFLDX-KFLDN+1)

! 32-point interpolations
DO JLEV=1,KFLEV
  ! optim: prefetch helps only when precisely targeted
#ifdef __INTEL_COMPILER
  DO JROF=KST,KEND,4
    ! Uniques prefetches for the loops to L2
    CALL MM_PREFETCH(PXSL(KL0(JROF,JLEV,1)),2)
    CALL MM_PREFETCH(PXSL(KL0(JROF,JLEV,2)),2)
    CALL MM_PREFETCH(PXSL(IL3+KL0(JROF,JLEV,1)),2)
    CALL MM_PREFETCH(PXSL(IL3+KL0(JROF,JLEV,2)),2)
    CALL MM_PREFETCH(PXSL(IL1+KL0(JROF,JLEV,0)),2)
    !CALL MM_PREFETCH(PXSL(IL1+KL0(JROF,JLEV,1)),2)
    !CALL MM_PREFETCH(PXSL(IL1+KL0(JROF,JLEV,2)),2)
    CALL MM_PREFETCH(PXSL(IL1+KL0(JROF,JLEV,3)),2)
    CALL MM_PREFETCH(PXSL(IL2+KL0(JROF,JLEV,0)),2)
    CALL MM_PREFETCH(PXSL(IL2+KL0(JROF,JLEV,1)),2)
    CALL MM_PREFETCH(PXSL(IL2+KL0(JROF,JLEV,2)),2)
    CALL MM_PREFETCH(PXSL(IL2+KL0(JROF,JLEV,3)),2)
  ENDDO
#endif

  CALL LINEARLEV(KFLEV,KPROMA,JLEV,KST,KEND,KL0(:,:,1:2),PDLO(:,:,1:2),PDLAT,PXSL,INX,Z0)
  CALL CUBICLEV(LQMH,KFLEV,KPROMA,JLEV,KST,KEND,KL0,PDLO,PCLO,PCLA,PXSL(IL1:),INX,Z1)
  CALL CUBICLEV(LQMH,KFLEV,KPROMA,JLEV,KST,KEND,KL0,PDLO,PCLO,PCLA,PXSL(IL2:),INX,Z2)
  CALL LINEARLEV(KFLEV,KPROMA,JLEV,KST,KEND,KL0(:,:,1:2),PDLO(:,:,1:2),PDLAT,PXSL(IL3:),&
    &INX,Z3)

  CALL CUBICLEV2(LQMV,KFLEV,KPROMA,JLEV,KST,KEND,PVINTW,Z0,Z1,Z2,Z3,PXF(:,JLEV))

  IF (LQM0) THEN
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = FMAXJ(PXF(JROF,JLEV),0._JPRB)
    ENDDO
  ENDIF
ENDDO

IF (LHOOK) CALL DR_HOOK('LAITRI_SCALAR',1,ZHOOK_HANDLE)

END SUBROUTINE LAITRI_SCALAR


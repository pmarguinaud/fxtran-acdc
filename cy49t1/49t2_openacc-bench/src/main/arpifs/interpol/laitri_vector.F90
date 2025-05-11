!option! -O extendreorder
SUBROUTINE LAITRI_VECTOR(KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,&
 & PDLAT,PCLA,PDLO,PCLO,KL0,PVINTW,PXSL,PXF)

!$ACDC singlecolumn  --process-pointers


USE PARKIND1 , ONLY : JPIM     ,JPRB     ,JPIB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK, JPHOOK

!------------------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM) ,INTENT(IN)             :: KSLB1 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KPROMA 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KFLEV 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KFLDN 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KFLDX 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KST 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KEND 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KQM
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KL0(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PDLO(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PCLO(KPROMA,KFLEV,3,2)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PVINTW(KPROMA,KFLEV,3)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PXSL(KSLB1*(KFLDX-KFLDN+1))
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PXF(KPROMA,KFLEV)

!------------------------------------------------------------------------------

INTEGER(KIND=JPIB) :: IV0L1, IV0L2, IV1L0, IV1L1, IV1L2, IV1L3, &
 & IV2L0, IV2L1, IV2L2, IV2L3, IV3L1, IV3L2
INTEGER(KIND=JPIM) :: JL,JROF,IQM
LOGICAL :: LQMH,LQMV,LQM0
REAL(KIND=JPRB) :: ZZF111, ZZF112, ZZF121, ZZF122
REAL(KIND=JPRB) :: ZZF211, ZZF212, ZZF221, ZZF222
REAL(KIND=JPRB) :: ZZ0, ZZ1, ZZ2, ZZ3
REAL(KIND=JPRB) :: ZZ01, ZZ02, ZZ10, ZZ11, ZZ12, ZZ13
REAL(KIND=JPRB) :: ZZ20, ZZ21, ZZ22, ZZ23, ZZ31, ZZ32

REAL(KIND=JPRB) :: ZMIN, ZMAX

#include "lainterpol.func.h"

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAITRI_VECTOR',0,ZHOOK_HANDLE)

! LQM options:
! - LQMV: px values bound to 2 central pxsl values in vertical interp
! - LQMH: px values bound to 2 central pxsl values in horizontal (lat/long) interps
! - LQM0: px values bound to 0 at lowest, after all 3 unlimited interps

! note: KQM can be negative (different meaning, further options)
IQM = ABS(KQM)
LQMH = IQM == 1.OR.IQM == 2
LQMV = IQM == 2.OR.IQM == 3
LQM0 = IQM >= 4

! 1. Interpolations
!    --------------

! offsets for getting values on interpolation stencil
IV0L1=1
IV1L1=1+KSLB1
IV2L1=IV1L1+KSLB1
IV3L1=IV2L1+KSLB1

! Warning: Always make sure that compiler is moving
!          the IF (KQM) blocks outside the loops.
IV0L2=2
IV1L0=  KSLB1
IV1L2=2+KSLB1
IV1L3=3+KSLB1
IV2L0=IV1L0+KSLB1
IV2L2=IV1L2+KSLB1
IV2L3=IV1L3+KSLB1
IV3L2=IV2L2+KSLB1

! 32-point interpolations
DO JL=1,KFLEV

  ! interpolations in longitude
!CDIR NODEP
!cdir gthreorder
!NEC$ ivdep
!NEC$ gather_reorder
!DIR$ PREFERVECTOR
  DO JROF=KST,KEND
    ! interpolations in longitude, stencil level 0
    ZZ10=PXSL(KL0(JROF,JL,1)+IV0L1)+PDLO(JROF,JL,1) &
     & *(PXSL(KL0(JROF,JL,1)+IV0L2)-PXSL(KL0(JROF,JL,1)+IV0L1))
    ZZ20=PXSL(KL0(JROF,JL,2)+IV0L1)+PDLO(JROF,JL,2) &
     & *(PXSL(KL0(JROF,JL,2)+IV0L2)-PXSL(KL0(JROF,JL,2)+IV0L1))
    ! interpolations in longitude, stencil level 1
    ZZ01=PXSL(KL0(JROF,JL,0)+IV1L1)+PDLO(JROF,JL,0) &
     & *(PXSL(KL0(JROF,JL,0)+IV1L2)-PXSL(KL0(JROF,JL,0)+IV1L1))
    ZZF111=PXSL(KL0(JROF,JL,1)+IV1L1)
    ZZF211=PXSL(KL0(JROF,JL,1)+IV1L2)
    ZZ11=PXSL(KL0(JROF,JL,1)+IV1L0) &
     & +PCLO(JROF,JL,1,1)*(ZZF111-PXSL(KL0(JROF,JL,1)+IV1L0)) &
     & +PCLO(JROF,JL,2,1)*(ZZF211-PXSL(KL0(JROF,JL,1)+IV1L0)) &
     & +PCLO(JROF,JL,3,1)* &
     & (PXSL(KL0(JROF,JL,1)+IV1L3)-PXSL(KL0(JROF,JL,1)+IV1L0))
    ZZF121=PXSL(KL0(JROF,JL,2)+IV1L1)
    ZZF221=PXSL(KL0(JROF,JL,2)+IV1L2)
    ZZ21=PXSL(KL0(JROF,JL,2)+IV1L0) &
     & +PCLO(JROF,JL,1,2)*(ZZF121-PXSL(KL0(JROF,JL,2)+IV1L0)) &
     & +PCLO(JROF,JL,2,2)*(ZZF221-PXSL(KL0(JROF,JL,2)+IV1L0)) &
     & +PCLO(JROF,JL,3,2)* &
     & (PXSL(KL0(JROF,JL,2)+IV1L3)-PXSL(KL0(JROF,JL,2)+IV1L0))
    ZZ31=PXSL(KL0(JROF,JL,3)+IV1L1)+PDLO(JROF,JL,3) &
     & *(PXSL(KL0(JROF,JL,3)+IV1L2)-PXSL(KL0(JROF,JL,3)+IV1L1))
    ! interpolations in longitude, stencil level 2
    ZZ02=PXSL(KL0(JROF,JL,0)+IV2L1)+PDLO(JROF,JL,0) &
     & *(PXSL(KL0(JROF,JL,0)+IV2L2)-PXSL(KL0(JROF,JL,0)+IV2L1))
    ZZF112=PXSL(KL0(JROF,JL,1)+IV2L1)
    ZZF212=PXSL(KL0(JROF,JL,1)+IV2L2)
    ZZ12=PXSL(KL0(JROF,JL,1)+IV2L0) &
     & +PCLO(JROF,JL,1,1)*(ZZF112-PXSL(KL0(JROF,JL,1)+IV2L0)) &
     & +PCLO(JROF,JL,2,1)*(ZZF212-PXSL(KL0(JROF,JL,1)+IV2L0)) &
     & +PCLO(JROF,JL,3,1)* &
     & (PXSL(KL0(JROF,JL,1)+IV2L3)-PXSL(KL0(JROF,JL,1)+IV2L0))
    ZZF122=PXSL(KL0(JROF,JL,2)+IV2L1)
    ZZF222=PXSL(KL0(JROF,JL,2)+IV2L2)
    ZZ22=PXSL(KL0(JROF,JL,2)+IV2L0) &
     & +PCLO(JROF,JL,1,2)*(ZZF122-PXSL(KL0(JROF,JL,2)+IV2L0)) &
     & +PCLO(JROF,JL,2,2)*(ZZF222-PXSL(KL0(JROF,JL,2)+IV2L0)) &
     & +PCLO(JROF,JL,3,2)* &
     & (PXSL(KL0(JROF,JL,2)+IV2L3)-PXSL(KL0(JROF,JL,2)+IV2L0))
    ZZ32=PXSL(KL0(JROF,JL,3)+IV2L1)+PDLO(JROF,JL,3) &
     & *(PXSL(KL0(JROF,JL,3)+IV2L2)-PXSL(KL0(JROF,JL,3)+IV2L1))
    ! interpolations in longitude, stencil level 3
    ZZ13=PXSL(KL0(JROF,JL,1)+IV3L1)+PDLO(JROF,JL,1) &
     & *(PXSL(KL0(JROF,JL,1)+IV3L2)-PXSL(KL0(JROF,JL,1)+IV3L1))
    ZZ23=PXSL(KL0(JROF,JL,2)+IV3L1)+PDLO(JROF,JL,2) &
     & *(PXSL(KL0(JROF,JL,2)+IV3L2)-PXSL(KL0(JROF,JL,2)+IV3L1))

    IF (LQMH) THEN
      ! bound ZZ11
      ZMIN=FMINJ(ZZF111,ZZF211)
      ZMAX=FMAXJ(ZZF111,ZZF211)
      ZZ11=FMAXJ(ZMIN,FMINJ(ZMAX,ZZ11))
      ! bound ZZ21
      ZMIN=FMINJ(ZZF121,ZZF221)
      ZMAX=FMAXJ(ZZF121,ZZF221)
      ZZ21=FMAXJ(ZMIN,FMINJ(ZMAX,ZZ21))
      ! bound ZZ12
      ZMIN=FMINJ(ZZF112,ZZF212)
      ZMAX=FMAXJ(ZZF112,ZZF212)
      ZZ12=FMAXJ(ZMIN,FMINJ(ZMAX,ZZ12))
      ! bound ZZ22
      ZMIN=FMINJ(ZZF122,ZZF222)
      ZMAX=FMAXJ(ZZF122,ZZF222)
      ZZ22=FMAXJ(ZMIN,FMINJ(ZMAX,ZZ22))
    ENDIF

    ! interpolations in latitude, stencil levels 0, 1, 2, 3
    ZZ0=ZZ10+PDLAT(JROF,JL)*(ZZ20-ZZ10)
    ZZ1=ZZ01+PCLA(JROF,JL,1)*(ZZ11-ZZ01) &
     & +PCLA(JROF,JL,2)*(ZZ21-ZZ01) &
     & +PCLA(JROF,JL,3)*(ZZ31-ZZ01)
    ZZ2=ZZ02+PCLA(JROF,JL,1)*(ZZ12-ZZ02) &
     & +PCLA(JROF,JL,2)*(ZZ22-ZZ02) &
     & +PCLA(JROF,JL,3)*(ZZ32-ZZ02)
    ZZ3=ZZ13+PDLAT(JROF,JL)*(ZZ23-ZZ13)
    IF (LQMH) THEN
      ! bound Z1:
      ZMIN=FMINJ(ZZ11,ZZ21)
      ZMAX=FMAXJ(ZZ11,ZZ21)
      ZZ1=FMAXJ(ZMIN,FMINJ(ZMAX,ZZ1))
      ! bound Z2:
      ZMIN=FMINJ(ZZ12,ZZ22)
      ZMAX=FMAXJ(ZZ12,ZZ22)
      ZZ2=FMAXJ(ZMIN,FMINJ(ZMAX,ZZ2))
    ENDIF
    ! final interpolation in vertical
    PXF(JROF,JL)=ZZ0 &
     & +PVINTW(JROF,JL,1)*(ZZ1-ZZ0) &
     & +PVINTW(JROF,JL,2)*(ZZ2-ZZ0) &
     & +PVINTW(JROF,JL,3)*(ZZ3-ZZ0)  
    IF (LQMV) THEN
      ! bound PXF:
      ZMIN=FMINJ(ZZ1,ZZ2)
      ZMAX=FMAXJ(ZZ1,ZZ2)
      PXF(JROF,JL)=FMAXJ(ZMIN,FMINJ(ZMAX,PXF(JROF,JL)))
    ENDIF
  ENDDO

  IF (LQM0) THEN
    DO JROF=KST,KEND
      PXF(JROF,JL) = FMAXJ(PXF(JROF,JL),0._JPRB)
    ENDDO
  ENDIF
ENDDO

!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAITRI_VECTOR',1,ZHOOK_HANDLE)

END SUBROUTINE LAITRI_VECTOR


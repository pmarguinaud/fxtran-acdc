!option! -O extendreorder
SUBROUTINE LAITRIQM3D(YDVETA,LDQM3DCONS,KSLB1,KPROMA,KST,KEND,KFLEV, &
 & KFLDN,KFLDX,KQM,PDLAT,PCLA,PDLO,PCLO,KL0,PVINTW,PDVER,PXSL,PXF)

!$ACDC singlecolumn  --process-pointers


! Purpose :
! -------
!   LAITRIQM3D - semi-Lagrangian scheme: tri-dimensional 32-point
!   interpolations with optional 3d quasi-monotonic limiters built in.
!   KQM=1 is a combined ecmwf quasi-cubic/linear scheme which is by 
!   construction quasi-monotone while KQM=2 is the standard 
!   Bermejo & Staniforth applied in 3D. 
!   KQM=2 is equivavalent to calling LAITRI(KQM=0) with
!   and then LAQMLITER() but slightly more efficient here.
!   Differs form the standard limiter KQM=2 in laitri as it is 3d
!   limiter applied only once at the end of all 1-d interpolations. 
!    

! Interface :
! ---------
!   INPUT:
!     YDVETA - defining the vertical coordinate: eta
!     LDQM3DCONS - Bermejo & Staniforth quasi-monotone limiter, see type_gfld in yom_ygfl 
!     KSLB1   - horizontal dimension for grid-point quantities
!     KPROMA  - horizontal dimension for interpolation point quantities
!     KST     - first element of arrays where computations are performed
!     KEND   - depth of work
!     KFLEV   - vertical dimension
!     KFLDN   - number of the first field
!     KFLDX   - number of the last field
!     KQM     - type of limiter
!               1: standard Bermejo-Staniforth
!               2: use linear interpolant which is qm by definition
!     PDLAT   - distance for horizontal linear interpolations in latitude
!     PCLA    - weights for horizontal cubic interpolations in latitude
!     PDLO    - distances for horizontal linear interpolations
!               in longitude (latitude rows 0, 1, 2, 3)
!     PCLO    - weights for horizontal cubic interpolations in longitude 
!               (latitude rows 1, 2)
!     KL0     - indices of the four western points of the 16 point
!               interpolation grid
!     PVINTW  - weights for cubic vertical interpolation
!     PDVER   - weights (distances) for vertical linear interpolation
!                    on a same vertical.
!     PXSL    - quantity to be interpolated
!   OUTPUT:
!     PXF     - interpolated variable

! Externals :
! ---------
!   None.

! Method :
! ------
!   cubic interpolation with embedded 3D quasi-monotone limiter. Two oprions are available:
!     - LQM3D limiter contained in LAQMLIMITER() but coded in a more efficient way
!     - LQML3D limiter i.e. use the linear interpolant when a new min/max generated    

! Reference :
! ---------

! Author :
! ------
! Michail Diamantakis

! Modifications :
! -------------
! Original : Feb-2016
! O. Marsden April 2017 : remove dependency on YOM_YGFL by passing in LDQM3DCONS as argument
! End modifications
!------------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMVERT  , ONLY : TVETA

!------------------------------------------------------------------------------

IMPLICIT NONE

TYPE(TVETA)       ,INTENT(IN)  :: YDVETA
LOGICAL           ,INTENT(IN)  :: LDQM3DCONS
INTEGER(KIND=JPIM),INTENT(IN)  :: KSLB1 
INTEGER(KIND=JPIM),INTENT(IN)  :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)  :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)  :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)  :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)  :: KST 
INTEGER(KIND=JPIM),INTENT(IN)  :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)  :: KQM
REAL(KIND=JPRB)   ,INTENT(IN)  :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)  :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(IN)  :: PDLO(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(IN)  :: PCLO(KPROMA,KFLEV,3,2)
INTEGER(KIND=JPIM),INTENT(IN)  :: KL0(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(IN)  :: PVINTW(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(IN)  :: PDVER(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)  :: PXSL(KSLB1*(KFLDX-KFLDN+1))
REAL(KIND=JPRB)   ,INTENT(OUT) :: PXF(KPROMA,KFLEV)

!------------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF, IQM, INX,IL1,IL2,IL3
REAL(KIND=JPRB) :: Z01,Z10,Z11,Z12,Z20,Z21,Z22,Z31
REAL(KIND=JPRB) :: ZSURPL(KPROMA)
REAL(KIND=JPRB) :: ZMIN, ZMAX, ZDMAX, ZDMIN, ZXF
REAL(KIND=JPRB) :: ZINF, ZINFLO1, ZINFLO2, ZSUP, ZSUPLO1, ZSUPLO2 
REAL(KIND=JPRB) :: Z0(KPROMA), Z1(KPROMA), Z2(KPROMA), Z3(KPROMA)
REAL(KIND=JPRB) :: ZN1(KPROMA), ZX1(KPROMA), ZN2(KPROMA), ZX2(KPROMA)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "interp.func.h"

!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAITRIQM3D',0,ZHOOK_HANDLE)

! 1. Interpolations
!    --------------

IL1=KSLB1
IL2=IL1+KSLB1
IL3=IL2+KSLB1

! 8 point quasi-monotone correction with vertical adjustment to improve conservation
IF (LDQM3DCONS.AND.(KQM==2)) THEN
  IQM=3
  ZSURPL(:)=0.0_JPRB
ELSE
  IQM=KQM
ENDIF

INX = KSLB1*(KFLDX-KFLDN+1)

! 32-point interpolations
DO JLEV=1,KFLEV
  ! interpolations on levels 0 and 3 (linear)
  !DIR$ IVDEP
  !NEC$ IVDEP
  DO JROF=KST,KEND
    Z11 = PXSL(KL0(JROF,JLEV,1)+1)
    Z11 = FDWEIGHT(PDLO(JROF,JLEV,1),Z11,PXSL(KL0(JROF,JLEV,1)+2))

    Z12 = PXSL(KL0(JROF,JLEV,2)+1)
    Z12 = FDWEIGHT(PDLO(JROF,JLEV,2),Z12,PXSL(KL0(JROF,JLEV,2)+2))

    Z0(JROF) = FDWEIGHT(PDLAT(JROF,JLEV),Z11,Z12)

    Z21 = PXSL(IL3+KL0(JROF,JLEV,1)+1)
    Z21 = FDWEIGHT(PDLO(JROF,JLEV,1),Z21,PXSL(IL3+KL0(JROF,JLEV,1)+2))

    Z22 = PXSL(IL3+KL0(JROF,JLEV,2)+1)
    Z22 = FDWEIGHT(PDLO(JROF,JLEV,2),Z22,PXSL(IL3+KL0(JROF,JLEV,2)+2))

    Z3(JROF) = FDWEIGHT(PDLAT(JROF,JLEV),Z21,Z22)
  ENDDO

  ! interpolations on levels 1 and 2 (cubic)
  !DIR$ IVDEP
  !NEC$ IVDEP
  DO JROF=KST,KEND
    ! level 1
    Z01 = PXSL(IL1+KL0(JROF,JLEV,0)+1)
    Z01 = FDWEIGHT(PDLO(JROF,JLEV,0),Z01,PXSL(IL1+KL0(JROF,JLEV,0)+2))

    Z10 = PXSL(IL1+KL0(JROF,JLEV,1))
    Z11 = PXSL(IL1+KL0(JROF,JLEV,1)+1)
    Z12 = PXSL(IL1+KL0(JROF,JLEV,1)+2)
    Z10 = FCWEIGHT(PCLO(JROF,JLEV,1,1),PCLO(JROF,JLEV,2,1),PCLO(JROF,JLEV,3,1),&
      &Z10,Z11,Z12,PXSL(IL1+KL0(JROF,JLEV,1)+3))

    Z20 = PXSL(IL1+KL0(JROF,JLEV,2))
    Z21 = PXSL(IL1+KL0(JROF,JLEV,2)+1)
    Z22 = PXSL(IL1+KL0(JROF,JLEV,2)+2)
    Z20 = FCWEIGHT(PCLO(JROF,JLEV,1,2),PCLO(JROF,JLEV,2,2),PCLO(JROF,JLEV,3,2),&
      &Z20,Z21,Z22,PXSL(IL1+KL0(JROF,JLEV,2)+3))

    Z31 = PXSL(IL1+KL0(JROF,JLEV,3)+1)
    Z31 = FDWEIGHT(PDLO(JROF,JLEV,3),Z31,PXSL(IL1+KL0(JROF,JLEV,3)+2))

    Z1(JROF) = FCWEIGHT(PCLA(JROF,JLEV,1),PCLA(JROF,JLEV,2),PCLA(JROF,JLEV,3),&
      &Z01,Z10,Z20,Z31)

    ZN1(JROF) = MIN(Z11,Z12,Z21,Z22)
    ZX1(JROF) = MAX(Z11,Z12,Z21,Z22)

    ! level 2
    Z01 = PXSL(IL2+KL0(JROF,JLEV,0)+1)
    Z01 = FDWEIGHT(PDLO(JROF,JLEV,0),Z01,PXSL(IL2+KL0(JROF,JLEV,0)+2))

    Z10 = PXSL(IL2+KL0(JROF,JLEV,1))
    Z11 = PXSL(IL2+KL0(JROF,JLEV,1)+1)
    Z12 = PXSL(IL2+KL0(JROF,JLEV,1)+2)
    Z10 = FCWEIGHT(PCLO(JROF,JLEV,1,1),PCLO(JROF,JLEV,2,1),PCLO(JROF,JLEV,3,1),&
      &Z10,Z11,Z12,PXSL(IL2+KL0(JROF,JLEV,1)+3))

    Z20 = PXSL(IL2+KL0(JROF,JLEV,2))
    Z21 = PXSL(IL2+KL0(JROF,JLEV,2)+1)
    Z22 = PXSL(IL2+KL0(JROF,JLEV,2)+2)
    Z20 = FCWEIGHT(PCLO(JROF,JLEV,1,2),PCLO(JROF,JLEV,2,2),PCLO(JROF,JLEV,3,2),&
      &Z20,Z21,Z22,PXSL(IL2+KL0(JROF,JLEV,2)+3))

    Z31 = PXSL(IL2+KL0(JROF,JLEV,3)+1)
    Z31 = FDWEIGHT(PDLO(JROF,JLEV,3),Z31,PXSL(IL2+KL0(JROF,JLEV,3)+2))

    Z2(JROF) = FCWEIGHT(PCLA(JROF,JLEV,1),PCLA(JROF,JLEV,2),PCLA(JROF,JLEV,3),&
      &Z01,Z10,Z20,Z31)

    ZN2(JROF) = MIN(Z11,Z12,Z21,Z22)
    ZX2(JROF) = MAX(Z11,Z12,Z21,Z22)
  ENDDO

  !DIR$ IVDEP
  !CDIR NODEP
  DO JROF=KST,KEND
    PXF(JROF,JLEV) = FCWEIGHT(PVINTW(JROF,JLEV,1),PVINTW(JROF,JLEV,2),PVINTW(JROF,JLEV,3),&
      &Z0(JROF),Z1(JROF),Z2(JROF),Z3(JROF))
  ENDDO

  IF (IQM==1) THEN
    DO JROF=KST,KEND
      ZMIN = MIN(ZN1(JROF),ZN2(JROF))
      ZMAX = MAX(ZX1(JROF),ZX2(JROF))

      ! limit
      IF (PXF(JROF,JLEV)>ZMAX.OR.PXF(JROF,JLEV)<ZMIN) THEN
        !     Linear interpolation.
        Z11 = PXSL(IL1+KL0(JROF,JLEV,1)+1)
        Z21 = PXSL(IL1+KL0(JROF,JLEV,2)+1)
        Z12 = PXSL(IL2+KL0(JROF,JLEV,1)+1)
        Z22 = PXSL(IL2+KL0(JROF,JLEV,2)+1)
        ZSUPLO1 = FDWEIGHT(PDLO(JROF,JLEV,1),Z11,PXSL(IL1+KL0(JROF,JLEV,1)+2))
        ZSUPLO2 = FDWEIGHT(PDLO(JROF,JLEV,2),Z21,PXSL(IL1+KL0(JROF,JLEV,2)+2))
        ZINFLO1 = FDWEIGHT(PDLO(JROF,JLEV,1),Z12,PXSL(IL2+KL0(JROF,JLEV,1)+2))
        ZINFLO2 = FDWEIGHT(PDLO(JROF,JLEV,2),Z22,PXSL(IL2+KL0(JROF,JLEV,2)+2))
        ZSUP = FDWEIGHT(PDLAT(JROF,JLEV),ZSUPLO1,ZSUPLO2)
        ZINF = FDWEIGHT(PDLAT(JROF,JLEV),ZINFLO1,ZINFLO2)
        PXF(JROF,JLEV) = FDWEIGHT(PDVER(JROF,JLEV),ZSUP,ZINF)
      ENDIF
    ENDDO
  ELSEIF (IQM==2) THEN
    DO JROF=KST,KEND
      ZMIN = MIN(ZN1(JROF),ZN2(JROF))
      ZMAX = MAX(ZX1(JROF),ZX2(JROF))

      ! limit
      PXF(JROF,JLEV)=MAX(ZMIN,MIN(ZMAX,PXF(JROF,JLEV)))
    ENDDO
  ELSEIF (IQM==3) THEN
    DO JROF=KST,KEND
      ZMIN = MIN(ZN1(JROF),ZN2(JROF))
      ZMAX = MAX(ZX1(JROF),ZX2(JROF))

      ZXF=PXF(JROF,JLEV)+ZSURPL(JROF)
      PXF(JROF,JLEV)=MAX(ZMIN,MIN(ZMAX,ZXF))
      ZDMAX=MAX(ZXF,ZMAX) - ZMAX
      ZDMIN=MIN(ZXF,ZMIN) - ZMIN
      IF(ABS(ZDMAX) >= ABS(ZDMIN)) THEN 
        ZSURPL(JROF)=ZDMAX*YDVETA%VETA_LAITRIQM3D(JLEV)
      ELSE
        ZSURPL(JROF)=ZDMIN*YDVETA%VETA_LAITRIQM3D(JLEV)
      ENDIF
    ENDDO
  ENDIF
ENDDO

IF (LHOOK) CALL DR_HOOK('LAITRIQM3D',1,ZHOOK_HANDLE)
END SUBROUTINE LAITRIQM3D

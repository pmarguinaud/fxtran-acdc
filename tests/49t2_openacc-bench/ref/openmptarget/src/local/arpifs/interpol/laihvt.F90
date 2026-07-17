SUBROUTINE LAIHVT(KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,PDLAT,PCLA,PDLO,PCLO,KL0,&
  PVDERW,PDZ,PXSL,PXF)

!$ACDC singlecolumn  --process-pointers


! Purpose :
! -------
!   LAIHVT - semi-Lagrangian scheme: tri-dimensional 32-point
!   interpolations (with optional quasi-monotonic treatment). Horizontal
!   high order interpolator fully controlled by weights, low order
!   interpolator always linear. Hermite cubic vertical interpolations.

! Interface :
! ---------
!   INPUT:
!     KSLB1  - horizontal dimension for grid-point quantities
!     KPROMA  - horizontal dimension for interpolation point quantities
!     KST     - first element of arrays where computations are performed
!     KEND   - depth of work
!     KFLEV   - vertical dimension
!     KFLDN   - number of the first field
!     KFLDX   - number of the last field
!     KQM     - index of monotonicity
!               0: not monotonous interpolation
!               1: horizontally quasi-monotonous interpolation
!               2: quasi-monotonous interpolation
!     PDLAT   - distance for horizontal linear interpolations in latitude
!     PCLA    - weights for horizontal cubic interpolations in latitude
!     PDLO    - distances for horizontal linear interpolations
!               in longitude (latitude rows 0, 1, 2, 3)
!     PCLO    - weights for horizontal cubic interpolations in longitude 
!               (latitude rows 1, 2)
!     KL0     - indices of the four western points of the 16 point
!               interpolation grid
!     PVDERW  - weights for computation of vertical derivatives.
!     PDZ     - weights for Hermite cubic vertical interpolation
!     PXSL    - quantity to be interpolated
!   OUTPUT:
!     PXF     - interpolated variable

! Externals :
! ---------
!   None.

! Method :
! ------
!   See documentation.

! Reference :
! ---------

! Author :
! ------
!   K. YESSAD
!   METEO-FRANCE, CNRM/GMAP.

! Modifications :
! -------------
!   Original : AUGUST 1996.
!   M.Hamrud      01-Oct-2003 CY28 Cleaning
!   F. Vana       16-Sep-2008 Weights driven interpolation.
!   03-Sep-2008 K. Yessad  Merge QM with not-QM version.
! End Modifications
!------------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB, JPIB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK, JPHOOK

!------------------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KSLB1 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KQM
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLO(KPROMA,KFLEV,3,2)
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVDERW(KPROMA,KFLEV,2,2) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDZ(KPROMA,KFLEV,4)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KSLB1*(KFLDX-KFLDN+1))
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXF(KPROMA,KFLEV)

!------------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JL,JROF,IL1,IL2,IL3
LOGICAL :: LQMH
REAL(KIND=JPRB) :: Z0(KPROMA),Z1(KPROMA),Z2(KPROMA),Z3(KPROMA)
REAL(KIND=JPRB) :: Z01,Z10,Z11,Z12,Z20,Z21,Z22,Z31,ZZ
REAL(KIND=JPRB) :: ZDV1,ZDV2
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "interp.func.h"

IF (LHOOK) CALL DR_HOOK('LAIHVT',0,ZHOOK_HANDLE)

LQMH = KQM == 1.OR.KQM == 2

IL1 = KSLB1
IL2 = IL1+KSLB1
IL3 = IL2+KSLB1

! 32-point interpolations (horizontal interpolations should remain identical
!  to what is done in LAITRI).

DO JL=1,KFLEV
  !DIR$ IVDEP
  !NEC$ IVDEP
  DO JROF=KST,KEND
    Z11 = PXSL(KL0(JROF,JL,1)+1)
    Z11 = FDWEIGHT(PDLO(JROF,JL,1),Z11,PXSL(KL0(JROF,JL,1)+2))

    Z12 = PXSL(KL0(JROF,JL,2)+1)
    Z12 = FDWEIGHT(PDLO(JROF,JL,2),Z12,PXSL(KL0(JROF,JL,2)+2))

    Z0(JROF) = FDWEIGHT(PDLAT(JROF,JL),Z11,Z12)

    Z21 = PXSL(IL3+KL0(JROF,JL,1)+1)
    Z21 = FDWEIGHT(PDLO(JROF,JL,1),Z21,PXSL(IL3+KL0(JROF,JL,1)+2))

    Z22 = PXSL(IL3+KL0(JROF,JL,2)+1)
    Z22 = FDWEIGHT(PDLO(JROF,JL,2),Z22,PXSL(IL3+KL0(JROF,JL,2)+2))

    Z3(JROF) = FDWEIGHT(PDLAT(JROF,JL),Z21,Z22)
  ENDDO

  !DIR$ IVDEP
  !NEC$ IVDEP
  DO JROF=KST,KEND
    ! level 1
    Z01 = PXSL(IL1+KL0(JROF,JL,0)+1)
    Z01 = FDWEIGHT(PDLO(JROF,JL,0),Z01,PXSL(IL1+KL0(JROF,JL,0)+2))

    Z10 = PXSL(IL1+KL0(JROF,JL,1))
    Z11 = PXSL(IL1+KL0(JROF,JL,1)+1)
    Z12 = PXSL(IL1+KL0(JROF,JL,1)+2)
    Z10 = FCWEIGHT(PCLO(JROF,JL,1,1),PCLO(JROF,JL,2,1),PCLO(JROF,JL,3,1),&
      &Z10,Z11,Z12,PXSL(IL1+KL0(JROF,JL,1)+3))

    Z20 = PXSL(IL1+KL0(JROF,JL,2))
    Z21 = PXSL(IL1+KL0(JROF,JL,2)+1)
    Z22 = PXSL(IL1+KL0(JROF,JL,2)+2)
    Z20 = FCWEIGHT(PCLO(JROF,JL,1,2),PCLO(JROF,JL,2,2),PCLO(JROF,JL,3,2),&
      &Z20,Z21,Z22,PXSL(IL1+KL0(JROF,JL,2)+3))

    IF (LQMH) THEN
      Z10 = FCLIPHIGH(Z10,Z11,Z12)
      Z10 = FCLIPLOW(Z10,Z11,Z12)
      Z20 = FCLIPHIGH(Z20,Z21,Z22)
      Z20 = FCLIPLOW(Z20,Z21,Z22)
    ENDIF

    Z31 = PXSL(IL1+KL0(JROF,JL,3)+1)
    Z31 = FDWEIGHT(PDLO(JROF,JL,3),Z31,PXSL(IL1+KL0(JROF,JL,3)+2))

    Z1(JROF) = FCWEIGHT(PCLA(JROF,JL,1),PCLA(JROF,JL,2),PCLA(JROF,JL,3),Z01,Z10,Z20,Z31)
    IF (LQMH) THEN
      ZZ = FCLIPHIGH(Z1(JROF),Z10,Z20)
      Z1(JROF) = FCLIPLOW(ZZ,Z10,Z20)
    ENDIF

    ! level 2
    Z01 = PXSL(IL2+KL0(JROF,JL,0)+1)
    Z01 = FDWEIGHT(PDLO(JROF,JL,0),Z01,PXSL(IL2+KL0(JROF,JL,0)+2))

    Z10 = PXSL(IL2+KL0(JROF,JL,1))
    Z11 = PXSL(IL2+KL0(JROF,JL,1)+1)
    Z12 = PXSL(IL2+KL0(JROF,JL,1)+2)
    Z10 = FCWEIGHT(PCLO(JROF,JL,1,1),PCLO(JROF,JL,2,1),PCLO(JROF,JL,3,1),&
      &Z10,Z11,Z12,PXSL(IL2+KL0(JROF,JL,1)+3))

    Z20 = PXSL(IL2+KL0(JROF,JL,2))
    Z21 = PXSL(IL2+KL0(JROF,JL,2)+1)
    Z22 = PXSL(IL2+KL0(JROF,JL,2)+2)
    Z20 = FCWEIGHT(PCLO(JROF,JL,1,2),PCLO(JROF,JL,2,2),PCLO(JROF,JL,3,2),&
      &Z20,Z21,Z22,PXSL(IL2+KL0(JROF,JL,2)+3))

    IF (LQMH) THEN
      Z10 = FCLIPHIGH(Z10,Z11,Z12)
      Z10 = FCLIPLOW(Z10,Z11,Z12)
      Z20 = FCLIPHIGH(Z20,Z21,Z22)
      Z20 = FCLIPLOW(Z20,Z21,Z22)
    ENDIF

    Z31 = PXSL(IL2+KL0(JROF,JL,3)+1)
    Z31 = FDWEIGHT(PDLO(JROF,JL,3),Z31,PXSL(IL2+KL0(JROF,JL,3)+2))

    Z2(JROF) = FCWEIGHT(PCLA(JROF,JL,1),PCLA(JROF,JL,2),PCLA(JROF,JL,3),Z01,Z10,Z20,Z31)
    IF (LQMH) THEN
      ZZ = FCLIPHIGH(Z2(JROF),Z10,Z20)
      Z2(JROF) = FCLIPLOW(ZZ,Z10,Z20)
    ENDIF
  ENDDO

  ! final interpolation in vertical
  DO JROF=KST,KEND
    ZDV1 = PVDERW(JROF,JL,1,1)*(Z1(JROF)-Z0(JROF))+PVDERW(JROF,JL,2,1)*(Z2(JROF)-Z1(JROF))
    ZDV2 = PVDERW(JROF,JL,1,2)*(Z2(JROF)-Z1(JROF))+PVDERW(JROF,JL,2,2)*(Z3(JROF)-Z2(JROF))
    PXF(JROF,JL) = PDZ(JROF,JL,1)*Z1(JROF)+PDZ(JROF,JL,2)*Z2(JROF)+PDZ(JROF,JL,3)*ZDV1+&
      &PDZ(JROF,JL,4)*ZDV2

    IF (KQM == 2) THEN
      PXF(JROF,JL) = FCLIPHIGH(PXF(JROF,JL),Z1(JROF),Z2(JROF))
      PXF(JROF,JL) = FCLIPLOW(PXF(JROF,JL),Z1(JROF),Z2(JROF))
    ENDIF
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK('LAIHVT',1,ZHOOK_HANDLE)
END SUBROUTINE LAIHVT

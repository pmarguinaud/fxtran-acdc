SUBROUTINE LAIDDI(KSLB1,KPROMA,KST,KEND,KFLEV, &
 & KFLDN,KFLDX,KQM, &
 & PCLA,PDLO,PCLO,KL0, &
 & PXSL,PXF)

!$ACDC singlecolumn  --process-pointers


! Purpose :
! -------
!   LAIDDI - semi-Lagrangian scheme: bi-dimensional 12-point
!   interpolations (in horizontal) with optional quasi-monotonic treatment. 
!   Type of high order interpolator fully controlled by weights, 
!   low order interpolator always linear.

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
!     PCLA    - weights for horizontal cubic interpolation in latitude
!     PDLO    - distances for horizontal linear interpolations 
!               in longitude (latitude rows 0, 3)
!     PCLO    - weights for horizontal cubic interpolations in longitude
!               (latitude rows 1, 2)
!     KL0     - indices of the four western points of the 16 point 
!               interpolation grid
!     PXSL    - quantity to be interpolated.
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
!   ??-Feb-1992 K. Yessad, after the subroutine LAGINT2 written by 
!   Maurice Imbard, Alain Craplet and Michel Rochas,
!   Meteo-France, CNRM/GMAP.

! Modifications :
! -------------
!   01-Oct-2003 M. Hamrud  CY28 Cleaning.
!   30-Jun-2008 J. Masek   High order interpolator fully driven by weights,
!     computation of all weights moved to (E)LASCAW.
!   03-Sep-2008 K. Yessad  Merge QM with not-QM version.
!  2021 H Petithomme: optimization
!      R. El Khatib 23-Feb-2023 Portability fix for NEC Sx-Aurora : remove an elemental attribute
! End Modifications
!------------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLA(KPROMA,KFLEV,3) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMA,KFLEV,0:3) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLO(KPROMA,KFLEV,3,2)
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMA,KFLEV,0:3) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KSLB1*(KFLDX-KFLDN+1)) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXF(KPROMA,KFLEV) 

!------------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV,JROF
LOGICAL :: LQM
REAL(KIND=JPRB) :: ZZ,Z01,Z10,Z11,Z12,Z20,Z21,Z22,Z31
!------------------------------------------------------------------------------

#include "interp.func.h"

!------------------------------------------------------------------------------

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('LAIDDI',0,ZHOOK_HANDLE)

!------------------------------------------------------------------------------

! 1. Interpolations
!    --------------
LQM = KQM == 1

DO JLEV=1,KFLEV
  ! interpolations on levels 1 and 2 (cubic)
  !DIR$ IVDEP
  !NEC$ IVDEP
  DO JROF=KST,KEND
    Z01 = PXSL(KL0(JROF,JLEV,0)+1)
    Z01 = FDWEIGHT(PDLO(JROF,JLEV,0),Z01,PXSL(KL0(JROF,JLEV,0)+2))

    Z10 = PXSL(KL0(JROF,JLEV,1))
    Z11 = PXSL(KL0(JROF,JLEV,1)+1)
    Z12 = PXSL(KL0(JROF,JLEV,1)+2)
    Z10 = FCWEIGHT(PCLO(JROF,JLEV,1,1),PCLO(JROF,JLEV,2,1),PCLO(JROF,JLEV,3,1),Z10,Z11,Z12,&
      &PXSL(KL0(JROF,JLEV,1)+3))

    Z20 = PXSL(KL0(JROF,JLEV,2))
    Z21 = PXSL(KL0(JROF,JLEV,2)+1)
    Z22 = PXSL(KL0(JROF,JLEV,2)+2)
    Z20 = FCWEIGHT(PCLO(JROF,JLEV,1,2),PCLO(JROF,JLEV,2,2),PCLO(JROF,JLEV,3,2),Z20,Z21,Z22,&
      &PXSL(KL0(JROF,JLEV,2)+3))

    IF (LQM) THEN
      Z10 = FCLIPHIGH(Z10,Z11,Z12)
      Z10 = FCLIPLOW(Z10,Z11,Z12)
      Z20 = FCLIPHIGH(Z20,Z21,Z22)
      Z20 = FCLIPLOW(Z20,Z21,Z22)
    ENDIF

    Z31 = PXSL(KL0(JROF,JLEV,3)+1)
    Z31 = FDWEIGHT(PDLO(JROF,JLEV,3),Z31,PXSL(KL0(JROF,JLEV,3)+2))

    PXF(JROF,JLEV) = FCWEIGHT(PCLA(JROF,JLEV,1),PCLA(JROF,JLEV,2),PCLA(JROF,JLEV,3),Z01,Z10,Z20,Z31)
    IF (LQM) THEN
      ZZ = FCLIPHIGH(PXF(JROF,JLEV),Z10,Z20)
      PXF(JROF,JLEV) = FCLIPLOW(ZZ,Z10,Z20)
    ENDIF
  ENDDO
ENDDO

!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAIDDI',1,ZHOOK_HANDLE)
END SUBROUTINE LAIDDI


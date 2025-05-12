!option! -O extendreorder
SUBROUTINE LAITRI_WENO(YDDYN,KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,CDTYPE,&
 & PDLAT,PCLA,PDLO,PCLO,KL0,KNOWENO,PCW,PVINTW,PXSL,PXF,PALPHA,PRDETAR,LDQM3DCONS,LDADD)

!$ACDC singlecolumn  --process-pointers


! Purpose :
! -------
!   LAITRI_WENO - semi-Lagrangian scheme: tri-dimensional 56-point
!   interpolations (using three overlapping 32-points stencils)
!   with optional quasi-monotonic treatment. Type of high
!   order interpolator fully controlled by weights, low order
!   interpolator always linear. The vertical interpolation 
!   is treated by the WENO technique.

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
!              -1: Bermejo, Staniforth MWR 1992, Vol. 20.
!               0: not monotonous interpolation
!               1: horizontally quasi-monotonous interpolation
!               2: quasi-monotonous interpolation
!               3: vertically quasi-monotonous interpolation
!     PDLAT   - distance for horizontal linear interpolations in latitude
!     PCLA    - weights for horizontal cubic interpolations in latitude
!     PDLO    - distances for horizontal linear interpolations
!               in longitude (latitude rows 0, 1, 2, 3)
!     PCLO    - weights for horizontal cubic interpolations in longitude 
!               (latitude rows 1, 2)
!     KL0     - indices of the four western points of the 16 point
!               interpolation grid
!     KNOWENO - special boundary treatment for WENO
!     PCW     - C_k functions for WENO
!     PVINTW  - weights for cubic vertical interpolation
!     PXSL    - quantity to be interpolated
!     PALPHA (OPTIONAL) - ALPHA (known as p) exponent for WENO
!     PRDETAR (OPTIONAL) - VRDETAR (vertical coordinate related par)
!     LDQM3DCONS (OPTIONAL) - conservativeness for Bermejo & Staniforth QM limiter
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
!   Filip Vana   27-July-2017 after work of A. Craciun, P. Smolikova & J. Masek
!                and the original LAITRI code.
!   (c) ECMWF

! Modifications :
! -------------
!    F. Vana  28-11-2018  passing ALPHA as an argument 
!    F. Vana  24-Jun-2019  QM/QMH fixers
!    F. Vana  29-Jan-2020  (single) precision fix
!    F. Vana   1-Jun-2020  QM3D limiter
!    F. Vana  25-Jan-2021  QM/QMH fixer limited to inner cube
!    H Petithomme 01/2024: cleaning with level functions
!------------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB     ,JPIB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMDYN   , ONLY : TDYN
!------------------------------------------------------------------------------

IMPLICIT NONE

TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
INTEGER(KIND=JPIM),INTENT(IN)    :: KSLB1 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KQM
CHARACTER(LEN=*)  ,INTENT(IN)    :: CDTYPE
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLO(KPROMA,KFLEV,3,2)
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMA,KFLEV,0:3)
INTEGER(KIND=JPIM),INTENT(IN)    :: KNOWENO(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCW(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVINTW(KPROMA,KFLEV,9)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KSLB1*(KFLDX-KFLDN+1))
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXF(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL,INTENT(IN) :: PALPHA
REAL(KIND=JPRB),OPTIONAL,INTENT(IN) :: PRDETAR(KFLEV)
LOGICAL        ,OPTIONAL,INTENT(IN) :: LDQM3DCONS,LDADD

!------------------------------------------------------------------------------

INTEGER(KIND=JPIB) :: IV9L1, IV9L2, IV0L0, IV0L1, IV0L2, IV0L3, IV1L0, IV1L1, IV1L2, IV1L3, &
 & IV2L0, IV2L1, IV2L2, IV2L3, IV3L0, IV3L1, IV3L2, IV3L3, IV4L1, IV4L2
INTEGER(KIND=JPIM) :: ISHIFT1, ISHIFT2
INTEGER(KIND=JPIM) :: JLEV, JROF
INTEGER(KIND=JPIM) :: JJ, J0,IQM,INX
LOGICAL :: LQMH,LQMV,LQM0,LLADD
REAL(KIND=JPRB) :: ZZF111, ZZF112, ZZF121, ZZF122
REAL(KIND=JPRB) :: ZZF211, ZZF212, ZZF221, ZZF222
REAL(KIND=JPRB) :: ZZF110, ZZF113, ZZF120, ZZF123
REAL(KIND=JPRB) :: ZZF210, ZZF213, ZZF220, ZZF223
REAL(KIND=JPRB) :: ZZ19, ZZ29, ZZ00, ZZ10, ZZ20, ZZ30, ZZ01, ZZ11, ZZ21, ZZ31
REAL(KIND=JPRB) :: ZZ02, ZZ12, ZZ22, ZZ32, ZZ03, ZZ13, ZZ23, ZZ33, ZZ14, ZZ24
REAL(KIND=JPRB) :: ZMIN, ZMAX
REAL(KIND=JPRB) :: Z19, Z29, Z14, Z24
REAL(KIND=JPRB) :: Z(KPROMA,0:5)
REAL(KIND=JPRB) :: ZXF(KPROMA,KFLEV)
REAL(KIND=JPRB) :: Z01,Z10,Z11,Z12,Z20,Z21,Z22,Z31

! Bermejo & Staniforth fixer
REAL(KIND=JPRB) :: ZF(8), ZX
REAL(KIND=JPRB) :: ZSURPL(KPROMA)
REAL(KIND=JPRB) :: ZRVETA,ZALPHA

! WENO specific variables
REAL(KIND=JPRB)    :: ZEPS, ZSW, ZEPS_MAX,ZZ
REAL(KIND=JPRB)    :: ZFX(1:3), ZBETA(1:3), ZWW(1:3)

!------------------------------------------------------------------------------

#include "interp.func.h"
#include "lainterpol.func.h"
#include "abor1.intfb.h"

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAITRI_WENO',0,ZHOOK_HANDLE)

ASSOCIATE (RBETAK => YDDYN%RBETAK)

LLADD = .FALSE.
IF (PRESENT(LDADD)) LLADD = LDADD

! LQM options:
! - LQMV: px values bound to 2 central pxsl values in vertical interp
! - LQMH: px values bound to 2 central pxsl values in horizontal (lat/long) interps
! - LQM0: px values bound to 0 at lowest, after all 3 unlimited interps

! note: KQM can be negative (different meaning, further options)
IF (KQM == -1) THEN
  LQMH = .FALSE.
  LQMV = .FALSE.
  LQM0 = .FALSE.
ELSE
  IQM = ABS(KQM)
  LQMH = IQM == 1.OR.IQM == 2
  LQMV = IQM == 2.OR.IQM == 3
  LQM0 = IQM >= 4
ENDIF

! 0. Set up
!    ------

ZEPS=1.E-6_JPRB  ! Note: The value here is not (directly) related to any machine precision
ZEPS_MAX=100._JPRB*EPSILON(ZEPS)

IF (KQM == -1) THEN
  ! Consistency check for Bermejo & Staniforth fixer
  IF (.NOT.PRESENT(PRDETAR)) CALL ABOR1(' LAITRI_WENO : MISSING ARGUMENT PRDETAR')
  IF (.NOT.PRESENT(LDQM3DCONS)) CALL ABOR1(' LAITRI_WENO : MISSING ARGUMENT LDQM3DCONS')

  ! Initialization
  ZSURPL(KST:KEND)=0.0_JPRB
ENDIF

! offsets for getting values on interpolation stencil
IV9L1=1-KSLB1
IV9L2=2-KSLB1
IV0L0=0
IV0L1=1
IV0L2=2
IV0L3=3
IV1L0=  KSLB1
IV1L1=1+KSLB1
IV1L2=2+KSLB1
IV1L3=3+KSLB1
IV2L0=IV1L0+KSLB1
IV2L1=IV1L1+KSLB1
IV2L2=IV1L2+KSLB1
IV2L3=IV1L3+KSLB1
IV3L0=IV2L0+KSLB1
IV3L1=IV2L1+KSLB1
IV3L2=IV2L2+KSLB1
IV3L3=IV2L3+KSLB1
IV4L1=IV3L1+KSLB1
IV4L2=IV3L2+KSLB1

INX = KSLB1*(KFLDX-KFLDN+1)

! 1. Interpolations
!    --------------

IF (CDTYPE=='SCALAR') THEN
  ! 56-point interpolations
  DO JLEV=1,KFLEV
    IF (PRESENT(PALPHA)) THEN
      ZALPHA=PALPHA
    ELSE
      ZALPHA=YDDYN%RALPHA
    ENDIF
    ! Treatment near the top (to avoid instabilitieas we don't allow any overshoots there)
    IF (JLEV <= YDDYN%NLEV_ZALPHA) THEN  ! done for uppermost NLEV_ZALPHA levels
      ZALPHA=MAX(ZALPHA,YDDYN%RALPHA_TOP)
    ENDIF

    ! NOTE: For the consistency reason when assembling the final quintic vertical
    !       interpolation the Z(1) and Z(4) MUST NOT BE SECURED by the QM(H) fixer.

    ! interpolation on levels 0 and 3, without QM
    !DIR$ IVDEP
    !NEC$ IVDEP
    DO JROF=KST,KEND
      ! level 0
      Z01 = PXSL(IV0L0+KL0(JROF,JLEV,0)+1)
      Z01 = FDWEIGHT(PDLO(JROF,JLEV,0),Z01,PXSL(IV0L0+KL0(JROF,JLEV,0)+2))

      Z10 = PXSL(IV0L0+KL0(JROF,JLEV,1))
      Z11 = PXSL(IV0L0+KL0(JROF,JLEV,1)+1)
      Z12 = PXSL(IV0L0+KL0(JROF,JLEV,1)+2)
      Z10 = FCWEIGHT(PCLO(JROF,JLEV,1,1),PCLO(JROF,JLEV,2,1),PCLO(JROF,JLEV,3,1),&
        &Z10,Z11,Z12,PXSL(IV0L0+KL0(JROF,JLEV,1)+3))

      Z20 = PXSL(IV0L0+KL0(JROF,JLEV,2))
      Z21 = PXSL(IV0L0+KL0(JROF,JLEV,2)+1)
      Z22 = PXSL(IV0L0+KL0(JROF,JLEV,2)+2)
      Z20 = FCWEIGHT(PCLO(JROF,JLEV,1,2),PCLO(JROF,JLEV,2,2),PCLO(JROF,JLEV,3,2),&
        &Z20,Z21,Z22,PXSL(IV0L0+KL0(JROF,JLEV,2)+3))

      Z31 = PXSL(IV0L0+KL0(JROF,JLEV,3)+1)
      Z31 = FDWEIGHT(PDLO(JROF,JLEV,3),Z31,PXSL(IV0L0+KL0(JROF,JLEV,3)+2))

      Z(JROF,1) = FCWEIGHT(PCLA(JROF,JLEV,1),PCLA(JROF,JLEV,2),PCLA(JROF,JLEV,3),&
        &Z01,Z10,Z20,Z31)

      ! level 3
      Z01 = PXSL(IV3L0+KL0(JROF,JLEV,0)+1)
      Z01 = FDWEIGHT(PDLO(JROF,JLEV,0),Z01,PXSL(IV3L0+KL0(JROF,JLEV,0)+2))

      Z10 = PXSL(IV3L0+KL0(JROF,JLEV,1))
      Z11 = PXSL(IV3L0+KL0(JROF,JLEV,1)+1)
      Z12 = PXSL(IV3L0+KL0(JROF,JLEV,1)+2)
      Z10 = FCWEIGHT(PCLO(JROF,JLEV,1,1),PCLO(JROF,JLEV,2,1),PCLO(JROF,JLEV,3,1),&
        &Z10,Z11,Z12,PXSL(IV3L0+KL0(JROF,JLEV,1)+3))

      Z20 = PXSL(IV3L0+KL0(JROF,JLEV,2))
      Z21 = PXSL(IV3L0+KL0(JROF,JLEV,2)+1)
      Z22 = PXSL(IV3L0+KL0(JROF,JLEV,2)+2)
      Z20 = FCWEIGHT(PCLO(JROF,JLEV,1,2),PCLO(JROF,JLEV,2,2),PCLO(JROF,JLEV,3,2),&
        &Z20,Z21,Z22,PXSL(IV3L0+KL0(JROF,JLEV,2)+3))

      Z31 = PXSL(IV3L0+KL0(JROF,JLEV,3)+1)
      Z31 = FDWEIGHT(PDLO(JROF,JLEV,3),Z31,PXSL(IV3L0+KL0(JROF,JLEV,3)+2))

      Z(JROF,4) = FCWEIGHT(PCLA(JROF,JLEV,1),PCLA(JROF,JLEV,2),PCLA(JROF,JLEV,3),&
        &Z01,Z10,Z20,Z31)
    ENDDO

    ! interpolation on levels 1 and 2, with QM
    !DIR$ IVDEP
    !NEC$ IVDEP
    DO JROF=KST,KEND
      ! level 1
      Z01 = PXSL(IV1L0+KL0(JROF,JLEV,0)+1)
      Z01 = FDWEIGHT(PDLO(JROF,JLEV,0),Z01,PXSL(IV1L0+KL0(JROF,JLEV,0)+2))

      Z10 = PXSL(IV1L0+KL0(JROF,JLEV,1))
      Z11 = PXSL(IV1L0+KL0(JROF,JLEV,1)+1)
      Z12 = PXSL(IV1L0+KL0(JROF,JLEV,1)+2)
      Z10 = FCWEIGHT(PCLO(JROF,JLEV,1,1),PCLO(JROF,JLEV,2,1),PCLO(JROF,JLEV,3,1),&
        &Z10,Z11,Z12,PXSL(IV1L0+KL0(JROF,JLEV,1)+3))

      Z20 = PXSL(IV1L0+KL0(JROF,JLEV,2))
      Z21 = PXSL(IV1L0+KL0(JROF,JLEV,2)+1)
      Z22 = PXSL(IV1L0+KL0(JROF,JLEV,2)+2)
      Z20 = FCWEIGHT(PCLO(JROF,JLEV,1,2),PCLO(JROF,JLEV,2,2),PCLO(JROF,JLEV,3,2),&
        &Z20,Z21,Z22,PXSL(IV1L0+KL0(JROF,JLEV,2)+3))

      IF (LQMH) THEN
        Z10 = FCLIPHIGH(Z10,Z11,Z12)
        Z10 = FCLIPLOW(Z10,Z11,Z12)
        Z20 = FCLIPHIGH(Z20,Z21,Z22)
        Z20 = FCLIPLOW(Z20,Z21,Z22)
      ENDIF

      Z31 = PXSL(IV1L0+KL0(JROF,JLEV,3)+1)
      Z31 = FDWEIGHT(PDLO(JROF,JLEV,3),Z31,PXSL(IV1L0+KL0(JROF,JLEV,3)+2))

      Z(JROF,2) = FCWEIGHT(PCLA(JROF,JLEV,1),PCLA(JROF,JLEV,2),PCLA(JROF,JLEV,3),&
        &Z01,Z10,Z20,Z31)
      IF (LQMH) THEN
        ZZ = FCLIPHIGH(Z(JROF,2),Z10,Z20)
        Z(JROF,2) = FCLIPLOW(ZZ,Z10,Z20)
      ENDIF

      ! level 2
      Z01 = PXSL(IV2L0+KL0(JROF,JLEV,0)+1)
      Z01 = FDWEIGHT(PDLO(JROF,JLEV,0),Z01,PXSL(IV2L0+KL0(JROF,JLEV,0)+2))

      Z10 = PXSL(IV2L0+KL0(JROF,JLEV,1))
      Z11 = PXSL(IV2L0+KL0(JROF,JLEV,1)+1)
      Z12 = PXSL(IV2L0+KL0(JROF,JLEV,1)+2)
      Z10 = FCWEIGHT(PCLO(JROF,JLEV,1,1),PCLO(JROF,JLEV,2,1),PCLO(JROF,JLEV,3,1),&
        &Z10,Z11,Z12,PXSL(IV2L0+KL0(JROF,JLEV,1)+3))

      Z20 = PXSL(IV2L0+KL0(JROF,JLEV,2))
      Z21 = PXSL(IV2L0+KL0(JROF,JLEV,2)+1)
      Z22 = PXSL(IV2L0+KL0(JROF,JLEV,2)+2)
      Z20 = FCWEIGHT(PCLO(JROF,JLEV,1,2),PCLO(JROF,JLEV,2,2),PCLO(JROF,JLEV,3,2),&
        &Z20,Z21,Z22,PXSL(IV2L0+KL0(JROF,JLEV,2)+3))

      IF (LQMH) THEN
        Z10 = FCLIPHIGH(Z10,Z11,Z12)
        Z10 = FCLIPLOW(Z10,Z11,Z12)
        Z20 = FCLIPHIGH(Z20,Z21,Z22)
        Z20 = FCLIPLOW(Z20,Z21,Z22)
      ENDIF

      Z31 = PXSL(IV2L0+KL0(JROF,JLEV,3)+1)
      Z31 = FDWEIGHT(PDLO(JROF,JLEV,3),Z31,PXSL(IV2L0+KL0(JROF,JLEV,3)+2))

      Z(JROF,3) = FCWEIGHT(PCLA(JROF,JLEV,1),PCLA(JROF,JLEV,2),PCLA(JROF,JLEV,3),&
        &Z01,Z10,Z20,Z31)
      IF (LQMH) THEN
        ZZ = FCLIPHIGH(Z(JROF,3),Z10,Z20)
        Z(JROF,3) = FCLIPLOW(ZZ,Z10,Z20)
      ENDIF
    ENDDO

    ! interpolations on shifted levels 9 and 4 (linear)
!DIR$ PREFERVECTOR
    DO JROF=KST,KEND
      ! interpolations level 9
      ISHIFT1=MAX(0,KNOWENO(JROF,JLEV))*KSLB1
      Z19 = PXSL(KL0(JROF,JLEV,1)+IV9L1+ISHIFT1)
      Z19 = FDWEIGHT(PDLO(JROF,JLEV,1),Z19,PXSL(KL0(JROF,JLEV,1)+IV9L2+ISHIFT1))
      Z29 = PXSL(KL0(JROF,JLEV,2)+IV9L1+ISHIFT1)
      Z29 = FDWEIGHT(PDLO(JROF,JLEV,2),Z29,PXSL(KL0(JROF,JLEV,2)+IV9L2+ISHIFT1))
      Z(JROF,0)=FDWEIGHT(PDLAT(JROF,JLEV),Z19,Z29)

      ! interpolations level 4
      ISHIFT2=MIN(0,KNOWENO(JROF,JLEV))*KSLB1
      Z14 = PXSL(KL0(JROF,JLEV,1)+IV4L1+ISHIFT2)
      Z14 = FDWEIGHT(PDLO(JROF,JLEV,1),Z14,PXSL(KL0(JROF,JLEV,1)+IV4L2+ISHIFT2))
      Z24 = PXSL(KL0(JROF,JLEV,2)+IV4L1+ISHIFT2)
      Z24 = FDWEIGHT(PDLO(JROF,JLEV,2),Z24,PXSL(KL0(JROF,JLEV,2)+IV4L2+ISHIFT2))
      Z(JROF,5)=FDWEIGHT(PDLAT(JROF,JLEV),Z14,Z24)
    ENDDO

    ! final interpolation in vertical by WENO scheme
    DO JROF=KST,KEND

      IF (YDDYN%LWENOBC.OR.(KNOWENO(JROF,JLEV) == 0)) THEN
        DO JJ=1,3
          SELECT CASE (JJ)
            CASE (1)
              J0 = 1
            CASE (2)
              J0 = 2 + MIN(0,KNOWENO(JROF,JLEV))
            CASE (3)
              J0 =     MAX(0,KNOWENO(JROF,JLEV))
          END SELECT

          ZFX(JJ)=Z(JROF,J0) &
           &  + PVINTW(JROF,JLEV,1+3*(JJ-1))*(Z(JROF,J0+1)-Z(JROF,J0)) &
           &  + PVINTW(JROF,JLEV,2+3*(JJ-1))*(Z(JROF,J0+2)-Z(JROF,J0)) &
           &  + PVINTW(JROF,JLEV,3+3*(JJ-1))*(Z(JROF,J0+3)-Z(JROF,J0))

          ZBETA(JJ)=RBETAK(JJ,1)*Z(JROF,J0+3)*Z(JROF,J0+3) +RBETAK(JJ,2)*Z(JROF,J0+2)*Z(JROF,J0+2) &
           &      + RBETAK(JJ,3)*Z(JROF,J0+1)*Z(JROF,J0+1) +RBETAK(JJ,4)*Z(JROF,J0)*Z(JROF,J0)     &
           &      + RBETAK(JJ,5)*Z(JROF,J0+3)*Z(JROF,J0+2) +RBETAK(JJ,6)*Z(JROF,J0+3)*Z(JROF,J0+1) &
           &      + RBETAK(JJ,7)*Z(JROF,J0+3)*Z(JROF,J0)   +RBETAK(JJ,8)*Z(JROF,J0+2)*Z(JROF,J0+1) &
           &      + RBETAK(JJ,9)*Z(JROF,J0+2)*Z(JROF,J0)   +RBETAK(JJ,10)*Z(JROF,J0+1)*Z(JROF,J0)

          ZWW(JJ)=(ZBETA(JJ)+ZEPS)**ZALPHA ! Denominator
          ZWW(JJ)=SIGN(MAX(ABS(ZWW(JJ)),ZEPS_MAX),ZWW(JJ))
          ZWW(JJ)=PCW(JROF,JLEV,JJ)/ZWW(JJ)
        ENDDO

        ZSW=ZWW(1)+ZWW(2)+ZWW(3)

        ZWW(1)=ZWW(1)/ZSW
        ZWW(2)=ZWW(2)/ZSW
        !ZWW(3)=ZWW(3)/ZSW  not used

        ZXF(JROF,JLEV)=ZWW(1)*(ZFX(1)-ZFX(3)) + ZWW(2)*(ZFX(2)-ZFX(3)) + ZFX(3)

      ELSE

        ! no WENO, just standard one interpolation
        ZXF(JROF,JLEV)=FCWEIGHT(PVINTW(JROF,JLEV,1),PVINTW(JROF,JLEV,2),&
          &PVINTW(JROF,JLEV,3),Z(JROF,1),Z(JROF,2),Z(JROF,3),Z(JROF,4))

      ENDIF
    ENDDO

    IF (LQMV) THEN
      DO JROF=KST,KEND
        ! bound ZXF:
        ZXF(JROF,JLEV)=FCLIPHIGH(ZXF(JROF,JLEV),Z(JROF,2),Z(JROF,3))
        ZXF(JROF,JLEV)=FCLIPLOW(ZXF(JROF,JLEV),Z(JROF,2),Z(JROF,3))
      ENDDO
    ENDIF
  ENDDO
ELSEIF (CDTYPE=='VECTOR') THEN
  ! Warning: Always make sure that compiler is moving
  !          the IF (KQM) blocks outside the loops.

  DO JLEV=1,KFLEV
    IF (PRESENT(PALPHA)) THEN
      ZALPHA=PALPHA
    ELSE
      ZALPHA=YDDYN%RALPHA
    ENDIF
    ! Treatment near the top (to avoid instabilitieas we don't allow any overshoots there)
    IF (JLEV <= YDDYN%NLEV_ZALPHA) THEN  ! done for uppermost NLEV_ZALPHA levels
      ZALPHA=MAX(ZALPHA,YDDYN%RALPHA_TOP)
    ENDIF

    ! interpolations in longitude
!CDIR NODEP
!cdir gthreorder
!DIR$ PREFERVECTOR
    DO JROF=KST,KEND
      ! interpolations in longitude, stencil level 9
      ISHIFT1=MAX(0,KNOWENO(JROF,JLEV))*KSLB1
      ZZ19=PXSL(KL0(JROF,JLEV,1)+IV9L1+ISHIFT1)+PDLO(JROF,JLEV,1) &
       & *(PXSL(KL0(JROF,JLEV,1)+IV9L2+ISHIFT1)-PXSL(KL0(JROF,JLEV,1)+IV9L1+ISHIFT1))
      ZZ29=PXSL(KL0(JROF,JLEV,2)+IV9L1+ISHIFT1)+PDLO(JROF,JLEV,2) &
       & *(PXSL(KL0(JROF,JLEV,2)+IV9L2+ISHIFT1)-PXSL(KL0(JROF,JLEV,2)+IV9L1+ISHIFT1))
      ! interpolations in longitude, stencil level 0
      ZZ00=PXSL(KL0(JROF,JLEV,0)+IV0L1)+PDLO(JROF,JLEV,0) &
       & *(PXSL(KL0(JROF,JLEV,0)+IV0L2)-PXSL(KL0(JROF,JLEV,0)+IV0L1))
      ZZF110=PXSL(KL0(JROF,JLEV,1)+IV0L1)
      ZZF210=PXSL(KL0(JROF,JLEV,1)+IV0L2)
      ZZ10=PXSL(KL0(JROF,JLEV,1)+IV0L0) &
       & +PCLO(JROF,JLEV,1,1)*(ZZF110-PXSL(KL0(JROF,JLEV,1)+IV0L0)) &
       & +PCLO(JROF,JLEV,2,1)*(ZZF210-PXSL(KL0(JROF,JLEV,1)+IV0L0)) &
       & +PCLO(JROF,JLEV,3,1)* &
       & (PXSL(KL0(JROF,JLEV,1)+IV0L3)-PXSL(KL0(JROF,JLEV,1)+IV0L0))
      ZZF120=PXSL(KL0(JROF,JLEV,2)+IV0L1)
      ZZF220=PXSL(KL0(JROF,JLEV,2)+IV0L2)
      ZZ20=PXSL(KL0(JROF,JLEV,2)+IV0L0) &
       & +PCLO(JROF,JLEV,1,2)*(ZZF120-PXSL(KL0(JROF,JLEV,2)+IV0L0)) &
       & +PCLO(JROF,JLEV,2,2)*(ZZF220-PXSL(KL0(JROF,JLEV,2)+IV0L0)) &
       & +PCLO(JROF,JLEV,3,2)* &
       & (PXSL(KL0(JROF,JLEV,2)+IV0L3)-PXSL(KL0(JROF,JLEV,2)+IV0L0))
      ZZ30=PXSL(KL0(JROF,JLEV,3)+IV0L1)+PDLO(JROF,JLEV,3) &
       & *(PXSL(KL0(JROF,JLEV,3)+IV0L2)-PXSL(KL0(JROF,JLEV,3)+IV0L1))
      ! interpolations in longitude, stencil level 1
      ZZ01=PXSL(KL0(JROF,JLEV,0)+IV1L1)+PDLO(JROF,JLEV,0) &
       & *(PXSL(KL0(JROF,JLEV,0)+IV1L2)-PXSL(KL0(JROF,JLEV,0)+IV1L1))
      ZZF111=PXSL(KL0(JROF,JLEV,1)+IV1L1)
      ZZF211=PXSL(KL0(JROF,JLEV,1)+IV1L2)
      ZZ11=PXSL(KL0(JROF,JLEV,1)+IV1L0) &
       & +PCLO(JROF,JLEV,1,1)*(ZZF111-PXSL(KL0(JROF,JLEV,1)+IV1L0)) &
       & +PCLO(JROF,JLEV,2,1)*(ZZF211-PXSL(KL0(JROF,JLEV,1)+IV1L0)) &
       & +PCLO(JROF,JLEV,3,1)* &
       & (PXSL(KL0(JROF,JLEV,1)+IV1L3)-PXSL(KL0(JROF,JLEV,1)+IV1L0))
      ZZF121=PXSL(KL0(JROF,JLEV,2)+IV1L1)
      ZZF221=PXSL(KL0(JROF,JLEV,2)+IV1L2)
      ZZ21=PXSL(KL0(JROF,JLEV,2)+IV1L0) &
       & +PCLO(JROF,JLEV,1,2)*(ZZF121-PXSL(KL0(JROF,JLEV,2)+IV1L0)) &
       & +PCLO(JROF,JLEV,2,2)*(ZZF221-PXSL(KL0(JROF,JLEV,2)+IV1L0)) &
       & +PCLO(JROF,JLEV,3,2)* &
       & (PXSL(KL0(JROF,JLEV,2)+IV1L3)-PXSL(KL0(JROF,JLEV,2)+IV1L0))
      ZZ31=PXSL(KL0(JROF,JLEV,3)+IV1L1)+PDLO(JROF,JLEV,3) &
       & *(PXSL(KL0(JROF,JLEV,3)+IV1L2)-PXSL(KL0(JROF,JLEV,3)+IV1L1))
      ! interpolations in longitude, stencil level 2
      ZZ02=PXSL(KL0(JROF,JLEV,0)+IV2L1)+PDLO(JROF,JLEV,0) &
       & *(PXSL(KL0(JROF,JLEV,0)+IV2L2)-PXSL(KL0(JROF,JLEV,0)+IV2L1))
      ZZF112=PXSL(KL0(JROF,JLEV,1)+IV2L1)
      ZZF212=PXSL(KL0(JROF,JLEV,1)+IV2L2)
      ZZ12=PXSL(KL0(JROF,JLEV,1)+IV2L0) &
       & +PCLO(JROF,JLEV,1,1)*(ZZF112-PXSL(KL0(JROF,JLEV,1)+IV2L0)) &
       & +PCLO(JROF,JLEV,2,1)*(ZZF212-PXSL(KL0(JROF,JLEV,1)+IV2L0)) &
       & +PCLO(JROF,JLEV,3,1)* &
       & (PXSL(KL0(JROF,JLEV,1)+IV2L3)-PXSL(KL0(JROF,JLEV,1)+IV2L0))
      ZZF122=PXSL(KL0(JROF,JLEV,2)+IV2L1)
      ZZF222=PXSL(KL0(JROF,JLEV,2)+IV2L2)
      ZZ22=PXSL(KL0(JROF,JLEV,2)+IV2L0) &
       & +PCLO(JROF,JLEV,1,2)*(ZZF122-PXSL(KL0(JROF,JLEV,2)+IV2L0)) &
       & +PCLO(JROF,JLEV,2,2)*(ZZF222-PXSL(KL0(JROF,JLEV,2)+IV2L0)) &
       & +PCLO(JROF,JLEV,3,2)* &
       & (PXSL(KL0(JROF,JLEV,2)+IV2L3)-PXSL(KL0(JROF,JLEV,2)+IV2L0))
      ZZ32=PXSL(KL0(JROF,JLEV,3)+IV2L1)+PDLO(JROF,JLEV,3) &
       & *(PXSL(KL0(JROF,JLEV,3)+IV2L2)-PXSL(KL0(JROF,JLEV,3)+IV2L1))
      ! interpolations in longitude, stencil level 3
      ZZ03=PXSL(KL0(JROF,JLEV,0)+IV3L1)+PDLO(JROF,JLEV,0) &
       & *(PXSL(KL0(JROF,JLEV,0)+IV3L2)-PXSL(KL0(JROF,JLEV,0)+IV3L1))
      ZZF113=PXSL(KL0(JROF,JLEV,1)+IV3L1)
      ZZF213=PXSL(KL0(JROF,JLEV,1)+IV3L2)
      ZZ13=PXSL(KL0(JROF,JLEV,1)+IV3L0) &
       & +PCLO(JROF,JLEV,1,1)*(ZZF113-PXSL(KL0(JROF,JLEV,1)+IV3L0)) &
       & +PCLO(JROF,JLEV,2,1)*(ZZF213-PXSL(KL0(JROF,JLEV,1)+IV3L0)) &
       & +PCLO(JROF,JLEV,3,1)* &
       & (PXSL(KL0(JROF,JLEV,1)+IV3L3)-PXSL(KL0(JROF,JLEV,1)+IV3L0))
      ZZF123=PXSL(KL0(JROF,JLEV,2)+IV3L1)
      ZZF223=PXSL(KL0(JROF,JLEV,2)+IV3L2)
      ZZ23=PXSL(KL0(JROF,JLEV,2)+IV3L0) &
       & +PCLO(JROF,JLEV,1,2)*(ZZF123-PXSL(KL0(JROF,JLEV,2)+IV3L0)) &
       & +PCLO(JROF,JLEV,2,2)*(ZZF223-PXSL(KL0(JROF,JLEV,2)+IV3L0)) &
       & +PCLO(JROF,JLEV,3,2)* &
       & (PXSL(KL0(JROF,JLEV,2)+IV3L3)-PXSL(KL0(JROF,JLEV,2)+IV3L0))
      ZZ33=PXSL(KL0(JROF,JLEV,3)+IV3L1)+PDLO(JROF,JLEV,3) &
       & *(PXSL(KL0(JROF,JLEV,3)+IV3L2)-PXSL(KL0(JROF,JLEV,3)+IV3L1))
      ! interpolations in longitude, stencil level 4
      ISHIFT2=MIN(0,KNOWENO(JROF,JLEV))*KSLB1
      ZZ14=PXSL(KL0(JROF,JLEV,1)+IV4L1+ISHIFT2)+PDLO(JROF,JLEV,1) &
       & *(PXSL(KL0(JROF,JLEV,1)+IV4L2+ISHIFT2)-PXSL(KL0(JROF,JLEV,1)+IV4L1+ISHIFT2))
      ZZ24=PXSL(KL0(JROF,JLEV,2)+IV4L1+ISHIFT2)+PDLO(JROF,JLEV,2) &
       & *(PXSL(KL0(JROF,JLEV,2)+IV4L2+ISHIFT2)-PXSL(KL0(JROF,JLEV,2)+IV4L1+ISHIFT2))

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

      ! interpolations in latitude, stencil levels 9, 0, 1, 2, 3, 4
      Z(JROF,0)=ZZ19+PDLAT(JROF,JLEV)*(ZZ29-ZZ19)
      Z(JROF,1)=ZZ00+PCLA(JROF,JLEV,1)*(ZZ10-ZZ00) &
       & +PCLA(JROF,JLEV,2)*(ZZ20-ZZ00) &
       & +PCLA(JROF,JLEV,3)*(ZZ30-ZZ00)
      Z(JROF,2)=ZZ01+PCLA(JROF,JLEV,1)*(ZZ11-ZZ01) &
       & +PCLA(JROF,JLEV,2)*(ZZ21-ZZ01) &
       & +PCLA(JROF,JLEV,3)*(ZZ31-ZZ01)
      Z(JROF,3)=ZZ02+PCLA(JROF,JLEV,1)*(ZZ12-ZZ02) &
       & +PCLA(JROF,JLEV,2)*(ZZ22-ZZ02) &
       & +PCLA(JROF,JLEV,3)*(ZZ32-ZZ02)
      Z(JROF,4)=ZZ03+PCLA(JROF,JLEV,1)*(ZZ13-ZZ03) &
       & +PCLA(JROF,JLEV,2)*(ZZ23-ZZ03) &
       & +PCLA(JROF,JLEV,3)*(ZZ33-ZZ03)
      Z(JROF,5)=ZZ14+PDLAT(JROF,JLEV)*(ZZ24-ZZ14)

      ! NOTE: For the consistency reasons when assembling the final quintic vertical
      !       interpolation the Z(1) and Z(4) MUST NOT BE SECURED by the QM(H) fixer.
      IF (LQMH) THEN
        ! bound Z(2):
        ZMIN=FMINJ(ZZ11,ZZ21)
        ZMAX=FMAXJ(ZZ11,ZZ21)
        Z(JROF,2)=FMAXJ(ZMIN,FMINJ(ZMAX,Z(JROF,2)))
        ! bound Z(3):
        ZMIN=FMINJ(ZZ12,ZZ22)
        ZMAX=FMAXJ(ZZ12,ZZ22)
        Z(JROF,3)=FMAXJ(ZMIN,FMINJ(ZMAX,Z(JROF,3)))
      ENDIF
    ENDDO

      ! final interpolation in vertical by WENO scheme

      ! manually unrolled code to ensure vectorization
      !    (see original JJ loop in the scalar code above)

    DO JROF=KST,KEND
      !JJ=1,J0=1
      ZFX(1)=Z(JROF,1) &
       &  + PVINTW(JROF,JLEV,1)*(Z(JROF,2)-Z(JROF,1)) &
       &  + PVINTW(JROF,JLEV,2)*(Z(JROF,3)-Z(JROF,1)) &
       &  + PVINTW(JROF,JLEV,3)*(Z(JROF,4)-Z(JROF,1))

      IF (YDDYN%LWENOBC.OR.(KNOWENO(JROF,JLEV) == 0)) THEN
        ! Skip this for the boundary areas 
        ZBETA(1)=RBETAK(1,1)*Z(JROF,4)*Z(JROF,4) +RBETAK(1,2)*Z(JROF,3)*Z(JROF,3) &
         &     + RBETAK(1,3)*Z(JROF,2)*Z(JROF,2) +RBETAK(1,4)*Z(JROF,1)*Z(JROF,1) &
         &     + RBETAK(1,5)*Z(JROF,4)*Z(JROF,3) +RBETAK(1,6)*Z(JROF,4)*Z(JROF,2) &
         &     + RBETAK(1,7)*Z(JROF,4)*Z(JROF,1) +RBETAK(1,8)*Z(JROF,3)*Z(JROF,2) &
         &     + RBETAK(1,9)*Z(JROF,3)*Z(JROF,1) +RBETAK(1,10)*Z(JROF,2)*Z(JROF,1)
        ZWW(1)=(ZBETA(1)+ZEPS)**ZALPHA ! Denominator
        ZWW(1)=SIGN(MAX(ABS(ZWW(1)),ZEPS_MAX),ZWW(1))
        ZWW(1)=PCW(JROF,JLEV,1)/ZWW(1)
        !JJ=2,J0=2
        J0=2+MIN(0,KNOWENO(JROF,JLEV))
        ZFX(2)=Z(JROF,J0) &
         &  + PVINTW(JROF,JLEV,4)*(Z(JROF,J0+1)-Z(JROF,J0)) &
         &  + PVINTW(JROF,JLEV,5)*(Z(JROF,J0+2)-Z(JROF,J0)) &
         &  + PVINTW(JROF,JLEV,6)*(Z(JROF,J0+3)-Z(JROF,J0))
        ZBETA(2)=RBETAK(2,1)*Z(JROF,J0+3)*Z(JROF,J0+3) +RBETAK(2,2)*Z(JROF,J0+2)*Z(JROF,J0+2) &
         &     + RBETAK(2,3)*Z(JROF,J0+1)*Z(JROF,J0+1) +RBETAK(2,4)*Z(JROF,J0)*Z(JROF,J0)     &
         &     + RBETAK(2,5)*Z(JROF,J0+3)*Z(JROF,J0+2) +RBETAK(2,6)*Z(JROF,J0+3)*Z(JROF,J0+1) &
         &     + RBETAK(2,7)*Z(JROF,J0+3)*Z(JROF,J0)   +RBETAK(2,8)*Z(JROF,J0+2)*Z(JROF,J0+1) &
         &     + RBETAK(2,9)*Z(JROF,J0+2)*Z(JROF,J0)   +RBETAK(2,10)*Z(JROF,J0+1)*Z(JROF,J0)
        ZWW(2)=(ZBETA(2)+ZEPS)**ZALPHA ! Denominator
        ZWW(2)=SIGN(MAX(ABS(ZWW(2)),ZEPS_MAX),ZWW(2))
        ZWW(2)=PCW(JROF,JLEV,2)/ZWW(2)
        ! JJ=3,J0=0
        J0=MAX(0,KNOWENO(JROF,JLEV))
        ZFX(3)=Z(JROF,J0) &
         &  + PVINTW(JROF,JLEV,7)*(Z(JROF,J0+1)-Z(JROF,J0)) &
         &  + PVINTW(JROF,JLEV,8)*(Z(JROF,J0+2)-Z(JROF,J0)) &
         &  + PVINTW(JROF,JLEV,9)*(Z(JROF,J0+3)-Z(JROF,J0))
        ZBETA(3)=RBETAK(3,1)*Z(JROF,J0+3)*Z(JROF,J0+3) +RBETAK(3,2)*Z(JROF,J0+2)*Z(JROF,J0+2) &
         &     + RBETAK(3,3)*Z(JROF,J0+1)*Z(JROF,J0+1) +RBETAK(3,4)*Z(JROF,J0)*Z(JROF,J0)     &
         &     + RBETAK(3,5)*Z(JROF,J0+3)*Z(JROF,J0+2) +RBETAK(3,6)*Z(JROF,J0+3)*Z(JROF,J0+1) &
         &     + RBETAK(3,7)*Z(JROF,J0+3)*Z(JROF,J0)   +RBETAK(3,8)*Z(JROF,J0+2)*Z(JROF,J0+1) &
         &     + RBETAK(3,9)*Z(JROF,J0+2)*Z(JROF,J0)   +RBETAK(3,10)*Z(JROF,J0+1)*Z(JROF,J0)
        ZWW(3)=(ZBETA(3)+ZEPS)**ZALPHA ! Denominator
        ZWW(3)=SIGN(MAX(ABS(ZWW(3)),ZEPS_MAX),ZWW(3))
        ZWW(3)=PCW(JROF,JLEV,3)/ZWW(3)

        ZSW=ZWW(1)+ZWW(2)+ZWW(3)

        ZWW(1)=ZWW(1)/ZSW 
        ZWW(2)=ZWW(2)/ZSW
        !ZWW(3)=ZWW(3)/ZSW  not used

        ZXF(JROF,JLEV)=ZWW(1)*(ZFX(1)-ZFX(3)) + ZWW(2)*(ZFX(2)-ZFX(3)) + ZFX(3)
      ELSE
        ZXF(JROF,JLEV)=ZFX(1)
      ENDIF
    ENDDO

    IF (LQMV) THEN
      DO JROF=KST,KEND
        ! bound ZXF:
        ZMIN=MIN(Z(JROF,2),Z(JROF,3))
        ZMAX=MAX(Z(JROF,2),Z(JROF,3))
        ZXF(JROF,JLEV)=MAX(ZMIN,MIN(ZMAX,ZXF(JROF,JLEV)))
      ENDDO
    ENDIF
  ENDDO
ELSE
  CALL ABOR1 ('LAITRI_WENO: UNEXPECTED CDTYPE')
ENDIF

!------------------------------------------------------------------------------
! 8 point quasi-monotone correction (Bermejo & Staniforth)
!   with conservation (when activated)

!------------------------------------------------------------------------------

! optim: option LLADD is distributed among QM cases in order to improve workload
IF (KQM == -1) THEN
  DO JLEV=1,KFLEV
    IF (JLEV == 1) THEN
      ZRVETA = 1.0_JPRB
    ELSE
      ZRVETA = PRDETAR(JLEV)
    ENDIF
    DO JROF=KST,KEND
      ! get surounding point values
      ZF(1)=PXSL(KL0(JROF,JLEV,1)+IV1L1) ! level above d.p. NW point
      ZF(2)=PXSL(KL0(JROF,JLEV,1)+IV1L2) ! level above d.p. NE point
      ZF(3)=PXSL(KL0(JROF,JLEV,2)+IV1L1) ! level above d.p. SW point
      ZF(4)=PXSL(KL0(JROF,JLEV,2)+IV1L2) ! level above d.p. SE point
      ZF(5)=PXSL(KL0(JROF,JLEV,1)+IV2L1) ! level below d.p. NW point
      ZF(6)=PXSL(KL0(JROF,JLEV,1)+IV2L2) ! level below d.p. NE point
      ZF(7)=PXSL(KL0(JROF,JLEV,2)+IV2L1) ! level below d.p. SW point
      ZF(8)=PXSL(KL0(JROF,JLEV,2)+IV2L2) ! level below d.p. SE point
      ! compute local min/max
      ZMIN=MINVAL(ZF(1:8))
      ZMAX=MAXVAL(ZF(1:8))
      ! limit & store surplus
      IF (LDQM3DCONS) THEN
        ZX=ZXF(JROF,JLEV)+ZSURPL(JROF)
        ZXF(JROF,JLEV)=MAX(ZMIN,MIN(ZMAX,ZX))
        ZSURPL(JROF)=(ZX-ZXF(JROF,JLEV))*ZRVETA
      ELSE
        ZXF(JROF,JLEV)=MAX(ZMIN,MIN(ZMAX,ZXF(JROF,JLEV)))
      ENDIF
    ENDDO

    IF (LLADD) THEN
      DO JROF=KST,KEND
        PXF(JROF,JLEV) = PXF(JROF,JLEV)+ZXF(JROF,JLEV)
      ENDDO
    ELSE
      DO JROF=KST,KEND
        PXF(JROF,JLEV) = ZXF(JROF,JLEV)
      ENDDO
    ENDIF
  ENDDO
ELSEIF (LLADD.AND.LQM0) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = PXF(JROF,JLEV)+MAX(ZXF(JROF,JLEV),0._JPRB)
    ENDDO
  ENDDO
ELSEIF (LQM0) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = MAX(ZXF(JROF,JLEV),0._JPRB)
    ENDDO
  ENDDO
ELSEIF (LLADD) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = PXF(JROF,JLEV)+ZXF(JROF,JLEV)
    ENDDO
  ENDDO
ELSE
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = ZXF(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('LAITRI_WENO',1,ZHOOK_HANDLE)

END SUBROUTINE LAITRI_WENO


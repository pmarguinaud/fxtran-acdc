SUBROUTINE LASCAW_CLATURB(YDSL,KFLEV,KPROMA,KST,KEND,KILA,PKHTURB,P3DTW,PCLA,PCLASLD)

!$ACDC singlecolumn  --process-pointers


! ------------------------------------------------------------------
! Purpose:
!   Modify PCLA and PCLASLD when 3D turbulence is active
!
! INPUT:
!   YDSL     - SL_STRUCT definition
!   KFLEV    - Vertical dimension
!   KPROMA   - horizontal dimension.
!   KST      - first element of arrays where computations are performed.
!   KEND    - depth of work.
!   LDT      - key for SLHD and horizontal turbulence.
!   KILA     - cf. ILA in LASCAW.
!   PDLAT    - distance for horizontal linear interpolations in latitude
!   PKHTURB  - horizontal exchange coefficients for 3D turbulence
!   PSLDW   - weights for SLHD Laplacian smoother in latitude
!   P3DTW   - weights for 3D turbulence Laplacian smoother in latitude
!
! OUTPUT:
!   PCLA    - weights for horizontal cubic interpolations in latitude.
!   PCLASLD - cf. PCLA, SLHD case.
!
! Author:
!   H Petithomme (Dec 2020): after lascaw_cla (original from K Yessad, 2009)
!     R. El Khatib 01-Jun-2022 Remove JPDUP
! ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIB,JPIM,JPRB
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK , JPHOOK
USE EINT_MOD , ONLY : SL_STRUCT

IMPLICIT NONE

TYPE(SL_STRUCT),    INTENT(IN)  :: YDSL
INTEGER(KIND=JPIM), INTENT(IN)  :: KFLEV
INTEGER(KIND=JPIM), INTENT(IN)  :: KPROMA
INTEGER(KIND=JPIM), INTENT(IN)  :: KST
INTEGER(KIND=JPIM), INTENT(IN)  :: KEND
INTEGER(KIND=JPIM), INTENT(IN)  :: KILA(KPROMA,KFLEV)
REAL(KIND=JPRB)   , INTENT(IN)  :: PKHTURB(KPROMA,KFLEV)
REAL(KIND=JPRB)   , INTENT(IN)  :: P3DTW(3,3,YDSL%NDGSAH:YDSL%NDGENH)
REAL(KIND=JPRB)   , INTENT(INOUT) :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   , INTENT(INOUT) :: PCLASLD(KPROMA,KFLEV,3)

INTEGER(KIND=JPIB) :: ILA64
INTEGER(KIND=JPIM) :: JROF,JLEV
REAL(KIND=JPRB) :: ZKH,ZWDS1,ZWDS2,ZWDS3

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('LASCAW_CLATURB',0,ZHOOK_HANDLE)

! apply the horizontal Laplacian to both PCLA and PCLASLD:
DO JLEV=1,KFLEV
  DO JROF=KST,KEND
    ILA64=KILA(JROF,JLEV)+1
    ZWDS1=P3DTW(1,1,ILA64)*PCLA(JROF,JLEV,1)&
     &   +P3DTW(1,2,ILA64)*PCLA(JROF,JLEV,2)&
     &   +P3DTW(1,3,ILA64)*PCLA(JROF,JLEV,3)
    ZWDS2=P3DTW(2,1,ILA64)*PCLA(JROF,JLEV,1)&
     &   +P3DTW(2,2,ILA64)*PCLA(JROF,JLEV,2)&
     &   +P3DTW(2,3,ILA64)*PCLA(JROF,JLEV,3)
    ZWDS3=P3DTW(3,1,ILA64)*PCLA(JROF,JLEV,1)&
     &   +P3DTW(3,2,ILA64)*PCLA(JROF,JLEV,2)&
     &   +P3DTW(3,3,ILA64)*PCLA(JROF,JLEV,3)

    ZKH = 2._JPRB*PKHTURB(JROF,JLEV)
    PCLA(JROF,JLEV,1)=PCLA(JROF,JLEV,1)+ZKH*(ZWDS1-PCLA(JROF,JLEV,1))
    PCLA(JROF,JLEV,2)=PCLA(JROF,JLEV,2)+ZKH*(ZWDS2-PCLA(JROF,JLEV,2))
    PCLA(JROF,JLEV,3)=PCLA(JROF,JLEV,3)+ZKH*(ZWDS3-PCLA(JROF,JLEV,3))
    ZWDS1=P3DTW(1,1,ILA64)*PCLASLD(JROF,JLEV,1)&
     &   +P3DTW(1,2,ILA64)*PCLASLD(JROF,JLEV,2)&
     &   +P3DTW(1,3,ILA64)*PCLASLD(JROF,JLEV,3)
    ZWDS2=P3DTW(2,1,ILA64)*PCLASLD(JROF,JLEV,1)&
     &   +P3DTW(2,2,ILA64)*PCLASLD(JROF,JLEV,2)&
     &   +P3DTW(2,3,ILA64)*PCLASLD(JROF,JLEV,3)
    ZWDS3=P3DTW(3,1,ILA64)*PCLASLD(JROF,JLEV,1)&
     &   +P3DTW(3,2,ILA64)*PCLASLD(JROF,JLEV,2)&
     &   +P3DTW(3,3,ILA64)*PCLASLD(JROF,JLEV,3)
    PCLASLD(JROF,JLEV,1)=PCLASLD(JROF,JLEV,1)+ZKH*(ZWDS1-PCLASLD(JROF,JLEV,1))
    PCLASLD(JROF,JLEV,2)=PCLASLD(JROF,JLEV,2)+ZKH*(ZWDS2-PCLASLD(JROF,JLEV,2))
    PCLASLD(JROF,JLEV,3)=PCLASLD(JROF,JLEV,3)+ZKH*(ZWDS3-PCLASLD(JROF,JLEV,3))
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK('LASCAW_CLATURB',1,ZHOOK_HANDLE)
END SUBROUTINE LASCAW_CLATURB

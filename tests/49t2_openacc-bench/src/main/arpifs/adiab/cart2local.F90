SUBROUTINE CART2LOCAL(KFLEV,KPROMA,KST,KEND,PGEMU,PGSQM2,PGECLO,PGESLO,PX,PY,PZ,PU,PV)

!$ACDC singlecolumn  --process-pointers

! Purpose:
!  convert from Cartesian repere to grid-point local repere
!
! Input:
!  PGEMU/GSQM2/GECLO/GESLO: geographical coordinates of grid-points
!  PX/PY/PZ: Cartesian 'geocentric' components of horizontal wind
!
! Output:
!  PU/PV: 'Local' U/V components of horizontal wind (ie local repere of each grid-point)
!
! Author:
!  H Petithomme, Dec 2023: extracted from larmes/larcinb

USE PARKIND1, ONLY: JPIM, JPRB
USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM) ,INTENT(IN)   :: KFLEV,KPROMA,KST,KEND
REAL(KIND=JPRB)    ,INTENT(IN)   :: PGEMU(KPROMA),PGSQM2(KPROMA),PGECLO(KPROMA),PGESLO(KPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)   :: PX(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)   :: PY(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)   :: PZ(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)  :: PU(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)  :: PV(KPROMA,KFLEV)

INTEGER(KIND=JPIM) :: JLEV,JROF
REAL(KIND=JPRB) :: ZX,ZY
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK("CART2LOCAL",0,ZHOOK_HANDLE)

! warning: ZX/ZY are used to protect against aliasing (X/Y aliased to U/V)
! both ZX and ZY are needed in case of aliasing with swapping of U and V
DO JLEV=1,KFLEV
  DO JROF=KST,KEND
    ZX = PX(JROF,JLEV)
    ZY = PY(JROF,JLEV)
    PU(JROF,JLEV)=ZY*PGECLO(JROF)-ZX*PGESLO(JROF)
    PV(JROF,JLEV)=-ZX*PGECLO(JROF)*PGEMU(JROF)-ZY*PGESLO(JROF)*PGEMU(JROF)+PZ(JROF,JLEV)*PGSQM2(JROF)
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK("CART2LOCAL",1,ZHOOK_HANDLE)
END SUBROUTINE CART2LOCAL


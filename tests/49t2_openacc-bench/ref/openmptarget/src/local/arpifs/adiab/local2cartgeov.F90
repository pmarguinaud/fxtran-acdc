SUBROUTINE LOCAL2CARTGEOV(KL1,KFLEV,KPROMA,KST,KEND,PGEMU,PGSQM2,PGECLO,PGESLO,PU,PV,PZ)

!$ACDC singlecolumn  --process-pointers

! Purpose:
!  convert from grid-point local repere to Cartesian repere
!
! Input:
!  PGEMU/GSQM2/GECLO/GESLO: geographical coordinates of grid-points
!  KL1: lower bound of wind arrays
!
! Input/output:
!  PU/PV (in): 'Local' U/V components of horizontal wind (ie local repere of each GP)
!  PU/PV/PZ (out): Cartesian 'geocentric' components of horizontal wind (X/Y/Z axes)
!
! Remark:
!  arrays can come from ZB1 or ZB2, hence true dimensions are 0:NFLEVG+1 or 1:NFLEVG
!  their lower bound is given by KL1, their upper bound is KFLEV (= NFLEVG, whatever case)
!  however, computations are always 1:KFLEV (= 1:NFLEVG)
!
! Author:
!  H Petithomme, Dec 2023: extracted from lavent

USE PARKIND1, ONLY: JPIM, JPRB
USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM) ,INTENT(IN)     :: KL1,KFLEV,KPROMA,KST,KEND
REAL(KIND=JPRB)    ,INTENT(IN)     :: PGEMU(KPROMA),PGSQM2(KPROMA),PGECLO(KPROMA),PGESLO(KPROMA)
REAL(KIND=JPRB)    ,INTENT(INOUT)  :: PU(KPROMA,KL1:KFLEV)
REAL(KIND=JPRB)    ,INTENT(INOUT)  :: PV(KPROMA,KL1:KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)    :: PZ(KPROMA,KL1:KFLEV)

INTEGER(KIND=JPIM) :: JLEV,JROF
REAL(KIND=JPRB) :: ZU,ZV
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK("LOCAL2CARTGEOV",0,ZHOOK_HANDLE)

! both ZU and ZV are needed in case of aliasing with swapping of U and V
DO JLEV=1,KFLEV
  DO JROF=KST,KEND
    ZU = PU(JROF,JLEV)
    ZV = PV(JROF,JLEV)
    PU(JROF,JLEV)=-ZU*PGESLO(JROF)-ZV*PGECLO(JROF)*PGEMU(JROF)
    PV(JROF,JLEV)=ZU*PGECLO(JROF)-ZV*PGESLO(JROF)*PGEMU(JROF)
    PZ(JROF,JLEV)=ZV*PGSQM2(JROF)
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK("LOCAL2CARTGEOV",1,ZHOOK_HANDLE)
END SUBROUTINE LOCAL2CARTGEOV


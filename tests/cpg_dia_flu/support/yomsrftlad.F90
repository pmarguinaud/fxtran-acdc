MODULE YOMSRFTLAD

USE PARKIND1  ,ONLY : JPRB,JPIM

IMPLICIT NONE

SAVE

! ------ SKIN TEMPERATURE FOR LINEARIZED PHYSICS

! GPSURF  : BUFFER FOR PERTURBATION OF TOP LAYER SURF. FIELDS 
!           (SKIN TEMPERATURE,...)
! NGSKIN  : NUMBER OF TOP SOIL FIELDS FOR PERTURBATION

! LREGSF  : .TRUE. if the regularization for SURF computation is used

TYPE :: TSRFTLAD
REAL(KIND=JPRB),ALLOCATABLE :: GPTSKIN0(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: GPTSKIN9(:,:,:)

INTEGER(KIND=JPIM) :: NGSKIN

LOGICAL :: LREGSF
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TSRFTLAD
!============================================================================

!!TYPE(TSRFTLAD), POINTER :: YRSRFTLAD => NULL()

!     ------------------------------------------------------------------

CONTAINS



END MODULE YOMSRFTLAD

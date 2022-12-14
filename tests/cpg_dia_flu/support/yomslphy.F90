MODULE YOMSLPHY

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

REAL(KIND=JPRB), PARAMETER :: RSLWX=0.5_JPRB

TYPE :: TSLPHY

!     ------------------------------------------------------------------

! * Variables for split ECMWF physics.

! RSLWX   : level of implicitness of semi-Lagrangian/physics.
! NVTEND  : third dimension of SAVTEND (number of 3D fields).
! SAVTEND : buffer to store the physical tendencies.

INTEGER(KIND=JPIM) :: NVTEND
REAL(KIND=JPRB),ALLOCATABLE :: SAVTEND(:,:,:,:)
! Pointers for SAVTEND
INTEGER(KIND=JPIM) :: MU_SAVTEND,MV_SAVTEND,MT_SAVTEND,MSAT_SAVTEND
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TSLPHY
!============================================================================
!TYPE(TSLPHY), POINTER :: YRSLPHY => NULL()

CONTAINS



END MODULE YOMSLPHY

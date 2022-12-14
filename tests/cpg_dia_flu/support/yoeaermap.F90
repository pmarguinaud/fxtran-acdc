MODULE YOEAERMAP

USE PARKIND1  ,ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERMAP* - GEOGRAPHICALLY DEPENDENT PARAMETERS
!     ------------------------------------------------------------------

TYPE :: TEAERMAP
INTEGER(KIND=JPIM) :: NDUSRCP(50)

REAL(KIND=JPRB)    :: RDDUAER(50)
REAL(KIND=JPRB)    :: RDUSRCP(10,2)
!---------------------------------------------------------------------
CONTAINS
   
END TYPE TEAERMAP
!=====================================================================

!!TYPE(TEAERMAP), POINTER :: YREAERMAP => NULL()

!     ------------------------------------------------------------------
! NDUSRCP ! INTEGER ! INDEX FOR DUST SOURCE TYPE
! RDUSRCP : REAL    ! 1: DUST SOURCE-RELATED REFERENCE THRESHOLD SPEED
! RDUSRCP : REAL    ! 2: DUST SOURCE-RELATED REFERENCE PARTICLE RADIUS
! RDDUAER ! REAL    ! DUST EMISSION POTENTIAL SCALING FACTOR
!     ------------------------------------------------------------------

CONTAINS



END MODULE YOEAERMAP




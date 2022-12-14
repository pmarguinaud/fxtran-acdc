MODULE YOMVSPLIP

USE PARKIND1 , ONLY : JPRB
IMPLICIT NONE

SAVE

! * Variables related to use of spline cubic vertical interpolations.
! RVSPTRI,RVSPC      : are used to re-profile the field to be interpolated
!                      in routine VSPLTRANS.
! RFVV: is used in the computation of the vertical weights.

TYPE TVSPLIP
REAL(KIND=JPRB),ALLOCATABLE :: RVSPTRI(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: RVSPC(:)
REAL(KIND=JPRB),ALLOCATABLE :: RFVV(:,:,:)
END TYPE TVSPLIP

END MODULE YOMVSPLIP

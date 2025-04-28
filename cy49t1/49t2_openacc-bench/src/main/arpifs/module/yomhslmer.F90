MODULE YOMHSLMER

USE PARKIND1 , ONLY : JPRB, JPRD
IMPLICIT NONE

SAVE

! * Horizontal geometry-dependent intermediate quantities used to
!   compute horizontal weights in the interpolator.
! RIPI        : bi-cubic interpolation coefficients: RIPI(.,j)=RIPI[j-1](.)
! RSLD        : auxiliary quantity for SLHD interpolation weights: RSLD(.,j)=RSLD[j](.)
! RSLDW       : weights for SLHD  Laplacian smoother in latitude
! R3DTW       : weights for 3D turb. Laplacian smoother in latitude

TYPE THSLMER
REAL(KIND=JPRD),ALLOCATABLE:: RIPI(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RSLD(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RSLDW(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: R3DTW(:,:,:)
END TYPE THSLMER

!!TYPE(THSLMER), POINTER :: YRHSLMER => NULL()

END MODULE YOMHSLMER

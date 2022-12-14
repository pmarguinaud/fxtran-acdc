MODULE YOMDIMF

USE PARKIND1 , ONLY : JPIM

IMPLICIT NONE

SAVE

TYPE :: TDIMF

! === NUMBER OF FIELDS ========================================================

! NFTHER : number of spectral thermodynamic variables
! NF3D   = number of 3D fields in the state of the model
! NFD2D  : number of 2D fields in the dynamics
! NFC2D  : number of 2D fields in the boundaries
! NPPM   : Number of interpolation methods in post-processing
! NS3D   : number of 3D fields in spectral space
! NS2D   : number of 2D fields in spectral space
! NS1D   : number of 1D fields in spectral space (for Aladin consistency -CF)
! NGRBSP2, NGRBSP3 - Grib codes for fields in SPA2 and SPA3

INTEGER(KIND=JPIM) :: NFTHER
INTEGER(KIND=JPIM) :: NF3D
INTEGER(KIND=JPIM) :: NFD2D
INTEGER(KIND=JPIM) :: NFC2D
!!INTEGER(KIND=JPIM) :: NPPM  !! moved to PARDIM in CY45
INTEGER(KIND=JPIM) :: NS3D
INTEGER(KIND=JPIM) :: NS2D
INTEGER(KIND=JPIM) :: NS1D
INTEGER(KIND=JPIM),ALLOCATABLE :: NGRBSP3(:)
INTEGER(KIND=JPIM),ALLOCATABLE :: NGRBSP2(:)


! === GRID POINT ARRAYS =======================================================

! LVOR  : controls the allocation of vorticity
! LADER : controls the allocation of vor div and derivatives
! LUVDER: controls the allocation of derivatives for u and v
! LSPT  : .TRUE. if temperature variable as spectral field

LOGICAL :: LVOR
LOGICAL :: LADER
LOGICAL :: LUVDER
LOGICAL :: LSPT
 
CONTAINS
  
   

END TYPE TDIMF

CONTAINS 
  



!!TYPE(TDIMF), POINTER :: YRDIMF => NULL()

!     ------------------------------------------------------------------
END MODULE YOMDIMF

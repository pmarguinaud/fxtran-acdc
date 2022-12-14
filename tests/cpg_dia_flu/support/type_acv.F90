MODULE TYPE_ACV

!     Purpose.
!     --------
!       Data and controls for augmented control variable

!     Author.
!     -------
!       S. Massart

!     Modifications.
!     --------------
!       Original    April-2018
!       S. Massart 19-Feb-2019 : Addition of more types
! ------------------------------------------------------------------


USE PARKIND1, ONLY: JPIM, JPRB

IMPLICIT NONE

SAVE

! ------------------------------------------------------------------

TYPE, PUBLIC :: TACVDIM
  !   ------------------------------------------------------------------
  !   LACV_1D     True to add  1D fields in the control variable
  !   LACV_2D     True to add  2D fields in the control variable
  !   LACV_3D     True to add  3D fields in the control variable
  !
  !   NACV_1D     Number of 1D fields to add 
  !   NACVSP_2D   Number of 2D spectral fields 
  !   NACVGP_2D   Number of 2D gird point fields 
  !   NACV_2D     Number of 2D fields (spectral + grib points)
  !   NACVSP_3D   Number of 3D spectral fields 
  !   NACVGP_3D   Number of 3D gird point fields 
  !   NACV_3D     Number of 3D fields (spectral + grib points)
  !   ------------------------------------------------------------------
  LOGICAL              :: LACV_1D
  LOGICAL              :: LACV_2D
  LOGICAL              :: LACV_3D
  INTEGER(KIND=JPIM)   :: NACV_1D
  INTEGER(KIND=JPIM)   :: NACVSP_2D
  INTEGER(KIND=JPIM)   :: NACVGP_2D
  INTEGER(KIND=JPIM)   :: NACV_2D
  INTEGER(KIND=JPIM)   :: NACVSP_3D
  INTEGER(KIND=JPIM)   :: NACVGP_3D
  INTEGER(KIND=JPIM)   :: NACV_3D
  INTEGER(KIND=JPIM)   :: NACV_ALL
END TYPE TACVDIM

TYPE TACV_CONFIG_BASE
  !   ------------------------------------------------------------------
  !   L_IN_1D     True if in 1D 
  !   L_IN_2D_SP  True if in spectral 2D
  !   L_IN_2D_GP  True if in grid point 2D
  !   L_IN_3D_SP  True if in spectral 3D
  !   L_IN_3D_GP  True if in grid point 3D
  !   CSETDESC    Description of each variable
  !
  !   ------------------------------------------------------------------
  LOGICAL              :: L_IN_1D
  LOGICAL              :: L_IN_2D_SP
  LOGICAL              :: L_IN_2D_GP
  LOGICAL              :: L_IN_3D_SP
  LOGICAL              :: L_IN_3D_GP
  CHARACTER(LEN =20)   :: CSETDESC
END TYPE TACV_CONFIG_BASE

TYPE TACV_CONFIG
  !   ------------------------------------------------------------------
  !   NACV        Total number of ACV 
  !   YRCONFIG    Configuration array
  !
  !   ------------------------------------------------------------------
  INTEGER(KIND=JPIM)   :: NACV
  TYPE(TACV_CONFIG_BASE), ALLOCATABLE :: YRCONFIG(:)
END TYPE TACV_CONFIG

TYPE, PUBLIC :: TACVGRIB
  !   ------------------------------------------------------------------
  !   MACVGRB_1D   Array with the grib numbers of the 1D fields
  !   MACVGRB_2D   Array with the grib numbers of the 2D fields (first SP and then GP)
  !   MACVGRB_2D   Array with the grib numbers of the 3D fields (first SP and then GP)
  !   MACVGRB_ALL  Array with the grib numbers of the all fields (1D + 2D + 3D)
  !   ------------------------------------------------------------------
  INTEGER(KIND=JPIM), ALLOCATABLE :: MACVGRB_1D(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: MACVGRB_2D(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: MACVGRB_3D(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: MACVGRB_ALL(:)
END TYPE TACVGRIB

TYPE, PUBLIC :: ACV_CONTAINER
  !     ------------------------------------------------------------------
  !   YDACV1D     ACV 1D type 
  !   RACV1D      ACV 1D fields
  !   RSPACV2D    ACV 2D spectral fields
  !   RGPACV2D    ACV 2D grid point fields
  !   RSPACV3D    ACV 3D spectral fields
  !   RGPACV3D    ACV 3D grid point fields
  !     ------------------------------------------------------------------
  REAL(KIND=JPRB), ALLOCATABLE :: RACV1D(:)
  REAL(KIND=JPRB), ALLOCATABLE :: RSPACV2D(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: RGPACV2D(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: RSPACV3D(:,:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: RGPACV3D(:,:,:)
END TYPE ACV_CONTAINER

!-----------------------------------------------------------------------
CONTAINS
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------



! ------------------------------------------------------------------



END MODULE TYPE_ACV

MODULE YOMVSLETA

!$ACDC methods 


USE PARKIND1 , ONLY : JPIM, JPRB
IMPLICIT NONE

SAVE

!*    * Vertical geometry-dependent intermediate quantities used to
!       compute vertical weights in the interpolator.
TYPE TVSLETA
  !   VCUICO: is used to compute denominators of weights
  !           for vertical interpolations
  !           applied to full-level variables.
  REAL(KIND=JPRB), ALLOCATABLE :: VCUICO(:,:)
  !   VCUICOH:is used to compute denominators of weights 
  !           for vertical interpolations
  !           applied to half-level variables.
  REAL(KIND=JPRB), ALLOCATABLE :: VCUICOH(:,:)
  !   VSLD  : auxiliary quantity for SLHD interpolation weights on full levels
  REAL(KIND=JPRB), ALLOCATABLE :: VSLD(:,:)
  !   VSLDH : auxiliary quantity for SLHD interpolation weights on half levels
  REAL(KIND=JPRB), ALLOCATABLE :: VSLDH(:,:)
  !   VSLDW : weights for SLHD vertical Laplacian smoother on full levels
  REAL(KIND=JPRB), ALLOCATABLE :: VSLDW(:,:,:)
  !   VSLDWH: weights for SLHD vertical Laplacian smoother on half levels
  REAL(KIND=JPRB), ALLOCATABLE :: VSLDWH(:,:,:)
  !   VSLVF : weights for SLVF vertical Laplacian smoother on full levels
  REAL(KIND=JPRB), ALLOCATABLE :: VSLVF(:,:,:)
  !   VSLVFH: weights for SLVF vertical Laplacian smoother on half levels
  REAL(KIND=JPRB), ALLOCATABLE :: VSLVFH(:,:,:)
  !   GAMMA_WENO: weights for vertical WENO interpolations determined from
  !               the polynomial identity
  REAL(KIND=JPRB), ALLOCATABLE :: GAMMA_WENO(:,:)
  !   VRDETAR: ratio (eta(lbar)-eta(lbar-1))/(eta(lbar-1)-eta(lbar-2)),
  !            is used in the interpolation routine LAITVSPCQM to
  !            ensure monotonicity and conservation properties.
  REAL(KIND=JPRB), ALLOCATABLE :: VRDETAR(:)
  !   NRLEVX: used to dimension NVAUTF and NVAUTH.
  INTEGER(KIND=JPIM)      :: NRLEVX = 1
  !   VRLEVX: REAL(NRLEVX)
  REAL(KIND=JPRB)         :: VRLEVX = 1.0_JPRB
  !   NVAUTF: NVAUTF(VRLEVX*eta) is the number of the layer (full level)
  !           immediately above "eta", and is bounded by 1 and nflevg-1.
  INTEGER(KIND=JPIM), ALLOCATABLE:: NVAUTF(:)
  !   NVAUTH: NVAUTH(VRLEVX*eta) is the number of the interlayer (half level)
  !           immediately above "eta", and is bounded by 0 and nflevg-1.
  INTEGER(KIND=JPIM), ALLOCATABLE :: NVAUTH(:)
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TVSLETA
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TVSLETA
PROCEDURE :: ACDC_HOST => ACDC_HOST_TVSLETA
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TVSLETA
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TVSLETA
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TVSLETA
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TVSLETA
END TYPE TVSLETA

!!TYPE(TVSLETA), POINTER :: YRVSLETA => NULL()

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TVSLETA (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TVSLETA), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TVSLETA (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TVSLETA), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TVSLETA (SELF)

IMPLICIT NONE
CLASS (TVSLETA), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TVSLETA (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TVSLETA), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TVSLETA (SELF, KLUN)

IMPLICIT NONE
CLASS (TVSLETA), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TVSLETA (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TVSLETA),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TVSLETA (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TVSLETA), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE YOMVSLETA

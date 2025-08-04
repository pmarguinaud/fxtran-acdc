MODULE YOMNRTAER

!$ACDC methods 

USE PARKIND1,ONLY : JPRB

IMPLICIT NONE

SAVE

!     --------------------------------------------------------------------
!*    *YOMNRTAER* - definition of variables for near real time aerosols
!     D. Martin-Perez 
!     --------------------------------------------------------------------

TYPE TNRTAER
!*
!     ------------------------------------------------------------------

!     VARIABLES to use with NRT aerosols:

LOGICAL :: LCAMS_NRT           ! Near real time aerosols from CAMS
LOGICAL :: LMOCA_NRT           ! Near real time aerosols from MOCAGE (not implemented yet)

LOGICAL :: LAEIFN              ! Switch for Ice nuclei
LOGICAL :: LAERDRDEP           ! Switch for activation of aerosol dry deposition
LOGICAL :: LAERSSEM            ! Switch for Sea salt emission.
LOGICAL :: LAECCN2CLDR         ! Switch for CCN to cloud droplet conversion

REAL(KIND=JPRB) :: SSMINLO     ! In cloud supersaturation for CCN calculation near the surface
REAL(KIND=JPRB) :: SSMINUP     ! In cloud supersaturation for CCN calculation at upper levels
REAL(KIND=JPRB) :: SSMAX       ! Maximum supersaturation
REAL(KIND=JPRB) :: SSHEIGHT    ! Above SSHEIGHT the minimum SS is SSMINUP
REAL(KIND=JPRB) :: SSFACVV     ! Factor for vertical velocity term of SS
REAL(KIND=JPRB) :: SSFACSS     ! Factor for coarse sea salt reduction of SS

REAL(KIND=JPRB) :: RPANMIN     ! Minimum particle number concentration (m-3)
REAL(KIND=JPRB) :: RCCNMIN     ! Minimum CCN concentration inside clouds (m-3)
REAL(KIND=JPRB) :: RCLDROPMIN  ! Minimum cloud droplet number concentration inside cloud (m-3)
REAL(KIND=JPRB) :: RIFNMINSIZE ! Minimum IFN size (micrometers)

CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TNRTAER
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TNRTAER
PROCEDURE :: ACDC_HOST => ACDC_HOST_TNRTAER
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TNRTAER
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TNRTAER
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TNRTAER
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TNRTAER
END TYPE TNRTAER

!     ------------------------------------------------------------------
INTERFACE

MODULE SUBROUTINE ACDC_COPY_TNRTAER (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TNRTAER), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TNRTAER (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TNRTAER), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TNRTAER (SELF)

IMPLICIT NONE
CLASS (TNRTAER), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TNRTAER (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TNRTAER), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TNRTAER (SELF, KLUN)

IMPLICIT NONE
CLASS (TNRTAER), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TNRTAER (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TNRTAER),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TNRTAER (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TNRTAER), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE YOMNRTAER

MODULE MODD_PHYEX

!$ACDC methods 

!
!> @file 
!!    MODD_PHYEX - decalration of the PHYEX structure gathering all the parametrisation strucutres of PHYEX
!! 
!!    PURPOSE 
!!    ------- 
!!       The purpose of this declarative module is to declare the 
!!       the PHYEX type that allows to gather all the different structures used
!!       by the paramteriation available in PHYEX
!! 
!!    AUTHOR
!!    ------
!!      S. Riette
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    Mar 2023
!!
!-------------------------------------------------------------------------------
!
USE MODD_CST, ONLY: CST_t
USE MODD_PARAM_ICE_n, ONLY: PARAM_ICE_t
USE MODD_RAIN_ICE_DESCR_n, ONLY: RAIN_ICE_DESCR_t
USE MODD_RAIN_ICE_PARAM_n, ONLY: RAIN_ICE_PARAM_t
USE MODD_CLOUDPAR_N, ONLY: CLOUDPAR_t
USE MODD_PARAM_MFSHALL_N, ONLY: PARAM_MFSHALL_t
USE MODD_TURB_n, ONLY: TURB_t
USE MODD_CTURB, ONLY: CSTURB_t
USE MODD_NEB_n, ONLY: NEB_t
USE MODD_PARAM_LIMA, ONLY: PARAM_LIMA_t
USE MODD_PARAM_LIMA_WARM, ONLY: PARAM_LIMA_WARM_t
USE MODD_PARAM_LIMA_COLD, ONLY: PARAM_LIMA_COLD_t
USE MODD_PARAM_LIMA_MIXED, ONLY: PARAM_LIMA_MIXED_t
USE MODD_NSV, ONLY: NSV_t
USE MODD_MISC, ONLY: MISC_t
!
IMPLICIT NONE
!
TYPE PHYEX_t
  ! Structures for the different parametrisations
  TYPE(CST_t)            :: CST              !< Physical constants
  TYPE(PARAM_ICE_t)      :: PARAM_ICEN       !< Control parameters for microphysics
  TYPE(RAIN_ICE_DESCR_t) :: RAIN_ICE_DESCRN  !< Microphysical descriptive constants
  TYPE(RAIN_ICE_PARAM_t) :: RAIN_ICE_PARAMN  !< Microphysical factors
  TYPE(CLOUDPAR_t)       :: CLOUDPARN        !< Some other microphysical values
  TYPE(PARAM_MFSHALL_t)  :: PARAM_MFSHALLN   !< Mass flux scheme free parameters
  TYPE(CSTURB_t)         :: CSTURB           !< Turbulence scheme constants
  TYPE(TURB_t)           :: TURBN            !< Turbulence scheme constants set by namelist
  TYPE(NEB_t)            :: NEBN             !< Cloud scheme constants
  TYPE(PARAM_LIMA_t)     :: PARAM_LIMA       !< Control parameters for LIMA microphysics
  TYPE(PARAM_LIMA_WARM_t):: PARAM_LIMA_WARM  !< Microphysical factors for LIMA (warm processes)
  TYPE(PARAM_LIMA_COLD_t):: PARAM_LIMA_COLD  !< Microphysical factors for LIMA (cold processes)
  TYPE(PARAM_LIMA_MIXED_t):: PARAM_LIMA_MIXED !< Microphysical factors for LIMA (mixed processes)
  TYPE(NSV_t)            :: TNSV             !< NSV indexes
  !
  ! Supplementary strucuture to hold model specific values
  TYPE(MISC_t)           :: MISC             !< Model specific values
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_PHYEX_T
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_PHYEX_T
PROCEDURE :: ACDC_HOST => ACDC_HOST_PHYEX_T
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_PHYEX_T
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_PHYEX_T
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_PHYEX_T
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_PHYEX_T
END TYPE PHYEX_t
!
INTERFACE

MODULE SUBROUTINE ACDC_COPY_PHYEX_T (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (PHYEX_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_PHYEX_T (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (PHYEX_T), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_PHYEX_T (SELF)

IMPLICIT NONE
CLASS (PHYEX_T), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_PHYEX_T (SELF, KLUN)

IMPLICIT NONE
CLASS (PHYEX_T), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_PHYEX_T (SELF, KLUN)

IMPLICIT NONE
CLASS (PHYEX_T), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_PHYEX_T (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (PHYEX_T),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_PHYEX_T (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (PHYEX_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE MODD_PHYEX

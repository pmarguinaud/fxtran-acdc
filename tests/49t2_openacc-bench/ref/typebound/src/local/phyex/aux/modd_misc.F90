MODULE MODD_MISC

!$ACDC methods  --skip-components MISC_T%TBUCONF

!
USE MODD_BUDGET, ONLY: TBUDGETCONF_t
IMPLICIT NONE
! 
!> @file  
!!    MODD_MISC - definition of a structure containing all the control parameters for arome
!!
!!    This is a structure specifically built for arome.
!!    All the constants needed to call the parametrisations in arome which are not
!!    contained in the different structures entering the parametrisation are put here.
!!    These keys are mainly in modd_conf in mesonh.
TYPE MISC_t
  !These values are not (yet) tuneable in arome.
  LOGICAL                        :: LMFCONV=.TRUE.       !< Use convective mass flux in the condensation scheme
  LOGICAL                        :: OCOMPUTE_SRC=.TRUE.  !< Compute s'r'
  INTEGER                        :: KMI=1                !< Model numer
  INTEGER                        :: KSPLIT=1             !< Number of small timestep for the turbulence scheme
  INTEGER                        :: KHALO=1              !< Size of the halo for parallel distribution (used in turb)
  CHARACTER(LEN=6)               :: CPROGRAM='AROME'     !< Name of the model
  LOGICAL                        :: ONOMIXLG=.FALSE.     !< Turbulence for lagrangian variables
  LOGICAL                        :: OOCEAN=.FALSE.       !< Ocean version of the turbulence scheme
  LOGICAL                        :: ODEEPOC=.FALSE.      !< Ocean version of the turbulence scheme
  LOGICAL                        :: OCOUPLES=.FALSE.     !< Ocean-atmo LES interactive coupling
  LOGICAL                        :: OBLOWSNOW=.FALSE.    !< Blowsnow
  REAL                           :: XRSNOW=1.            !< Blowing factor
  CHARACTER(LEN=4), DIMENSION(2) :: HLBCX='CYCL'         !< Boundary condition
  CHARACTER(LEN=4), DIMENSION(2) :: HLBCY='CYCL'         !< Boundary condition
  LOGICAL                        :: OIBM=.FALSE.         !< Run with IBM
  LOGICAL                        :: OFLYER=.FALSE.       !< MesoNH flyer diagnostic
  REAL                           :: XCEI_MAX=1.          !< Turbulence at cloud edges
  REAL                           :: XCEI_MIN=0           !< Turbulence at cloud edges.
  REAL                           :: XCOEF_AMPL_SAT=0     !< Turbulence at cloud edges.
  LOGICAL                        :: ODIAG_IN_RUN=.FALSE. !< LES diagnostics
  CHARACTER(LEN=4)               :: HTURBLEN_CL='NONE'   !< Turbulence length in clouds
  LOGICAL                        :: O2D=.FALSE.          !< 2D version of the turbulence

  !These values are computed from the model setup
  LOGICAL                        :: OFLAT                !< Flat configuration

  !Budget configuration
  TYPE(TBUDGETCONF_t)            :: TBUCONF              !< Budget configuration
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_MISC_T
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_MISC_T
PROCEDURE :: ACDC_HOST => ACDC_HOST_MISC_T
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_MISC_T
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_MISC_T
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_MISC_T
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_MISC_T
END TYPE MISC_t
INTERFACE

MODULE SUBROUTINE ACDC_COPY_MISC_T (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (MISC_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_MISC_T (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (MISC_T), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_MISC_T (SELF)

IMPLICIT NONE
CLASS (MISC_T), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_MISC_T (SELF, KLUN)

IMPLICIT NONE
CLASS (MISC_T), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_MISC_T (SELF, KLUN)

IMPLICIT NONE
CLASS (MISC_T), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_MISC_T (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (MISC_T),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_MISC_T (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (MISC_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE MODD_MISC

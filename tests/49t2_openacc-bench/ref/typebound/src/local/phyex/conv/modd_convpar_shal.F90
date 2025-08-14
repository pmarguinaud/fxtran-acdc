!     ######spl
      MODULE MODD_CONVPAR_SHAL

!$ACDC methods 

!     ########################
!
!!****  *MODD_CONVPAR_SHAL* - Declaration of convection constants
!!
!!    PURPOSE
!!    -------
!!      The purpose of this declarative module is to declare  the
!!      constants in the deep convection parameterization.
!!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_CONVPAR_SHAL)
!!
!!    AUTHOR
!!    ------
!!      P. Bechtold   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/96
!!   Last modified  04/10/98
!!      E. Bazile   05/05/09
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
TYPE CONVPAR_SHAL
REAL :: XA25        ! 25 km x 25 km reference grid area
!
REAL :: XCRAD       ! cloud radius
REAL :: XCTIME_SHAL ! convective adjustment time
REAL :: XCDEPTH     ! minimum necessary cloud depth
REAL :: XCDEPTH_D   ! maximum allowed cloud thickness
REAL :: XDTPERT     ! add small Temp perturb. at LCL
REAL :: XATPERT     ! Parameter for temp Perturb
REAL :: XBTPERT     ! Parameter for temp Perturb
                    ! (XATPERT* TKE/Cp + XBTPERT) * XDTPERT
REAL :: XENTR       ! entrainment constant (m/Pa) = 0.2 (m)
!
REAL :: XZLCL       ! maximum allowed allowed height
                    ! difference between departure level and surface
REAL :: XZPBL       ! minimum mixed layer depth to sustain convection
REAL :: XWTRIG      ! constant in vertical velocity trigger
!
!
REAL :: XNHGAM      ! accounts for non-hydrost. pressure
                    ! in buoyancy term of w equation
                    ! = 2 / (1+gamma)
REAL :: XTFRZ1      ! begin of freezing interval
REAL :: XTFRZ2      ! end of freezing interval
!
!
REAL :: XSTABT      ! factor to assure stability in  fractional time
                    ! integration, routine CONVECT_CLOSURE
REAL :: XSTABC      ! factor to assure stability in CAPE adjustment,
                    !  routine CONVECT_CLOSURE
REAL :: XAW,XBW     ! Parameters for WLCL = XAW * W + XBW
LOGICAL :: LLSMOOTH ! Default=TRUE but not necessary
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_CONVPAR_SHAL
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_CONVPAR_SHAL
PROCEDURE :: ACDC_HOST => ACDC_HOST_CONVPAR_SHAL
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_CONVPAR_SHAL
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_CONVPAR_SHAL
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_CONVPAR_SHAL
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_CONVPAR_SHAL
END TYPE CONVPAR_SHAL
!Keep global variables for parts of the code not ported to the type ye
REAL :: XA25        ! 25 km x 25 km reference grid area
!
REAL :: XCRAD       ! cloud radius
REAL :: XCTIME_SHAL ! convective adjustment time
REAL :: XCDEPTH     ! minimum necessary cloud depth
REAL :: XCDEPTH_D   ! maximum allowed cloud thickness
REAL :: XDTPERT     ! add small Temp perturb. at LCL
REAL :: XATPERT     ! Parameter for temp Perturb
REAL :: XBTPERT     ! Parameter for temp Perturb
                    ! (XATPERT* TKE/Cp + XBTPERT) * XDTPERT
REAL :: XENTR       ! entrainment constant (m/Pa) = 0.2 (m)
!
REAL :: XZLCL       ! maximum allowed allowed height
                    ! difference between departure level and surface
REAL :: XZPBL       ! minimum mixed layer depth to sustain convection
REAL :: XWTRIG      ! constant in vertical velocity trigger
!
!
REAL :: XNHGAM      ! accounts for non-hydrost. pressure
                    ! in buoyancy term of w equation
                    ! = 2 / (1+gamma)
REAL :: XTFRZ1      ! begin of freezing interval
REAL :: XTFRZ2      ! end of freezing interval
!
!
REAL :: XSTABT      ! factor to assure stability in  fractional time
                    ! integration, routine CONVECT_CLOSURE
REAL :: XSTABC      ! factor to assure stability in CAPE adjustment,
                    !  routine CONVECT_CLOSURE
REAL :: XAW,XBW     ! Parameters for WLCL = XAW * W + XBW
LOGICAL :: LLSMOOTH ! Default=TRUE but not necessary
!
INTERFACE

MODULE SUBROUTINE ACDC_COPY_CONVPAR_SHAL (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (CONVPAR_SHAL), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_CONVPAR_SHAL (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (CONVPAR_SHAL), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_CONVPAR_SHAL (SELF)

IMPLICIT NONE
CLASS (CONVPAR_SHAL), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_CONVPAR_SHAL (SELF, KLUN)

IMPLICIT NONE
CLASS (CONVPAR_SHAL), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_CONVPAR_SHAL (SELF, KLUN)

IMPLICIT NONE
CLASS (CONVPAR_SHAL), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_CONVPAR_SHAL (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (CONVPAR_SHAL),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_CONVPAR_SHAL (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (CONVPAR_SHAL), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE MODD_CONVPAR_SHAL

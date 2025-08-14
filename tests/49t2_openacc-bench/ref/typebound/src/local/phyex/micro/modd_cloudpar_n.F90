!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######################
      MODULE MODD_CLOUDPAR_n

!$ACDC methods 

!     ######################
!
!!****  *MODD_CLOUDPAR$n* - declaration of the model-n dependant Microphysics 
!!   constants 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     model-n dependant Microhysics constants.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_CLOUDPARn)
!!          
!!    AUTHOR
!!    ------
!!	E. Richard   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/12/95                      
!!       J.-P. Pinty   29/11/02 add C3R5, ICE2, ICE4, ELEC
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE CLOUDPAR_t
!
  INTEGER :: NSPLITR      !< Number of required small time step integration
                          !! for rain sedimentation computation
  INTEGER :: NSPLITG      !< Number of required small time step integration
                          !! for ice hydrometeor sedimentation computation
!
!
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_CLOUDPAR_T
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_CLOUDPAR_T
PROCEDURE :: ACDC_HOST => ACDC_HOST_CLOUDPAR_T
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_CLOUDPAR_T
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_CLOUDPAR_T
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_CLOUDPAR_T
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_CLOUDPAR_T
END TYPE CLOUDPAR_t

TYPE(CLOUDPAR_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: CLOUDPAR_MODEL
TYPE(CLOUDPAR_t), POINTER, SAVE :: CLOUDPARN => NULL()
INTEGER, POINTER :: NSPLITR=>NULL()
INTEGER, POINTER :: NSPLITG=>NULL()

INTERFACE

MODULE SUBROUTINE ACDC_COPY_CLOUDPAR_T (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (CLOUDPAR_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_CLOUDPAR_T (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (CLOUDPAR_T), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_CLOUDPAR_T (SELF)

IMPLICIT NONE
CLASS (CLOUDPAR_T), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_CLOUDPAR_T (SELF, KLUN)

IMPLICIT NONE
CLASS (CLOUDPAR_T), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_CLOUDPAR_T (SELF, KLUN)

IMPLICIT NONE
CLASS (CLOUDPAR_T), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_CLOUDPAR_T (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (CLOUDPAR_T),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_CLOUDPAR_T (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (CLOUDPAR_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

CONTAINS

SUBROUTINE CLOUDPAR_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
CLOUDPARN => CLOUDPAR_MODEL(KTO)
!
! Save current state for allocated arrays
!
! Current model is set to model KTO
NSPLITR=>CLOUDPAR_MODEL(KTO)%NSPLITR
NSPLITG=>CLOUDPAR_MODEL(KTO)%NSPLITG

END SUBROUTINE CLOUDPAR_GOTO_MODEL

END MODULE MODD_CLOUDPAR_n

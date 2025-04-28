MODULE YOECND

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     -----------------------------------------------------------------

TYPE :: TECND
REAL(KIND=JPRB) :: REPFLM
REAL(KIND=JPRB) :: REPQMI
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TECND
!============================================================================

!!TYPE(TECND), POINTER :: YRECND => NULL()

!     -----------------------------------------------------------------
!*    CONTROL PARAMETERS FOR MOIST PROCESSES

! REPFLM :  Minimum flux to avoid zero division in ice proportion
!           computations
! REPQMI :  Minimum specific humidity (security within QNEGAT)
!     -----------------------------------------------------------------

CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  IMPLICIT NONE
  CLASS(TECND), INTENT(IN) :: SELF
  INTEGER     , INTENT(IN) :: KDEPTH
  INTEGER     , INTENT(IN) :: KOUTNO

  INTEGER :: IDEPTHLOC

  IDEPTHLOC = KDEPTH+2
  
  WRITE(KOUTNO,*) REPEAT(' ',KDEPTH   ) // 'model%yrml_phy_ec%yrecnd : '
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'REPFLM = ',SELF%REPFLM
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'REPQMI = ',SELF%REPQMI
 
END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOECND

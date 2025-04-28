MODULE YOEGWDWMS

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

! ------  CONTROLS FOR SIMPLIFIED NON-OROGRAPHIC GRAVITY WAVE SCHEME

! LREGNOGWD: .TRUE. if the regularization for non-orographic GWD is used

TYPE :: TEGWDWMS
LOGICAL :: LREGWWMS
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TEGWDWMS
!============================================================================

!!TYPE(TEGWDWMS), POINTER :: YREGWDWMS => NULL()

!     --------------------------------------------------------------------
CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  IMPLICIT NONE
  CLASS(TEGWDWMS), INTENT(IN) :: SELF
  INTEGER        , INTENT(IN) :: KDEPTH
  INTEGER        , INTENT(IN) :: KOUTNO

  INTEGER :: IDEPTHLOC

  IDEPTHLOC = KDEPTH+2
  
  WRITE(KOUTNO,*) REPEAT(' ',KDEPTH)    // 'model%yrml_phy_slin%yregwdwms : '
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LREGWWMS = ', SELF%LREGWWMS

END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOEGWDWMS

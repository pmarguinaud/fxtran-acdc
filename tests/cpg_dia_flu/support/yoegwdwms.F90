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
   
END TYPE TEGWDWMS
!============================================================================

!!TYPE(TEGWDWMS), POINTER :: YREGWDWMS => NULL()

!     --------------------------------------------------------------------
CONTAINS



END MODULE YOEGWDWMS

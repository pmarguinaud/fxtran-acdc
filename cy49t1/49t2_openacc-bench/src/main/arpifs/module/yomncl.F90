MODULE YOMNCL

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

! ------ CLOUD CHARACTERISTICS FOR SIMPLIFIED SCHEME

! LNCLIN   : .TRUE. IF (A,L,I) grid-point upper air fields to be read
!            on input for new cloud scheme of the linearized model
! LREGCL   : .TRUE. if the regularization in the cloud scheme is used

TYPE :: TNCL
LOGICAL :: LNCLIN
LOGICAL :: LREGCL
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TNCL
!============================================================================

!!TYPE(TNCL), POINTER :: YRNCL => NULL()

!     ------------------------------------------------------------------
CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
IMPLICIT NONE
CLASS(TNCL), INTENT(IN) :: SELF
INTEGER    , INTENT(IN) :: KDEPTH
INTEGER    , INTENT(IN) :: KOUTNO

INTEGER :: IDEPTHLOC

IDEPTHLOC = KDEPTH+2

WRITE(KOUTNO,*) REPEAT(' ',KDEPTH   ) // 'model%yrml_phy_slin%yrncl : '
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LNCLIN = ', SELF%LNCLIN
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LREGCL = ', SELF%LREGCL

END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOMNCL

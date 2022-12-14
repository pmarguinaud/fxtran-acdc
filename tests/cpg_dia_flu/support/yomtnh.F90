MODULE YOMTNH

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!   -----------------------------------------------------------------

!*   Buffer for trajectory array at time t-dt (nonhydrostatic dynamics)

!    NLENNH95B                    : length of buffer for 3D fields
!    TRAJNH(NLENNH95B,0:NSTOP)    : buffer for 3D fields

TYPE :: TTNH
INTEGER(KIND=JPIM) :: NLENNH95B
REAL(KIND=JPRB),ALLOCATABLE:: TRAJNH(:,:)
!--------------------------------------------------------------
CONTAINS
  
   

END TYPE TTNH

!!TYPE(TTNH), POINTER :: YRTNH => NULL()
!==============================================================

!!TYPE(TPTRSLB1), POINTER :: YRPTRSLB1 => NULL()

CONTAINS 
  

!   ----------------------------------------------------------------
END MODULE YOMTNH

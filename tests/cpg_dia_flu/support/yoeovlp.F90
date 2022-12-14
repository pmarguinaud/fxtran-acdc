MODULE YOEOVLP

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*     *YOEOVLP* VERTICAL DISTRIBUTION OF CLOUD OVERLAP PARAMETER
!     ------------------------------------------------------------------

TYPE :: TEOVLP
REAL(KIND=JPRB),ALLOCATABLE:: RA1OVLP(:)
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TEOVLP
!============================================================================

!!TYPE(TEOVLP), POINTER :: YREOVLP => NULL()

!     J.-J. MORCRETTE    E.C.M.W.F.     01/02/16

!      NAME     TYPE      PURPOSE
!      ----     ----      -------

!     *RA1OVLP* REAL      Alpha1 (Hogan, Illingworth, 2001)

!     ------------------------------------------------------------------

CONTAINS



END MODULE YOEOVLP

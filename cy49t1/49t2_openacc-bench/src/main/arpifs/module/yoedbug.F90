MODULE YOEDBUG

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     -------------------------------------------------
!*    ** *YOEDBUG* - CONTROL OPTIONS FOR DEBUGGING HELP
!     -------------------------------------------------

TYPE :: TEDBUG
INTEGER(KIND=JPIM) :: KSTPDBG(10), NSTPDBG
END TYPE TEDBUG

!!TYPE(TEDBUG), POINTER :: YREDBUG => NULL()

!     ------------------------------------------------------------------
END MODULE YOEDBUG


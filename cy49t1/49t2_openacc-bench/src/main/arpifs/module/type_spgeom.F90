MODULE TYPE_SPGEOM

! Module for spectral geometry structures.

USE PARKIND1 , ONLY : JPIM, JPRB

!     ------------------------------------------------------------------

IMPLICIT NONE
PRIVATE

SAVE

! GMR     : coefficients for spectral multiplication by GM.
! SCGMAP  : coefficients for multiplication by (GM**2) in spectral space (global model).
! ESCGMAP : coefficients for multiplication by (GM**2) in spectral space (LAM model).

TYPE, PUBLIC :: TSPGEOM
REAL(KIND=JPRB),ALLOCATABLE :: GMR(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SCGMAP(:,:)
REAL(KIND=JPRB) :: ESCGMAP(3)
CONTAINS
FINAL :: TSPGEOM_FINAL
END TYPE TSPGEOM

!!TYPE(TSPGEOM), POINTER,PUBLIC :: YSPGEOM => NULL()

CONTAINS

SUBROUTINE TSPGEOM_FINAL(THIS)
  TYPE(TSPGEOM) :: THIS
  ! If we don't add this, we may get a internal compiler error
END SUBROUTINE TSPGEOM_FINAL

END MODULE TYPE_SPGEOM

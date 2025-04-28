MODULE YOMOROG

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Orography: structure TOROG

! OROG       : grid-point surface orography "Phi_s".
! OROGL      : zonal component of "grad Phi_s".
! OROGM      : meridian component of "grad Phi_s".
! OROGLL,OROGMM,OROGLM: second-order reduced derivatives of surface orography.

TYPE TOROG
  REAL(KIND=JPRB), ALLOCATABLE :: OROG(:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGL(:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGM(:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGLL(:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGMM(:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGLM(:)
END TYPE TOROG

TYPE TOROG_BLOCKED
  REAL(KIND=JPRB), ALLOCATABLE :: OROG(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGL(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGM(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGLL(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGMM(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: OROGLM(:,:)
END TYPE TOROG_BLOCKED

! ------------------------------------------------------------------

END MODULE YOMOROG

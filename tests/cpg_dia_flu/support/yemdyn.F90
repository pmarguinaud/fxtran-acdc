MODULE YEMDYN

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

TYPE  :: TEDYN
!     ------------------------------------------------------------------

!===========   MAIN HORIZONTAL DIFFUSION SCHEME  ==============================

! * LEVEL AND WAVENUMBER DEPENDENT INVERSE CHARACTERISTIC TIMES:
! RDIVORE  : for diffusion of vorticity.
! RDIDIVE  : for diffusion of divergence.
! RDITE    : for diffusion of temperature.
! RDIGFLE  : for diffusion of GFL vars.
! RDIPDE   : for diffusion of pressure departure (NH).
! RDIVDE   : for diffusion of vertical divergence (NH).
! RDISPE   : for diffusion of surface pressure.

REAL(KIND=JPRB),ALLOCATABLE:: RDIVORE(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDIDIVE(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDITE(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDIGFLE(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDIPDE(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDIVDE(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDISPE(:)

!===========   ADDITIONAL HORIZONTAL DIFFUSION SCHEME FOR SLHD  ===============

! * LEVEL AND WAVENUMBER DEPENDENT INVERSE CHARACTERISTIC TIMES:
! RDSVORE  : for diffusion of vorticity.
! RDSDIVE  : for diffusion of divergence.
! RDSVDE   : for diffusion of vertical divergence (NH).

REAL(KIND=JPRB),ALLOCATABLE:: RDSVORE(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDSDIVE(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: RDSVDE(:,:)

!===========   "LGRADSP" OPTION  ==============================================

! REFILV: for filtering of vorticity.
! REFILD: for filtering of pressure departure.
REAL(KIND=JPRB),ALLOCATABLE :: REFILV(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: REFILD(:,:)

!===========   SEMI-IMPLICIT SCHEME ===========================================

! LESIDG   : .F.: Semi-implicit-scheme with reduced divergence.
!            .T.: Semi-implicit scheme with not reduced divergence.
! RTHRESIDG: threshold on RSTRET for activation of LESIDG option

LOGICAL :: LESIDG
REAL(KIND=JPRB) :: RTHRESIDG

!===========   LAM MODEL MASS CORRECTOR =======================================

! * XMALD:  imposed dry air mass for mass corrector in LAM models

REAL(KIND=JPRB) :: XMALD

!===========   MISCELLANEOUS  =================================================

! TCDIS    : ??? (no comment provided, used in the transform package).

REAL(KIND=JPRB) :: TCDIS

END TYPE TEDYN

!     ------------------------------------------------------------------
END MODULE YEMDYN

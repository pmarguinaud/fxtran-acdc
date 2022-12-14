MODULE YOMRADF

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

! EMTD    - longwave net flux
! TRSW    - shortwave net transmissivity (multiply by incoming SW to get flux)
! EMTC    - clear-sky net longwave flux
! TRSC    - clear-sky net shortwave transmissivity

! TAUAER  - prognostic aerosol variable for radiation and clouds

! SRSWD   - downward SW radiation at the surface 
! SRLWD   - downward LW radiation at the surface
! SRLWDC  - clear-sky downward LW radiation at the surface
! SRSWDC  - clear-sky downward SW radiation at the surface
! SRSWDCS - clear-sky NET SW radiation at the surface
! SRLWDCS - clear-sky NET LW radiation at the surface
! SRSWDV  - downward SW visible radiation at the surface
! SRSWDUV - downward SW ultraviolet/visible radiation at the surface
! SRSWPAR - downward SW PAR radiation at the surface
! SRSWUVB - downward UV-B radiation at the surface
! SRSWPARC- downward clear-sky SW PAR radiation at the surface
! SRSWTINC- TOA incident solar radiation 
! RMOON   - M-F military application
! SRSWTINC- TOA incident solar radiation
! SRFDIR  - total sky direct downward SW radiation
! SRCDIR  - clear-sky direct downward SW radiation
! DerivativeLw - derivative to update LW radiation between calls to full radiation scheme

! This type is used to pass radiative fluxes from the radiation scheme
! (see RADDRV) to the heating-rate calculation (see RADHEATN), and
! the arrays are allocated in SUECRAD
TYPE :: TRADF
! Dimesioned NPROMA,NFLEVG+1,NGPBLKS:
REAL(KIND=JPRB),ALLOCATABLE :: EMTD(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: TRSW(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: EMTC(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: TRSC(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE :: EMTU(:,:,:)

! Dimesioned NPROMA,NFLEVG,6,NGPBLKS:
REAL(KIND=JPRB),ALLOCATABLE :: TAUAER(:,:,:,:)

! Dimesioned NPROMA,NGPBLKS:
REAL(KIND=JPRB),ALLOCATABLE :: SRSWD(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRLWDC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRLWD(:,:,:) ! NPROMA,NLWOUT,NGPBLKS
REAL(KIND=JPRB),ALLOCATABLE :: SRSWDC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRSWDCS(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRLWDCS(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRSWDV(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRSWDUV(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: EDRO(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRSWPAR(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRSWUVB(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRSWPARC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRSWTINC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRFDIR(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SRCDIR(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: RMOON(:,:)
! The following is a pointer rather than an allocatable in order that
! pointers to it may be passed to subroutines (specifically to
! RADHEATN) even if it is not associated with any data, which occurs
! if LApproxLwUpdate is set to .FALSE.
REAL(KIND=JPRB),POINTER      :: DERIVATIVELW(:,:,:)=>NULL()
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TRADF
!============================================================================

!!TYPE(TRADF), POINTER :: YRRADF => NULL()

CONTAINS



END MODULE YOMRADF

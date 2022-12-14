MODULE YOE_SPECTRAL_PLANCK

! YOE_SPECTRAL_PLANCK
!
! PURPOSE
! -------
!   Calculate Planck function integrated across user-specified
!   spectral intervals, used in RADHEATN by approximate longwave
!   update scheme to modify longwave fluxes to account for the
!   spectral emissivity on the high-resolution model grid (rather than
!   the lower resolution grid seen by the radiation scheme).
!
! INTERFACE
! ---------
!   Call the INIT member routine to configure the look-up table of the
!   TSPECRALPLANCK type, followed by any number of CALC calls with the
!   temperatures at which the Planck function is required. FREE then
!   deallocates memory.
! 
! AUTHOR
! ------
!   Robin Hogan, ECMWF
!   Original: 2019-02-04
!
! MODIFICATIONS
! -------------
!   A Dawson 2019-08-05 avoid single precision overflow in INIT

!-----------------------------------------------------------------------

USE PARKIND1,         ONLY :   JPRB,JPRD,JPIM
IMPLICIT NONE
SAVE

!-----------------------------------------------------------------------
! Type for storing Planck function look-up table
TYPE TSPECTRALPLANCK
  ! Number of intervals over which the integrated Planck function is
  ! required. Note that an interval need not be contiguous in
  ! wavelength.
  INTEGER(KIND=JPIM) :: NINTERVALS

  ! Number of temperatures in look-up table
  INTEGER(KIND=JPIM) :: NTEMPS

  ! Start temperature and temperature spacing of look-up table
  REAL(KIND=JPRB) :: TEMP1, DTEMP

  ! Integrated Planck functions in look-up table, dimensioned
  ! (NINTERVALS,NTEMPS)
  REAL(KIND=JPRB),    ALLOCATABLE :: PLANCK_LUT(:,:)

  ! Store interval data
  REAL(KIND=JPRB),    ALLOCATABLE :: WAVLEN_BOUND(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: INTERVAL_MAP(:)

CONTAINS
  
  
  
  

END TYPE TSPECTRALPLANCK

CONTAINS

!-----------------------------------------------------------------------
! Generate a Planck function look-up table consisting of KINTERVALS
! spectral intervals (which need not be contiguous in wavelength),
! whose wavelength bounds are defined by PWAVLEN_BOUND and mapping
! on to KINTERVALS described by KINTERVAL_MAP.

  
  
!-----------------------------------------------------------------------
! Calculate Planck function in spectral intervals from temperature



!-----------------------------------------------------------------------
! Print look-up table to a unit



!-----------------------------------------------------------------------
! Free allocated memory



END MODULE YOE_SPECTRAL_PLANCK

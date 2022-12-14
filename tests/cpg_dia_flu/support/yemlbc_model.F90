MODULE YEMLBC_MODEL

! Purpose :
! -------
!    Forcing a LAM model by another model: part 0B
!    - forcing by lateral boundary conditions
!    - pressure tendency coupling
!    - spectral nudging

! Interface :
! ---------
!    Empty.

! External :
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------

! Author :
! ------
!    K. YESSAD (CNRM/GMAP) after YEMBICU, YEMDYN, YEMGT3B, SUEBICU, SUEDYN, SUESC2.
! Original : December 2010

! Modifications :
! -------------
! Daan Degrauwe: Feb 2012 Boyd biperiodization         
! T. Wilhelmsson and K. Yessad (Oct 2013) Geometry and setup refactoring.
! B. Bochenek (Oct 2013): Weights for LBC interpolation
! K. Yessad (July 2014): Move some variables.
! F. Taillefer (Aug 2015)  Add control of no coupling at all in canari by namelist
! M. Hortal (Dec 2014): Upper boundary relaxation
! P. Marguinaud (Oct 2016) : Port to single precision
! J. Vivoda (Mar 2017): Fixing bug in options LQCPL and LCCPL
! WARNING! The bug is in swapping LBC buffers P*CPL entering ESC2R. Fix does
! not remove it, but adjusts interpolation weights EWB accordingly. Clean
! solution is to correct swapping of LBC buffers, otherwise the code will
! remain difficult to understand.
! H. Dhouioui (Sep 2017) renamed from elbc0b_mod.F90
! O. Vignes (Feb 2020): Upper boundary relaxation fixes
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB, JPRD
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK
USE YOMDYNA  , ONLY : YRDYNA
USE YOMGMV   , ONLY : TGMV
USE YEMLBC_INIT,ONLY : JPALFNM


IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITION
!            ---------------

TYPE :: TELBC_MODEL

!      2.    DECLARATIONS
!            ------------

!      2.0   Control frequency of LBC.

! NEFRCL   : frequency of updating the lateral boundary coupling fields.
!            The LBC fields will be updated every NEFRCL time steps.
! NETLS1   : Time step of the first set of lateral boundary  fields.
! TEFRCL   : time interval between two updatings of the lateral boundary fields

INTEGER(KIND=JPIM) :: NEFRCL
INTEGER(KIND=JPIM) :: NETLS1
REAL(KIND=JPRB) :: TEFRCL

!      2.1   LECOBI.

! LECOBI  : T if there is coupling and biperiodicisation

LOGICAL :: LECOBI

!      2.2   Namelist variables for relaxation coefficients (resp for GMV, GMVS, GFL).

REAL(KIND=JPRB) :: EPA_GMV(JPALFNM)
REAL(KIND=JPRB) :: EPA_GMVS(JPALFNM)
REAL(KIND=JPRB) :: EPA_GFL(JPALFNM)

!      2.3   Relaxation coefficients.

! EALFA_GMV    : relaxation coefficients alpha for GMV.
! EALFA_GMVS   : relaxation coefficients alpha for GMVS.
! EALFA_GFL    : relaxation coefficients alpha for GFL.
! EALFA_TENC   : relaxation coefficients alpha for LTENC (GMVS only).
! EALFAGT3GMV  : ALFA (relax. coef.) of coupling points for GMV
! EALFAGT3GMVS : ALFA (relax. coef.) of coupling points for GMVS
! EALFAGT3GFL  : ALFA (relax. coef.) of coupling points for GFL
! EALFAU_GMV   : relaxation coefficients alpha for GMV (upper boundary).
! EALFAU_GFL   : relaxation coefficients alpha for GFL (upper boundary).

REAL(KIND=JPRB),ALLOCATABLE:: EALFA_GMV(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFA_GMVS(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFA_GFL(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFA_TENC(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAGT3GMV(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAGT3GMVS(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAGT3GFL(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAU_GMV(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EALFAU_GFL(:,:)

!      2.4   Other variables for grid-point coupling.

! GMGT3        : GM array of coupling points
! GMGT4        : GM array of coupling points (upper boundary).
! EWB          : weights for couplings
! EWBDFIFW     : weights for forward DFI
! EWBDFIBW     : weights for backward DFI
! RTENC        : multiplier of EALFA in the tendency coupling scheme
!                for stability reasons (RTENC<=1. close to 1)

REAL(KIND=JPRB),ALLOCATABLE:: GMGT3(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: GMGT4(:)
REAL(KIND=JPRB),ALLOCATABLE:: EWB(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EWBDFIFW(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: EWBDFIBW(:,:,:,:)
REAL(KIND=JPRB) :: RTENC

!      2.5   Other variables for spectral nudging.

! LSPNUSPDL  : .TRUE. if spectral nudging on Ps is relevant on this MPI task
! RNUDTFRAC  : Time fraction for spectral nudging
! NEFRSPCPL  : frequency of spectral nudging
! NEK0,NEK1  : lower and upper limits for total wavenumber for spectral nudging
! NEN1,NEN2  : lower and upper model levels for spectral nudging
! SPNUDVOR   : spectral nudging coeficient for vorticity
! SPNUDDIV   : spectral nudging coeficient for divergence
! SPNUDT     : spectral nudging coeficient for temperature
! SPNUDQ     : spectral nudging coeficient for specific humidity
! SPNUDSP    : spectral nudging coeficient for surface pressure
! LNUDSPGFL  : An array to control if any spectral GFL, for nudging

LOGICAL, ALLOCATABLE :: LNUDSPGFL(:)
LOGICAL :: LSPNUSPDL
REAL(KIND=JPRB) :: RNUDTFRAC
INTEGER(KIND=JPIM) :: NEFRSPCPL
INTEGER(KIND=JPIM) :: NEK0
INTEGER(KIND=JPIM) :: NEK1
INTEGER(KIND=JPIM) :: NEN1
INTEGER(KIND=JPIM) :: NEN2
REAL(KIND=JPRB) :: SPNUDVOR
REAL(KIND=JPRB) :: SPNUDDIV
REAL(KIND=JPRB) :: SPNUDT
REAL(KIND=JPRB) :: SPNUDQ
REAL(KIND=JPRB) :: SPNUDSP
REAL(KIND=JPRB) :: RNUTENC

END TYPE TELBC_MODEL


!=============================================================================

CONTAINS





!=============================================================================

END MODULE YEMLBC_MODEL

MODULE YOE_UVRAD

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOE_UVRAD* - COEFFICIENTS FOR ULTRAVIOLET RADIATION PROCESSOR
!     ------------------------------------------------------------------

TYPE :: TEUVRAD
INTEGER(KIND=JPIM) :: NRADUV, NUVTIM
INTEGER(KIND=JPIM) :: IPUV(3000), JCOP(3000), JUVLAM(3000)
LOGICAL :: LUVPROC, LUVTDEP, LUVDBG, LUVAERP, LO3_CHEM_UV

REAL(KIND=JPRB) :: RK250(3000), RTUV1(3000), RTUV2(3000), RSUVB0(3000), RAYUVB(3000)
REAL(KIND=JPRB) :: RASA(4), RASB(4), RASC(4), RASD(4), RASE(4), RASF(4)
REAL(KIND=JPRB) :: RFA0(4), RFA1(4), RFB0(4), RFB1(4), RFB2(4), RFB3(4), &
 & RFC0(4), RFC1(4), RFC2(4), RFC3(4), RFD0(4), RFD1(4), RFD2(4), RFD3(4)
REAL(KIND=JPRB) :: RTAUVA(4,6), RPIUVA(4,6), RCGUVA(4,6)
REAL(KIND=JPRB) :: RXPO(3), RXPL(3), RCIEAS(3000), RSUVB(3000), RUVLAM(3000)
REAL(KIND=JPRB) :: RFCAER, RMUZUV
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TEUVRAD
!============================================================================

!!TYPE(TEUVRAD), POINTER :: YREUVRAD => NULL()

!     -----------------------------------------------------------------
! NRADUV : INTEGER : number of UV intervals describing the 280-400 nm part of 
!                    the spectrum (depends on spectral resolution)
! NUV    : INTEGER : number of UV intervals describing the 280-400 nm part of 
!                    the spectrum (depends on spectral resolution)
! NUVTIM : INTEGER : maximum length of time within a forecast during which 
!                    the UV processor is called
! JPUV   : INTEGER : array for mapping the UV spectral indices onto the standard SW 
!                    intervals  
! JCOP   : INTEGER : array for mapping the UV spectral optical properties (clouds 
!                    and aerosols) onto the standard SW spectral optical properties
! JUVLAM : INTEGER : array of UV wavelength indices

! LUVPROC: LOGICAL : .T. if UV processor is activated (.F. operationally)
! LUVTDEP: LOGICAL : .T. if T-dependence is accounted for (.T. by default)
! LUVDBG : LOGICAL : .T. for debugging
! LUVAERP: LOGICAL : .T. if GEMS/MACC prognostic aerosols are used, 
!                    .F. for Tegen et al climatological aerosols

! RK250  : REAL    : O3 absorption coefficient at 250K
! RTUVn  : REAL    : first and second order term in (T-250) development of temperature dependence 
!                    of absorption coefficient
! RSUVB0 : REAL    : fraction of incident solar radiation in UV spectral intervals 
! RAYUVB : REAL    : Rayleigh scattering reference   
! RASx   : REAL    : Slingo's optical properties for liquid water clouds
! RFxn   : REAL    : Fu et al. optical properties for ice clouds: 
!                    polynomial development of the usual optical properties
! RTAUVA : REAL    : weighting factor w.r.t. standardized 550 nm optical depth 
!                    for climatological aerosols
! RPIUVA : REAL    : single scattering albedo for climatological aerosols
! RCGUVA : REAL    : asymmetry factor for climatological aerosols

! RXP0, RXPL : REAL: parameters defining the CIE erythemal action spectrum
! RCIEAS : REAL    : skin response function for computing bilogically effective dose
! RSUVB  : REAL    : spectral UV at TOA 
! RFCAER : REAL    : possible weighting factor to be applied to aerosol optical depth 
!                    (for sensitivity studies; default is 1.) 
! RMUZUV : REAL    : solar zenith angle threshold for activating the UV calculations

!     -----------------------------------------------------------------

CONTAINS



END MODULE YOE_UVRAD


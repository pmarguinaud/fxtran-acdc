MODULE YOE_UVRAD

!$ACDC methods 


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
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
PROCEDURE :: ACDC_COPY => ACDC_COPY_TEUVRAD
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TEUVRAD
PROCEDURE :: ACDC_HOST => ACDC_HOST_TEUVRAD
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TEUVRAD
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TEUVRAD
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TEUVRAD
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TEUVRAD
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

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TEUVRAD (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TEUVRAD), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TEUVRAD (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TEUVRAD), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TEUVRAD (SELF)

IMPLICIT NONE
CLASS (TEUVRAD), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TEUVRAD (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TEUVRAD), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TEUVRAD (SELF, KLUN)

IMPLICIT NONE
CLASS (TEUVRAD), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TEUVRAD (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TEUVRAD),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TEUVRAD (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TEUVRAD), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
IMPLICIT NONE
CLASS(TEUVRAD), INTENT(IN) :: SELF
INTEGER       , INTENT(IN) :: KDEPTH
INTEGER       , INTENT(IN) :: KOUTNO

INTEGER :: IDEPTHLOC

IDEPTHLOC = KDEPTH+2

WRITE(KOUTNO,*) REPEAT(' ',KDEPTH   ) // 'model%yrml_phy_rad%yreuvrad : '
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'NRADUV = ', SELF%NRADUV
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'NUVTIM = ', SELF%NUVTIM
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'IPUV SUM = ', SUM(SELF%IPUV)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'JCOP SUM = ', SUM(SELF%JCOP)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'JUVLAM SUM = ', SUM(SELF%JUVLAM)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LUVPROC = ', SELF%LUVPROC
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LUVTDEP = ', SELF%LUVTDEP
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LUVDBG = ', SELF%LUVDBG
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LUVAERP = ', SELF%LUVAERP
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LO3_CHEM_UV = ', SELF%LO3_CHEM_UV
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RK250 SUM = ', SUM(SELF%RK250)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RTUV1 SUM = ', SUM(SELF%RTUV1)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RTUV2 SUM = ', SUM(SELF%RTUV2)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RSUVB0 SUM = ', SUM(SELF%RSUVB0)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RAYUVB SUM = ', SUM(SELF%RAYUVB)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RASA SUM = ', SUM(SELF%RASA)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RASB SUM = ', SUM(SELF%RASB)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RASC SUM = ', SUM(SELF%RASC)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RASD SUM = ', SUM(SELF%RASD)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RASE SUM = ', SUM(SELF%RASE)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RASF SUM = ', SUM(SELF%RASF)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFA0 SUM = ', SUM(SELF%RFA0)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFA1 SUM = ', SUM(SELF%RFA1)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFB0 SUM = ', SUM(SELF%RFB0)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFB1 SUM = ', SUM(SELF%RFB1)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFB2 SUM = ', SUM(SELF%RFB2)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFB3 SUM = ', SUM(SELF%RFB3)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFC0 SUM = ', SUM(SELF%RFC0)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFC1 SUM = ', SUM(SELF%RFC1)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFC2 SUM = ', SUM(SELF%RFC2)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFC3 SUM = ', SUM(SELF%RFC3)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFD0 SUM = ', SUM(SELF%RFD0)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFD1 SUM = ', SUM(SELF%RFD1)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFD2 SUM = ', SUM(SELF%RFD2)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFD3 SUM = ', SUM(SELF%RFD3)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RTAUVA SUM = ', SUM(SELF%RTAUVA)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RPIUVA SUM = ', SUM(SELF%RPIUVA)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RCGUVA SUM = ', SUM(SELF%RCGUVA)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RXPO SUM = ', SUM(SELF%RXPO)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RXPL SUM = ', SUM(SELF%RXPL)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RCIEAS SUM = ', SUM(SELF%RCIEAS)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RSUVB SUM = ', SUM(SELF%RSUVB)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RUVLAM SUM = ', SUM(SELF%RUVLAM)
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RFCAER = ',SELF%RFCAER
WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'RMUZUV = ',SELF%RMUZUV

END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOE_UVRAD


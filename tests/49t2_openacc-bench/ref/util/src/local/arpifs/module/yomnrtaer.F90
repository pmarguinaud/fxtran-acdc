MODULE YOMNRTAER

!$ACDC methods 

USE PARKIND1,ONLY : JPRB

IMPLICIT NONE

SAVE

!     --------------------------------------------------------------------
!*    *YOMNRTAER* - definition of variables for near real time aerosols
!     D. Martin-Perez 
!     --------------------------------------------------------------------

TYPE TNRTAER
!*
!     ------------------------------------------------------------------

!     VARIABLES to use with NRT aerosols:

LOGICAL :: LCAMS_NRT           ! Near real time aerosols from CAMS
LOGICAL :: LMOCA_NRT           ! Near real time aerosols from MOCAGE (not implemented yet)

LOGICAL :: LAEIFN              ! Switch for Ice nuclei
LOGICAL :: LAERDRDEP           ! Switch for activation of aerosol dry deposition
LOGICAL :: LAERSSEM            ! Switch for Sea salt emission.
LOGICAL :: LAECCN2CLDR         ! Switch for CCN to cloud droplet conversion

REAL(KIND=JPRB) :: SSMINLO     ! In cloud supersaturation for CCN calculation near the surface
REAL(KIND=JPRB) :: SSMINUP     ! In cloud supersaturation for CCN calculation at upper levels
REAL(KIND=JPRB) :: SSMAX       ! Maximum supersaturation
REAL(KIND=JPRB) :: SSHEIGHT    ! Above SSHEIGHT the minimum SS is SSMINUP
REAL(KIND=JPRB) :: SSFACVV     ! Factor for vertical velocity term of SS
REAL(KIND=JPRB) :: SSFACSS     ! Factor for coarse sea salt reduction of SS

REAL(KIND=JPRB) :: RPANMIN     ! Minimum particle number concentration (m-3)
REAL(KIND=JPRB) :: RCCNMIN     ! Minimum CCN concentration inside clouds (m-3)
REAL(KIND=JPRB) :: RCLDROPMIN  ! Minimum cloud droplet number concentration inside cloud (m-3)
REAL(KIND=JPRB) :: RIFNMINSIZE ! Minimum IFN size (micrometers)

END TYPE TNRTAER

!     ------------------------------------------------------------------
END MODULE YOMNRTAER

MODULE YOMPARAR

!$ACDC methods 


USE PARKIND1  ,ONLY : JPIM     ,JPRB
!
USE MODD_PHYEX, ONLY: PHYEX_t
IMPLICIT NONE

SAVE

TYPE TPARAR
!*
!     ------------------------------------------------------------------

!     VARIABLES pour utiliser la PHYSIQUE de meso_NH :
!     VARIABLES to use the MESO-NH physics:

CHARACTER(LEN=4)   :: CMICRO      !< Microphysics scheme ('ICE3', 'ICE4' or 'LIMA')
CHARACTER(LEN=4)   :: CTURB       !< Turbulence scheme ('TKEL', 'NONE')
TYPE(PHYEX_t)      :: PHYEX       !< PHYEX constants
!
INTEGER(KIND=JPIM) :: NRR, NRRL, NRRI   !number of microphysical species
INTEGER(KIND=JPIM) :: NSV         !number of passiv variables in MesoNH,
                                  ! always 0 in AROME
INTEGER(KIND=JPIM) :: NSWB_MNH    !number of SW bands for surface
                                  ! (must be equal to NSW !!)
INTEGER(KIND=JPIM) :: NGPAR       !number of fields in the buffer containing
                                  ! the 2D pseudo-historical variables.
INTEGER(KIND=JPIM) :: MINPRR      !pointer on INPRR
INTEGER(KIND=JPIM) :: MACPRR      !pointer on ACPRR
INTEGER(KIND=JPIM) :: MINPRS      !pointer on INPRS
INTEGER(KIND=JPIM) :: MACPRS      !pointer on ACPRS
INTEGER(KIND=JPIM) :: MINPRG      !pointer on INPRG
INTEGER(KIND=JPIM) :: MACPRG      !pointer on ACPRG
INTEGER(KIND=JPIM) :: MALBDIR     !pointer on ALBDIR
INTEGER(KIND=JPIM) :: MALBSCA     !pointer on ALBSCA
INTEGER(KIND=JPIM) :: MRAIN       !pointer on surface rain
INTEGER(KIND=JPIM) :: MSNOW       !pointer on surface snow
INTEGER(KIND=JPIM) :: MGZ0        !pointer on GZ0
INTEGER(KIND=JPIM) :: MGZ0H       !pointer on GZ0H
INTEGER(KIND=JPIM) :: MVQS        !pointer on surface moisture
INTEGER(KIND=JPIM) :: MVTS        !pointer on surface temperature
INTEGER(KIND=JPIM) :: MVEMIS      !pointer on surface emissivity
INTEGER(KIND=JPIM) :: MSWDIR      !pointer on SW direct surface flux
INTEGER(KIND=JPIM) :: MSWDIF      !pointer on SW surface diffuse flux
INTEGER(KIND=JPIM) :: MLSM        !pointer on land-sea mask
INTEGER(KIND=JPIM) :: MCD         !pointer on drag coefficient
INTEGER(KIND=JPIM) :: MCH         !pointer on heat coefficient

REAL(KIND=JPRB), DIMENSION(:), ALLOCATABLE  :: XSW_BANDS  !SW spectral bands
! for ext. surface scheme
LOGICAL :: LOLSMC      ! Land/sea mask for cloud droplet number conc.
LOGICAL :: LOTOWNC     ! Town mask for cloud droplet number conc.

LOGICAL :: LKOGAN      ! Use Kogan autocoversion of liquid
LOGICAL :: LMODICEDEP  ! Logical switch for alternative dep/evap of ice
LOGICAL :: LICERAD     ! Assume higher fraction of condensate for
                       ! ice/snow/graupel than the actual cloud cover in
                       ! radiation
REAL(KIND=JPRB) :: RADGR  ! Tuning of ice for radiation, TO BE REMOVED
REAL(KIND=JPRB) :: RADSN  ! Tuning of ice for radiation, TO BE REMOVED
LOGICAL         :: LLCRIT    ! True if temperature dependent
                             ! critical condensation in EDMFm
LOGICAL         :: LTOTPREC  ! True if precipitation tendencies
                             ! from the sub-grid scheme are
                             ! added to the total precip tendencies.
LOGICAL         :: LTOTPRECL ! As LTOTPREC but updraft fraction untouched
LOGICAL         :: LDUALMF   ! Key activating the dual updraft shallow convection EDMFm
LOGICAL         :: LSCAWAREMF! Key activating a (scale aware) reduction in the dry and moist updrafts in EDMFm
! Maximal Vertical Wind Speed in BL thermal
REAL(KIND=JPRB) :: XVMAXTHP      ! coef in Vz in plain
REAL(KIND=JPRB) :: XVMAXTHM      ! coef in Vz in mountain

! * for the squall line case:
LOGICAL :: LSQUALL ! use for the squall line case
INTEGER(KIND=JPIM) :: NREFROI1 !starting point for cooling (lsquall case)
INTEGER(KIND=JPIM) :: NREFROI2 !end point for cooling (lsquall case)
REAL(KIND=JPRB) :: VSQUALL ! mean velocity displacement of the squall line (lsquall case)

!* for MNH budget anlysis
LOGICAL :: LAROBU_ENABLE ! for MNH budget anlysis
!
!* For radiation :
REAL(KIND=JPRB) :: XCQVR ! reduction factor of water vapour used for radiation computation.
REAL(KIND=JPRB) :: GQVPLIM ! pressure value over which qv is damped towards 0 for radiation.
REAL(KIND=JPRB) :: GQVTOP ! qv value at the top of the atmopshere.
LOGICAL :: LQVTOP ! to activate modification of qv in input to radiation.
LOGICAL :: LIMIT_TENDQ ! to avoid negative qv (pb AEARO)

END TYPE TPARAR

!!TYPE(TPARAR), POINTER :: YRPARAR => NULL()


!     ------------------------------------------------------------------
END MODULE YOMPARAR

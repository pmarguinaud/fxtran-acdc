MODULE YOEAERATM

USE PARKIND1    , ONLY : JPIM, JPRB
USE YOE_AERODIAG, ONLY : NPAERODIAG

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERATM* - CONTROL PARAMETERS FOR AEROSOLS IN THE ATMOSPHERE
!     ------------------------------------------------------------------

TYPE :: TYPE_AERO_DESC
SEQUENCE
! Automatically initialised keys to match GFL
CHARACTER(LEN=16)  :: CNAME                 ! ARPEGE field name
INTEGER(KIND=JPIM) :: IGRBCODE              ! GRIB code
! Initialised from namelist
INTEGER(KIND=JPIM) :: NTYP                  ! Aerosol type number
INTEGER(KIND=JPIM) :: NBIN                  ! Aerosol bin number within type
INTEGER(KIND=JPIM) :: IGRIBDIAG(NPAERODIAG) ! GRIB codes of diagnostics
REAL(KIND=JPRB)    :: RDDEPVSEA             ! Dry deposition velocity over sea
REAL(KIND=JPRB)    :: RDDEPVLIC             ! Dry deposition velocity over land or ice
REAL(KIND=JPRB)    :: RSEDIMV               ! Sedimentation velocity
REAL(KIND=JPRB)    :: RSCAVIN               ! In-cloud scavenging fraction
REAL(KIND=JPRB)    :: RSCAVBCR              ! Below-cloud scavenging coefficient for rain
REAL(KIND=JPRB)    :: RSCAVBCS              ! Below-cloud scavenging coefficitne for snow
CHARACTER(LEN=16)  :: COPTCLASS             ! Optical property class
CHARACTER(LEN=16)  :: CHYGCLASS             ! Hygroscopicity class
INTEGER(KIND=JPIM) :: IAEROCV               ! Aerosol control variable (0=none, 1=fine, 2=coarse)
END TYPE TYPE_AERO_DESC

TYPE :: TEAERATM
INTEGER(KIND=JPIM) :: NAERCONF
INTEGER(KIND=JPIM) :: NINIDAY   
INTEGER(KIND=JPIM) :: NXT3DAER
INTEGER(KIND=JPIM) :: NDD1, NSS1
INTEGER(KIND=JPIM) :: NBCOPTP, NDDOPTP, NOMOPTP, NSSOPTP, NSUOPTP
INTEGER(KIND=JPIM) :: NVISWL
INTEGER(KIND=JPIM) :: NMAXTAER
INTEGER(KIND=JPIM) :: NTAER
INTEGER(KIND=JPIM) :: NTYPAER(10)
INTEGER(KIND=JPIM) :: NAER_BLNUCL
INTEGER(KIND=JPIM) :: NAERSCAV

REAL(KIND=JPRB) :: RGRATE

REAL(KIND=JPRB) :: REPSCAER

LOGICAL :: LAERCLIMG, LAERCLIMZ, LAERCLIST, LAERDRYDP, LAERHYGRO, LAERLISI 
LOGICAL :: LAERNGAT , LAERSEDIM, LAERSURF , LAERELVS , LAER6SDIA,LAERSEDIMSS
LOGICAL :: LAERGTOP , LAERRAD  , LAERCCN  , LAEROPT(9),LAERINIT , LAERVOL
LOGICAL :: LAERCSTR , LAERDIAG1, LAERDIAG2, LAERRRTM , LAERUVP
LOGICAL :: LAEREXTR , LAERGBUD , LAERPRNT
LOGICAL :: LAERNITRATE
LOGICAL :: LAERSOA_CHEM 
LOGICAL :: LAERSOAEMIS_FLUX
LOGICAL :: LSEASALT_RH80
LOGICAL :: LAERDUSTSOURCE
LOGICAL :: LAERDUST_NEWBIN

! Pre-computed conversion factors according to LSEASALT_RH80.
! Computed in SU_AERW after reading namelist and calling SU_AERP
! to initialise growth tables.
REAL(KIND=JPRB) :: RSS_DRY_DIAFAC, RSS_DRY_DENSFAC, RSS_DRY_MASSFAC
REAL(KIND=JPRB) :: RSS_RH80_DIAFAC, RSS_RH80_DENSFAC, RSS_RH80_MASSFAC

TYPE(TYPE_AERO_DESC), POINTER :: YAERO_DESC(:) => NULL()
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TEAERATM
!============================================================================

TYPE(TEAERATM), POINTER :: YREAERATM => NULL()

!     ------------------------------------------------------------------
! NDD1       : location of first bin for desert dust
! NSS1       : location of first bin for sea-salt
! NBCOPTP    : index for choosing the black carbon LW and SW optic.prop. (1: Boucher, 2: Bond, Bergstrom, 3: Stier et al.)
! NDDOPTP    : index for choosing the dust LW and SW optic.prop. (1: Dubovik(SW)+Fouquart(LW), 2: Fouquart, 3: Woodward)
! NOMOPTP    : index for choosing the organic carbon optic.prop.
! NSSOPTP    : index for choosing the sea salt optic.prop.
! NSUOPTP    : index for choosing the sulphate optic.prop.
! NVISWL     : index of wavelength for visibility computations
! RMFMIN     : minimum mass flux for convective aerosol transport
! RGRATE     : transformation rate from hygrophopic to hygrophilic for BC and OM aerosols

! REPSCAER   : security on aerosol concentration: always >= 1.E-18

! LAERCLIMG  : .T. to start prognostic aerosols with geographical monthly 
!                  mean climatology
! LAERCLIMZ  : .T. to start prognostic aerosols with zonal annual mean 
!                  climatology
! LAERCLIST  : .T. to start prognostic aerosols with geographical monthly 
!                  mean climatology for background stratospheric only
! LAERDRYDP  : .T. dry deposition is active
! LAERHYDRO  : .T. hygroscopic effects on BC and OM aerosols
! LAERNGAT   : .T. prevents negative aerosol concentrations
! NAERSCAV   : aerosol scanvenging scheme: 1=historical, 2=from CB05, 3=Luo et al. 2019
! LAERSEDIM  : .T. sedimentation is active
! LAERSEDIMSS  : .T. special sedimentation for sea-salt is active
! LAERSURF   : .T. if surface emissions
! LAERELVS   : .T. if "elevated" source
! LAER6SDIA  : .T. if radiance diagnostics with 6S
! LAERGTOP   : .T. if gas-to-particle conversion for SO2/SO4
! LAERRAD    : .T. if there is any prognostic aerosols used for RT
! LAEROPT(.) : .T. if a given aerosol type is radiatively interactive
! LAERCCN    : .T. if prognostic aerosols are used to define the Re of liq.wat.clds
! LAERUVP    : .T. if prognostic aerosols are used in UV-processor
! LAERCSTR   : .T. if climatological stratospheric aerosols are used in radiation 
!                  schemes with the prognostic tropospheric aerosols.
! LAERRRTM   : .T. if RRTM schemes get the information from the prognostic aerosols
! LAERINIT   : .T. if analysed prognostic aerosols are ONLY used as "climatological" aerosols
! LAERVOL    : .T. if volcanic aerosol is considered

! NMAXTAER   : MAXIMUM TOTAL NUMBER OF AEROSOLS
! NTAER      : TOTAL NUMBER OF AEROSOLS
! NTYPAER( ) : NBINAER
!        (1) :         3/9 FOR SEA-SALT 
!        (2) :         3/9 FOR DESERT DUST
!        (3) :         2 FOR ORGANIC MATTERS
!        (4) :         2 FOR BLACK CARBON
!        (5) :         2 FOR SO2/SO4 (ESSENTIALLY TROPOSPHERIC)
!        (6) :         2 FOR NITRATES
!        (7) :         1 FOR AMMONIUM
!        (8) :         1 FOR FLY ASH
!        (9) :         2 FOR SO2/SO4 OF VOLCANIC ORIGIN

! LAERSOA_CHEM: use SOA sources scaled from CO fluxes in aer_src.F90.
! LAERSOAEMIS_FLUX: SOA sources emitted as surface fluxes
! LSEASALT_RH80: transport sea salt at fixed 80% RH (as done historically) rather than dry (as all other aerosols)
! YAERO_DESC: aerosol descriptors (metadata moved out of GFL and hard-coded source)
!     ------------------------------------------------------------------

CONTAINS



END MODULE YOEAERATM


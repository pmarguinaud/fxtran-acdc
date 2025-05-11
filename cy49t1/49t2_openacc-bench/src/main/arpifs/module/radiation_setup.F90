MODULE RADIATION_SETUP

!$ACDC methods  --skip-components TRADIATION%RAD_CONFIG


! RADIATION_SETUP - Setting up modular radiation scheme
!
! (C) Copyright 2015- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! PURPOSE
! -------
!   The modular radiation scheme is contained in a separate
!   library. SETUP_RADIATION_SCHEME in this module sets up a small
!   derived type that contains the configuration object for the
!   radiation scheme, plus a small number of additional variables
!   needed for its implemenation in the IFS.
!
! INTERFACE
! ---------
!   SETUP_RADIATION_SCHEME is called from SUECRAD.  The radiation
!   scheme is actually run using the RADIATION_SCHEME routine (not in
!   this module).
!
! AUTHOR
! ------
!   Robin Hogan, ECMWF
!   Original: 2015-09-16
!
! MODIFICATIONS
! -------------
!   2017-03-03  R. Hogan   Put global variables in TRADIATION derived type
!   2017-11-17  S. Remy    Add Nitrates and SOA if NAERMACC=0
!   2017-11-28  R. Hogan   Delta scaling applied to particles only
!   2018-01-11  R. Hogan   Capability to scale solar spectrum in each band
!   2018-04-20  A. Bozzo   Added capability to read in aerosol optical properties
!                          at selected wavelengths
!   2019-01-21  R. Hogan   Explicit albedo and emissivity spectral definitions
!                          leading to smarter weighting in ecRad
!   2021-01-24  R. Hogan   Added ecCKD via NSWGASOPTICS and NLWGASOPTICS
!   2022-02-2   P. Ukkonen Added RRTMGP gas optics and controlling via NGASOPTICSSCHEME

!-----------------------------------------------------------------------

USE PARKIND1
USE RADIATION_CONFIG
USE YOEAEROP    ,ONLY : ALF_SU, ALF_OM, ALF_DD, ALF_SS, ALF_BC, ALF_NI, ALF_AM, ALF_SOA, &
     &  ASY_SU, ASY_OM, ASY_DD, ASY_SS, ASY_BC, ASY_NI, ASY_AM, ASY_SOA, &
     &  OMG_SU, OMG_OM, OMG_DD, OMG_SS, OMG_BC, OMG_NI, OMG_AM, OMG_SOA, &
     & RALI_BC, RALI_DD, RALI_OM, RALI_SU, RALI_SS, RALI_NI, RALI_AM, RALI_SOA
USE YOE_SPECTRAL_PLANCK, ONLY : INIT

IMPLICIT NONE

SAVE

! Background aerosol is specified in an ugly way: using the old Tegen
! fields that are in terms of optical depth, and converted to mass
! mixing ratio via the relevant mass-extinction coefficient. The
! following are the indices to the aerosol types used to describe
! tropospheric and stratospheric background aerosol.
INTEGER(KIND=JPIM), PARAMETER :: ITYPE_TROP_BG_AER = 8 ! hydrophobic organic
INTEGER(KIND=JPIM), PARAMETER :: ITYPE_STRAT_BG_AER=12 ! non-absorbing sulphate

! This derived type contains configuration information for the
! radiation scheme plus a few additional variables and parameters
! needed for the IFS interface to it
TYPE :: TRADIATION

  ! Configuration information for the ecRad radiation scheme
  type(config_type)  :: rad_config

  ! Ultraviolet weightings
  INTEGER(KIND=JPIM) :: NWEIGHT_UV
  INTEGER(KIND=JPIM) :: IBAND_UV(100)
  REAL(KIND=JPRB)    :: WEIGHT_UV(100)
  ! Photosynthetically active radiation weightings
  INTEGER(KIND=JPIM) :: NWEIGHT_PAR
  INTEGER(KIND=JPIM) :: IBAND_PAR(100)
  REAL(KIND=JPRB)    :: WEIGHT_PAR(100)
  ! Mass-extinction coefficient (m2 kg-1) of tropospheric and
  ! stratospheric background aerosol at 550 nm
  REAL(KIND=JPRB)    :: TROP_BG_AER_MASS_EXT
  REAL(KIND=JPRB)    :: STRAT_BG_AER_MASS_EXT
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TRADIATION


CONTAINS

  ! This routine copies information between the IFS radiation
  ! configuration (stored mostly in YDERAD) and the radiation
  ! configuration of the modular radiation scheme (stored in
  ! PRADIATION%rad_config).  The optional input logical LDOUTPUT
  ! controls whether to print lots of information during the setup
  ! stage (default is no).
  


  


  

  SUBROUTINE SETUP_RADIATION_SCHEME_GPU(YDERDI,YDEAERATM,YDCOMPO,YDEPHY,YDERAD,PRADIATION,LDOUTPUT,FILE_NAME, lacc)

    USE YOMHOOK
    USE YOMLUN
    !USE YOESRTWN, ONLY : NMPSRTM
    USE YOERAD,   ONLY : TERAD
    USE YOEPHY,   ONLY : TEPHY
    USE YOEAERATM,ONLY : TEAERATM
    USE YOMCOMPO
    USE YOERDI  , ONLY : TERDI

    USE RADIATION_INTERFACE
    USE RADIATION_AEROSOL_OPTICS

    ! Radiation configuration information
    TYPE(TERDI)       ,INTENT(INOUT):: YDERDI
    TYPE(TEAERATM)    ,INTENT(INOUT)          :: YDEAERATM
    TYPE(TCOMPO)      ,INTENT(IN)             :: YDCOMPO
    TYPE(TEPHY)       ,INTENT(IN)             :: YDEPHY
    TYPE(TERAD)       ,INTENT(INOUT)          :: YDERAD
    TYPE(TRADIATION)  ,INTENT(INOUT), TARGET  :: PRADIATION
    CHARACTER(LEN=512),INTENT(IN), OPTIONAL   :: FILE_NAME

    ! Whether or not to print out information on the radiation scheme
    ! configuration
    LOGICAL, INTENT(IN), OPTIONAL :: LDOUTPUT

    ! Verbosity of configuration information 0=none, 1=warning,
    ! 2=info, 3=progress, 4=detailed, 5=debug
    INTEGER(KIND=JPIM) :: IVERBOSESETUP
    INTEGER(KIND=JPIM) :: ISTAT

    ! Data directory name
    CHARACTER(LEN=256) :: CL_DATA_DIR

    ! Arrays to avoid temporaries
    REAL(KIND=JPRB)    :: ZWAVBOUND(15)
    INTEGER(KIND=JPIM) :: IBAND(16)



    ! Do we use the nearest ecRad band to the albedo/emissivity
    ! intervals, or a more intelligent weighting?
    LOGICAL :: LL_DO_NEAREST_SW_ALBEDO, LL_DO_NEAREST_LW_EMISS

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
    logical, intent (in) :: lacc

#include "posname.intfb.h"
#include "abor1.intfb.h"

    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_RADIATION_SCHEME_GPU',0,ZHOOK_HANDLE)

    ! *** GENERAL SETUP ***
    ASSOCIATE(RAD_CONFIG=>PRADIATION%RAD_CONFIG,&
              & LAERNITRATE=>YDCOMPO%LAERNITRATE, &
              & LAERSOA=>YDCOMPO%LAERSOA, &
              & LAERVOL=>YDEAERATM%LAERVOL, &
              & YSPECTPLANCK=>YDERAD%YSPECTPLANCK)

    ! Configure verbosity of setup of radiation scheme
    IVERBOSESETUP = 4 ! Provide plenty of information
    IF (PRESENT(LDOUTPUT)) THEN
      IF (.NOT. LDOUTPUT) THEN
        IVERBOSESETUP = 1 ! Warnings and errors only
      ENDIF
    ENDIF
    RAD_CONFIG%IVERBOSESETUP = IVERBOSESETUP

    IF (IVERBOSESETUP > 1) THEN
      WRITE(NULOUT,'(a)') '-------------------------------------------------------------------------------'
      WRITE(NULOUT,'(a)') 'RADIATION_SETUP: ecRad 1.6'
    ENDIF

    ! Normal operation of the radiation scheme displays only errors
    ! and warnings
    RAD_CONFIG%IVERBOSE = 1

    ! Read data directory name from the DATA environment variable
    ! CALL GETENV("DATA", CL_DATA_DIR)
    ! IF (CL_DATA_DIR /= " ") THEN
    !   RAD_CONFIG%DIRECTORY_NAME = TRIM(CL_DATA_DIR) // "/ifsdata"
    ! ELSE
    !   ! If DATA not present, use the current directory
    !   RAD_CONFIG%DIRECTORY_NAME = "."
    ! ENDIF
    ! Temporary solution
    RAD_CONFIG%DIRECTORY_NAME = '/scratch/work/seity/EcRad49'

    ! Do we do Hogan and Bozzo (2015) approximate longwave updates?
    RAD_CONFIG%DO_LW_DERIVATIVES = YDERAD%LAPPROXLWUPDATE

    ! If we are to perform Hogan and Bozzo (2015) approximate
    ! shortwave updates then we need the downwelling direct and
    ! diffuse shortwave fluxes at the surface in each albedo spectral
    ! interval
    RAD_CONFIG%DO_CANOPY_FLUXES_SW = YDERAD%LAPPROXSWUPDATE

    ! If we are to perform approximate longwave updates and we are
    ! using the new 6-interval longwave emissivity scheme then we need
    ! ecRad to compute the downwelling surface longwave fluxes in each
    ! emissivity spectral interval
    IF (YDERAD%NLWOUT > 1) THEN
      RAD_CONFIG%DO_CANOPY_FLUXES_LW = .TRUE.
    ENDIF

    ! Surface spectral fluxes are needed for UV and PAR calculations
    RAD_CONFIG%DO_SURFACE_SW_SPECTRAL_FLUX = .TRUE.


    ! *** SETUP GAS OPTICS ***

    !IF (YDERAD%NSWGASOPTICS == 0 .AND. YDERAD%NLWGASOPTICS == 0) THEN
    IF (YDERAD%NGASOPTICSSCHEME == 0) THEN
    !RAD_CONFIG%DO_SETUP_IFSRRTM = .FALSE.
      ! already set-up RRTMG, so the setup_radiation routine below
      ! does not have to
!      RAD_CONFIG%I_GAS_MODEL                = IGasModelIFSRRTMG
      RAD_CONFIG%USE_GENERAL_CLOUD_OPTICS   = .FALSE.
      RAD_CONFIG%USE_GENERAL_AEROSOL_OPTICS = .FALSE.
      RAD_CONFIG%DO_SETUP_IFSRRTM           = .FALSE.

    ! ELSEIF (YDERAD%NSWGASOPTICS == 0 .OR. YDERAD%NLWGASOPTICS == 0) THEN

    !   WRITE(NULERR,'(a)') '*** Error: NSWGASOPTICS and NLWGASOPTICS must either both be zero or both nonzero'
    !   CALL ABOR1('RADIATION_SETUP: error interpreting NSWGASOPTICS and NLWGASOPTICS')

    ! ELSE

    ELSEIF (YDERAD%NGASOPTICSSCHEME == 1) THEN
      ! ecCKD gas optics scheme
!      RAD_CONFIG%I_GAS_MODEL                = IGasModelECCKD
      RAD_CONFIG%USE_GENERAL_CLOUD_OPTICS   = .TRUE.
      RAD_CONFIG%USE_GENERAL_AEROSOL_OPTICS = .TRUE.

      ! If NSWGASOPTICS or NLWGASOPTICS are negative then use the
      ! default models; if positive then interpret as the number of g
      ! points
      IF (YDERAD%NSWGASOPTICS == 64) THEN
        ! 19-band model with near-IR bands for each window to
        ! accurately capture cloud absorption, and fine bands in the
        ! UV for UV index
        RAD_CONFIG%GAS_OPTICS_SW_OVERRIDE_FILE_NAME &
            & = 'ecckd-1.2_sw_climate_window-64b_ckd-definition.nc'
      ELSEIF (YDERAD%NSWGASOPTICS > 0) THEN
        ! Usually the 16- and 32-point models are available
        WRITE(RAD_CONFIG%GAS_OPTICS_SW_OVERRIDE_FILE_NAME, '(a,i0,a)') &
            &  'ecckd-1.0_sw_climate_rgb-', YDERAD%NSWGASOPTICS, &
            &  'b_ckd-definition.nc'
      ELSE
        RAD_CONFIG%GAS_OPTICS_SW_OVERRIDE_FILE_NAME &
            & = 'ecckd-1.0_sw_climate_rgb-32b_ckd-definition.nc'
      ENDIF

      IF (YDERAD%NLWGASOPTICS == 64) THEN
        ! 13-band model
        RAD_CONFIG%GAS_OPTICS_LW_OVERRIDE_FILE_NAME &
            & = 'ecckd-1.2_lw_climate_narrow-64b_ckd-definition.nc'
      ELSEIF (YDERAD%NLWGASOPTICS > 0) THEN
        ! Usually the 16- and 32-point models are available
        WRITE(RAD_CONFIG%GAS_OPTICS_LW_OVERRIDE_FILE_NAME, '(a,i0,a)') &
            &  'ecckd-1.0_lw_climate_fsck-', YDERAD%NLWGASOPTICS, &
            &  'b_ckd-definition.nc'
      ELSE
        RAD_CONFIG%GAS_OPTICS_LW_OVERRIDE_FILE_NAME &
            &  = 'ecckd-1.0_lw_climate_fsck-32b_ckd-definition.nc'
      ENDIF

    ELSEIF (YDERAD%NGASOPTICSSCHEME == 2 .OR. YDERAD%NGASOPTICSSCHEME == 3) THEN

      ! RRTMGP / RRTMGP-NN (Neural Network) gas optics scheme
      ! RRTMGP requires the gases to be known when the look-up-tables are loaded
      ! within setup_radiation
      ALLOCATE(RAD_CONFIG%RRTMGP_GAS_NAMES(11)) ! All present gases + nitrogen
      RAD_CONFIG%RRTMGP_GAS_NAMES = (/'n2    ', 'h2o   ', 'co2   ', 'o3    ', &
          &                           'n2o   ', 'ch4   ', 'o2    ', 'cfc11 ', &
          &                           'cfc12 ', 'hcfc22', 'ccl4  ' /)

      RAD_CONFIG%USE_GENERAL_CLOUD_OPTICS   = .TRUE.
      RAD_CONFIG%USE_GENERAL_AEROSOL_OPTICS = .TRUE.

      IF (YDERAD%NGASOPTICSSCHEME == 2) THEN
        ! RRTMGP gas optics scheme
 !       RAD_CONFIG%I_GAS_MODEL                = IGasModelRRTMGP
      ELSE
        ! RRTMGP-NN gas optics scheme
 !       RAD_CONFIG%I_GAS_MODEL                = IGasModelRRTMGP_NN
      ENDIF

    ELSE 

      WRITE(NULERR,'(a)') '*** Error: NGASOPTICSSCHEME must be 0, 1, 2, or 3'
      CALL ABOR1('RADIATION_SETUP: error interpreting NGASOPTICSSCHEME')

    ENDIF


    ! *** SETUP CLOUD OPTICS ***

    ! Setup liquid optics
    IF (YDERAD%NLIQOPT == 2) THEN
      RAD_CONFIG%I_LIQ_MODEL = ILIQUIDMODELSLINGO
    ELSEIF (YDERAD%NLIQOPT == 4) THEN
      RAD_CONFIG%I_LIQ_MODEL = ILIQUIDMODELSOCRATES
    ELSE
      WRITE(NULERR,'(a,i0)') '*** Error: Unavailable liquid optics model in modular radiation scheme: NLIQOPT=', &
           &  YDERAD%NLIQOPT
      CALL ABOR1('RADIATION_SETUP: error interpreting NLIQOPT')
    ENDIF

    ! Setup ice optics
    IF (YDERAD%NICEOPT == 3) THEN
      RAD_CONFIG%I_ICE_MODEL = IICEMODELFU
      IF (YDERAD%LFU_LW_ICE_OPTICS_BUG) THEN
        RAD_CONFIG%DO_FU_LW_ICE_OPTICS_BUG = .TRUE.
      ENDIF
    ELSEIF (YDERAD%NICEOPT == 4) THEN
      RAD_CONFIG%I_ICE_MODEL = IICEMODELBARAN
    ELSE
      WRITE(NULERR,'(a,i0)') '*** Error: Unavailable ice optics model in modular radiation scheme: NICEOPT=', &
           &  YDERAD%NICEOPT
!!      CALL ABOR1('RADIATION_SETUP: error interpreting NICEOPT')   !db fix
    ENDIF

    ! For consistency with earlier versions of the IFS radiation
    ! scheme, until 45R1 we performed shortwave delta-Eddington
    ! scaling after the merge of the cloud, aerosol and gas optical
    ! properties.  Setting this to "false" does the scaling on the
    ! cloud and aerosol properties separately before merging with
    ! gases, which is more physically appropriate. The impact is very
    ! small (see item 6 of table 2 of Technical Memo 787).
    RAD_CONFIG%DO_SW_DELTA_SCALING_WITH_GASES = .FALSE.


    ! *** SETUP AEROSOLS ***

    RAD_CONFIG%USE_AEROSOLS = .TRUE.

    ! If monochromatic aerosol properties are available they will be
    ! read in automatically so the following is not needed
    !IF (YDEAERATM%LAERRAD) RAD_CONFIG%AEROSOL_OPTICS%READ_MONOCHROMATIC_OPTICS=.TRUE.

    IF (YDEAERATM%LAERCCN .OR. YDEAERATM%LAERRRTM .OR. YDERAD%NAERMACC == 1) THEN
      ! Using MACC climatology or prognostic aerosol variables - in
      ! this case the aerosol optics file will be chosen automatically

      ! 12 IFS aerosol classes: 1-3 Sea salt, 4-6 Boucher desert dust,
      ! 7 hydrophilic organics, 8 hydrophobic organics, 9&10
      ! hydrophobic black carbon, 11 ammonium sulphate, 12 inactive
      ! SO2
      RAD_CONFIG%N_AEROSOL_TYPES = 12

      ! Indices to the aerosol optical properties in
      ! aerosol_ifs_rrtm_*.nc, for each class, where negative numbers
      ! index hydrophilic aerosol types and positive numbers index
      ! hydrophobic aerosol types
      RAD_CONFIG%I_AEROSOL_TYPE_MAP = 0 ! There can be up to 256 types
      RAD_CONFIG%I_AEROSOL_TYPE_MAP(1:12) = (/&
           &  -1,&! Sea salt, size bin 1 (OPAC)
           &  -2,&! Sea salt, size bin 2 (OPAC)
           &  -3,&! Sea salt, size bin 3 (OPAC)
           &  7,&! Desert dust, size bin 1 (Woodward 2001)
           &  8,&! Desert dust, size bin 2 (Woodward 2001)
           &  9,&! Desert dust, size bin 3 (Woodward 2001)
           &  -4,&! Hydrophilic organic matter (Hess, OPAC)
           &  13,&! Hydrophobic organic matter (Hess, OPAC)
           &  15,&! Black carbon (Hess, OPAC)
           &  15,&! Black carbon (Hess, OPAC)
           &  -6,&! Ammonium sulphate (GACP)
           &  18 /)  ! Stratospheric sulphate (GACP) [ climatology only ]

      IF (YDERAD%NAERMACC == 0) THEN
      ! Only if prognostic aerosols in radiation

        RAD_CONFIG%I_AEROSOL_TYPE_MAP(4:6) = (/ 10, 11, 12 /) ! Desert dust composite

        RAD_CONFIG%I_AEROSOL_TYPE_MAP(12) = 0 ! Sulphur dioxide gas precursor (inactive)

        ! Note that LAERCHEM is accounted for in RADINTG by skipping index 12
        ! of the input array, rather than shifting the indices here.

        IF (LAERNITRATE) THEN
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(11) = -7 ! Sulphate (GACP) without ammonium factor
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(RAD_CONFIG%N_AEROSOL_TYPES+1:RAD_CONFIG%N_AEROSOL_TYPES+3) = (/&
           &  -12 ,& ! Fine-mode nitrate (GLOMAP)
           &  -13,& ! Coarse-mode nitrate (GLOMAP)
           &  -11 /) ! Ammonium sulphate (GACP) [for ammonium nitrate]
          RAD_CONFIG%N_AEROSOL_TYPES = RAD_CONFIG%N_AEROSOL_TYPES + 3
        ENDIF
        IF (LAERSOA) THEN
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(7:8) = (/ -5, 14 /) ! Primary OM Brown et al., 2018
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(RAD_CONFIG%N_AEROSOL_TYPES+1:RAD_CONFIG%N_AEROSOL_TYPES+2) = (/&
           &  -9 ,& ! Hydrophilic biogenic SOA
           &  -10 /) ! Hydrophilic anthropogenic SOA
          RAD_CONFIG%N_AEROSOL_TYPES = RAD_CONFIG%N_AEROSOL_TYPES + 2
        ENDIF
        IF (LAERVOL) THEN
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(RAD_CONFIG%N_AEROSOL_TYPES+1:RAD_CONFIG%N_AEROSOL_TYPES+3) = (/&
           &   9 ,& ! Volcanic fly ash (treat as coarse dust)
           &  -6 ,& ! Volcanic sulphate (treat as normal ammonium sulphate)
           &   0 /) ! Volcanic sulphur dioxide gas precursor (inactive)
          RAD_CONFIG%N_AEROSOL_TYPES = RAD_CONFIG%N_AEROSOL_TYPES + 3
        ENDIF
      ENDIF

      ! Background aerosol mass-extinction coefficients are obtained
      ! after the configuration files have been read - see later in
      ! this routine.

      ! The default aerosol optics file is the following - please
      ! update here, not in radiation/module/radiation_config.F90
      RAD_CONFIG%AEROSOL_OPTICS_OVERRIDE_FILE_NAME = 'aerosol_ifs_rrtm_48R1_v2.nc'

    ELSE
      ! Using Tegen climatology
      RAD_CONFIG%N_AEROSOL_TYPES = 6
      RAD_CONFIG%I_AEROSOL_TYPE_MAP = 0 ! There can be up to 256 types
      RAD_CONFIG%I_AEROSOL_TYPE_MAP(1:6) = (/&
           &  1,&! Continental background
           &  2,&! Maritime
           &  3,&! Desert
           &  4,&! Urban
           &  5,&! Volcanic active
           &  6 /)  ! Stratospheric background

      ! Manually set the aerosol optics file name (the directory will
      ! be added automatically)
      RAD_CONFIG%AEROSOL_OPTICS_OVERRIDE_FILE_NAME = 'aerosol_ifs_rrtm_tegen.nc'
    ENDIF

    ! *** SETUP SOLVER ***

    ! 3D effects are off by default
    RAD_CONFIG%DO_3D_EFFECTS = .FALSE.

    ! Select longwave solver
    SELECT CASE (YDERAD%NLWSOLVER)
    CASE(0)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERMCICA
    CASE(1)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERSPARTACUS
    CASE(2)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERSPARTACUS
      RAD_CONFIG%DO_3D_EFFECTS = .TRUE.
    CASE(3)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERTRIPLECLOUDS
    CASE(4)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERCLOUDLESS
    CASE DEFAULT
      WRITE(NULERR,'(a,i0)') '*** Error: Unknown value for NLWSOLVER: ', YDERAD%NLWSOLVER
      CALL ABOR1('RADIATION_SETUP: error interpreting NLWSOLVER')
    END SELECT

    ! Select shortwave solver
    SELECT CASE (YDERAD%NSWSOLVER)
    CASE(0)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERMCICA
    CASE(1)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERSPARTACUS
      RAD_CONFIG%DO_3D_EFFECTS = .FALSE.
      IF (YDERAD%NLWSOLVER == 2) THEN
        CALL ABOR1('RADIATION_SETUP: cannot represent 3D effects in LW but not SW')
      ENDIF
    CASE(2)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERSPARTACUS
      RAD_CONFIG%DO_3D_EFFECTS = .TRUE.
      IF (YDERAD%NLWSOLVER == 1) THEN
        CALL ABOR1('RADIATION_SETUP: cannot represent 3D effects in SW but not LW')
      ENDIF
    CASE(3)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERTRIPLECLOUDS
    CASE(4)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERCLOUDLESS
    CASE DEFAULT
      WRITE(NULERR,'(a,i0)') '*** Error: Unknown value for NSWSOLVER: ', YDERAD%NSWSOLVER
      CALL ABOR1('RADIATION_SETUP: error interpreting NSWSOLVER')
    END SELECT

    ! For stability the cloud effective size can't be too small in
    ! SPARTACUS
    RAD_CONFIG%MIN_CLOUD_EFFECTIVE_SIZE = 500.0_JPRB

    ! SPARTACUS solver requires delta scaling to be done separately
    ! for clouds & aerosols
    IF (RAD_CONFIG%I_SOLVER_SW == ISOLVERSPARTACUS) THEN
      RAD_CONFIG%DO_SW_DELTA_SCALING_WITH_GASES = .FALSE.
    ENDIF

    ! Do we represent longwave scattering?
    RAD_CONFIG%DO_LW_CLOUD_SCATTERING = .FALSE.
    RAD_CONFIG%DO_LW_AEROSOL_SCATTERING = .FALSE.
    SELECT CASE (YDERAD%NLWSCATTERING)
    CASE(1)
      RAD_CONFIG%DO_LW_CLOUD_SCATTERING = .TRUE.
    CASE(2)
      RAD_CONFIG%DO_LW_CLOUD_SCATTERING = .TRUE.
      IF (YDERAD%NAERMACC > 0) THEN
        ! Tegen climatology omits data required to do longwave
        ! scattering by aerosols, so only turn this on with a more
        ! recent scattering database
        RAD_CONFIG%DO_LW_AEROSOL_SCATTERING = .TRUE.
      ENDIF
    END SELECT

    SELECT CASE (YDERAD%NCLOUDOVERLAP)
    CASE (1)
      RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPMAXIMUMRANDOM
    CASE (2)
      ! Use Exponential-Exponential cloud overlap to match original IFS
      ! implementation of Raisanen cloud generator
      RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPEXPONENTIAL
    CASE (3)
      RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPEXPONENTIALRANDOM
    CASE DEFAULT
      WRITE(NULERR,'(a,i0)') '*** Error: Unknown value for NCLOUDOVERLAP: ', YDERAD%NCLOUDOVERLAP
      CALL ABOR1('RADIATION_SETUP: error interpreting NCLOUDOVERLAP')
    END SELECT

    ! Change cloud overlap to exponential-random if Tripleclouds or
    ! SPARTACUS selected as both the shortwave and longwave solvers
    IF (RAD_CONFIG%I_OVERLAP_SCHEME /= IOVERLAPEXPONENTIALRANDOM &
         & .AND. (     RAD_CONFIG%I_SOLVER_SW == ISOLVERTRIPLECLOUDS &
         &        .OR. RAD_CONFIG%I_SOLVER_LW == ISOLVERTRIPLECLOUDS &
         &        .OR. RAD_CONFIG%I_SOLVER_SW == ISOLVERSPARTACUS &
         &        .OR. RAD_CONFIG%I_SOLVER_LW == ISOLVERSPARTACUS)) THEN
      IF (RAD_CONFIG%I_SOLVER_SW == RAD_CONFIG%I_SOLVER_LW) THEN
        WRITE(NULOUT,'(a)') 'Warning: Tripleclouds/SPARTACUS solver selected so changing cloud overlap to Exp-Ran'
        RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPEXPONENTIALRANDOM
      ELSE
        ! If the solvers are not the same and exponential-random has
        ! not been selected then abort
        WRITE(NULERR,'(a)') '*** Error: Tripleclouds and SPARTACUS solvers can only simulate exponential-random overlap'
        CALL ABOR1('RADIATION_SETUP: Cloud overlap incompatible with solver')
      ENDIF

      ! For additional stability in SPARTACUS solver it helps if the
      ! cloud fraction threshold is higher than the default of 1.0e-6
      ! used for McICA; this is done for Tripleclouds too so that it
      ! is a good control for SPARTACUS.
      RAD_CONFIG%CLOUD_FRACTION_THRESHOLD = 2.5E-5_JPRB
    ENDIF


    ! Number of longwave surface emissivity intervals to use:
    ! Traditional approach: one value of emissivty for parts of the
    ! spectrum on either side of the infrared atmospheric window
    ! (PEMIR), and one value for the window itself (PEMIW)
    YDERAD%NLWEMISS = 2
    ! ...and the longwave approximate update scheme uses a single
    ! broadband emissivity
    YDERAD%NLWOUT   = 1
    ! Create a spectral Planck look-up table, used by RADHEATN.  Note
    ! that this routine makes use of the length of its third argument.
    ! The wavelength bounds (metres) allow for the first emissivity to
    ! represent values outside the infrared atmospheric window, and the
    ! second emissivity to represent values within it.
    ! CALL YDERAD%YSPECTPLANCK%INIT(2, [ 8.0E-6_JPRB, 13.0E-6_JPRB ], &
    !      &  [ 1,2,1 ])
    CALL INIT(YDERAD%YSPECTPLANCK,2, [ 8.0E-6_JPRB, 13.0E-6_JPRB ], &
    &  [ 1,2,1 ])
    ! Populate the mapping between the 14 RRTM shortwave bands and the
    ! 6 albedo inputs.
    YDERAD%NSW = 6
    ZWAVBOUND(1:5) = [ 0.25e-6_jprb, 0.44e-6_jprb, 0.69e-6_jprb, &
         &             1.19e-6_jprb, 2.38e-6_jprb ]
    IBAND(1:6)  = [ 1,2,3,4,5,6 ]
    ! If NALBEDOSCHEME==2 then we are using the 6-component MODIS
    ! albedo climatology, and a weighted average is used to compute
    ! the albedos in each ecRad spectral band. If NALBEDOSCHEME==3
    ! then we use the diffuse part of the 4 components but still with
    ! a weighted average. Otherwise the older behaviour is followed:
    ! the nearest albedo interval to each band is selected, resulting
    ! in a discrete mapping that matches the one in YOESRTWN:NMPSRTM.
    ! Note that this tends to bias albedo high because there is a lot
    ! of energy around the interface between the UV-Vis and Near-IR
    ! channels, so this should be close to the 0.7 microns intended by
    ! the MODIS dataset, not shifted to the nearest RRTM band boundary
    ! at 0.625 microns.
    LL_DO_NEAREST_SW_ALBEDO = .FALSE.
    ! CALL RAD_CONFIG%DEFINE_SW_ALBEDO_INTERVALS(YDERAD%NSW, ZWAVBOUND, IBAND, &
    !      &  DO_NEAREST=LL_DO_NEAREST_SW_ALBEDO)
    CALL DEFINE_SW_ALBEDO_INTERVALS_GPU(RAD_CONFIG, YDERAD%NSW, ZWAVBOUND, IBAND, &
         &  DO_NEAREST=LL_DO_NEAREST_SW_ALBEDO, lacc=lacc)

    ! Likewise between the 16 RRTM longwave bands and the NLWEMISS
    ! emissivity inputs - these are defined in suecrad.F90.
    LL_DO_NEAREST_LW_EMISS = .TRUE.
    CALL RAD_CONFIG%DEFINE_LW_EMISS_INTERVALS_GPU(UBOUND(YSPECTPLANCK%INTERVAL_MAP,1), &
         &  YSPECTPLANCK%WAVLEN_BOUND, YSPECTPLANCK%INTERVAL_MAP, &
         &  DO_NEAREST=LL_DO_NEAREST_LW_EMISS, lacc=lacc)

    ! Do we scale the incoming solar radiation in each band?
    IF (YDERAD%NSOLARSPECTRUM == 1) THEN
      IF (RAD_CONFIG%N_BANDS_SW /= 14) THEN
        WRITE(NULERR,'(a)') '*** Error: Shortwave must have 14 bands to apply spectral scaling'
        CALL ABOR1('RADIATION_SETUP: Shortwave must have 14 bands to apply spectral scaling')
      ELSE
        RAD_CONFIG%USE_SPECTRAL_SOLAR_SCALING = .TRUE.
      ENDIF
    ENDIF

    ! *** IMPLEMENT SETTINGS ***

    ! For advanced configuration, the configuration data for the
    ! "radiation" project can specified directly in the namelist.
    ! However, the variable naming convention is not consistent with
    ! the rest of the IFS.  For basic configuration there are specific
    ! variables in the NAERAD namelist available in the YDERAD
    ! structure.
    CALL POSNAME(NULNAM, 'RADIATION', ISTAT)
    SELECT CASE (ISTAT)
      CASE(0)
        CALL RAD_CONFIG%READ_GPU(UNIT=NULNAM, lacc=lacc)
      CASE(1)
        WRITE(NULOUT,'(a)') 'Namelist RADIATION not found, using settings from NAERAD only'
      CASE DEFAULT
        CALL ABOR1('RADIATION_SETUP: error reading RADIATION section of namelist file')
    END SELECT
    IF (PRESENT(FILE_NAME)) THEN
      CALL RAD_CONFIG%READ_GPU(FILE_NAME=FILE_NAME, lacc=lacc)
    ENDIF

    ! Use configuration data to set-up radiation scheme, including
    ! reading scattering datafiles
    CALL SETUP_RADIATION_GPU(YDERDI,RAD_CONFIG, lacc=lacc)

    ! Print configuration
    IF (IVERBOSESETUP > 1) THEN
      WRITE(NULOUT,'(a)') 'Radiation scheme settings:'
      CALL RAD_CONFIG%PRINT_GPU(IVERBOSE=IVERBOSESETUP, lacc=lacc)
    ENDIF

    ! Get spectral weightings for UV and PAR
    CALL RAD_CONFIG%GET_SW_WEIGHTS_GPU(0.2E-6_JPRB, 0.4415E-6_JPRB,&
         &  PRADIATION%NWEIGHT_UV, PRADIATION%IBAND_UV, PRADIATION%WEIGHT_UV,&
         &  'ultraviolet', lacc=lacc)
    CALL RAD_CONFIG%GET_SW_WEIGHTS_GPU(0.4E-6_JPRB, 0.7E-6_JPRB,&
         &  PRADIATION%NWEIGHT_PAR, PRADIATION%IBAND_PAR, PRADIATION%WEIGHT_PAR,&
         &  'photosynthetically active radiation, PAR', lacc=lacc)

    IF (YDERAD%NAERMACC > 0) THEN
      ! With the MACC aerosol climatology we need to add in the
      ! background aerosol afterwards using the Tegen arrays.  In this
      ! case we first configure the background aerosol mass-extinction
      ! coefficient at 550 nm
      PRADIATION%TROP_BG_AER_MASS_EXT  = DRY_AEROSOL_MASS_EXTINCTION_GPU(RAD_CONFIG,&
           &                                   ITYPE_TROP_BG_AER, 550.0E-9_JPRB, lacc)
      PRADIATION%STRAT_BG_AER_MASS_EXT = DRY_AEROSOL_MASS_EXTINCTION_GPU(RAD_CONFIG,&
           &                                   ITYPE_STRAT_BG_AER, 550.0E-9_JPRB, lacc)
      WRITE(NULOUT,'(a,i0)') 'Tropospheric background uses aerosol type ',&
           &                 ITYPE_TROP_BG_AER
      WRITE(NULOUT,'(a,i0)') 'Stratospheric background uses aerosol type ',&
           &                 ITYPE_STRAT_BG_AER
    ELSE
      PRADIATION%TROP_BG_AER_MASS_EXT  = 0.0_JPRB
      PRADIATION%STRAT_BG_AER_MASS_EXT = 0.0_JPRB
      PRADIATION%TROP_BG_AER_MASS_EXT  = 0.0_JPRB
      PRADIATION%STRAT_BG_AER_MASS_EXT = 0.0_JPRB
    ENDIF

    IF(YDEAERATM%LAERRRTM) THEN
       CALL SETUP_MONO_AER_OPTICS_GPU(PRADIATION, YDCOMPO, lacc=lacc)
    ENDIF
      
    IF (IVERBOSESETUP > 1) THEN
      WRITE(NULOUT,'(a)') '-------------------------------------------------------------------------------'
    ENDIF

    END ASSOCIATE

    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_RADIATION_SCHEME_GPU',1,ZHOOK_HANDLE)

  END SUBROUTINE SETUP_RADIATION_SCHEME_GPU

  SUBROUTINE SETUP_MONO_AER_OPTICS_GPU(PRADIATION, YDCOMPO, lacc)

    !subroutine to pass the monochromatic aerosol optical properties 
    !to the variables needed by CAMS for aerosol diagnostics and 
    !data assimilation (sobstitutes the old SU_AEROP)

    ! Note that ALF_* are NOT in SI base units [m2 kg-1] as the NetCDF files
    ! are, but [m2 g-1], hence the 1.e-3 conversion factor.

    USE YOMHOOK
    USE RADIATION_AEROSOL_OPTICS_DATA
    USE YOMLUN
    USE YOMCOMPO

    TYPE(TRADIATION), INTENT(IN)  :: PRADIATION
    TYPE(TCOMPO),     INTENT(IN)  :: YDCOMPO

    !fills the old variables for monochromatic aerosol optics
    !using the new structure and values read in from the NetCDF file
    !defined above for the general aerosol optics
    INTEGER(KIND=JPIM) :: IRH
    INTEGER(KIND=JPIM) :: IOM,ISU

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
    logical, intent (in) :: lacc


    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_MONO_AER_OPTICS_GPU',0,ZHOOK_HANDLE)

    ASSOCIATE(RAD_CONFIG=>PRADIATION%RAD_CONFIG,&
              & LAERNITRATE=>YDCOMPO%LAERNITRATE, &
              & LAERSOA=>YDCOMPO%LAERSOA)
    ASSOCIATE(AO=>RAD_CONFIG%AEROSOL_OPTICS)

    WRITE(NULOUT,'(a)') 'Setting up monochromatic aerosol optics for diagnostic/DA'


!-- DUST
    ALF_DD(1,:) = AO%MASS_EXT_MONO_PHOBIC(:,10) * 1.e-3_JPRB
    ALF_DD(2,:) = AO%MASS_EXT_MONO_PHOBIC(:,11) * 1.e-3_JPRB
    ALF_DD(3,:) = AO%MASS_EXT_MONO_PHOBIC(:,12) * 1.e-3_JPRB

    OMG_DD(1,:) = AO%SSA_MONO_PHOBIC(:,10)
    OMG_DD(2,:) = AO%SSA_MONO_PHOBIC(:,11)
    OMG_DD(3,:) = AO%SSA_MONO_PHOBIC(:,12)

    ASY_DD(1,:) = AO%G_MONO_PHOBIC(:,10)
    ASY_DD(2,:) = AO%G_MONO_PHOBIC(:,11)
    ASY_DD(3,:) = AO%G_MONO_PHOBIC(:,12)

    RALI_DD(1,:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,10)
    RALI_DD(2,:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,11)
    RALI_DD(3,:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,12)

!-- BLACK CARBON
    ALF_BC(:) = AO%MASS_EXT_MONO_PHOBIC(:,15) * 1.e-3_JPRB
    OMG_BC(:) = AO%SSA_MONO_PHOBIC(:,15)
    ASY_BC(:) = AO%G_MONO_PHOBIC(:,15)
    RALI_BC(:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,15)

    IF (LAERNITRATE) THEN
      ISU=7 ! SU does not include ammonium
    ELSE
      ISU=6 ! SU includes ammonium
    ENDIF

    IF (LAERSOA) THEN
      IOM=5 ! OM is primary only
    ELSE
      IOM=4 ! OM is primary and secondary combined
    ENDIF

    DO IRH=1,12
!-- ORGANIC MATTER
       ALF_OM(IRH,:) = AO%MASS_EXT_MONO_PHILIC(:,IRH,IOM) * 1.e-3_JPRB
       OMG_OM(IRH,:) = AO%SSA_MONO_PHILIC(:,IRH,IOM)
       ASY_OM(IRH,:) = AO%G_MONO_PHILIC(:,IRH,IOM)
       RALI_OM(IRH,:) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,IOM)
!-- SULFATES
       ALF_SU(IRH,:) = (AO%MASS_EXT_MONO_PHILIC(:,IRH,ISU)) * 1.e-3_JPRB
       OMG_SU(IRH,:) = AO%SSA_MONO_PHILIC(:,IRH,ISU)
       ASY_SU(IRH,:) = AO%G_MONO_PHILIC(:,IRH,ISU)
       RALI_SU(IRH,:) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,ISU)
!-- SEA SALT
       ALF_SS(IRH,:,1) = AO%MASS_EXT_MONO_PHILIC(:,IRH,1) * 1.e-3_JPRB
       ALF_SS(IRH,:,2) = AO%MASS_EXT_MONO_PHILIC(:,IRH,2) * 1.e-3_JPRB
       ALF_SS(IRH,:,3) = AO%MASS_EXT_MONO_PHILIC(:,IRH,3) * 1.e-3_JPRB

       OMG_SS(IRH,:,1) = AO%SSA_MONO_PHILIC(:,IRH,1)
       OMG_SS(IRH,:,2) = AO%SSA_MONO_PHILIC(:,IRH,2)
       OMG_SS(IRH,:,3) = AO%SSA_MONO_PHILIC(:,IRH,3)

       ASY_SS(IRH,:,1) = AO%G_MONO_PHILIC(:,IRH,1)
       ASY_SS(IRH,:,2) = AO%G_MONO_PHILIC(:,IRH,2)
       ASY_SS(IRH,:,3) = AO%G_MONO_PHILIC(:,IRH,3)

       RALI_SS(IRH,:,1) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,1)
       RALI_SS(IRH,:,2) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,2)
       RALI_SS(IRH,:,3) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,3)

!-- NITRATE
       ALF_NI(IRH,:,1) = AO%MASS_EXT_MONO_PHILIC(:,IRH,12) * 1.e-3_JPRB
       OMG_NI(IRH,:,1) = AO%SSA_MONO_PHILIC(:,IRH,12)
       ASY_NI(IRH,:,1) = AO%G_MONO_PHILIC(:,IRH,12)
       RALI_NI(IRH,:,1) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,12)

       ALF_NI(IRH,:,2) = AO%MASS_EXT_MONO_PHILIC(:,IRH,13) * 1.e-3_JPRB
       OMG_NI(IRH,:,2) = AO%SSA_MONO_PHILIC(:,IRH,13)
       ASY_NI(IRH,:,2) = AO%G_MONO_PHILIC(:,IRH,13)
       RALI_NI(IRH,:,2) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,13)

!-- AMMONIUM
       ALF_AM(IRH,:) = AO%MASS_EXT_MONO_PHILIC(:,IRH,11) * 1.e-3_JPRB
       OMG_AM(IRH,:) = AO%SSA_MONO_PHILIC(:,IRH,11)
       ASY_AM(IRH,:) = AO%G_MONO_PHILIC(:,IRH,11)
       RALI_AM(IRH,:) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,11)

!-- SOA
       ALF_SOA(IRH,:,1) = AO%MASS_EXT_MONO_PHILIC(:,IRH,9) * 1.e-3_JPRB
       OMG_SOA(IRH,:,1) = AO%SSA_MONO_PHILIC(:,IRH,9)
       ASY_SOA(IRH,:,1) = AO%G_MONO_PHILIC(:,IRH,9)
       RALI_SOA(IRH,:,1) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,9)

       ALF_SOA(IRH,:,2) = AO%MASS_EXT_MONO_PHILIC(:,IRH,10) * 1.e-3_JPRB
       OMG_SOA(IRH,:,2) = AO%SSA_MONO_PHILIC(:,IRH,10)
       ASY_SOA(IRH,:,2) = AO%G_MONO_PHILIC(:,IRH,10)
       RALI_SOA(IRH,:,2) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,10)
    ENDDO

    END ASSOCIATE
    END ASSOCIATE

    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_MONO_AER_OPTICS_GPU',1,ZHOOK_HANDLE)
    
  END SUBROUTINE SETUP_MONO_AER_OPTICS_GPU

  SUBROUTINE SETUP_RADIATION_SCHEME_CPU(YDERDI,YDEAERATM,YDCOMPO,YDEPHY,YDERAD,PRADIATION,LDOUTPUT,FILE_NAME)

    USE YOMHOOK
    USE YOMLUN
    !USE YOESRTWN, ONLY : NMPSRTM
    USE YOERAD,   ONLY : TERAD
    USE YOEPHY,   ONLY : TEPHY
    USE YOEAERATM,ONLY : TEAERATM
    USE YOMCOMPO
    USE YOERDI  , ONLY : TERDI

    USE RADIATION_INTERFACE
    USE RADIATION_AEROSOL_OPTICS

    ! Radiation configuration information
    TYPE(TERDI)       ,INTENT(INOUT):: YDERDI
    TYPE(TEAERATM)    ,INTENT(INOUT)          :: YDEAERATM
    TYPE(TCOMPO)      ,INTENT(IN)             :: YDCOMPO
    TYPE(TEPHY)       ,INTENT(IN)             :: YDEPHY
    TYPE(TERAD)       ,INTENT(INOUT)          :: YDERAD
    TYPE(TRADIATION)  ,INTENT(INOUT), TARGET  :: PRADIATION
    CHARACTER(LEN=512),INTENT(IN), OPTIONAL   :: FILE_NAME

    ! Whether or not to print out information on the radiation scheme
    ! configuration
    LOGICAL, INTENT(IN), OPTIONAL :: LDOUTPUT

    ! Verbosity of configuration information 0=none, 1=warning,
    ! 2=info, 3=progress, 4=detailed, 5=debug
    INTEGER(KIND=JPIM) :: IVERBOSESETUP
    INTEGER(KIND=JPIM) :: ISTAT

    ! Data directory name
    CHARACTER(LEN=256) :: CL_DATA_DIR

    ! Arrays to avoid temporaries
    REAL(KIND=JPRB)    :: ZWAVBOUND(15)
    INTEGER(KIND=JPIM) :: IBAND(16)



    ! Do we use the nearest ecRad band to the albedo/emissivity
    ! intervals, or a more intelligent weighting?
    LOGICAL :: LL_DO_NEAREST_SW_ALBEDO, LL_DO_NEAREST_LW_EMISS

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "posname.intfb.h"
#include "abor1.intfb.h"

    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_RADIATION_SCHEME_CPU',0,ZHOOK_HANDLE)

    ! *** GENERAL SETUP ***
    ASSOCIATE(RAD_CONFIG=>PRADIATION%RAD_CONFIG,&
              & LAERNITRATE=>YDCOMPO%LAERNITRATE, &
              & LAERSOA=>YDCOMPO%LAERSOA, &
              & LAERVOL=>YDEAERATM%LAERVOL, &
              & YSPECTPLANCK=>YDERAD%YSPECTPLANCK)

    ! Configure verbosity of setup of radiation scheme
    IVERBOSESETUP = 4 ! Provide plenty of information
    IF (PRESENT(LDOUTPUT)) THEN
      IF (.NOT. LDOUTPUT) THEN
        IVERBOSESETUP = 1 ! Warnings and errors only
      ENDIF
    ENDIF
    RAD_CONFIG%IVERBOSESETUP = IVERBOSESETUP

    IF (IVERBOSESETUP > 1) THEN
      WRITE(NULOUT,'(a)') '-------------------------------------------------------------------------------'
      WRITE(NULOUT,'(a)') 'RADIATION_SETUP: ecRad 1.6'
    ENDIF

    ! Normal operation of the radiation scheme displays only errors
    ! and warnings
    RAD_CONFIG%IVERBOSE = 1

    ! Read data directory name from the DATA environment variable
    ! CALL GETENV("DATA", CL_DATA_DIR)
    ! IF (CL_DATA_DIR /= " ") THEN
    !   RAD_CONFIG%DIRECTORY_NAME = TRIM(CL_DATA_DIR) // "/ifsdata"
    ! ELSE
    !   ! If DATA not present, use the current directory
    !   RAD_CONFIG%DIRECTORY_NAME = "."
    ! ENDIF
    ! Temporary solution
    RAD_CONFIG%DIRECTORY_NAME = '/scratch/work/seity/EcRad49'

    ! Do we do Hogan and Bozzo (2015) approximate longwave updates?
    RAD_CONFIG%DO_LW_DERIVATIVES = YDERAD%LAPPROXLWUPDATE

    ! If we are to perform Hogan and Bozzo (2015) approximate
    ! shortwave updates then we need the downwelling direct and
    ! diffuse shortwave fluxes at the surface in each albedo spectral
    ! interval
    RAD_CONFIG%DO_CANOPY_FLUXES_SW = YDERAD%LAPPROXSWUPDATE

    ! If we are to perform approximate longwave updates and we are
    ! using the new 6-interval longwave emissivity scheme then we need
    ! ecRad to compute the downwelling surface longwave fluxes in each
    ! emissivity spectral interval
    IF (YDERAD%NLWOUT > 1) THEN
      RAD_CONFIG%DO_CANOPY_FLUXES_LW = .TRUE.
    ENDIF

    ! Surface spectral fluxes are needed for UV and PAR calculations
    RAD_CONFIG%DO_SURFACE_SW_SPECTRAL_FLUX = .TRUE.


    ! *** SETUP GAS OPTICS ***

    !IF (YDERAD%NSWGASOPTICS == 0 .AND. YDERAD%NLWGASOPTICS == 0) THEN
    IF (YDERAD%NGASOPTICSSCHEME == 0) THEN
    !RAD_CONFIG%DO_SETUP_IFSRRTM = .FALSE.
      ! already set-up RRTMG, so the setup_radiation routine below
      ! does not have to
!      RAD_CONFIG%I_GAS_MODEL                = IGasModelIFSRRTMG
      RAD_CONFIG%USE_GENERAL_CLOUD_OPTICS   = .FALSE.
      RAD_CONFIG%USE_GENERAL_AEROSOL_OPTICS = .FALSE.
      RAD_CONFIG%DO_SETUP_IFSRRTM           = .FALSE.

    ! ELSEIF (YDERAD%NSWGASOPTICS == 0 .OR. YDERAD%NLWGASOPTICS == 0) THEN

    !   WRITE(NULERR,'(a)') '*** Error: NSWGASOPTICS and NLWGASOPTICS must either both be zero or both nonzero'
    !   CALL ABOR1('RADIATION_SETUP: error interpreting NSWGASOPTICS and NLWGASOPTICS')

    ! ELSE

    ELSEIF (YDERAD%NGASOPTICSSCHEME == 1) THEN
      ! ecCKD gas optics scheme
!      RAD_CONFIG%I_GAS_MODEL                = IGasModelECCKD
      RAD_CONFIG%USE_GENERAL_CLOUD_OPTICS   = .TRUE.
      RAD_CONFIG%USE_GENERAL_AEROSOL_OPTICS = .TRUE.

      ! If NSWGASOPTICS or NLWGASOPTICS are negative then use the
      ! default models; if positive then interpret as the number of g
      ! points
      IF (YDERAD%NSWGASOPTICS == 64) THEN
        ! 19-band model with near-IR bands for each window to
        ! accurately capture cloud absorption, and fine bands in the
        ! UV for UV index
        RAD_CONFIG%GAS_OPTICS_SW_OVERRIDE_FILE_NAME &
            & = 'ecckd-1.2_sw_climate_window-64b_ckd-definition.nc'
      ELSEIF (YDERAD%NSWGASOPTICS > 0) THEN
        ! Usually the 16- and 32-point models are available
        WRITE(RAD_CONFIG%GAS_OPTICS_SW_OVERRIDE_FILE_NAME, '(a,i0,a)') &
            &  'ecckd-1.0_sw_climate_rgb-', YDERAD%NSWGASOPTICS, &
            &  'b_ckd-definition.nc'
      ELSE
        RAD_CONFIG%GAS_OPTICS_SW_OVERRIDE_FILE_NAME &
            & = 'ecckd-1.0_sw_climate_rgb-32b_ckd-definition.nc'
      ENDIF

      IF (YDERAD%NLWGASOPTICS == 64) THEN
        ! 13-band model
        RAD_CONFIG%GAS_OPTICS_LW_OVERRIDE_FILE_NAME &
            & = 'ecckd-1.2_lw_climate_narrow-64b_ckd-definition.nc'
      ELSEIF (YDERAD%NLWGASOPTICS > 0) THEN
        ! Usually the 16- and 32-point models are available
        WRITE(RAD_CONFIG%GAS_OPTICS_LW_OVERRIDE_FILE_NAME, '(a,i0,a)') &
            &  'ecckd-1.0_lw_climate_fsck-', YDERAD%NLWGASOPTICS, &
            &  'b_ckd-definition.nc'
      ELSE
        RAD_CONFIG%GAS_OPTICS_LW_OVERRIDE_FILE_NAME &
            &  = 'ecckd-1.0_lw_climate_fsck-32b_ckd-definition.nc'
      ENDIF

    ELSEIF (YDERAD%NGASOPTICSSCHEME == 2 .OR. YDERAD%NGASOPTICSSCHEME == 3) THEN

      ! RRTMGP / RRTMGP-NN (Neural Network) gas optics scheme
      ! RRTMGP requires the gases to be known when the look-up-tables are loaded
      ! within setup_radiation
      ALLOCATE(RAD_CONFIG%RRTMGP_GAS_NAMES(11)) ! All present gases + nitrogen
      RAD_CONFIG%RRTMGP_GAS_NAMES = (/'n2    ', 'h2o   ', 'co2   ', 'o3    ', &
          &                           'n2o   ', 'ch4   ', 'o2    ', 'cfc11 ', &
          &                           'cfc12 ', 'hcfc22', 'ccl4  ' /)

      RAD_CONFIG%USE_GENERAL_CLOUD_OPTICS   = .TRUE.
      RAD_CONFIG%USE_GENERAL_AEROSOL_OPTICS = .TRUE.

      IF (YDERAD%NGASOPTICSSCHEME == 2) THEN
        ! RRTMGP gas optics scheme
 !       RAD_CONFIG%I_GAS_MODEL                = IGasModelRRTMGP
      ELSE
        ! RRTMGP-NN gas optics scheme
 !       RAD_CONFIG%I_GAS_MODEL                = IGasModelRRTMGP_NN
      ENDIF

    ELSE 

      WRITE(NULERR,'(a)') '*** Error: NGASOPTICSSCHEME must be 0, 1, 2, or 3'
      CALL ABOR1('RADIATION_SETUP: error interpreting NGASOPTICSSCHEME')

    ENDIF


    ! *** SETUP CLOUD OPTICS ***

    ! Setup liquid optics
    IF (YDERAD%NLIQOPT == 2) THEN
      RAD_CONFIG%I_LIQ_MODEL = ILIQUIDMODELSLINGO
    ELSEIF (YDERAD%NLIQOPT == 4) THEN
      RAD_CONFIG%I_LIQ_MODEL = ILIQUIDMODELSOCRATES
    ELSE
      WRITE(NULERR,'(a,i0)') '*** Error: Unavailable liquid optics model in modular radiation scheme: NLIQOPT=', &
           &  YDERAD%NLIQOPT
      CALL ABOR1('RADIATION_SETUP: error interpreting NLIQOPT')
    ENDIF

    ! Setup ice optics
    IF (YDERAD%NICEOPT == 3) THEN
      RAD_CONFIG%I_ICE_MODEL = IICEMODELFU
      IF (YDERAD%LFU_LW_ICE_OPTICS_BUG) THEN
        RAD_CONFIG%DO_FU_LW_ICE_OPTICS_BUG = .TRUE.
      ENDIF
    ELSEIF (YDERAD%NICEOPT == 4) THEN
      RAD_CONFIG%I_ICE_MODEL = IICEMODELBARAN
    ELSE
      WRITE(NULERR,'(a,i0)') '*** Error: Unavailable ice optics model in modular radiation scheme: NICEOPT=', &
           &  YDERAD%NICEOPT
!!      CALL ABOR1('RADIATION_SETUP: error interpreting NICEOPT')   !db fix
    ENDIF

    ! For consistency with earlier versions of the IFS radiation
    ! scheme, until 45R1 we performed shortwave delta-Eddington
    ! scaling after the merge of the cloud, aerosol and gas optical
    ! properties.  Setting this to "false" does the scaling on the
    ! cloud and aerosol properties separately before merging with
    ! gases, which is more physically appropriate. The impact is very
    ! small (see item 6 of table 2 of Technical Memo 787).
    RAD_CONFIG%DO_SW_DELTA_SCALING_WITH_GASES = .FALSE.


    ! *** SETUP AEROSOLS ***

    RAD_CONFIG%USE_AEROSOLS = .TRUE.

    ! If monochromatic aerosol properties are available they will be
    ! read in automatically so the following is not needed
    !IF (YDEAERATM%LAERRAD) RAD_CONFIG%AEROSOL_OPTICS%READ_MONOCHROMATIC_OPTICS=.TRUE.

    IF (YDEAERATM%LAERCCN .OR. YDEAERATM%LAERRRTM .OR. YDERAD%NAERMACC == 1) THEN
      ! Using MACC climatology or prognostic aerosol variables - in
      ! this case the aerosol optics file will be chosen automatically

      ! 12 IFS aerosol classes: 1-3 Sea salt, 4-6 Boucher desert dust,
      ! 7 hydrophilic organics, 8 hydrophobic organics, 9&10
      ! hydrophobic black carbon, 11 ammonium sulphate, 12 inactive
      ! SO2
      RAD_CONFIG%N_AEROSOL_TYPES = 12

      ! Indices to the aerosol optical properties in
      ! aerosol_ifs_rrtm_*.nc, for each class, where negative numbers
      ! index hydrophilic aerosol types and positive numbers index
      ! hydrophobic aerosol types
      RAD_CONFIG%I_AEROSOL_TYPE_MAP = 0 ! There can be up to 256 types
      RAD_CONFIG%I_AEROSOL_TYPE_MAP(1:12) = (/&
           &  -1,&! Sea salt, size bin 1 (OPAC)
           &  -2,&! Sea salt, size bin 2 (OPAC)
           &  -3,&! Sea salt, size bin 3 (OPAC)
           &  7,&! Desert dust, size bin 1 (Woodward 2001)
           &  8,&! Desert dust, size bin 2 (Woodward 2001)
           &  9,&! Desert dust, size bin 3 (Woodward 2001)
           &  -4,&! Hydrophilic organic matter (Hess, OPAC)
           &  13,&! Hydrophobic organic matter (Hess, OPAC)
           &  15,&! Black carbon (Hess, OPAC)
           &  15,&! Black carbon (Hess, OPAC)
           &  -6,&! Ammonium sulphate (GACP)
           &  18 /)  ! Stratospheric sulphate (GACP) [ climatology only ]

      IF (YDERAD%NAERMACC == 0) THEN
      ! Only if prognostic aerosols in radiation

        RAD_CONFIG%I_AEROSOL_TYPE_MAP(4:6) = (/ 10, 11, 12 /) ! Desert dust composite

        RAD_CONFIG%I_AEROSOL_TYPE_MAP(12) = 0 ! Sulphur dioxide gas precursor (inactive)

        ! Note that LAERCHEM is accounted for in RADINTG by skipping index 12
        ! of the input array, rather than shifting the indices here.

        IF (LAERNITRATE) THEN
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(11) = -7 ! Sulphate (GACP) without ammonium factor
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(RAD_CONFIG%N_AEROSOL_TYPES+1:RAD_CONFIG%N_AEROSOL_TYPES+3) = (/&
           &  -12 ,& ! Fine-mode nitrate (GLOMAP)
           &  -13,& ! Coarse-mode nitrate (GLOMAP)
           &  -11 /) ! Ammonium sulphate (GACP) [for ammonium nitrate]
          RAD_CONFIG%N_AEROSOL_TYPES = RAD_CONFIG%N_AEROSOL_TYPES + 3
        ENDIF
        IF (LAERSOA) THEN
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(7:8) = (/ -5, 14 /) ! Primary OM Brown et al., 2018
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(RAD_CONFIG%N_AEROSOL_TYPES+1:RAD_CONFIG%N_AEROSOL_TYPES+2) = (/&
           &  -9 ,& ! Hydrophilic biogenic SOA
           &  -10 /) ! Hydrophilic anthropogenic SOA
          RAD_CONFIG%N_AEROSOL_TYPES = RAD_CONFIG%N_AEROSOL_TYPES + 2
        ENDIF
        IF (LAERVOL) THEN
          RAD_CONFIG%I_AEROSOL_TYPE_MAP(RAD_CONFIG%N_AEROSOL_TYPES+1:RAD_CONFIG%N_AEROSOL_TYPES+3) = (/&
           &   9 ,& ! Volcanic fly ash (treat as coarse dust)
           &  -6 ,& ! Volcanic sulphate (treat as normal ammonium sulphate)
           &   0 /) ! Volcanic sulphur dioxide gas precursor (inactive)
          RAD_CONFIG%N_AEROSOL_TYPES = RAD_CONFIG%N_AEROSOL_TYPES + 3
        ENDIF
      ENDIF

      ! Background aerosol mass-extinction coefficients are obtained
      ! after the configuration files have been read - see later in
      ! this routine.

      ! The default aerosol optics file is the following - please
      ! update here, not in radiation/module/radiation_config.F90
      RAD_CONFIG%AEROSOL_OPTICS_OVERRIDE_FILE_NAME = 'aerosol_ifs_rrtm_48R1_v2.nc'

    ELSE
      ! Using Tegen climatology
      RAD_CONFIG%N_AEROSOL_TYPES = 6
      RAD_CONFIG%I_AEROSOL_TYPE_MAP = 0 ! There can be up to 256 types
      RAD_CONFIG%I_AEROSOL_TYPE_MAP(1:6) = (/&
           &  1,&! Continental background
           &  2,&! Maritime
           &  3,&! Desert
           &  4,&! Urban
           &  5,&! Volcanic active
           &  6 /)  ! Stratospheric background

      ! Manually set the aerosol optics file name (the directory will
      ! be added automatically)
      RAD_CONFIG%AEROSOL_OPTICS_OVERRIDE_FILE_NAME = 'aerosol_ifs_rrtm_tegen.nc'
    ENDIF

    ! *** SETUP SOLVER ***

    ! 3D effects are off by default
    RAD_CONFIG%DO_3D_EFFECTS = .FALSE.

    ! Select longwave solver
    SELECT CASE (YDERAD%NLWSOLVER)
    CASE(0)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERMCICA
    CASE(1)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERSPARTACUS
    CASE(2)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERSPARTACUS
      RAD_CONFIG%DO_3D_EFFECTS = .TRUE.
    CASE(3)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERTRIPLECLOUDS
    CASE(4)
      RAD_CONFIG%I_SOLVER_LW = ISOLVERCLOUDLESS
    CASE DEFAULT
      WRITE(NULERR,'(a,i0)') '*** Error: Unknown value for NLWSOLVER: ', YDERAD%NLWSOLVER
      CALL ABOR1('RADIATION_SETUP: error interpreting NLWSOLVER')
    END SELECT

    ! Select shortwave solver
    SELECT CASE (YDERAD%NSWSOLVER)
    CASE(0)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERMCICA
    CASE(1)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERSPARTACUS
      RAD_CONFIG%DO_3D_EFFECTS = .FALSE.
      IF (YDERAD%NLWSOLVER == 2) THEN
        CALL ABOR1('RADIATION_SETUP: cannot represent 3D effects in LW but not SW')
      ENDIF
    CASE(2)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERSPARTACUS
      RAD_CONFIG%DO_3D_EFFECTS = .TRUE.
      IF (YDERAD%NLWSOLVER == 1) THEN
        CALL ABOR1('RADIATION_SETUP: cannot represent 3D effects in SW but not LW')
      ENDIF
    CASE(3)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERTRIPLECLOUDS
    CASE(4)
      RAD_CONFIG%I_SOLVER_SW = ISOLVERCLOUDLESS
    CASE DEFAULT
      WRITE(NULERR,'(a,i0)') '*** Error: Unknown value for NSWSOLVER: ', YDERAD%NSWSOLVER
      CALL ABOR1('RADIATION_SETUP: error interpreting NSWSOLVER')
    END SELECT

    ! For stability the cloud effective size can't be too small in
    ! SPARTACUS
    RAD_CONFIG%MIN_CLOUD_EFFECTIVE_SIZE = 500.0_JPRB

    ! SPARTACUS solver requires delta scaling to be done separately
    ! for clouds & aerosols
    IF (RAD_CONFIG%I_SOLVER_SW == ISOLVERSPARTACUS) THEN
      RAD_CONFIG%DO_SW_DELTA_SCALING_WITH_GASES = .FALSE.
    ENDIF

    ! Do we represent longwave scattering?
    RAD_CONFIG%DO_LW_CLOUD_SCATTERING = .FALSE.
    RAD_CONFIG%DO_LW_AEROSOL_SCATTERING = .FALSE.
    SELECT CASE (YDERAD%NLWSCATTERING)
    CASE(1)
      RAD_CONFIG%DO_LW_CLOUD_SCATTERING = .TRUE.
    CASE(2)
      RAD_CONFIG%DO_LW_CLOUD_SCATTERING = .TRUE.
      IF (YDERAD%NAERMACC > 0) THEN
        ! Tegen climatology omits data required to do longwave
        ! scattering by aerosols, so only turn this on with a more
        ! recent scattering database
        RAD_CONFIG%DO_LW_AEROSOL_SCATTERING = .TRUE.
      ENDIF
    END SELECT

    SELECT CASE (YDERAD%NCLOUDOVERLAP)
    CASE (1)
      RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPMAXIMUMRANDOM
    CASE (2)
      ! Use Exponential-Exponential cloud overlap to match original IFS
      ! implementation of Raisanen cloud generator
      RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPEXPONENTIAL
    CASE (3)
      RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPEXPONENTIALRANDOM
    CASE DEFAULT
      WRITE(NULERR,'(a,i0)') '*** Error: Unknown value for NCLOUDOVERLAP: ', YDERAD%NCLOUDOVERLAP
      CALL ABOR1('RADIATION_SETUP: error interpreting NCLOUDOVERLAP')
    END SELECT

    ! Change cloud overlap to exponential-random if Tripleclouds or
    ! SPARTACUS selected as both the shortwave and longwave solvers
    IF (RAD_CONFIG%I_OVERLAP_SCHEME /= IOVERLAPEXPONENTIALRANDOM &
         & .AND. (     RAD_CONFIG%I_SOLVER_SW == ISOLVERTRIPLECLOUDS &
         &        .OR. RAD_CONFIG%I_SOLVER_LW == ISOLVERTRIPLECLOUDS &
         &        .OR. RAD_CONFIG%I_SOLVER_SW == ISOLVERSPARTACUS &
         &        .OR. RAD_CONFIG%I_SOLVER_LW == ISOLVERSPARTACUS)) THEN
      IF (RAD_CONFIG%I_SOLVER_SW == RAD_CONFIG%I_SOLVER_LW) THEN
        WRITE(NULOUT,'(a)') 'Warning: Tripleclouds/SPARTACUS solver selected so changing cloud overlap to Exp-Ran'
        RAD_CONFIG%I_OVERLAP_SCHEME = IOVERLAPEXPONENTIALRANDOM
      ELSE
        ! If the solvers are not the same and exponential-random has
        ! not been selected then abort
        WRITE(NULERR,'(a)') '*** Error: Tripleclouds and SPARTACUS solvers can only simulate exponential-random overlap'
        CALL ABOR1('RADIATION_SETUP: Cloud overlap incompatible with solver')
      ENDIF

      ! For additional stability in SPARTACUS solver it helps if the
      ! cloud fraction threshold is higher than the default of 1.0e-6
      ! used for McICA; this is done for Tripleclouds too so that it
      ! is a good control for SPARTACUS.
      RAD_CONFIG%CLOUD_FRACTION_THRESHOLD = 2.5E-5_JPRB
    ENDIF


    ! Number of longwave surface emissivity intervals to use:
    ! Traditional approach: one value of emissivty for parts of the
    ! spectrum on either side of the infrared atmospheric window
    ! (PEMIR), and one value for the window itself (PEMIW)
    YDERAD%NLWEMISS = 2
    ! ...and the longwave approximate update scheme uses a single
    ! broadband emissivity
    YDERAD%NLWOUT   = 1
    ! Create a spectral Planck look-up table, used by RADHEATN.  Note
    ! that this routine makes use of the length of its third argument.
    ! The wavelength bounds (metres) allow for the first emissivity to
    ! represent values outside the infrared atmospheric window, and the
    ! second emissivity to represent values within it.
    ! CALL YDERAD%YSPECTPLANCK%INIT(2, [ 8.0E-6_JPRB, 13.0E-6_JPRB ], &
    !      &  [ 1,2,1 ])
    CALL INIT(YDERAD%YSPECTPLANCK,2, [ 8.0E-6_JPRB, 13.0E-6_JPRB ], &
    &  [ 1,2,1 ])
    ! Populate the mapping between the 14 RRTM shortwave bands and the
    ! 6 albedo inputs.
    YDERAD%NSW = 6
    ZWAVBOUND(1:5) = [ 0.25e-6_jprb, 0.44e-6_jprb, 0.69e-6_jprb, &
         &             1.19e-6_jprb, 2.38e-6_jprb ]
    IBAND(1:6)  = [ 1,2,3,4,5,6 ]
    ! If NALBEDOSCHEME==2 then we are using the 6-component MODIS
    ! albedo climatology, and a weighted average is used to compute
    ! the albedos in each ecRad spectral band. If NALBEDOSCHEME==3
    ! then we use the diffuse part of the 4 components but still with
    ! a weighted average. Otherwise the older behaviour is followed:
    ! the nearest albedo interval to each band is selected, resulting
    ! in a discrete mapping that matches the one in YOESRTWN:NMPSRTM.
    ! Note that this tends to bias albedo high because there is a lot
    ! of energy around the interface between the UV-Vis and Near-IR
    ! channels, so this should be close to the 0.7 microns intended by
    ! the MODIS dataset, not shifted to the nearest RRTM band boundary
    ! at 0.625 microns.
    LL_DO_NEAREST_SW_ALBEDO = .FALSE.
    ! CALL RAD_CONFIG%DEFINE_SW_ALBEDO_INTERVALS(YDERAD%NSW, ZWAVBOUND, IBAND, &
    !      &  DO_NEAREST=LL_DO_NEAREST_SW_ALBEDO)
    CALL DEFINE_SW_ALBEDO_INTERVALS_CPU(RAD_CONFIG, YDERAD%NSW, ZWAVBOUND, IBAND, &
         &  DO_NEAREST=LL_DO_NEAREST_SW_ALBEDO)

    ! Likewise between the 16 RRTM longwave bands and the NLWEMISS
    ! emissivity inputs - these are defined in suecrad.F90.
    LL_DO_NEAREST_LW_EMISS = .TRUE.
    CALL RAD_CONFIG%DEFINE_LW_EMISS_INTERVALS_CPU(UBOUND(YSPECTPLANCK%INTERVAL_MAP,1), &
         &  YSPECTPLANCK%WAVLEN_BOUND, YSPECTPLANCK%INTERVAL_MAP, &
         &  DO_NEAREST=LL_DO_NEAREST_LW_EMISS)

    ! Do we scale the incoming solar radiation in each band?
    IF (YDERAD%NSOLARSPECTRUM == 1) THEN
      IF (RAD_CONFIG%N_BANDS_SW /= 14) THEN
        WRITE(NULERR,'(a)') '*** Error: Shortwave must have 14 bands to apply spectral scaling'
        CALL ABOR1('RADIATION_SETUP: Shortwave must have 14 bands to apply spectral scaling')
      ELSE
        RAD_CONFIG%USE_SPECTRAL_SOLAR_SCALING = .TRUE.
      ENDIF
    ENDIF

    ! *** IMPLEMENT SETTINGS ***

    ! For advanced configuration, the configuration data for the
    ! "radiation" project can specified directly in the namelist.
    ! However, the variable naming convention is not consistent with
    ! the rest of the IFS.  For basic configuration there are specific
    ! variables in the NAERAD namelist available in the YDERAD
    ! structure.
    CALL POSNAME(NULNAM, 'RADIATION', ISTAT)
    SELECT CASE (ISTAT)
      CASE(0)
        CALL RAD_CONFIG%READ_CPU(UNIT=NULNAM)
      CASE(1)
        WRITE(NULOUT,'(a)') 'Namelist RADIATION not found, using settings from NAERAD only'
      CASE DEFAULT
        CALL ABOR1('RADIATION_SETUP: error reading RADIATION section of namelist file')
    END SELECT
    IF (PRESENT(FILE_NAME)) THEN
      CALL RAD_CONFIG%READ_CPU(FILE_NAME=FILE_NAME)
    ENDIF

    ! Use configuration data to set-up radiation scheme, including
    ! reading scattering datafiles
    CALL SETUP_RADIATION_CPU(YDERDI,RAD_CONFIG)

    ! Print configuration
    IF (IVERBOSESETUP > 1) THEN
      WRITE(NULOUT,'(a)') 'Radiation scheme settings:'
      CALL RAD_CONFIG%PRINT_CPU(IVERBOSE=IVERBOSESETUP)
    ENDIF

    ! Get spectral weightings for UV and PAR
    CALL RAD_CONFIG%GET_SW_WEIGHTS_CPU(0.2E-6_JPRB, 0.4415E-6_JPRB,&
         &  PRADIATION%NWEIGHT_UV, PRADIATION%IBAND_UV, PRADIATION%WEIGHT_UV,&
         &  'ultraviolet')
    CALL RAD_CONFIG%GET_SW_WEIGHTS_CPU(0.4E-6_JPRB, 0.7E-6_JPRB,&
         &  PRADIATION%NWEIGHT_PAR, PRADIATION%IBAND_PAR, PRADIATION%WEIGHT_PAR,&
         &  'photosynthetically active radiation, PAR')

    IF (YDERAD%NAERMACC > 0) THEN
      ! With the MACC aerosol climatology we need to add in the
      ! background aerosol afterwards using the Tegen arrays.  In this
      ! case we first configure the background aerosol mass-extinction
      ! coefficient at 550 nm
      PRADIATION%TROP_BG_AER_MASS_EXT  = DRY_AEROSOL_MASS_EXTINCTION_CPU(RAD_CONFIG,&
           &                                   ITYPE_TROP_BG_AER, 550.0E-9_JPRB)
      PRADIATION%STRAT_BG_AER_MASS_EXT = DRY_AEROSOL_MASS_EXTINCTION_CPU(RAD_CONFIG,&
           &                                   ITYPE_STRAT_BG_AER, 550.0E-9_JPRB)
      WRITE(NULOUT,'(a,i0)') 'Tropospheric background uses aerosol type ',&
           &                 ITYPE_TROP_BG_AER
      WRITE(NULOUT,'(a,i0)') 'Stratospheric background uses aerosol type ',&
           &                 ITYPE_STRAT_BG_AER
    ELSE
      PRADIATION%TROP_BG_AER_MASS_EXT  = 0.0_JPRB
      PRADIATION%STRAT_BG_AER_MASS_EXT = 0.0_JPRB
      PRADIATION%TROP_BG_AER_MASS_EXT  = 0.0_JPRB
      PRADIATION%STRAT_BG_AER_MASS_EXT = 0.0_JPRB
    ENDIF

    IF(YDEAERATM%LAERRRTM) THEN
       CALL SETUP_MONO_AER_OPTICS_CPU(PRADIATION, YDCOMPO)
    ENDIF
      
    IF (IVERBOSESETUP > 1) THEN
      WRITE(NULOUT,'(a)') '-------------------------------------------------------------------------------'
    ENDIF

    END ASSOCIATE

    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_RADIATION_SCHEME_CPU',1,ZHOOK_HANDLE)

  END SUBROUTINE SETUP_RADIATION_SCHEME_CPU

  SUBROUTINE SETUP_MONO_AER_OPTICS_CPU(PRADIATION, YDCOMPO)

    !subroutine to pass the monochromatic aerosol optical properties 
    !to the variables needed by CAMS for aerosol diagnostics and 
    !data assimilation (sobstitutes the old SU_AEROP)

    ! Note that ALF_* are NOT in SI base units [m2 kg-1] as the NetCDF files
    ! are, but [m2 g-1], hence the 1.e-3 conversion factor.

    USE YOMHOOK
    USE RADIATION_AEROSOL_OPTICS_DATA
    USE YOMLUN
    USE YOMCOMPO

    TYPE(TRADIATION), INTENT(IN)  :: PRADIATION
    TYPE(TCOMPO),     INTENT(IN)  :: YDCOMPO

    !fills the old variables for monochromatic aerosol optics
    !using the new structure and values read in from the NetCDF file
    !defined above for the general aerosol optics
    INTEGER(KIND=JPIM) :: IRH
    INTEGER(KIND=JPIM) :: IOM,ISU

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_MONO_AER_OPTICS_CPU',0,ZHOOK_HANDLE)

    ASSOCIATE(RAD_CONFIG=>PRADIATION%RAD_CONFIG,&
              & LAERNITRATE=>YDCOMPO%LAERNITRATE, &
              & LAERSOA=>YDCOMPO%LAERSOA)
    ASSOCIATE(AO=>RAD_CONFIG%AEROSOL_OPTICS)

    WRITE(NULOUT,'(a)') 'Setting up monochromatic aerosol optics for diagnostic/DA'


!-- DUST
    ALF_DD(1,:) = AO%MASS_EXT_MONO_PHOBIC(:,10) * 1.e-3_JPRB
    ALF_DD(2,:) = AO%MASS_EXT_MONO_PHOBIC(:,11) * 1.e-3_JPRB
    ALF_DD(3,:) = AO%MASS_EXT_MONO_PHOBIC(:,12) * 1.e-3_JPRB

    OMG_DD(1,:) = AO%SSA_MONO_PHOBIC(:,10)
    OMG_DD(2,:) = AO%SSA_MONO_PHOBIC(:,11)
    OMG_DD(3,:) = AO%SSA_MONO_PHOBIC(:,12)

    ASY_DD(1,:) = AO%G_MONO_PHOBIC(:,10)
    ASY_DD(2,:) = AO%G_MONO_PHOBIC(:,11)
    ASY_DD(3,:) = AO%G_MONO_PHOBIC(:,12)

    RALI_DD(1,:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,10)
    RALI_DD(2,:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,11)
    RALI_DD(3,:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,12)

!-- BLACK CARBON
    ALF_BC(:) = AO%MASS_EXT_MONO_PHOBIC(:,15) * 1.e-3_JPRB
    OMG_BC(:) = AO%SSA_MONO_PHOBIC(:,15)
    ASY_BC(:) = AO%G_MONO_PHOBIC(:,15)
    RALI_BC(:) = AO%LIDAR_RATIO_MONO_PHOBIC(:,15)

    IF (LAERNITRATE) THEN
      ISU=7 ! SU does not include ammonium
    ELSE
      ISU=6 ! SU includes ammonium
    ENDIF

    IF (LAERSOA) THEN
      IOM=5 ! OM is primary only
    ELSE
      IOM=4 ! OM is primary and secondary combined
    ENDIF

    DO IRH=1,12
!-- ORGANIC MATTER
       ALF_OM(IRH,:) = AO%MASS_EXT_MONO_PHILIC(:,IRH,IOM) * 1.e-3_JPRB
       OMG_OM(IRH,:) = AO%SSA_MONO_PHILIC(:,IRH,IOM)
       ASY_OM(IRH,:) = AO%G_MONO_PHILIC(:,IRH,IOM)
       RALI_OM(IRH,:) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,IOM)
!-- SULFATES
       ALF_SU(IRH,:) = (AO%MASS_EXT_MONO_PHILIC(:,IRH,ISU)) * 1.e-3_JPRB
       OMG_SU(IRH,:) = AO%SSA_MONO_PHILIC(:,IRH,ISU)
       ASY_SU(IRH,:) = AO%G_MONO_PHILIC(:,IRH,ISU)
       RALI_SU(IRH,:) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,ISU)
!-- SEA SALT
       ALF_SS(IRH,:,1) = AO%MASS_EXT_MONO_PHILIC(:,IRH,1) * 1.e-3_JPRB
       ALF_SS(IRH,:,2) = AO%MASS_EXT_MONO_PHILIC(:,IRH,2) * 1.e-3_JPRB
       ALF_SS(IRH,:,3) = AO%MASS_EXT_MONO_PHILIC(:,IRH,3) * 1.e-3_JPRB

       OMG_SS(IRH,:,1) = AO%SSA_MONO_PHILIC(:,IRH,1)
       OMG_SS(IRH,:,2) = AO%SSA_MONO_PHILIC(:,IRH,2)
       OMG_SS(IRH,:,3) = AO%SSA_MONO_PHILIC(:,IRH,3)

       ASY_SS(IRH,:,1) = AO%G_MONO_PHILIC(:,IRH,1)
       ASY_SS(IRH,:,2) = AO%G_MONO_PHILIC(:,IRH,2)
       ASY_SS(IRH,:,3) = AO%G_MONO_PHILIC(:,IRH,3)

       RALI_SS(IRH,:,1) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,1)
       RALI_SS(IRH,:,2) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,2)
       RALI_SS(IRH,:,3) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,3)

!-- NITRATE
       ALF_NI(IRH,:,1) = AO%MASS_EXT_MONO_PHILIC(:,IRH,12) * 1.e-3_JPRB
       OMG_NI(IRH,:,1) = AO%SSA_MONO_PHILIC(:,IRH,12)
       ASY_NI(IRH,:,1) = AO%G_MONO_PHILIC(:,IRH,12)
       RALI_NI(IRH,:,1) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,12)

       ALF_NI(IRH,:,2) = AO%MASS_EXT_MONO_PHILIC(:,IRH,13) * 1.e-3_JPRB
       OMG_NI(IRH,:,2) = AO%SSA_MONO_PHILIC(:,IRH,13)
       ASY_NI(IRH,:,2) = AO%G_MONO_PHILIC(:,IRH,13)
       RALI_NI(IRH,:,2) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,13)

!-- AMMONIUM
       ALF_AM(IRH,:) = AO%MASS_EXT_MONO_PHILIC(:,IRH,11) * 1.e-3_JPRB
       OMG_AM(IRH,:) = AO%SSA_MONO_PHILIC(:,IRH,11)
       ASY_AM(IRH,:) = AO%G_MONO_PHILIC(:,IRH,11)
       RALI_AM(IRH,:) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,11)

!-- SOA
       ALF_SOA(IRH,:,1) = AO%MASS_EXT_MONO_PHILIC(:,IRH,9) * 1.e-3_JPRB
       OMG_SOA(IRH,:,1) = AO%SSA_MONO_PHILIC(:,IRH,9)
       ASY_SOA(IRH,:,1) = AO%G_MONO_PHILIC(:,IRH,9)
       RALI_SOA(IRH,:,1) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,9)

       ALF_SOA(IRH,:,2) = AO%MASS_EXT_MONO_PHILIC(:,IRH,10) * 1.e-3_JPRB
       OMG_SOA(IRH,:,2) = AO%SSA_MONO_PHILIC(:,IRH,10)
       ASY_SOA(IRH,:,2) = AO%G_MONO_PHILIC(:,IRH,10)
       RALI_SOA(IRH,:,2) = AO%LIDAR_RATIO_MONO_PHILIC(:,IRH,10)
    ENDDO

    END ASSOCIATE
    END ASSOCIATE

    IF (LHOOK) CALL DR_HOOK('RADIATION_SETUP:SETUP_MONO_AER_OPTICS_CPU',1,ZHOOK_HANDLE)
    
  END SUBROUTINE SETUP_MONO_AER_OPTICS_CPU

  SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
    IMPLICIT NONE
    CLASS(TRADIATION),  INTENT(IN) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: KDEPTH
    INTEGER(KIND=JPIM), INTENT(IN) :: KOUTNO
    INTEGER(KIND=JPIM) :: IDEPTHLOC

    IDEPTHLOC = KDEPTH+2
    
    WRITE(KOUTNO,*) REPEAT(' ',KDEPTH   ) // 'model%yrml_phy_rad%yradiation : '
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // '** we should print content of rad_config, not done yet**'
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'NWEIGHT_UV = ', SELF%NWEIGHT_UV
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'IBAND_UV SUM = ', SUM(SELF%IBAND_UV)
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'WEIGHT_UV SUM = ', SUM(SELF%WEIGHT_UV)
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'NWEIGHT_PAR = ', SELF%NWEIGHT_PAR
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'IBAND_PAR SUM = ', SUM(SELF%IBAND_PAR)
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'WEIGHT_PAR SUM = ', SUM(SELF%WEIGHT_PAR)
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'TROP_BG_AER_MASS_EXT = ', SELF%TROP_BG_AER_MASS_EXT
    WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'STRAT_BG_AER_MASS_EXT = ', SELF%STRAT_BG_AER_MASS_EXT
    
  END SUBROUTINE PRINT_CONFIGURATION

END MODULE RADIATION_SETUP


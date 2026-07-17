
MODULE SURFACE_VIEWS_DIAGNOSTIC_MODULE

!$ACDC methods --methods-list=host,wipe,copy --skip-components sfv --field-api --field-api-class sfv

  ! The SURFACE_VIEWS type contains namespaced access to groups of
  ! array pointers according to individual surface variable groups.
  !
  !
  ! Variable naming conventions
  ! ---------------------------
  ! The top-level type `SURFACE_VIEWS_TYPE` holds multiple group
  ! types, each prefixed with `GSP_` for prognostic and `GSD_` for
  ! diagnostic variable groups.
  !
  ! Each group type holds a list of array views (pointers to
  ! sub-sections of the gobal array), each prefixed with `P` to
  ! indicate a thread-local view pointer. The backend storage for each
  ! of these view pointers is provided by `FIELD_2D/FIELD_3D` objects,
  ! a reference to which is also stored on the group types and
  ! prefixed with `F_`.

USE PARKIND1, ONLY: JPIM, JPRB
USE FIELD_MODULE
USE SURFACE_VARIABLES_MOD

IMPLICIT NONE

TYPE SURFACE_VIEW_GROUP_VARSF
  REAL(KIND=JPRB), POINTER :: PZ0F(:)   ! gravity * surface roughness length
!>REAL(KIND=JPRB)          :: PZ0F (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALBF(:)   ! surface shortwave albedo
!>REAL(KIND=JPRB)          :: PALBF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PEMISF(:)   ! surface longwave emissivity
!>REAL(KIND=JPRB)          :: PEMISF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGETRL(:)   ! standard deviation of orography
!>REAL(KIND=JPRB)          :: PGETRL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSM(:)   ! land-sea mask
!>REAL(KIND=JPRB)          :: PLSM (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVEG(:)   ! vegetation cover
!>REAL(KIND=JPRB)          :: PVEG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVRLAN(:)   ! anisotropy of the sub-grid scale orography
!>REAL(KIND=JPRB)          :: PVRLAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVRLDI(:)   ! angle of the direction of orography with the x axis
!>REAL(KIND=JPRB)          :: PVRLDI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSIG(:)   ! characteristic orographic slope
!>REAL(KIND=JPRB)          :: PSIG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALBSF(:)   ! soil shortwave albedo
!>REAL(KIND=JPRB)          :: PALBSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAN(:)   ! fraction of land
!>REAL(KIND=JPRB)          :: PLAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSST(:)   ! (open) sea surface temperature
!>REAL(KIND=JPRB)          :: PSST (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSS(:)   ! sea surface salinity
!>REAL(KIND=JPRB)          :: PSSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLZ0H(:)   ! logarithm of roughness length for heat
!>REAL(KIND=JPRB)          :: PLZ0H (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCVL(:)   ! low vegetation cover
!>REAL(KIND=JPRB)          :: PCVL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCO2TYP(:)   ! CO2 photosynthesis type (c3/c4) for low vegetation cover
!>REAL(KIND=JPRB)          :: PCO2TYP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCVH(:)   ! high vegetation cover
!>REAL(KIND=JPRB)          :: PCVH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCUR(:)   ! urban cover
!>REAL(KIND=JPRB)          :: PCUR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTVL(:)   ! low vegetation type
!>REAL(KIND=JPRB)          :: PTVL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTVH(:)   ! high vegetation type
!>REAL(KIND=JPRB)          :: PTVH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAIL(:)   ! low vegetation LAI
!>REAL(KIND=JPRB)          :: PLAIL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAIH(:)   ! high vegetation LAI
!>REAL(KIND=JPRB)          :: PLAIH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFWET(:)   ! wetland fraction
!>REAL(KIND=JPRB)          :: PFWET (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSOTY(:)   ! soil type
!>REAL(KIND=JPRB)          :: PSOTY (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCLK(:)   ! lake cover
!>REAL(KIND=JPRB)          :: PCLK (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDL(:)   ! lake depth
!>REAL(KIND=JPRB)          :: PDL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCI(:)   ! sea ice fraction
!>REAL(KIND=JPRB)          :: PCI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUCUR(:)   ! U-component of the ocean current
!>REAL(KIND=JPRB)          :: PUCUR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVCUR(:)   ! V-component of the ocean current
!>REAL(KIND=JPRB)          :: PVCUR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZ0RLF(:)   ! gravity * vegetation roughness length
!>REAL(KIND=JPRB)          :: PZ0RLF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCGPP(:)   ! GPP bias correction factor
!>REAL(KIND=JPRB)          :: PCGPP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCREC(:)   ! REC bias correction factor
!>REAL(KIND=JPRB)          :: PCREC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSDFOR(:)   ! SD filtered orography
!>REAL(KIND=JPRB)          :: PSDFOR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALUVP(:)   ! MODIS-derived parallel albedo for shortwave radiation
!>REAL(KIND=JPRB)          :: PALUVP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALUVD(:)   ! MODIS-derived diffuse albedo for shortwave radiation
!>REAL(KIND=JPRB)          :: PALUVD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALNIP(:)   ! MODIS-derived parallel albedo for longwave radiation
!>REAL(KIND=JPRB)          :: PALNIP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALNID(:)   ! MODIS-derived diffuse albedo for longwave radiation
!>REAL(KIND=JPRB)          :: PALNID (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFP1(:)   ! surface orography in the 2nd part of FULLPOS-927
!>REAL(KIND=JPRB)          :: PFP1 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSO2DD(:)   ! sulphate dry dep velocity
!>REAL(KIND=JPRB)          :: PSO2DD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDMSO(:)   ! oceanic DMS
!>REAL(KIND=JPRB)          :: PDMSO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PURBF(:)   ! Urban fraction
!>REAL(KIND=JPRB)          :: PURBF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFCA1(:)   ! Fraction of calcite over dust 1st bin
!>REAL(KIND=JPRB)          :: PFCA1 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFCA2(:)   ! Fraction of calcite over dust 2nd bin
!>REAL(KIND=JPRB)          :: PFCA2 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAERDEP(:)   ! dust emission potential
!>REAL(KIND=JPRB)          :: PAERDEP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAERLTS(:)   ! dust lifting threshold speed
!>REAL(KIND=JPRB)          :: PAERLTS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAERSCC(:)   ! dust soil clay content
!>REAL(KIND=JPRB)          :: PAERSCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDSF(:)   ! dust source function
!>REAL(KIND=JPRB)          :: PDSF (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMFLXO   ! total chemistry flux (emissions + deposition)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMWDFLX   ! wet deposition chemistry flux
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMDDFLX   ! dry deposition chemistry flux
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PCHEMDV   ! chemistry deposition velocity
  REAL(KIND=JPRB), POINTER :: PNUDM(:)   ! nudging mask
!>REAL(KIND=JPRB)          :: PNUDM (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PEMIS2D   ! 2D emission fields for composition
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PEMIS2DAUX   ! 2D emission auxiliary fields for composition
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VARSF), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_Z0F=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALBF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_EMISF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_GETRL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSM=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VEG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VRLAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VRLDI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SIG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALBSF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SST=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LZ0H=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CVL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CO2TYP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CVH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CUR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TVL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TVH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAIL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAIH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FWET=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SOTY=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CLK=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UCUR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VCUR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_Z0RLF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CGPP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CREC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SDFOR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALUVP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALUVD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALNIP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALNID=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FP1=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SO2DD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DMSO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_URBF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FCA1=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FCA2=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AERDEP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AERLTS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AERSCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DSF=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMFLXO
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMWDFLX
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMDDFLX
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_CHEMDV
  CLASS (FIELD_2RB), POINTER :: F_NUDM=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_EMIS2D
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_EMIS2DAUX
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VARSF_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VARSF_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VARSF_FINAL
END TYPE SURFACE_VIEW_GROUP_VARSF

TYPE SURFACE_VIEW_GROUP_VCLIH
  REAL(KIND=JPRB), POINTER :: PTCCH(:)   ! total convective cloudiness
!>REAL(KIND=JPRB)          :: PTCCH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSCCH(:)   ! convective cloud summit
!>REAL(KIND=JPRB)          :: PSCCH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBCCH(:)   ! convective cloud base
!>REAL(KIND=JPRB)          :: PBCCH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPBLH(:)   ! PBL height
!>REAL(KIND=JPRB)          :: PPBLH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSPSH(:)   ! variable for prognostic convection scheme (ALARO)
!>REAL(KIND=JPRB)          :: PSPSH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PQSH(:)   ! surface moisture historic variable (used by TOUCANS)
!>REAL(KIND=JPRB)          :: PQSH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPCL(:)   ! 
!>REAL(KIND=JPRB)          :: PPCL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPSL(:)   ! 
!>REAL(KIND=JPRB)          :: PPSL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPCN(:)   ! 
!>REAL(KIND=JPRB)          :: PPCN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPSN(:)   ! 
!>REAL(KIND=JPRB)          :: PPSN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PEVA(:)   ! 
!>REAL(KIND=JPRB)          :: PEVA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIH), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_TCCH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SCCH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BCCH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PBLH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SPSH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_QSH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PCL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PSL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PCN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PSN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_EVA=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIH_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIH_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIH_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIH

TYPE SURFACE_VIEW_GROUP_VCLIK
  REAL(KIND=JPRB), POINTER :: PUDGRO(:)   ! ud top position (accsu)
!>REAL(KIND=JPRB)          :: PUDGRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIK), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_UDGRO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIK_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIK_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIK_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIK

TYPE SURFACE_VIEW_GROUP_VCLIP
  REAL(KIND=JPRB), POINTER :: PTPC(:)   ! climatological deep layer temperature
!>REAL(KIND=JPRB)          :: PTPC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PWPC(:)   ! climatological deep layer moisture
!>REAL(KIND=JPRB)          :: PWPC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIP), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_TPC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_WPC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIP_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIP_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIP_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIP

TYPE SURFACE_VIEW_GROUP_VCLIV
  REAL(KIND=JPRB), POINTER :: PARG(:)   ! silt percentage within soil
!>REAL(KIND=JPRB)          :: PARG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSAB(:)   ! percentage of sand within the soil
!>REAL(KIND=JPRB)          :: PSAB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PD2(:)   ! soil depth
!>REAL(KIND=JPRB)          :: PD2 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIVEG(:)   ! type of vegetation
!>REAL(KIND=JPRB)          :: PIVEG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PRSMIN(:)   ! stomatal minimum resistance
!>REAL(KIND=JPRB)          :: PRSMIN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAI(:)   ! leaf area index
!>REAL(KIND=JPRB)          :: PLAI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PHV(:)   ! resistance to evapotranspiration
!>REAL(KIND=JPRB)          :: PHV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZ0H(:)   ! gravity * roughness length for heat
!>REAL(KIND=JPRB)          :: PZ0H (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALS(:)   ! albedo of bare ground
!>REAL(KIND=JPRB)          :: PALS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALV(:)   ! albedo of vegetation
!>REAL(KIND=JPRB)          :: PALV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIV), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_ARG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SAB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_D2=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IVEG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_RSMIN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_HV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_Z0H=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALV=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIV_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIV_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIV_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIV

TYPE SURFACE_VIEW_GROUP_VCLIA
  REAL(KIND=JPRB), POINTER :: PSEA(:)   ! aerosol sea
!>REAL(KIND=JPRB)          :: PSEA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLAN(:)   ! aerosol land
!>REAL(KIND=JPRB)          :: PLAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSOO(:)   ! aerosol soot
!>REAL(KIND=JPRB)          :: PSOO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDES(:)   ! aerosol desert
!>REAL(KIND=JPRB)          :: PDES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSUL(:)   ! aerosol sulfate
!>REAL(KIND=JPRB)          :: PSUL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVOL(:)   ! aerosol volcano
!>REAL(KIND=JPRB)          :: PVOL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIA), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_SEA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SOO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SUL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VOL=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIA_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIA_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIA_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIA

TYPE SURFACE_VIEW_GROUP_VCLIN
  REAL(KIND=JPRB), POINTER :: PTOP(:)   ! index of convective cloud top
!>REAL(KIND=JPRB)          :: PTOP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBAS(:)   ! index of convective cloud base
!>REAL(KIND=JPRB)          :: PBAS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PACPR(:)   ! averaged convective precipitaion rate
!>REAL(KIND=JPRB)          :: PACPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PACCPR(:)   ! accumulated total precipitaion for assimilation
!>REAL(KIND=JPRB)          :: PACCPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PACCPR5(:)   ! accumulated total precipitaion for assimilation (trajectory)
!>REAL(KIND=JPRB)          :: PACCPR5 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VCLIN), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_TOP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BAS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ACPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ACCPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ACCPR5=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VCLIN_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VCLIN_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VCLIN_FINAL
END TYPE SURFACE_VIEW_GROUP_VCLIN

TYPE SURFACE_VIEW_GROUP_VDIAGO2
  REAL(KIND=JPRB), POINTER :: POCDEP(:)   ! bottom layer depth
!>REAL(KIND=JPRB)          :: POCDEP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUSTRC(:)   ! taux clim.
!>REAL(KIND=JPRB)          :: PUSTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVSTRC(:)   ! tauy clim.
!>REAL(KIND=JPRB)          :: PVSTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO2), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_OCDEP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_USTRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VSTRC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VDIAGO2_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VDIAGO2_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VDIAGO2_FINAL
END TYPE SURFACE_VIEW_GROUP_VDIAGO2

TYPE SURFACE_VIEW_GROUP_VDIAGO3
  REAL(KIND=JPRB), POINTER :: PDIFM(:,:)   ! viscosity
!>REAL(KIND=JPRB)          :: PDIFM (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PDIFT(:,:)   ! diff. coef. of temp
!>REAL(KIND=JPRB)          :: PDIFT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PDIFS(:,:)   ! diff. coef. of salinity
!>REAL(KIND=JPRB)          :: PDIFS (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PADVT(:,:)   ! correction term for temp.
!>REAL(KIND=JPRB)          :: PADVT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PADVS(:,:)   ! correction term for sal.
!>REAL(KIND=JPRB)          :: PADVS (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PTRI0(:,:)   ! coef. for solving matrix.
!>REAL(KIND=JPRB)          :: PTRI0 (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PTRI1(:,:)   ! coef. for solving matrix.
!>REAL(KIND=JPRB)          :: PTRI1 (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PSWDK(:,:)   ! radiation term
!>REAL(KIND=JPRB)          :: PSWDK (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PZO(:,:)   ! depth of layer
!>REAL(KIND=JPRB)          :: PZO (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PHO(:,:)   ! depth of interface layer
!>REAL(KIND=JPRB)          :: PHO (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PDO(:,:)   ! layer thickness
!>REAL(KIND=JPRB)          :: PDO (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PHO_INV(:,:)   ! 1 / YHO
!>REAL(KIND=JPRB)          :: PHO_INV (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PUOC(:,:)   ! U velocity clim.
!>REAL(KIND=JPRB)          :: PUOC (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PVOC(:,:)   ! V velocity clim.
!>REAL(KIND=JPRB)          :: PVOC (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: POTKE(:,:)   ! ocean turb. kin. energy
!>REAL(KIND=JPRB)          :: POTKE (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_V3D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO3), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_DIFM=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_DIFT=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_DIFS=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_ADVT=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_ADVS=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_TRI0=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_TRI1=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_SWDK=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_ZO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_HO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_DO=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_HO_INV=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_UOC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_VOC=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_OTKE=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VDIAGO3_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VDIAGO3_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VDIAGO3_FINAL
END TYPE SURFACE_VIEW_GROUP_VDIAGO3

TYPE SURFACE_VIEW_GROUP_VDIAG
  REAL(KIND=JPRB), POINTER :: PLSP(:)   ! Large scale precipitation
!>REAL(KIND=JPRB)          :: PLSP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCP(:)   ! Convective precipitation
!>REAL(KIND=JPRB)          :: PCP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSF(:)   ! Snowfall
!>REAL(KIND=JPRB)          :: PSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PFZRA(:)   ! Freezing rain
!>REAL(KIND=JPRB)          :: PFZRA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBLD(:)   ! Boundary layer dissipation
!>REAL(KIND=JPRB)          :: PBLD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSHF(:)   ! Surface sensible heat flux
!>REAL(KIND=JPRB)          :: PSSHF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSLHF(:)   ! Surface latent heat flux
!>REAL(KIND=JPRB)          :: PSLHF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PNEE(:)   ! Surface net ecosystem exchange of CO2
!>REAL(KIND=JPRB)          :: PNEE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGPP(:)   ! Surface gross primary production of CO2
!>REAL(KIND=JPRB)          :: PGPP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PREC(:)   ! Surface ecosystem respiration of CO2
!>REAL(KIND=JPRB)          :: PREC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMSL(:)   ! Mean sea level pressure
!>REAL(KIND=JPRB)          :: PMSL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSP(:)   ! Surface pressure
!>REAL(KIND=JPRB)          :: PSP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCC(:)   ! Total cloud cover
!>REAL(KIND=JPRB)          :: PTCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10U(:)   ! U-wind at 10 m
!>REAL(KIND=JPRB)          :: P10U (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10V(:)   ! V-wind at 10 m
!>REAL(KIND=JPRB)          :: P10V (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P2T(:)   ! Temperature at 2 m
!>REAL(KIND=JPRB)          :: P2T (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P2D(:)   ! Dewpoint temperature at 2 m
!>REAL(KIND=JPRB)          :: P2D (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P2SH(:)   ! Specific humidity at 2 m
!>REAL(KIND=JPRB)          :: P2SH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSR(:)   ! Surface solar radiation
!>REAL(KIND=JPRB)          :: PSSR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTR(:)   ! Surface thermal radiation
!>REAL(KIND=JPRB)          :: PSTR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTSR(:)   ! Top solar radiation
!>REAL(KIND=JPRB)          :: PTSR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTTR(:)   ! Top thermal radiation
!>REAL(KIND=JPRB)          :: PTTR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PEWSS(:)   ! Instantaneous surface U-wind stress
!>REAL(KIND=JPRB)          :: PEWSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PNSSS(:)   ! Instantaneous surface V-wind stress
!>REAL(KIND=JPRB)          :: PNSSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PE(:)   ! Water evaporation
!>REAL(KIND=JPRB)          :: PE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPEV(:)   ! Potential evaporation
!>REAL(KIND=JPRB)          :: PPEV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCCC(:)   ! Convective cloud cover
!>REAL(KIND=JPRB)          :: PCCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLCC(:)   ! Low cloud cover
!>REAL(KIND=JPRB)          :: PLCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMCC(:)   ! Medium cloud cover
!>REAL(KIND=JPRB)          :: PMCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PHCC(:)   ! High cloud cover
!>REAL(KIND=JPRB)          :: PHCC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLGWS(:)   ! Zonal gravity wave stress
!>REAL(KIND=JPRB)          :: PLGWS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMGWS(:)   ! Meridian gravity wave stress
!>REAL(KIND=JPRB)          :: PMGWS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGWD(:)   ! Gravity wave dissipation
!>REAL(KIND=JPRB)          :: PGWD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMX2T(:)   ! Maximum temperature at 2 m
!>REAL(KIND=JPRB)          :: PMX2T (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMN2T(:)   ! Minimum temperature at 2 m
!>REAL(KIND=JPRB)          :: PMN2T (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMX2T6   ! Bins for maximum temperature at 2 m since last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMN2T6   ! Bins for minimum temperature at 2 m since last 6 hours
  REAL(KIND=JPRB), POINTER :: PRO(:)   ! Runoff (total)
!>REAL(KIND=JPRB)          :: PRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSRO(:)   ! Runoff surface
!>REAL(KIND=JPRB)          :: PSRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRO(:)   ! Runoff sub-surface
!>REAL(KIND=JPRB)          :: PSSRO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PALB(:)   ! (surface shortwave) albedo
!>REAL(KIND=JPRB)          :: PALB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIEWSS(:)   ! Instantaneous surface zonal component of stress
!>REAL(KIND=JPRB)          :: PIEWSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PINSSS(:)   ! Instantaneous surface meridian component of stress
!>REAL(KIND=JPRB)          :: PINSSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PISSHF(:)   ! Instantaneous surface heat flux
!>REAL(KIND=JPRB)          :: PISSHF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIE(:)   ! Instantaneous surface moisture flux
!>REAL(KIND=JPRB)          :: PIE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PINEE(:)   ! Instantaneous net ecosystem exchange of CO2
!>REAL(KIND=JPRB)          :: PINEE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIGPP(:)   ! Instantaneous gross primary production of CO2
!>REAL(KIND=JPRB)          :: PIGPP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PIREC(:)   ! Instantaneous ecosystem respiration of CO2
!>REAL(KIND=JPRB)          :: PIREC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PICH4(:)   ! Instantaneous wetland CH4 flux
!>REAL(KIND=JPRB)          :: PICH4 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCSF(:)   ! Convective snow fall
!>REAL(KIND=JPRB)          :: PCSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSSF(:)   ! Large scale snowfall
!>REAL(KIND=JPRB)          :: PLSSF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMXTPR(:)   ! Max precip rate since last post-processing
!>REAL(KIND=JPRB)          :: PMXTPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMNTPR(:)   ! Min precip rate since last post-processing
!>REAL(KIND=JPRB)          :: PMNTPR (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMXTPR6   ! Max precip rate in last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMNTPR6   ! Min precip rate in last 6 hours
  REAL(KIND=JPRB), POINTER :: PTPR(:)   ! Total precipitation rate
!>REAL(KIND=JPRB)          :: PTPR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSRR(:)   ! Large scale rain rate
!>REAL(KIND=JPRB)          :: PLSRR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCRR(:)   ! Convective rain rate
!>REAL(KIND=JPRB)          :: PCRR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSSFR(:)   ! Large scale snowfall rate
!>REAL(KIND=JPRB)          :: PLSSFR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCSFR(:)   ! Convective snowfall rate
!>REAL(KIND=JPRB)          :: PCSFR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPTYPE(:)   ! Precipitation type
!>REAL(KIND=JPRB)          :: PPTYPE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PILSPF(:)   ! Large-scale precipitation fraction (inst.)
!>REAL(KIND=JPRB)          :: PILSPF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZ0F(:)   ! Gravity * surface roughness length
!>REAL(KIND=JPRB)          :: PZ0F (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLZ0H(:)   ! Logarithm of z0 times heat flux
!>REAL(KIND=JPRB)          :: PLZ0H (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVIWVE(:)   ! Vertical integral of eastward water vapour flux
!>REAL(KIND=JPRB)          :: PVIWVE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVIWVN(:)   ! Vertical integral of northward water vapour flux
!>REAL(KIND=JPRB)          :: PVIWVN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCW(:)   ! Total water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCWV(:)   ! Total water vapor content in a vertical column
!>REAL(KIND=JPRB)          :: PTCWV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCLW(:)   ! Total liquid water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCLW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCIW(:)   ! Total ice water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCIW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCRW(:)   ! Total rain water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCRW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCSW(:)   ! Total snow water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCSW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCSLW(:)   ! Total supercooled liquid water content in a vertical column
!>REAL(KIND=JPRB)          :: PTCSLW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRD(:)   ! Downward surface solar radiation
!>REAL(KIND=JPRB)          :: PSSRD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTRD(:)   ! Downward surface thermic radiation
!>REAL(KIND=JPRB)          :: PSTRD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRDC(:)   ! Clear-sky downward surface solar radiation
!>REAL(KIND=JPRB)          :: PSSRDC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTRDC(:)   ! Claer-sky downward surface thermal radiation
!>REAL(KIND=JPRB)          :: PSTRDC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PBLH(:)   ! Height of boundary layer
!>REAL(KIND=JPRB)          :: PBLH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSUND(:)   ! Sunshine duration
!>REAL(KIND=JPRB)          :: PSUND (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSPAR(:)   ! Surface downward PARadiation
!>REAL(KIND=JPRB)          :: PSPAR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSUVB(:)   ! Surface downward UV-B radiation
!>REAL(KIND=JPRB)          :: PSUVB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSFDIR(:)   ! Surface total sky direct downward SW radiation
!>REAL(KIND=JPRB)          :: PSFDIR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSCDIR(:)   ! Surface clear-sky direct downward SW radiation
!>REAL(KIND=JPRB)          :: PSCDIR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSDSRP(:)   ! Surface total-sky direct beam downward SW radiation
!>REAL(KIND=JPRB)          :: PSDSRP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCAPE(:)   ! Conv.avail.potential energy (CAPE)
!>REAL(KIND=JPRB)          :: PCAPE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMUCAPE(:)   ! Maximum unstable CAPE
!>REAL(KIND=JPRB)          :: PMUCAPE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCAPE50(:)   ! CAPE from 50 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCAPE50 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCAPE100(:)   ! CAPE from 100 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCAPE100 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPDEPL(:)   ! Parcel departure level-pressure for MUCAPE
!>REAL(KIND=JPRB)          :: PPDEPL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCAPES(:)   ! CAPE-Shear
!>REAL(KIND=JPRB)          :: PCAPES (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMXCAP6   ! Bins for maximum CAPE in last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PMXCAPS6   ! Bins for maximum CAPE-Shear in last 6 hours
  REAL(KIND=JPRB), POINTER :: PTROPOTP(:)   ! Pressure of thermal Tropopause
!>REAL(KIND=JPRB)          :: PTROPOTP (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTSRC(:)   ! Top solar radiation clear sky
!>REAL(KIND=JPRB)          :: PTSRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTTRC(:)   ! Top thermal radiation clear sky
!>REAL(KIND=JPRB)          :: PTTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSSRC(:)   ! Surface solar radiation clear sky
!>REAL(KIND=JPRB)          :: PSSRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTRC(:)   ! Surface thermal radiation clear sky
!>REAL(KIND=JPRB)          :: PSTRC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PES(:)   ! Evaporation of snow
!>REAL(KIND=JPRB)          :: PES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSMLT(:)   ! Snow melt
!>REAL(KIND=JPRB)          :: PSMLT (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10FG(:)   ! Wind gust at 10 m (max since previous pp)
!>REAL(KIND=JPRB)          :: P10FG (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: P10FG6   ! Bins for wind gust at 10 m (max since last 6 hours)
  REAL(KIND=JPRB), POINTER :: P10FGCV(:)   ! convective wind gust at 10m for current time level (m/s)
!>REAL(KIND=JPRB)          :: P10FGCV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PI10FG(:)   ! Wind gust at 10 m ("instantaneous")
!>REAL(KIND=JPRB)          :: PI10FG (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLSPF(:)   ! Large scale precipitation fraction
!>REAL(KIND=JPRB)          :: PLSPF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTCO3(:)   ! Total ozone content in a vertical column
!>REAL(KIND=JPRB)          :: PTCO3 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVIMD(:)   ! Vertically integrated mass divergence
!>REAL(KIND=JPRB)          :: PVIMD (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSPARC(:)   ! Surface clear-sky parallel radiation
!>REAL(KIND=JPRB)          :: PSPARC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PSTINC(:)   ! Top of atmosphere incident solar radiation
!>REAL(KIND=JPRB)          :: PSTINC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCBASE(:)   ! Cloud base level
!>REAL(KIND=JPRB)          :: PCBASE (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P0DEGL(:)   ! Zero deg. level
!>REAL(KIND=JPRB)          :: P0DEGL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PM10DEGL(:)   ! -10 deg. level
!>REAL(KIND=JPRB)          :: PM10DEGL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVISIH(:)   ! Horizontal visibility
!>REAL(KIND=JPRB)          :: PVISIH (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCIN(:)   ! CIN
!>REAL(KIND=JPRB)          :: PCIN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCIN50(:)   ! CIN from 50 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCIN50 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PMLCIN100(:)   ! CIN from 100 hPa mixed-layer
!>REAL(KIND=JPRB)          :: PMLCIN100 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PKINDEX(:)   ! Convective K-Index
!>REAL(KIND=JPRB)          :: PKINDEX (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTTINDEX(:)   ! Convective TT-Index
!>REAL(KIND=JPRB)          :: PTTINDEX (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCBASEA(:)   ! Cloud base aviation
!>REAL(KIND=JPRB)          :: PCBASEA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCTOPC(:)   ! Cloud top convective
!>REAL(KIND=JPRB)          :: PCTOPC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZTWETB0(:)   ! Height of 0 deg wet bulb temperature
!>REAL(KIND=JPRB)          :: PZTWETB0 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZTWETB1(:)   ! Height of 1 deg wet bulb temperature
!>REAL(KIND=JPRB)          :: PZTWETB1 (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PTCGHG   ! Total column greenhouse gases
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PTCCHEM   ! Total column chemistry
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:,:) :: PAERODIAG   ! Per-aerosol-type diagnostics
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:,:) :: PAERO_WVL_DIAG   ! Per-wavelength aerosol optical diagnostics
  REAL(KIND=JPRB), POINTER :: P100U(:)   ! 100m zonal wind
!>REAL(KIND=JPRB)          :: P100U (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P100V(:)   ! 100m meridional wind
!>REAL(KIND=JPRB)          :: P100V (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P200U(:)   ! 200m zonal wind
!>REAL(KIND=JPRB)          :: P200U (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P200V(:)   ! 200m meridional wind
!>REAL(KIND=JPRB)          :: P200V (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZUST(:)   ! Friction velocity
!>REAL(KIND=JPRB)          :: PZUST (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10NU(:)   ! 10m zonal neutral wind
!>REAL(KIND=JPRB)          :: P10NU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: P10NV(:)   ! 10m meridional neutral wind
!>REAL(KIND=JPRB)          :: P10NV (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDNDZN(:)   ! Minimum vertical refractivity gradient
!>REAL(KIND=JPRB)          :: PDNDZN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDNDZA(:)   ! Mean vertical refractivity gradient
!>REAL(KIND=JPRB)          :: PDNDZA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PDCTB(:)   ! Duct base height
!>REAL(KIND=JPRB)          :: PDCTB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTPLB(:)   ! Trapping layer base height
!>REAL(KIND=JPRB)          :: PTPLB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTPLT(:)   ! Trapping layer top height
!>REAL(KIND=JPRB)          :: PTPLT (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODSS(:)   ! optical depth sea salt aerosols
!>REAL(KIND=JPRB)          :: PODSS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODDU(:)   ! optical depth dust aerosols
!>REAL(KIND=JPRB)          :: PODDU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODOM(:)   ! optical depth organic m. aerosols
!>REAL(KIND=JPRB)          :: PODOM (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODBC(:)   ! optical depth black C aerosols
!>REAL(KIND=JPRB)          :: PODBC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODSU(:)   ! optical depth sulphate aerosols
!>REAL(KIND=JPRB)          :: PODSU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODNI(:)   ! optical depth nitrate aerosols
!>REAL(KIND=JPRB)          :: PODNI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODAM(:)   ! optical depth ammonium aerosols
!>REAL(KIND=JPRB)          :: PODAM (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODSOA(:)   ! optical depth secondary organic aerosols
!>REAL(KIND=JPRB)          :: PODSOA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODVFA(:)   ! optical depth volcanic flying ash
!>REAL(KIND=JPRB)          :: PODVFA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODVSU(:)   ! optical depth volcanic sulphate aerosols
!>REAL(KIND=JPRB)          :: PODVSU (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PODTOACC(:)   ! optical depth total aerosol accumulated
!>REAL(KIND=JPRB)          :: PODTOACC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAEPM1(:)   ! particulate matter le 1 um
!>REAL(KIND=JPRB)          :: PAEPM1 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAEPM25(:)   ! particulate matter le 2.5um
!>REAL(KIND=JPRB)          :: PAEPM25 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PAEPM10(:)   ! particulate matter le 10 um
!>REAL(KIND=JPRB)          :: PAEPM10 (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUVBED(:)   ! UV biologically effective dose
!>REAL(KIND=JPRB)          :: PUVBED (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUVBEDCS(:)   ! UV biologically effective dose clear sky
!>REAL(KIND=JPRB)          :: PUVBEDCS (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLITOTI(:)   ! instantaneous total lightning flash density
!>REAL(KIND=JPRB)          :: PLITOTI (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PLICGI(:)   ! instantaneous cloud-to-ground lightning flash density
!>REAL(KIND=JPRB)          :: PLICGI (YDCPG_OPTS%KLON)
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PLITOTA6   ! Bins for averaged total lightning over last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:) :: PLICGA6   ! Bins for averaged cloud-to-ground lightning over last 6 hours
  TYPE(FIELD_2RB_VIEW), ALLOCATABLE, DIMENSION(:,:) :: PPTYPEOCC6   ! Bins for accum freq of each precip type over last 6 hours
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VDIAG), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_LSP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_FZRA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BLD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSHF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SLHF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_NEE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_GPP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_REC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MSL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10U=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10V=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_2T=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_2D=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_2SH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TSR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TTR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_EWSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_NSSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_E=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PEV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_HCC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LGWS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MGWS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_GWD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MX2T=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MN2T=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MX2T6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MN2T6
  CLASS (FIELD_2RB), POINTER :: F_RO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SRO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ALB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IEWSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_INSSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ISSHF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_INEE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IGPP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_IREC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ICH4=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CSF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSSF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MXTPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MNTPR=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MXTPR6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MNTPR6
  CLASS (FIELD_2RB), POINTER :: F_TPR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSRR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CRR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSSFR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CSFR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PTYPE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ILSPF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_Z0F=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LZ0H=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VIWVE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VIWVN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCWV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCLW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCIW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCRW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCSW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCSLW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STRD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRDC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STRDC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_BLH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SUND=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SPAR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SUVB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SFDIR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SCDIR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SDSRP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CAPE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MUCAPE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCAPE50=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCAPE100=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PDEPL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CAPES=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MXCAP6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_MXCAPS6
  CLASS (FIELD_2RB), POINTER :: F_TROPOTP=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TSRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TTRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SSRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STRC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SMLT=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10FG=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_10FG6
  CLASS (FIELD_2RB), POINTER :: F_10FGCV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_I10FG=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LSPF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TCO3=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VIMD=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_SPARC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_STINC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CBASE=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_0DEGL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_M10DEGL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VISIH=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CIN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCIN50=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_MLCIN100=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_KINDEX=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TTINDEX=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CBASEA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CTOPC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZTWETB0=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZTWETB1=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_TCGHG
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_TCCHEM
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:,:) :: F_AERODIAG
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:,:) :: F_AERO_WVL_DIAG
  CLASS (FIELD_2RB), POINTER :: F_100U=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_100V=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_200U=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_200V=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZUST=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10NU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_10NV=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DNDZN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DNDZA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_DCTB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TPLB=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TPLT=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODSS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODDU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODOM=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODBC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODSU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODNI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODAM=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODSOA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODVFA=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODVSU=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ODTOACC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AEPM1=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AEPM25=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_AEPM10=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UVBED=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UVBEDCS=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LITOTI=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_LICGI=>NULL()
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_LITOTA6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:) :: F_LICGA6
  TYPE (FIELD_2RB_PTR), ALLOCATABLE, DIMENSION(:,:) :: F_PTYPEOCC6
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VDIAG_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VDIAG_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VDIAG_FINAL
END TYPE SURFACE_VIEW_GROUP_VDIAG

TYPE SURFACE_VIEW_GROUP_SATSIM
  REAL(KIND=JPRB), POINTER :: PCLBT(:,:)   ! Cloudy brightness temperature
!>REAL(KIND=JPRB)          :: PCLBT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_SMD%NLEVS)
  REAL(KIND=JPRB), POINTER :: PCSBT(:,:)   ! Clear-sky brightness temperature
!>REAL(KIND=JPRB)          :: PCSBT (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_SMD%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_SATSIM), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_CLBT=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_CSBT=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_SATSIM_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_SATSIM_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_SATSIM_FINAL
END TYPE SURFACE_VIEW_GROUP_SATSIM

TYPE SURFACE_VIEW_GROUP_WAVES
  REAL(KIND=JPRB), POINTER :: PCHAR(:)   ! Charnock parameter as modified by the wave model.
!>REAL(KIND=JPRB)          :: PCHAR (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCHARHQ(:)   ! Charnock for heat and moisture from the wave model.
!>REAL(KIND=JPRB)          :: PCHARHQ (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUSTOKES(:)   ! U-component of the surface Stokes drift.
!>REAL(KIND=JPRB)          :: PUSTOKES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVSTOKES(:)   ! V-component of the surface Stokes drift.
!>REAL(KIND=JPRB)          :: PVSTOKES (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTAUOCX(:)   ! U-component of the Momentum flux to ocean.
!>REAL(KIND=JPRB)          :: PTAUOCX (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PTAUOCY(:)   ! V-component of the Momentum flux to ocean.
!>REAL(KIND=JPRB)          :: PTAUOCY (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PPHIOC(:)   ! Energy flux to ocean.
!>REAL(KIND=JPRB)          :: PPHIOC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PWSEMEAN(:)   ! Windsea variance.
!>REAL(KIND=JPRB)          :: PWSEMEAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PWSFMEAN(:)   ! Windsea mean frequency.
!>REAL(KIND=JPRB)          :: PWSFMEAN (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_WAVES), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_CHAR=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CHARHQ=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_USTOKES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VSTOKES=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TAUOCX=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_TAUOCY=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_PHIOC=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_WSEMEAN=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_WSFMEAN=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_WAVES_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_WAVES_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_WAVES_FINAL
END TYPE SURFACE_VIEW_GROUP_WAVES

TYPE SURFACE_VIEW_GROUP_WAM
  REAL(KIND=JPRB), POINTER :: PU10N(:)   ! 10m neutral wind U-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PU10N (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PV10N(:)   ! 10m neutral wind V-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PV10N (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PRHO(:)   ! surface density passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PRHO (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PZIL(:)   ! ZI/L passed to the wave model (used for gustiness in WAM).
!>REAL(KIND=JPRB)          :: PZIL (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCIF(:)   ! Sea ice fraction passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PCIF (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PCLK(:)   ! Lake cover passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PCLK (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PUCURW(:)   ! Ocean current    U-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PUCURW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PVCURW(:)   ! Ocean current    V-component passed to the wave model (WAM).
!>REAL(KIND=JPRB)          :: PVCURW (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_WAM), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_U10N=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_V10N=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_RHO=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_ZIL=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CIF=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_CLK=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_UCURW=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_VCURW=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_WAM_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_WAM_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_WAM_FINAL
END TYPE SURFACE_VIEW_GROUP_WAM

TYPE SURFACE_VIEW_GROUP_PRECFRAC
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_PRECFRAC), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_PRECFRAC_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_PRECFRAC_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_PRECFRAC_FINAL
END TYPE SURFACE_VIEW_GROUP_PRECFRAC

TYPE SURFACE_VIEW_GROUP_VEXTRA
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VEXTRA), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VEXTRA_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VEXTRA_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VEXTRA_FINAL
END TYPE SURFACE_VIEW_GROUP_VEXTRA

TYPE SURFACE_VIEW_GROUP_VEXTRDI
  REAL(KIND=JPRB), POINTER :: PXEDR(:,:)   ! Eddy diffusivity rate
!>REAL(KIND=JPRB)          :: PXEDR (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_DID%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VEXTRDI), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_XEDR=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VEXTRDI_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VEXTRDI_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VEXTRDI_FINAL
END TYPE SURFACE_VIEW_GROUP_VEXTRDI

TYPE SURFACE_VIEW_GROUP_VPRECIP
  REAL(KIND=JPRB), POINTER :: PPRECIP(:,:)   ! Diagnostic of precipitations type
!>REAL(KIND=JPRB)          :: PPRECIP (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_XPD%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_PRECIP=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VPRECIP_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VPRECIP_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VPRECIP_FINAL
END TYPE SURFACE_VIEW_GROUP_VPRECIP

TYPE SURFACE_VIEW_GROUP_VPRECIP2
  REAL(KIND=JPRB), POINTER :: PPRECIP2(:,:)   ! Diagnostic of precipitations type
!>REAL(KIND=JPRB)          :: PPRECIP2 (YDCPG_OPTS%KLON, YDCPG_OPTS%YRSURF_DIMS%YSD_XP2D%NLEVS)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP2), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_PRECIP2=>NULL()
  CLASS (FIELD_4RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VPRECIP2_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VPRECIP2_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VPRECIP2_FINAL
END TYPE SURFACE_VIEW_GROUP_VPRECIP2

TYPE SURFACE_VIEW_GROUP_VEXTR2
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VEXTR2), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VEXTR2_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VEXTR2_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VEXTR2_FINAL
END TYPE SURFACE_VIEW_GROUP_VEXTR2

TYPE SURFACE_VIEW_GROUP_SFORC
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_SFORC), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_SFORC_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_SFORC_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_SFORC_FINAL
END TYPE SURFACE_VIEW_GROUP_SFORC

TYPE SURFACE_VIEW_GROUP_SFLUX
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_SFLUX), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_SFLUX_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_SFLUX_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_SFLUX_FINAL
END TYPE SURFACE_VIEW_GROUP_SFLUX

TYPE SURFACE_VIEW_GROUP_VO3ABC
  REAL(KIND=JPRB), POINTER :: PA(:)   ! A climatological ozone profile
!>REAL(KIND=JPRB)          :: PA (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PB(:)   ! B climatological ozone profile
!>REAL(KIND=JPRB)          :: PB (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PC(:)   ! C climatological ozone profile
!>REAL(KIND=JPRB)          :: PC (YDCPG_OPTS%KLON)
  REAL(KIND=JPRB), POINTER :: PGROUP(:,:)

  TYPE(SURFACE_VARIABLE_GROUP_VO3ABC), POINTER :: VARIABLE_GROUP
  CLASS (FIELD_2RB), POINTER :: F_A=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_B=>NULL()
  CLASS (FIELD_2RB), POINTER :: F_C=>NULL()
  CLASS (FIELD_3RB), POINTER :: F_GROUP

CONTAINS
  PROCEDURE :: INIT => SURFACE_VIEW_GROUP_VO3ABC_INIT
  PROCEDURE :: UPDATE_VIEW => SURFACE_VIEW_GROUP_VO3ABC_UPDATE_VIEW
  PROCEDURE :: FINAL => SURFACE_VIEW_GROUP_VO3ABC_FINAL
END TYPE SURFACE_VIEW_GROUP_VO3ABC


CONTAINS

  SUBROUTINE SURFACE_VIEW_GROUP_VARSF_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VARSF) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VARSF), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_Z0F => VARIABLE_GROUP%VZ0F%FT0
    SELF%F_ALBF => VARIABLE_GROUP%VALBF%FT0
    SELF%F_EMISF => VARIABLE_GROUP%VEMISF%FT0
    SELF%F_GETRL => VARIABLE_GROUP%VGETRL%FT0
    SELF%F_LSM => VARIABLE_GROUP%VLSM%FT0
    SELF%F_VEG => VARIABLE_GROUP%VVEG%FT0
    SELF%F_VRLAN => VARIABLE_GROUP%VVRLAN%FT0
    SELF%F_VRLDI => VARIABLE_GROUP%VVRLDI%FT0
    SELF%F_SIG => VARIABLE_GROUP%VSIG%FT0
    SELF%F_ALBSF => VARIABLE_GROUP%VALBSF%FT0
    SELF%F_LAN => VARIABLE_GROUP%VLAN%FT0
    SELF%F_SST => VARIABLE_GROUP%VSST%FT0
    SELF%F_SSS => VARIABLE_GROUP%VSSS%FT0
    SELF%F_LZ0H => VARIABLE_GROUP%VLZ0H%FT0
    SELF%F_CVL => VARIABLE_GROUP%VCVL%FT0
    SELF%F_CO2TYP => VARIABLE_GROUP%VCO2TYP%FT0
    SELF%F_CVH => VARIABLE_GROUP%VCVH%FT0
    SELF%F_CUR => VARIABLE_GROUP%VCUR%FT0
    SELF%F_TVL => VARIABLE_GROUP%VTVL%FT0
    SELF%F_TVH => VARIABLE_GROUP%VTVH%FT0
    SELF%F_LAIL => VARIABLE_GROUP%VLAIL%FT0
    SELF%F_LAIH => VARIABLE_GROUP%VLAIH%FT0
    SELF%F_FWET => VARIABLE_GROUP%VFWET%FT0
    SELF%F_SOTY => VARIABLE_GROUP%VSOTY%FT0
    SELF%F_CLK => VARIABLE_GROUP%VCLK%FT0
    SELF%F_DL => VARIABLE_GROUP%VDL%FT0
    SELF%F_CI => VARIABLE_GROUP%VCI%FT0
    SELF%F_UCUR => VARIABLE_GROUP%VUCUR%FT0
    SELF%F_VCUR => VARIABLE_GROUP%VVCUR%FT0
    SELF%F_Z0RLF => VARIABLE_GROUP%VZ0RLF%FT0
    SELF%F_CGPP => VARIABLE_GROUP%VCGPP%FT0
    SELF%F_CREC => VARIABLE_GROUP%VCREC%FT0
    SELF%F_SDFOR => VARIABLE_GROUP%VSDFOR%FT0
    SELF%F_ALUVP => VARIABLE_GROUP%VALUVP%FT0
    SELF%F_ALUVD => VARIABLE_GROUP%VALUVD%FT0
    SELF%F_ALNIP => VARIABLE_GROUP%VALNIP%FT0
    SELF%F_ALNID => VARIABLE_GROUP%VALNID%FT0
    SELF%F_FP1 => VARIABLE_GROUP%VFP1%FT0
    SELF%F_SO2DD => VARIABLE_GROUP%VSO2DD%FT0
    SELF%F_DMSO => VARIABLE_GROUP%VDMSO%FT0
    SELF%F_URBF => VARIABLE_GROUP%VURBF%FT0
    SELF%F_FCA1 => VARIABLE_GROUP%VFCA1%FT0
    SELF%F_FCA2 => VARIABLE_GROUP%VFCA2%FT0
    SELF%F_AERDEP => VARIABLE_GROUP%VAERDEP%FT0
    SELF%F_AERLTS => VARIABLE_GROUP%VAERLTS%FT0
    SELF%F_AERSCC => VARIABLE_GROUP%VAERSCC%FT0
    SELF%F_DSF => VARIABLE_GROUP%VDSF%FT0
    IF (ALLOCATED(VARIABLE_GROUP%VCHEMFLXO)) THEN
      ALLOCATE(SELF%PCHEMFLXO(SIZE(VARIABLE_GROUP%VCHEMFLXO)))
      ALLOCATE(SELF%F_CHEMFLXO(SIZE(VARIABLE_GROUP%VCHEMFLXO)))
      DO I=1, SIZE(VARIABLE_GROUP%VCHEMFLXO)
        SELF%F_CHEMFLXO(I)%PTR => VARIABLE_GROUP%VCHEMFLXO(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VCHEMWDFLX)) THEN
      ALLOCATE(SELF%PCHEMWDFLX(SIZE(VARIABLE_GROUP%VCHEMWDFLX)))
      ALLOCATE(SELF%F_CHEMWDFLX(SIZE(VARIABLE_GROUP%VCHEMWDFLX)))
      DO I=1, SIZE(VARIABLE_GROUP%VCHEMWDFLX)
        SELF%F_CHEMWDFLX(I)%PTR => VARIABLE_GROUP%VCHEMWDFLX(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VCHEMDDFLX)) THEN
      ALLOCATE(SELF%PCHEMDDFLX(SIZE(VARIABLE_GROUP%VCHEMDDFLX)))
      ALLOCATE(SELF%F_CHEMDDFLX(SIZE(VARIABLE_GROUP%VCHEMDDFLX)))
      DO I=1, SIZE(VARIABLE_GROUP%VCHEMDDFLX)
        SELF%F_CHEMDDFLX(I)%PTR => VARIABLE_GROUP%VCHEMDDFLX(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VCHEMDV)) THEN
      ALLOCATE(SELF%PCHEMDV(SIZE(VARIABLE_GROUP%VCHEMDV)))
      ALLOCATE(SELF%F_CHEMDV(SIZE(VARIABLE_GROUP%VCHEMDV)))
      DO I=1, SIZE(VARIABLE_GROUP%VCHEMDV)
        SELF%F_CHEMDV(I)%PTR => VARIABLE_GROUP%VCHEMDV(I)%FT0
      END DO
    END IF
    SELF%F_NUDM => VARIABLE_GROUP%VNUDM%FT0
    IF (ALLOCATED(VARIABLE_GROUP%VEMIS2D)) THEN
      ALLOCATE(SELF%PEMIS2D(SIZE(VARIABLE_GROUP%VEMIS2D)))
      ALLOCATE(SELF%F_EMIS2D(SIZE(VARIABLE_GROUP%VEMIS2D)))
      DO I=1, SIZE(VARIABLE_GROUP%VEMIS2D)
        SELF%F_EMIS2D(I)%PTR => VARIABLE_GROUP%VEMIS2D(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VEMIS2DAUX)) THEN
      ALLOCATE(SELF%PEMIS2DAUX(SIZE(VARIABLE_GROUP%VEMIS2DAUX)))
      ALLOCATE(SELF%F_EMIS2DAUX(SIZE(VARIABLE_GROUP%VEMIS2DAUX)))
      DO I=1, SIZE(VARIABLE_GROUP%VEMIS2DAUX)
        SELF%F_EMIS2DAUX(I)%PTR => VARIABLE_GROUP%VEMIS2DAUX(I)%FT0
      END DO
    END IF
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VARSF_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VARSF_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VARSF) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_Z0F))  SELF%PZ0F => SELF%F_Z0F%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALBF))  SELF%PALBF => SELF%F_ALBF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_EMISF))  SELF%PEMISF => SELF%F_EMISF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_GETRL))  SELF%PGETRL => SELF%F_GETRL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LSM))  SELF%PLSM => SELF%F_LSM%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VEG))  SELF%PVEG => SELF%F_VEG%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VRLAN))  SELF%PVRLAN => SELF%F_VRLAN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VRLDI))  SELF%PVRLDI => SELF%F_VRLDI%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SIG))  SELF%PSIG => SELF%F_SIG%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALBSF))  SELF%PALBSF => SELF%F_ALBSF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LAN))  SELF%PLAN => SELF%F_LAN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SST))  SELF%PSST => SELF%F_SST%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SSS))  SELF%PSSS => SELF%F_SSS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LZ0H))  SELF%PLZ0H => SELF%F_LZ0H%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CVL))  SELF%PCVL => SELF%F_CVL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CO2TYP))  SELF%PCO2TYP => SELF%F_CO2TYP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CVH))  SELF%PCVH => SELF%F_CVH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CUR))  SELF%PCUR => SELF%F_CUR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TVL))  SELF%PTVL => SELF%F_TVL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TVH))  SELF%PTVH => SELF%F_TVH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LAIL))  SELF%PLAIL => SELF%F_LAIL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LAIH))  SELF%PLAIH => SELF%F_LAIH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_FWET))  SELF%PFWET => SELF%F_FWET%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SOTY))  SELF%PSOTY => SELF%F_SOTY%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CLK))  SELF%PCLK => SELF%F_CLK%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DL))  SELF%PDL => SELF%F_DL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CI))  SELF%PCI => SELF%F_CI%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_UCUR))  SELF%PUCUR => SELF%F_UCUR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VCUR))  SELF%PVCUR => SELF%F_VCUR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_Z0RLF))  SELF%PZ0RLF => SELF%F_Z0RLF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CGPP))  SELF%PCGPP => SELF%F_CGPP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CREC))  SELF%PCREC => SELF%F_CREC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SDFOR))  SELF%PSDFOR => SELF%F_SDFOR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALUVP))  SELF%PALUVP => SELF%F_ALUVP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALUVD))  SELF%PALUVD => SELF%F_ALUVD%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALNIP))  SELF%PALNIP => SELF%F_ALNIP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALNID))  SELF%PALNID => SELF%F_ALNID%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_FP1))  SELF%PFP1 => SELF%F_FP1%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SO2DD))  SELF%PSO2DD => SELF%F_SO2DD%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DMSO))  SELF%PDMSO => SELF%F_DMSO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_URBF))  SELF%PURBF => SELF%F_URBF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_FCA1))  SELF%PFCA1 => SELF%F_FCA1%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_FCA2))  SELF%PFCA2 => SELF%F_FCA2%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_AERDEP))  SELF%PAERDEP => SELF%F_AERDEP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_AERLTS))  SELF%PAERLTS => SELF%F_AERLTS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_AERSCC))  SELF%PAERSCC => SELF%F_AERSCC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DSF))  SELF%PDSF => SELF%F_DSF%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_CHEMFLXO)) THEN
      DO I=1, SIZE(SELF%F_CHEMFLXO)
        SELF%PCHEMFLXO(I)%P => SELF%F_CHEMFLXO(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_CHEMWDFLX)) THEN
      DO I=1, SIZE(SELF%F_CHEMWDFLX)
        SELF%PCHEMWDFLX(I)%P => SELF%F_CHEMWDFLX(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_CHEMDDFLX)) THEN
      DO I=1, SIZE(SELF%F_CHEMDDFLX)
        SELF%PCHEMDDFLX(I)%P => SELF%F_CHEMDDFLX(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_CHEMDV)) THEN
      DO I=1, SIZE(SELF%F_CHEMDV)
        SELF%PCHEMDV(I)%P => SELF%F_CHEMDV(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ASSOCIATED(SELF%F_NUDM))  SELF%PNUDM => SELF%F_NUDM%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_EMIS2D)) THEN
      DO I=1, SIZE(SELF%F_EMIS2D)
        SELF%PEMIS2D(I)%P => SELF%F_EMIS2D(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_EMIS2DAUX)) THEN
      DO I=1, SIZE(SELF%F_EMIS2DAUX)
        SELF%PEMIS2DAUX(I)%P => SELF%F_EMIS2DAUX(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VARSF_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VARSF_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VARSF) :: SELF

    IF (ALLOCATED (SELF%PCHEMFLXO)) DEALLOCATE (SELF%PCHEMFLXO)
    IF (ALLOCATED (SELF%F_CHEMFLXO)) DEALLOCATE (SELF%F_CHEMFLXO)
    IF (ALLOCATED (SELF%PCHEMWDFLX)) DEALLOCATE (SELF%PCHEMWDFLX)
    IF (ALLOCATED (SELF%F_CHEMWDFLX)) DEALLOCATE (SELF%F_CHEMWDFLX)
    IF (ALLOCATED (SELF%PCHEMDDFLX)) DEALLOCATE (SELF%PCHEMDDFLX)
    IF (ALLOCATED (SELF%F_CHEMDDFLX)) DEALLOCATE (SELF%F_CHEMDDFLX)
    IF (ALLOCATED (SELF%PCHEMDV)) DEALLOCATE (SELF%PCHEMDV)
    IF (ALLOCATED (SELF%F_CHEMDV)) DEALLOCATE (SELF%F_CHEMDV)
    IF (ALLOCATED (SELF%PEMIS2D)) DEALLOCATE (SELF%PEMIS2D)
    IF (ALLOCATED (SELF%F_EMIS2D)) DEALLOCATE (SELF%F_EMIS2D)
    IF (ALLOCATED (SELF%PEMIS2DAUX)) DEALLOCATE (SELF%PEMIS2DAUX)
    IF (ALLOCATED (SELF%F_EMIS2DAUX)) DEALLOCATE (SELF%F_EMIS2DAUX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VARSF_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VCLIH) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIH), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_TCCH => VARIABLE_GROUP%VTCCH%FT0
    SELF%F_SCCH => VARIABLE_GROUP%VSCCH%FT0
    SELF%F_BCCH => VARIABLE_GROUP%VBCCH%FT0
    SELF%F_PBLH => VARIABLE_GROUP%VPBLH%FT0
    SELF%F_SPSH => VARIABLE_GROUP%VSPSH%FT0
    SELF%F_QSH => VARIABLE_GROUP%VQSH%FT0
    SELF%F_PCL => VARIABLE_GROUP%VPCL%FT0
    SELF%F_PSL => VARIABLE_GROUP%VPSL%FT0
    SELF%F_PCN => VARIABLE_GROUP%VPCN%FT0
    SELF%F_PSN => VARIABLE_GROUP%VPSN%FT0
    SELF%F_EVA => VARIABLE_GROUP%VEVA%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VCLIH) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_TCCH))  SELF%PTCCH => SELF%F_TCCH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SCCH))  SELF%PSCCH => SELF%F_SCCH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_BCCH))  SELF%PBCCH => SELF%F_BCCH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PBLH))  SELF%PPBLH => SELF%F_PBLH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SPSH))  SELF%PSPSH => SELF%F_SPSH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_QSH))  SELF%PQSH => SELF%F_QSH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PCL))  SELF%PPCL => SELF%F_PCL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PSL))  SELF%PPSL => SELF%F_PSL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PCN))  SELF%PPCN => SELF%F_PCN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PSN))  SELF%PPSN => SELF%F_PSN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_EVA))  SELF%PEVA => SELF%F_EVA%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIH) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIH_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VCLIK) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIK), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_UDGRO => VARIABLE_GROUP%VUDGRO%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VCLIK) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_UDGRO))  SELF%PUDGRO => SELF%F_UDGRO%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIK) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIK_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VCLIP) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIP), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_TPC => VARIABLE_GROUP%VTPC%FT0
    SELF%F_WPC => VARIABLE_GROUP%VWPC%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VCLIP) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_TPC))  SELF%PTPC => SELF%F_TPC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_WPC))  SELF%PWPC => SELF%F_WPC%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIP) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIP_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VCLIV) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIV), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_ARG => VARIABLE_GROUP%VARG%FT0
    SELF%F_SAB => VARIABLE_GROUP%VSAB%FT0
    SELF%F_D2 => VARIABLE_GROUP%VD2%FT0
    SELF%F_IVEG => VARIABLE_GROUP%VIVEG%FT0
    SELF%F_RSMIN => VARIABLE_GROUP%VRSMIN%FT0
    SELF%F_LAI => VARIABLE_GROUP%VLAI%FT0
    SELF%F_HV => VARIABLE_GROUP%VHV%FT0
    SELF%F_Z0H => VARIABLE_GROUP%VZ0H%FT0
    SELF%F_ALS => VARIABLE_GROUP%VALS%FT0
    SELF%F_ALV => VARIABLE_GROUP%VALV%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VCLIV) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_ARG))  SELF%PARG => SELF%F_ARG%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SAB))  SELF%PSAB => SELF%F_SAB%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_D2))  SELF%PD2 => SELF%F_D2%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_IVEG))  SELF%PIVEG => SELF%F_IVEG%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_RSMIN))  SELF%PRSMIN => SELF%F_RSMIN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LAI))  SELF%PLAI => SELF%F_LAI%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_HV))  SELF%PHV => SELF%F_HV%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_Z0H))  SELF%PZ0H => SELF%F_Z0H%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALS))  SELF%PALS => SELF%F_ALS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALV))  SELF%PALV => SELF%F_ALV%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIV) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIV_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VCLIA) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIA), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_SEA => VARIABLE_GROUP%VSEA%FT0
    SELF%F_LAN => VARIABLE_GROUP%VLAN%FT0
    SELF%F_SOO => VARIABLE_GROUP%VSOO%FT0
    SELF%F_DES => VARIABLE_GROUP%VDES%FT0
    SELF%F_SUL => VARIABLE_GROUP%VSUL%FT0
    SELF%F_VOL => VARIABLE_GROUP%VVOL%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VCLIA) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_SEA))  SELF%PSEA => SELF%F_SEA%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LAN))  SELF%PLAN => SELF%F_LAN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SOO))  SELF%PSOO => SELF%F_SOO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DES))  SELF%PDES => SELF%F_DES%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SUL))  SELF%PSUL => SELF%F_SUL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VOL))  SELF%PVOL => SELF%F_VOL%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIA) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIA_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VCLIN) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VCLIN), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_TOP => VARIABLE_GROUP%VTOP%FT0
    SELF%F_BAS => VARIABLE_GROUP%VBAS%FT0
    SELF%F_ACPR => VARIABLE_GROUP%VACPR%FT0
    SELF%F_ACCPR => VARIABLE_GROUP%VACCPR%FT0
    SELF%F_ACCPR5 => VARIABLE_GROUP%VACCPR5%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VCLIN) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_TOP))  SELF%PTOP => SELF%F_TOP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_BAS))  SELF%PBAS => SELF%F_BAS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ACPR))  SELF%PACPR => SELF%F_ACPR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ACCPR))  SELF%PACCPR => SELF%F_ACCPR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ACCPR5))  SELF%PACCPR5 => SELF%F_ACCPR5%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VCLIN) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VCLIN_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VDIAGO2) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VDIAGO2), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_OCDEP => VARIABLE_GROUP%VOCDEP%FT0
    SELF%F_USTRC => VARIABLE_GROUP%VUSTRC%FT0
    SELF%F_VSTRC => VARIABLE_GROUP%VVSTRC%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VDIAGO2) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_OCDEP))  SELF%POCDEP => SELF%F_OCDEP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_USTRC))  SELF%PUSTRC => SELF%F_USTRC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VSTRC))  SELF%PVSTRC => SELF%F_VSTRC%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VDIAGO2) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO2_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VDIAGO3) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VDIAGO3), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_DIFM => VARIABLE_GROUP%VDIFM%FT0
    SELF%F_DIFT => VARIABLE_GROUP%VDIFT%FT0
    SELF%F_DIFS => VARIABLE_GROUP%VDIFS%FT0
    SELF%F_ADVT => VARIABLE_GROUP%VADVT%FT0
    SELF%F_ADVS => VARIABLE_GROUP%VADVS%FT0
    SELF%F_TRI0 => VARIABLE_GROUP%VTRI0%FT0
    SELF%F_TRI1 => VARIABLE_GROUP%VTRI1%FT0
    SELF%F_SWDK => VARIABLE_GROUP%VSWDK%FT0
    SELF%F_ZO => VARIABLE_GROUP%VZO%FT0
    SELF%F_HO => VARIABLE_GROUP%VHO%FT0
    SELF%F_DO => VARIABLE_GROUP%VDO%FT0
    SELF%F_HO_INV => VARIABLE_GROUP%VHO_INV%FT0
    SELF%F_UOC => VARIABLE_GROUP%VUOC%FT0
    SELF%F_VOC => VARIABLE_GROUP%VVOC%FT0
    SELF%F_OTKE => VARIABLE_GROUP%VOTKE%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VDIAGO3) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_DIFM))  SELF%PDIFM => SELF%F_DIFM%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DIFT))  SELF%PDIFT => SELF%F_DIFT%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DIFS))  SELF%PDIFS => SELF%F_DIFS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ADVT))  SELF%PADVT => SELF%F_ADVT%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ADVS))  SELF%PADVS => SELF%F_ADVS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TRI0))  SELF%PTRI0 => SELF%F_TRI0%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TRI1))  SELF%PTRI1 => SELF%F_TRI1%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SWDK))  SELF%PSWDK => SELF%F_SWDK%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ZO))  SELF%PZO => SELF%F_ZO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_HO))  SELF%PHO => SELF%F_HO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DO))  SELF%PDO => SELF%F_DO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_HO_INV))  SELF%PHO_INV => SELF%F_HO_INV%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_UOC))  SELF%PUOC => SELF%F_UOC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VOC))  SELF%PVOC => SELF%F_VOC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_OTKE))  SELF%POTKE => SELF%F_OTKE%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VDIAGO3) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAGO3_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VDIAG) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VDIAG), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_LSP => VARIABLE_GROUP%VLSP%FT0
    SELF%F_CP => VARIABLE_GROUP%VCP%FT0
    SELF%F_SF => VARIABLE_GROUP%VSF%FT0
    SELF%F_FZRA => VARIABLE_GROUP%VFZRA%FT0
    SELF%F_BLD => VARIABLE_GROUP%VBLD%FT0
    SELF%F_SSHF => VARIABLE_GROUP%VSSHF%FT0
    SELF%F_SLHF => VARIABLE_GROUP%VSLHF%FT0
    SELF%F_NEE => VARIABLE_GROUP%VNEE%FT0
    SELF%F_GPP => VARIABLE_GROUP%VGPP%FT0
    SELF%F_REC => VARIABLE_GROUP%VREC%FT0
    SELF%F_MSL => VARIABLE_GROUP%VMSL%FT0
    SELF%F_SP => VARIABLE_GROUP%VSP%FT0
    SELF%F_TCC => VARIABLE_GROUP%VTCC%FT0
    SELF%F_10U => VARIABLE_GROUP%V10U%FT0
    SELF%F_10V => VARIABLE_GROUP%V10V%FT0
    SELF%F_2T => VARIABLE_GROUP%V2T%FT0
    SELF%F_2D => VARIABLE_GROUP%V2D%FT0
    SELF%F_2SH => VARIABLE_GROUP%V2SH%FT0
    SELF%F_SSR => VARIABLE_GROUP%VSSR%FT0
    SELF%F_STR => VARIABLE_GROUP%VSTR%FT0
    SELF%F_TSR => VARIABLE_GROUP%VTSR%FT0
    SELF%F_TTR => VARIABLE_GROUP%VTTR%FT0
    SELF%F_EWSS => VARIABLE_GROUP%VEWSS%FT0
    SELF%F_NSSS => VARIABLE_GROUP%VNSSS%FT0
    SELF%F_E => VARIABLE_GROUP%VE%FT0
    SELF%F_PEV => VARIABLE_GROUP%VPEV%FT0
    SELF%F_CCC => VARIABLE_GROUP%VCCC%FT0
    SELF%F_LCC => VARIABLE_GROUP%VLCC%FT0
    SELF%F_MCC => VARIABLE_GROUP%VMCC%FT0
    SELF%F_HCC => VARIABLE_GROUP%VHCC%FT0
    SELF%F_LGWS => VARIABLE_GROUP%VLGWS%FT0
    SELF%F_MGWS => VARIABLE_GROUP%VMGWS%FT0
    SELF%F_GWD => VARIABLE_GROUP%VGWD%FT0
    SELF%F_MX2T => VARIABLE_GROUP%VMX2T%FT0
    SELF%F_MN2T => VARIABLE_GROUP%VMN2T%FT0
    IF (ALLOCATED(VARIABLE_GROUP%VMX2T6)) THEN
      ALLOCATE(SELF%PMX2T6(SIZE(VARIABLE_GROUP%VMX2T6)))
      ALLOCATE(SELF%F_MX2T6(SIZE(VARIABLE_GROUP%VMX2T6)))
      DO I=1, SIZE(VARIABLE_GROUP%VMX2T6)
        SELF%F_MX2T6(I)%PTR => VARIABLE_GROUP%VMX2T6(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VMN2T6)) THEN
      ALLOCATE(SELF%PMN2T6(SIZE(VARIABLE_GROUP%VMN2T6)))
      ALLOCATE(SELF%F_MN2T6(SIZE(VARIABLE_GROUP%VMN2T6)))
      DO I=1, SIZE(VARIABLE_GROUP%VMN2T6)
        SELF%F_MN2T6(I)%PTR => VARIABLE_GROUP%VMN2T6(I)%FT0
      END DO
    END IF
    SELF%F_RO => VARIABLE_GROUP%VRO%FT0
    SELF%F_SRO => VARIABLE_GROUP%VSRO%FT0
    SELF%F_SSRO => VARIABLE_GROUP%VSSRO%FT0
    SELF%F_ALB => VARIABLE_GROUP%VALB%FT0
    SELF%F_IEWSS => VARIABLE_GROUP%VIEWSS%FT0
    SELF%F_INSSS => VARIABLE_GROUP%VINSSS%FT0
    SELF%F_ISSHF => VARIABLE_GROUP%VISSHF%FT0
    SELF%F_IE => VARIABLE_GROUP%VIE%FT0
    SELF%F_INEE => VARIABLE_GROUP%VINEE%FT0
    SELF%F_IGPP => VARIABLE_GROUP%VIGPP%FT0
    SELF%F_IREC => VARIABLE_GROUP%VIREC%FT0
    SELF%F_ICH4 => VARIABLE_GROUP%VICH4%FT0
    SELF%F_CSF => VARIABLE_GROUP%VCSF%FT0
    SELF%F_LSSF => VARIABLE_GROUP%VLSSF%FT0
    SELF%F_MXTPR => VARIABLE_GROUP%VMXTPR%FT0
    SELF%F_MNTPR => VARIABLE_GROUP%VMNTPR%FT0
    IF (ALLOCATED(VARIABLE_GROUP%VMXTPR6)) THEN
      ALLOCATE(SELF%PMXTPR6(SIZE(VARIABLE_GROUP%VMXTPR6)))
      ALLOCATE(SELF%F_MXTPR6(SIZE(VARIABLE_GROUP%VMXTPR6)))
      DO I=1, SIZE(VARIABLE_GROUP%VMXTPR6)
        SELF%F_MXTPR6(I)%PTR => VARIABLE_GROUP%VMXTPR6(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VMNTPR6)) THEN
      ALLOCATE(SELF%PMNTPR6(SIZE(VARIABLE_GROUP%VMNTPR6)))
      ALLOCATE(SELF%F_MNTPR6(SIZE(VARIABLE_GROUP%VMNTPR6)))
      DO I=1, SIZE(VARIABLE_GROUP%VMNTPR6)
        SELF%F_MNTPR6(I)%PTR => VARIABLE_GROUP%VMNTPR6(I)%FT0
      END DO
    END IF
    SELF%F_TPR => VARIABLE_GROUP%VTPR%FT0
    SELF%F_LSRR => VARIABLE_GROUP%VLSRR%FT0
    SELF%F_CRR => VARIABLE_GROUP%VCRR%FT0
    SELF%F_LSSFR => VARIABLE_GROUP%VLSSFR%FT0
    SELF%F_CSFR => VARIABLE_GROUP%VCSFR%FT0
    SELF%F_PTYPE => VARIABLE_GROUP%VPTYPE%FT0
    SELF%F_ILSPF => VARIABLE_GROUP%VILSPF%FT0
    SELF%F_Z0F => VARIABLE_GROUP%VZ0F%FT0
    SELF%F_LZ0H => VARIABLE_GROUP%VLZ0H%FT0
    SELF%F_VIWVE => VARIABLE_GROUP%VVIWVE%FT0
    SELF%F_VIWVN => VARIABLE_GROUP%VVIWVN%FT0
    SELF%F_TCW => VARIABLE_GROUP%VTCW%FT0
    SELF%F_TCWV => VARIABLE_GROUP%VTCWV%FT0
    SELF%F_TCLW => VARIABLE_GROUP%VTCLW%FT0
    SELF%F_TCIW => VARIABLE_GROUP%VTCIW%FT0
    SELF%F_TCRW => VARIABLE_GROUP%VTCRW%FT0
    SELF%F_TCSW => VARIABLE_GROUP%VTCSW%FT0
    SELF%F_TCSLW => VARIABLE_GROUP%VTCSLW%FT0
    SELF%F_SSRD => VARIABLE_GROUP%VSSRD%FT0
    SELF%F_STRD => VARIABLE_GROUP%VSTRD%FT0
    SELF%F_SSRDC => VARIABLE_GROUP%VSSRDC%FT0
    SELF%F_STRDC => VARIABLE_GROUP%VSTRDC%FT0
    SELF%F_BLH => VARIABLE_GROUP%VBLH%FT0
    SELF%F_SUND => VARIABLE_GROUP%VSUND%FT0
    SELF%F_SPAR => VARIABLE_GROUP%VSPAR%FT0
    SELF%F_SUVB => VARIABLE_GROUP%VSUVB%FT0
    SELF%F_SFDIR => VARIABLE_GROUP%VSFDIR%FT0
    SELF%F_SCDIR => VARIABLE_GROUP%VSCDIR%FT0
    SELF%F_SDSRP => VARIABLE_GROUP%VSDSRP%FT0
    SELF%F_CAPE => VARIABLE_GROUP%VCAPE%FT0
    SELF%F_MUCAPE => VARIABLE_GROUP%VMUCAPE%FT0
    SELF%F_MLCAPE50 => VARIABLE_GROUP%VMLCAPE50%FT0
    SELF%F_MLCAPE100 => VARIABLE_GROUP%VMLCAPE100%FT0
    SELF%F_PDEPL => VARIABLE_GROUP%VPDEPL%FT0
    SELF%F_CAPES => VARIABLE_GROUP%VCAPES%FT0
    IF (ALLOCATED(VARIABLE_GROUP%VMXCAP6)) THEN
      ALLOCATE(SELF%PMXCAP6(SIZE(VARIABLE_GROUP%VMXCAP6)))
      ALLOCATE(SELF%F_MXCAP6(SIZE(VARIABLE_GROUP%VMXCAP6)))
      DO I=1, SIZE(VARIABLE_GROUP%VMXCAP6)
        SELF%F_MXCAP6(I)%PTR => VARIABLE_GROUP%VMXCAP6(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VMXCAPS6)) THEN
      ALLOCATE(SELF%PMXCAPS6(SIZE(VARIABLE_GROUP%VMXCAPS6)))
      ALLOCATE(SELF%F_MXCAPS6(SIZE(VARIABLE_GROUP%VMXCAPS6)))
      DO I=1, SIZE(VARIABLE_GROUP%VMXCAPS6)
        SELF%F_MXCAPS6(I)%PTR => VARIABLE_GROUP%VMXCAPS6(I)%FT0
      END DO
    END IF
    SELF%F_TROPOTP => VARIABLE_GROUP%VTROPOTP%FT0
    SELF%F_TSRC => VARIABLE_GROUP%VTSRC%FT0
    SELF%F_TTRC => VARIABLE_GROUP%VTTRC%FT0
    SELF%F_SSRC => VARIABLE_GROUP%VSSRC%FT0
    SELF%F_STRC => VARIABLE_GROUP%VSTRC%FT0
    SELF%F_ES => VARIABLE_GROUP%VES%FT0
    SELF%F_SMLT => VARIABLE_GROUP%VSMLT%FT0
    SELF%F_10FG => VARIABLE_GROUP%V10FG%FT0
    IF (ALLOCATED(VARIABLE_GROUP%V10FG6)) THEN
      ALLOCATE(SELF%P10FG6(SIZE(VARIABLE_GROUP%V10FG6)))
      ALLOCATE(SELF%F_10FG6(SIZE(VARIABLE_GROUP%V10FG6)))
      DO I=1, SIZE(VARIABLE_GROUP%V10FG6)
        SELF%F_10FG6(I)%PTR => VARIABLE_GROUP%V10FG6(I)%FT0
      END DO
    END IF
    SELF%F_10FGCV => VARIABLE_GROUP%V10FGCV%FT0
    SELF%F_I10FG => VARIABLE_GROUP%VI10FG%FT0
    SELF%F_LSPF => VARIABLE_GROUP%VLSPF%FT0
    SELF%F_TCO3 => VARIABLE_GROUP%VTCO3%FT0
    SELF%F_VIMD => VARIABLE_GROUP%VVIMD%FT0
    SELF%F_SPARC => VARIABLE_GROUP%VSPARC%FT0
    SELF%F_STINC => VARIABLE_GROUP%VSTINC%FT0
    SELF%F_CBASE => VARIABLE_GROUP%VCBASE%FT0
    SELF%F_0DEGL => VARIABLE_GROUP%V0DEGL%FT0
    SELF%F_M10DEGL => VARIABLE_GROUP%VM10DEGL%FT0
    SELF%F_VISIH => VARIABLE_GROUP%VVISIH%FT0
    SELF%F_CIN => VARIABLE_GROUP%VCIN%FT0
    SELF%F_MLCIN50 => VARIABLE_GROUP%VMLCIN50%FT0
    SELF%F_MLCIN100 => VARIABLE_GROUP%VMLCIN100%FT0
    SELF%F_KINDEX => VARIABLE_GROUP%VKINDEX%FT0
    SELF%F_TTINDEX => VARIABLE_GROUP%VTTINDEX%FT0
    SELF%F_CBASEA => VARIABLE_GROUP%VCBASEA%FT0
    SELF%F_CTOPC => VARIABLE_GROUP%VCTOPC%FT0
    SELF%F_ZTWETB0 => VARIABLE_GROUP%VZTWETB0%FT0
    SELF%F_ZTWETB1 => VARIABLE_GROUP%VZTWETB1%FT0
    IF (ALLOCATED(VARIABLE_GROUP%VTCGHG)) THEN
      ALLOCATE(SELF%PTCGHG(SIZE(VARIABLE_GROUP%VTCGHG)))
      ALLOCATE(SELF%F_TCGHG(SIZE(VARIABLE_GROUP%VTCGHG)))
      DO I=1, SIZE(VARIABLE_GROUP%VTCGHG)
        SELF%F_TCGHG(I)%PTR => VARIABLE_GROUP%VTCGHG(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VTCCHEM)) THEN
      ALLOCATE(SELF%PTCCHEM(SIZE(VARIABLE_GROUP%VTCCHEM)))
      ALLOCATE(SELF%F_TCCHEM(SIZE(VARIABLE_GROUP%VTCCHEM)))
      DO I=1, SIZE(VARIABLE_GROUP%VTCCHEM)
        SELF%F_TCCHEM(I)%PTR => VARIABLE_GROUP%VTCCHEM(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VAERODIAG)) THEN
      MYSHAPE = SHAPE(VARIABLE_GROUP%VAERODIAG)
      ALLOCATE(SELF%PAERODIAG(MYSHAPE(1), MYSHAPE(2)))
      ALLOCATE(SELF%F_AERODIAG(MYSHAPE(1), MYSHAPE(2)))
      DO J=1, MYSHAPE(2)
        DO I=1, MYSHAPE(1)
          SELF%F_AERODIAG(I,J)%PTR => VARIABLE_GROUP%VAERODIAG(I,J)%FT0
        END DO
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VAERO_WVL_DIAG)) THEN
      MYSHAPE = SHAPE(VARIABLE_GROUP%VAERO_WVL_DIAG)
      ALLOCATE(SELF%PAERO_WVL_DIAG(MYSHAPE(1), MYSHAPE(2)))
      ALLOCATE(SELF%F_AERO_WVL_DIAG(MYSHAPE(1), MYSHAPE(2)))
      DO J=1, MYSHAPE(2)
        DO I=1, MYSHAPE(1)
          SELF%F_AERO_WVL_DIAG(I,J)%PTR => VARIABLE_GROUP%VAERO_WVL_DIAG(I,J)%FT0
        END DO
      END DO
    END IF
    SELF%F_100U => VARIABLE_GROUP%V100U%FT0
    SELF%F_100V => VARIABLE_GROUP%V100V%FT0
    SELF%F_200U => VARIABLE_GROUP%V200U%FT0
    SELF%F_200V => VARIABLE_GROUP%V200V%FT0
    SELF%F_ZUST => VARIABLE_GROUP%VZUST%FT0
    SELF%F_10NU => VARIABLE_GROUP%V10NU%FT0
    SELF%F_10NV => VARIABLE_GROUP%V10NV%FT0
    SELF%F_DNDZN => VARIABLE_GROUP%VDNDZN%FT0
    SELF%F_DNDZA => VARIABLE_GROUP%VDNDZA%FT0
    SELF%F_DCTB => VARIABLE_GROUP%VDCTB%FT0
    SELF%F_TPLB => VARIABLE_GROUP%VTPLB%FT0
    SELF%F_TPLT => VARIABLE_GROUP%VTPLT%FT0
    SELF%F_ODSS => VARIABLE_GROUP%VODSS%FT0
    SELF%F_ODDU => VARIABLE_GROUP%VODDU%FT0
    SELF%F_ODOM => VARIABLE_GROUP%VODOM%FT0
    SELF%F_ODBC => VARIABLE_GROUP%VODBC%FT0
    SELF%F_ODSU => VARIABLE_GROUP%VODSU%FT0
    SELF%F_ODNI => VARIABLE_GROUP%VODNI%FT0
    SELF%F_ODAM => VARIABLE_GROUP%VODAM%FT0
    SELF%F_ODSOA => VARIABLE_GROUP%VODSOA%FT0
    SELF%F_ODVFA => VARIABLE_GROUP%VODVFA%FT0
    SELF%F_ODVSU => VARIABLE_GROUP%VODVSU%FT0
    SELF%F_ODTOACC => VARIABLE_GROUP%VODTOACC%FT0
    SELF%F_AEPM1 => VARIABLE_GROUP%VAEPM1%FT0
    SELF%F_AEPM25 => VARIABLE_GROUP%VAEPM25%FT0
    SELF%F_AEPM10 => VARIABLE_GROUP%VAEPM10%FT0
    SELF%F_UVBED => VARIABLE_GROUP%VUVBED%FT0
    SELF%F_UVBEDCS => VARIABLE_GROUP%VUVBEDCS%FT0
    SELF%F_LITOTI => VARIABLE_GROUP%VLITOTI%FT0
    SELF%F_LICGI => VARIABLE_GROUP%VLICGI%FT0
    IF (ALLOCATED(VARIABLE_GROUP%VLITOTA6)) THEN
      ALLOCATE(SELF%PLITOTA6(SIZE(VARIABLE_GROUP%VLITOTA6)))
      ALLOCATE(SELF%F_LITOTA6(SIZE(VARIABLE_GROUP%VLITOTA6)))
      DO I=1, SIZE(VARIABLE_GROUP%VLITOTA6)
        SELF%F_LITOTA6(I)%PTR => VARIABLE_GROUP%VLITOTA6(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VLICGA6)) THEN
      ALLOCATE(SELF%PLICGA6(SIZE(VARIABLE_GROUP%VLICGA6)))
      ALLOCATE(SELF%F_LICGA6(SIZE(VARIABLE_GROUP%VLICGA6)))
      DO I=1, SIZE(VARIABLE_GROUP%VLICGA6)
        SELF%F_LICGA6(I)%PTR => VARIABLE_GROUP%VLICGA6(I)%FT0
      END DO
    END IF
    IF (ALLOCATED(VARIABLE_GROUP%VPTYPEOCC6)) THEN
      MYSHAPE = SHAPE(VARIABLE_GROUP%VPTYPEOCC6)
      ALLOCATE(SELF%PPTYPEOCC6(MYSHAPE(1), MYSHAPE(2)))
      ALLOCATE(SELF%F_PTYPEOCC6(MYSHAPE(1), MYSHAPE(2)))
      DO J=1, MYSHAPE(2)
        DO I=1, MYSHAPE(1)
          SELF%F_PTYPEOCC6(I,J)%PTR => VARIABLE_GROUP%VPTYPEOCC6(I,J)%FT0
        END DO
      END DO
    END IF
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VDIAG) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_LSP))  SELF%PLSP => SELF%F_LSP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CP))  SELF%PCP => SELF%F_CP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SF))  SELF%PSF => SELF%F_SF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_FZRA))  SELF%PFZRA => SELF%F_FZRA%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_BLD))  SELF%PBLD => SELF%F_BLD%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SSHF))  SELF%PSSHF => SELF%F_SSHF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SLHF))  SELF%PSLHF => SELF%F_SLHF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_NEE))  SELF%PNEE => SELF%F_NEE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_GPP))  SELF%PGPP => SELF%F_GPP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_REC))  SELF%PREC => SELF%F_REC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MSL))  SELF%PMSL => SELF%F_MSL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SP))  SELF%PSP => SELF%F_SP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCC))  SELF%PTCC => SELF%F_TCC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_10U))  SELF%P10U => SELF%F_10U%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_10V))  SELF%P10V => SELF%F_10V%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_2T))  SELF%P2T => SELF%F_2T%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_2D))  SELF%P2D => SELF%F_2D%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_2SH))  SELF%P2SH => SELF%F_2SH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SSR))  SELF%PSSR => SELF%F_SSR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_STR))  SELF%PSTR => SELF%F_STR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TSR))  SELF%PTSR => SELF%F_TSR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TTR))  SELF%PTTR => SELF%F_TTR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_EWSS))  SELF%PEWSS => SELF%F_EWSS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_NSSS))  SELF%PNSSS => SELF%F_NSSS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_E))  SELF%PE => SELF%F_E%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PEV))  SELF%PPEV => SELF%F_PEV%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CCC))  SELF%PCCC => SELF%F_CCC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LCC))  SELF%PLCC => SELF%F_LCC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MCC))  SELF%PMCC => SELF%F_MCC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_HCC))  SELF%PHCC => SELF%F_HCC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LGWS))  SELF%PLGWS => SELF%F_LGWS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MGWS))  SELF%PMGWS => SELF%F_MGWS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_GWD))  SELF%PGWD => SELF%F_GWD%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MX2T))  SELF%PMX2T => SELF%F_MX2T%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MN2T))  SELF%PMN2T => SELF%F_MN2T%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_MX2T6)) THEN
      DO I=1, SIZE(SELF%F_MX2T6)
        SELF%PMX2T6(I)%P => SELF%F_MX2T6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_MN2T6)) THEN
      DO I=1, SIZE(SELF%F_MN2T6)
        SELF%PMN2T6(I)%P => SELF%F_MN2T6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ASSOCIATED(SELF%F_RO))  SELF%PRO => SELF%F_RO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SRO))  SELF%PSRO => SELF%F_SRO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SSRO))  SELF%PSSRO => SELF%F_SSRO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ALB))  SELF%PALB => SELF%F_ALB%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_IEWSS))  SELF%PIEWSS => SELF%F_IEWSS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_INSSS))  SELF%PINSSS => SELF%F_INSSS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ISSHF))  SELF%PISSHF => SELF%F_ISSHF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_IE))  SELF%PIE => SELF%F_IE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_INEE))  SELF%PINEE => SELF%F_INEE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_IGPP))  SELF%PIGPP => SELF%F_IGPP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_IREC))  SELF%PIREC => SELF%F_IREC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ICH4))  SELF%PICH4 => SELF%F_ICH4%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CSF))  SELF%PCSF => SELF%F_CSF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LSSF))  SELF%PLSSF => SELF%F_LSSF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MXTPR))  SELF%PMXTPR => SELF%F_MXTPR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MNTPR))  SELF%PMNTPR => SELF%F_MNTPR%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_MXTPR6)) THEN
      DO I=1, SIZE(SELF%F_MXTPR6)
        SELF%PMXTPR6(I)%P => SELF%F_MXTPR6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_MNTPR6)) THEN
      DO I=1, SIZE(SELF%F_MNTPR6)
        SELF%PMNTPR6(I)%P => SELF%F_MNTPR6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ASSOCIATED(SELF%F_TPR))  SELF%PTPR => SELF%F_TPR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LSRR))  SELF%PLSRR => SELF%F_LSRR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CRR))  SELF%PCRR => SELF%F_CRR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LSSFR))  SELF%PLSSFR => SELF%F_LSSFR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CSFR))  SELF%PCSFR => SELF%F_CSFR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PTYPE))  SELF%PPTYPE => SELF%F_PTYPE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ILSPF))  SELF%PILSPF => SELF%F_ILSPF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_Z0F))  SELF%PZ0F => SELF%F_Z0F%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LZ0H))  SELF%PLZ0H => SELF%F_LZ0H%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VIWVE))  SELF%PVIWVE => SELF%F_VIWVE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VIWVN))  SELF%PVIWVN => SELF%F_VIWVN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCW))  SELF%PTCW => SELF%F_TCW%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCWV))  SELF%PTCWV => SELF%F_TCWV%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCLW))  SELF%PTCLW => SELF%F_TCLW%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCIW))  SELF%PTCIW => SELF%F_TCIW%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCRW))  SELF%PTCRW => SELF%F_TCRW%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCSW))  SELF%PTCSW => SELF%F_TCSW%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCSLW))  SELF%PTCSLW => SELF%F_TCSLW%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SSRD))  SELF%PSSRD => SELF%F_SSRD%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_STRD))  SELF%PSTRD => SELF%F_STRD%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SSRDC))  SELF%PSSRDC => SELF%F_SSRDC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_STRDC))  SELF%PSTRDC => SELF%F_STRDC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_BLH))  SELF%PBLH => SELF%F_BLH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SUND))  SELF%PSUND => SELF%F_SUND%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SPAR))  SELF%PSPAR => SELF%F_SPAR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SUVB))  SELF%PSUVB => SELF%F_SUVB%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SFDIR))  SELF%PSFDIR => SELF%F_SFDIR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SCDIR))  SELF%PSCDIR => SELF%F_SCDIR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SDSRP))  SELF%PSDSRP => SELF%F_SDSRP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CAPE))  SELF%PCAPE => SELF%F_CAPE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MUCAPE))  SELF%PMUCAPE => SELF%F_MUCAPE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MLCAPE50))  SELF%PMLCAPE50 => SELF%F_MLCAPE50%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MLCAPE100))  SELF%PMLCAPE100 => SELF%F_MLCAPE100%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PDEPL))  SELF%PPDEPL => SELF%F_PDEPL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CAPES))  SELF%PCAPES => SELF%F_CAPES%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_MXCAP6)) THEN
      DO I=1, SIZE(SELF%F_MXCAP6)
        SELF%PMXCAP6(I)%P => SELF%F_MXCAP6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_MXCAPS6)) THEN
      DO I=1, SIZE(SELF%F_MXCAPS6)
        SELF%PMXCAPS6(I)%P => SELF%F_MXCAPS6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ASSOCIATED(SELF%F_TROPOTP))  SELF%PTROPOTP => SELF%F_TROPOTP%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TSRC))  SELF%PTSRC => SELF%F_TSRC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TTRC))  SELF%PTTRC => SELF%F_TTRC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SSRC))  SELF%PSSRC => SELF%F_SSRC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_STRC))  SELF%PSTRC => SELF%F_STRC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ES))  SELF%PES => SELF%F_ES%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SMLT))  SELF%PSMLT => SELF%F_SMLT%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_10FG))  SELF%P10FG => SELF%F_10FG%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_10FG6)) THEN
      DO I=1, SIZE(SELF%F_10FG6)
        SELF%P10FG6(I)%P => SELF%F_10FG6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ASSOCIATED(SELF%F_10FGCV))  SELF%P10FGCV => SELF%F_10FGCV%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_I10FG))  SELF%PI10FG => SELF%F_I10FG%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LSPF))  SELF%PLSPF => SELF%F_LSPF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TCO3))  SELF%PTCO3 => SELF%F_TCO3%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VIMD))  SELF%PVIMD => SELF%F_VIMD%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_SPARC))  SELF%PSPARC => SELF%F_SPARC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_STINC))  SELF%PSTINC => SELF%F_STINC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CBASE))  SELF%PCBASE => SELF%F_CBASE%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_0DEGL))  SELF%P0DEGL => SELF%F_0DEGL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_M10DEGL))  SELF%PM10DEGL => SELF%F_M10DEGL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VISIH))  SELF%PVISIH => SELF%F_VISIH%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CIN))  SELF%PCIN => SELF%F_CIN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MLCIN50))  SELF%PMLCIN50 => SELF%F_MLCIN50%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_MLCIN100))  SELF%PMLCIN100 => SELF%F_MLCIN100%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_KINDEX))  SELF%PKINDEX => SELF%F_KINDEX%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TTINDEX))  SELF%PTTINDEX => SELF%F_TTINDEX%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CBASEA))  SELF%PCBASEA => SELF%F_CBASEA%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CTOPC))  SELF%PCTOPC => SELF%F_CTOPC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ZTWETB0))  SELF%PZTWETB0 => SELF%F_ZTWETB0%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ZTWETB1))  SELF%PZTWETB1 => SELF%F_ZTWETB1%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_TCGHG)) THEN
      DO I=1, SIZE(SELF%F_TCGHG)
        SELF%PTCGHG(I)%P => SELF%F_TCGHG(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_TCCHEM)) THEN
      DO I=1, SIZE(SELF%F_TCCHEM)
        SELF%PTCCHEM(I)%P => SELF%F_TCCHEM(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_AERODIAG)) THEN
      DO J=1, SIZE(SELF%F_AERODIAG, DIM=2)
        DO I=1, SIZE(SELF%F_AERODIAG, DIM=1)
          SELF%PAERODIAG(I,J)%P => SELF%F_AERODIAG(I,J)%PTR%GET_VIEW(BLOCK_INDEX)
        END DO
      END DO
    END IF
    IF (ALLOCATED(SELF%F_AERO_WVL_DIAG)) THEN
      DO J=1, SIZE(SELF%F_AERO_WVL_DIAG, DIM=2)
        DO I=1, SIZE(SELF%F_AERO_WVL_DIAG, DIM=1)
          SELF%PAERO_WVL_DIAG(I,J)%P => SELF%F_AERO_WVL_DIAG(I,J)%PTR%GET_VIEW(BLOCK_INDEX)
        END DO
      END DO
    END IF
    IF (ASSOCIATED(SELF%F_100U))  SELF%P100U => SELF%F_100U%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_100V))  SELF%P100V => SELF%F_100V%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_200U))  SELF%P200U => SELF%F_200U%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_200V))  SELF%P200V => SELF%F_200V%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ZUST))  SELF%PZUST => SELF%F_ZUST%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_10NU))  SELF%P10NU => SELF%F_10NU%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_10NV))  SELF%P10NV => SELF%F_10NV%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DNDZN))  SELF%PDNDZN => SELF%F_DNDZN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DNDZA))  SELF%PDNDZA => SELF%F_DNDZA%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_DCTB))  SELF%PDCTB => SELF%F_DCTB%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TPLB))  SELF%PTPLB => SELF%F_TPLB%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TPLT))  SELF%PTPLT => SELF%F_TPLT%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODSS))  SELF%PODSS => SELF%F_ODSS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODDU))  SELF%PODDU => SELF%F_ODDU%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODOM))  SELF%PODOM => SELF%F_ODOM%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODBC))  SELF%PODBC => SELF%F_ODBC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODSU))  SELF%PODSU => SELF%F_ODSU%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODNI))  SELF%PODNI => SELF%F_ODNI%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODAM))  SELF%PODAM => SELF%F_ODAM%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODSOA))  SELF%PODSOA => SELF%F_ODSOA%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODVFA))  SELF%PODVFA => SELF%F_ODVFA%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODVSU))  SELF%PODVSU => SELF%F_ODVSU%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ODTOACC))  SELF%PODTOACC => SELF%F_ODTOACC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_AEPM1))  SELF%PAEPM1 => SELF%F_AEPM1%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_AEPM25))  SELF%PAEPM25 => SELF%F_AEPM25%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_AEPM10))  SELF%PAEPM10 => SELF%F_AEPM10%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_UVBED))  SELF%PUVBED => SELF%F_UVBED%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_UVBEDCS))  SELF%PUVBEDCS => SELF%F_UVBEDCS%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LITOTI))  SELF%PLITOTI => SELF%F_LITOTI%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_LICGI))  SELF%PLICGI => SELF%F_LICGI%GET_VIEW(BLOCK_INDEX)
    IF (ALLOCATED(SELF%F_LITOTA6)) THEN
      DO I=1, SIZE(SELF%F_LITOTA6)
        SELF%PLITOTA6(I)%P => SELF%F_LITOTA6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_LICGA6)) THEN
      DO I=1, SIZE(SELF%F_LICGA6)
        SELF%PLICGA6(I)%P => SELF%F_LICGA6(I)%PTR%GET_VIEW(BLOCK_INDEX)
      END DO
    END IF
    IF (ALLOCATED(SELF%F_PTYPEOCC6)) THEN
      DO J=1, SIZE(SELF%F_PTYPEOCC6, DIM=2)
        DO I=1, SIZE(SELF%F_PTYPEOCC6, DIM=1)
          SELF%PPTYPEOCC6(I,J)%P => SELF%F_PTYPEOCC6(I,J)%PTR%GET_VIEW(BLOCK_INDEX)
        END DO
      END DO
    END IF

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VDIAG) :: SELF

    IF (ALLOCATED (SELF%PMX2T6)) DEALLOCATE (SELF%PMX2T6)
    IF (ALLOCATED (SELF%F_MX2T6)) DEALLOCATE (SELF%F_MX2T6)
    IF (ALLOCATED (SELF%PMN2T6)) DEALLOCATE (SELF%PMN2T6)
    IF (ALLOCATED (SELF%F_MN2T6)) DEALLOCATE (SELF%F_MN2T6)
    IF (ALLOCATED (SELF%PMXTPR6)) DEALLOCATE (SELF%PMXTPR6)
    IF (ALLOCATED (SELF%F_MXTPR6)) DEALLOCATE (SELF%F_MXTPR6)
    IF (ALLOCATED (SELF%PMNTPR6)) DEALLOCATE (SELF%PMNTPR6)
    IF (ALLOCATED (SELF%F_MNTPR6)) DEALLOCATE (SELF%F_MNTPR6)
    IF (ALLOCATED (SELF%PMXCAP6)) DEALLOCATE (SELF%PMXCAP6)
    IF (ALLOCATED (SELF%F_MXCAP6)) DEALLOCATE (SELF%F_MXCAP6)
    IF (ALLOCATED (SELF%PMXCAPS6)) DEALLOCATE (SELF%PMXCAPS6)
    IF (ALLOCATED (SELF%F_MXCAPS6)) DEALLOCATE (SELF%F_MXCAPS6)
    IF (ALLOCATED (SELF%P10FG6)) DEALLOCATE (SELF%P10FG6)
    IF (ALLOCATED (SELF%F_10FG6)) DEALLOCATE (SELF%F_10FG6)
    IF (ALLOCATED (SELF%PTCGHG)) DEALLOCATE (SELF%PTCGHG)
    IF (ALLOCATED (SELF%F_TCGHG)) DEALLOCATE (SELF%F_TCGHG)
    IF (ALLOCATED (SELF%PTCCHEM)) DEALLOCATE (SELF%PTCCHEM)
    IF (ALLOCATED (SELF%F_TCCHEM)) DEALLOCATE (SELF%F_TCCHEM)
    IF (ALLOCATED (SELF%PAERODIAG)) DEALLOCATE (SELF%PAERODIAG)
    IF (ALLOCATED (SELF%F_AERODIAG)) DEALLOCATE (SELF%F_AERODIAG)
    IF (ALLOCATED (SELF%PAERO_WVL_DIAG)) DEALLOCATE (SELF%PAERO_WVL_DIAG)
    IF (ALLOCATED (SELF%F_AERO_WVL_DIAG)) DEALLOCATE (SELF%F_AERO_WVL_DIAG)
    IF (ALLOCATED (SELF%PLITOTA6)) DEALLOCATE (SELF%PLITOTA6)
    IF (ALLOCATED (SELF%F_LITOTA6)) DEALLOCATE (SELF%F_LITOTA6)
    IF (ALLOCATED (SELF%PLICGA6)) DEALLOCATE (SELF%PLICGA6)
    IF (ALLOCATED (SELF%F_LICGA6)) DEALLOCATE (SELF%F_LICGA6)
    IF (ALLOCATED (SELF%PPTYPEOCC6)) DEALLOCATE (SELF%PPTYPEOCC6)
    IF (ALLOCATED (SELF%F_PTYPEOCC6)) DEALLOCATE (SELF%F_PTYPEOCC6)
  END SUBROUTINE SURFACE_VIEW_GROUP_VDIAG_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_SATSIM) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_SATSIM), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_CLBT => VARIABLE_GROUP%VCLBT%FT0
    SELF%F_CSBT => VARIABLE_GROUP%VCSBT%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_SATSIM) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_CLBT))  SELF%PCLBT => SELF%F_CLBT%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CSBT))  SELF%PCSBT => SELF%F_CSBT%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_SATSIM) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_SATSIM_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_WAVES_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_WAVES) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_WAVES), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_CHAR => VARIABLE_GROUP%VCHAR%FT0
    SELF%F_CHARHQ => VARIABLE_GROUP%VCHARHQ%FT0
    SELF%F_USTOKES => VARIABLE_GROUP%VUSTOKES%FT0
    SELF%F_VSTOKES => VARIABLE_GROUP%VVSTOKES%FT0
    SELF%F_TAUOCX => VARIABLE_GROUP%VTAUOCX%FT0
    SELF%F_TAUOCY => VARIABLE_GROUP%VTAUOCY%FT0
    SELF%F_PHIOC => VARIABLE_GROUP%VPHIOC%FT0
    SELF%F_WSEMEAN => VARIABLE_GROUP%VWSEMEAN%FT0
    SELF%F_WSFMEAN => VARIABLE_GROUP%VWSFMEAN%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_WAVES_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_WAVES_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_WAVES) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_CHAR))  SELF%PCHAR => SELF%F_CHAR%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CHARHQ))  SELF%PCHARHQ => SELF%F_CHARHQ%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_USTOKES))  SELF%PUSTOKES => SELF%F_USTOKES%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VSTOKES))  SELF%PVSTOKES => SELF%F_VSTOKES%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TAUOCX))  SELF%PTAUOCX => SELF%F_TAUOCX%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_TAUOCY))  SELF%PTAUOCY => SELF%F_TAUOCY%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_PHIOC))  SELF%PPHIOC => SELF%F_PHIOC%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_WSEMEAN))  SELF%PWSEMEAN => SELF%F_WSEMEAN%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_WSFMEAN))  SELF%PWSFMEAN => SELF%F_WSFMEAN%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_WAVES_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_WAVES_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_WAVES) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_WAVES_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_WAM_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_WAM) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_WAM), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_U10N => VARIABLE_GROUP%VU10N%FT0
    SELF%F_V10N => VARIABLE_GROUP%VV10N%FT0
    SELF%F_RHO => VARIABLE_GROUP%VRHO%FT0
    SELF%F_ZIL => VARIABLE_GROUP%VZIL%FT0
    SELF%F_CIF => VARIABLE_GROUP%VCIF%FT0
    SELF%F_CLK => VARIABLE_GROUP%VCLK%FT0
    SELF%F_UCURW => VARIABLE_GROUP%VUCURW%FT0
    SELF%F_VCURW => VARIABLE_GROUP%VVCURW%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_WAM_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_WAM_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_WAM) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_U10N))  SELF%PU10N => SELF%F_U10N%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_V10N))  SELF%PV10N => SELF%F_V10N%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_RHO))  SELF%PRHO => SELF%F_RHO%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_ZIL))  SELF%PZIL => SELF%F_ZIL%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CIF))  SELF%PCIF => SELF%F_CIF%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_CLK))  SELF%PCLK => SELF%F_CLK%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_UCURW))  SELF%PUCURW => SELF%F_UCURW%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_VCURW))  SELF%PVCURW => SELF%F_VCURW%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_WAM_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_WAM_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_WAM) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_WAM_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_PRECFRAC) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_PRECFRAC), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_PRECFRAC) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_PRECFRAC) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_PRECFRAC_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VEXTRA) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VEXTRA), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VEXTRA) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VEXTRA) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRA_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VEXTRDI) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VEXTRDI), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_XEDR => VARIABLE_GROUP%VXEDR%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VEXTRDI) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_XEDR))  SELF%PXEDR => SELF%F_XEDR%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VEXTRDI) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTRDI_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VPRECIP) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VPRECIP), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_PRECIP => VARIABLE_GROUP%VPRECIP%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VPRECIP) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_PRECIP))  SELF%PPRECIP => SELF%F_PRECIP%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VPRECIP) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VPRECIP2) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VPRECIP2), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_PRECIP2 => VARIABLE_GROUP%VPRECIP2%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VPRECIP2) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_PRECIP2))  SELF%PPRECIP2 => SELF%F_PRECIP2%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VPRECIP2) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VPRECIP2_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VEXTR2) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VEXTR2), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VEXTR2) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VEXTR2) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VEXTR2_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_SFORC_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_SFORC) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_SFORC), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_SFORC_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_SFORC_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_SFORC) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_SFORC_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_SFORC_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_SFORC) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_SFORC_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_SFLUX) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_SFLUX), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_SFLUX) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_SFLUX) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_SFLUX_FINAL

  SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_INIT(SELF, VARIABLE_GROUP)
    ! Constructor of the array view type for a surface variable group
    CLASS(SURFACE_VIEW_GROUP_VO3ABC) :: SELF
    TYPE(SURFACE_VARIABLE_GROUP_VO3ABC), TARGET, INTENT(INOUT) :: VARIABLE_GROUP
    INTEGER(KIND=JPIM) :: I, J, MYSHAPE(2)

    ! Create a view for the "T0" field only for diagnostic fields
    SELF%F_A => VARIABLE_GROUP%VA%FT0
    SELF%F_B => VARIABLE_GROUP%VB%FT0
    SELF%F_C => VARIABLE_GROUP%VC%FT0
    SELF%VARIABLE_GROUP => VARIABLE_GROUP
    SELF%F_GROUP => VARIABLE_GROUP%F_GROUP
  END SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_INIT

  SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_UPDATE_VIEW(SELF, BLOCK_INDEX)
    ! Extract local array views from field objects
    CLASS(SURFACE_VIEW_GROUP_VO3ABC) :: SELF
    INTEGER(KIND=JPIM), INTENT(IN) :: BLOCK_INDEX
    INTEGER(KIND=JPIM) :: I, J

    ! Set up the view pointer for the "T0" field
    IF (ASSOCIATED(SELF%F_A))  SELF%PA => SELF%F_A%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_B))  SELF%PB => SELF%F_B%GET_VIEW(BLOCK_INDEX)
    IF (ASSOCIATED(SELF%F_C))  SELF%PC => SELF%F_C%GET_VIEW(BLOCK_INDEX)

    IF (ASSOCIATED(SELF%F_GROUP))  SELF%PGROUP => SELF%F_GROUP%GET_VIEW(BLOCK_INDEX)
  END SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_UPDATE_VIEW

  SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_FINAL (SELF)
    CLASS(SURFACE_VIEW_GROUP_VO3ABC) :: SELF

  END SUBROUTINE SURFACE_VIEW_GROUP_VO3ABC_FINAL

  
END MODULE SURFACE_VIEWS_DIAGNOSTIC_MODULE


MODULE SURFACE_VARIABLES_MOD

!$ACDC methods --methods-list=host,wipe,copy --field-api --field-api-class sfc

  ! The SURFACE_VARIABLES type provides namespaced access to individual
  ! groups of surface VARIABLE objects via group-specific container
  ! types.
  !
  ! The generated group types contain the set of VARIABLE objects
  ! configured for each surface group. The VARIABLES in turn that hold
  ! metadata for surface fields and provide access to the underlying
  ! storage FIELD objects and respective data pointers for
  ! thread-parallel regions.

USE PARKIND1, ONLY: JPIM, JPRB
USE VARIABLE_MODULE
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE

IMPLICIT NONE

! Prognostic variable group types
TYPE SURFACE_VARIABLE_GROUP_SOILB
  ! Prognostic surface variable group
  TYPE(VARIABLE_3RB) :: VT   ! temperature
  TYPE(VARIABLE_3RB) :: VQ   ! liquid water content
  TYPE(VARIABLE_3RB) :: VTL   ! ice water content (for MF)

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SOILB_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_SOILB
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_SOILB
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_SOILB
END TYPE SURFACE_VARIABLE_GROUP_SOILB

TYPE SURFACE_VARIABLE_GROUP_SNOWG
  ! Prognostic surface variable group
  TYPE(VARIABLE_3RB) :: VF   ! content of surface snow
  TYPE(VARIABLE_3RB) :: VA   ! snow albedo
  TYPE(VARIABLE_3RB) :: VR   ! snow density
  TYPE(VARIABLE_3RB) :: VT   ! total albedo (diagnostic for MF for LVGSN)
  TYPE(VARIABLE_3RB) :: VW   ! Liquid water content

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SNOWG_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_SNOWG
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_SNOWG
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_SNOWG
END TYPE SURFACE_VARIABLE_GROUP_SNOWG

TYPE SURFACE_VARIABLE_GROUP_LAKEB
  ! Prognostic surface variable group
  TYPE(VARIABLE_2RB) :: VLICT   ! lake ice temperature
  TYPE(VARIABLE_2RB) :: VLMLT   ! lake mixed-layer temperature
  TYPE(VARIABLE_2RB) :: VLTLT   ! lake total layer temperature
  TYPE(VARIABLE_2RB) :: VLBLT   ! lake bottom layer temperature
  TYPE(VARIABLE_2RB) :: VLSHF   ! lake shape factor
  TYPE(VARIABLE_2RB) :: VLICD   ! lake ice depth
  TYPE(VARIABLE_2RB) :: VLMLD   ! lake mixed-layer depth

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_LAKEB_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_LAKEB
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_LAKEB
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_LAKEB
END TYPE SURFACE_VARIABLE_GROUP_LAKEB

TYPE SURFACE_VARIABLE_GROUP_RESVR
  ! Prognostic surface variable group
  TYPE(VARIABLE_2RB) :: VT   ! skin temperature (Ts)
  TYPE(VARIABLE_2RB) :: VW   ! skin water content (Wskin) at ECMWF superficial reservoir water content (Ws) at MF
  TYPE(VARIABLE_2RB) :: VFC   ! skin water content (Wl) at MF
  TYPE(VARIABLE_2RB) :: VIC   ! superficial reservoir ice
  TYPE(VARIABLE_2RB) :: VFP1   ! interpolated Ts for 2nd part of 927-FULLPOS

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_RESVR_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_RESVR
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_RESVR
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_RESVR
END TYPE SURFACE_VARIABLE_GROUP_RESVR

TYPE SURFACE_VARIABLE_GROUP_CLS
  ! Prognostic surface variable group
  TYPE(VARIABLE_2RB) :: VTCLS   ! 2m temperature
  TYPE(VARIABLE_2RB) :: VHUCLS   ! 2m humidity
  TYPE(VARIABLE_2RB) :: VUCLS   ! 10m U-wind
  TYPE(VARIABLE_2RB) :: VVCLS   ! 10m V-wind
  TYPE(VARIABLE_2RB) :: VNUCLS   ! 10m neutral U-wind
  TYPE(VARIABLE_2RB) :: VNVCLS   ! 10m neutral V-wind

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_CLS_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_CLS
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_CLS
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_CLS
END TYPE SURFACE_VARIABLE_GROUP_CLS

TYPE SURFACE_VARIABLE_GROUP_OML
  ! Prognostic surface variable group
  TYPE(VARIABLE_3RB) :: VTO   ! temperature
  TYPE(VARIABLE_3RB) :: VSO   ! salinity
  TYPE(VARIABLE_3RB) :: VUO   ! U velocity
  TYPE(VARIABLE_3RB) :: VVO   ! V velocity

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_OML_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_OML
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_OML
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_OML
END TYPE SURFACE_VARIABLE_GROUP_OML

TYPE SURFACE_VARIABLE_GROUP_EXTRP
  ! Prognostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_EXTRP_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_EXTRP
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_EXTRP
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_EXTRP
END TYPE SURFACE_VARIABLE_GROUP_EXTRP

TYPE SURFACE_VARIABLE_GROUP_XTRP2
  ! Prognostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_XTRP2_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_XTRP2
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_XTRP2
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_XTRP2
END TYPE SURFACE_VARIABLE_GROUP_XTRP2

TYPE SURFACE_VARIABLE_GROUP_CANRI
  ! Prognostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_CANRI_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_CANRI
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_CANRI
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_CANRI
END TYPE SURFACE_VARIABLE_GROUP_CANRI


! Diagnostic variable group types
TYPE SURFACE_VARIABLE_GROUP_VARSF
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VZ0F   ! gravity * surface roughness length
  TYPE(VARIABLE_2RB) :: VALBF   ! surface shortwave albedo
  TYPE(VARIABLE_2RB) :: VEMISF   ! surface longwave emissivity
  TYPE(VARIABLE_2RB) :: VGETRL   ! standard deviation of orography
  TYPE(VARIABLE_2RB) :: VLSM   ! land-sea mask
  TYPE(VARIABLE_2RB) :: VVEG   ! vegetation cover
  TYPE(VARIABLE_2RB) :: VVRLAN   ! anisotropy of the sub-grid scale orography
  TYPE(VARIABLE_2RB) :: VVRLDI   ! angle of the direction of orography with the x axis
  TYPE(VARIABLE_2RB) :: VSIG   ! characteristic orographic slope
  TYPE(VARIABLE_2RB) :: VALBSF   ! soil shortwave albedo
  TYPE(VARIABLE_2RB) :: VLAN   ! fraction of land
  TYPE(VARIABLE_2RB) :: VSST   ! (open) sea surface temperature
  TYPE(VARIABLE_2RB) :: VSSS   ! sea surface salinity
  TYPE(VARIABLE_2RB) :: VLZ0H   ! logarithm of roughness length for heat
  TYPE(VARIABLE_2RB) :: VCVL   ! low vegetation cover
  TYPE(VARIABLE_2RB) :: VCO2TYP   ! CO2 photosynthesis type (c3/c4) for low vegetation cover
  TYPE(VARIABLE_2RB) :: VCVH   ! high vegetation cover
  TYPE(VARIABLE_2RB) :: VCUR   ! urban cover
  TYPE(VARIABLE_2RB) :: VTVL   ! low vegetation type
  TYPE(VARIABLE_2RB) :: VTVH   ! high vegetation type
  TYPE(VARIABLE_2RB) :: VLAIL   ! low vegetation LAI
  TYPE(VARIABLE_2RB) :: VLAIH   ! high vegetation LAI
  TYPE(VARIABLE_2RB) :: VFWET   ! wetland fraction
  TYPE(VARIABLE_2RB) :: VSOTY   ! soil type
  TYPE(VARIABLE_2RB) :: VCLK   ! lake cover
  TYPE(VARIABLE_2RB) :: VDL   ! lake depth
  TYPE(VARIABLE_2RB) :: VCI   ! sea ice fraction
  TYPE(VARIABLE_2RB) :: VUCUR   ! U-component of the ocean current
  TYPE(VARIABLE_2RB) :: VVCUR   ! V-component of the ocean current
  TYPE(VARIABLE_2RB) :: VZ0RLF   ! gravity * vegetation roughness length
  TYPE(VARIABLE_2RB) :: VCGPP   ! GPP bias correction factor
  TYPE(VARIABLE_2RB) :: VCREC   ! REC bias correction factor
  TYPE(VARIABLE_2RB) :: VSDFOR   ! SD filtered orography
  TYPE(VARIABLE_2RB) :: VALUVP   ! MODIS-derived parallel albedo for shortwave radiation
  TYPE(VARIABLE_2RB) :: VALUVD   ! MODIS-derived diffuse albedo for shortwave radiation
  TYPE(VARIABLE_2RB) :: VALNIP   ! MODIS-derived parallel albedo for longwave radiation
  TYPE(VARIABLE_2RB) :: VALNID   ! MODIS-derived diffuse albedo for longwave radiation
  TYPE(VARIABLE_2RB) :: VFP1   ! surface orography in the 2nd part of FULLPOS-927
  TYPE(VARIABLE_2RB) :: VSO2DD   ! sulphate dry dep velocity
  TYPE(VARIABLE_2RB) :: VDMSO   ! oceanic DMS
  TYPE(VARIABLE_2RB) :: VURBF   ! Urban fraction
  TYPE(VARIABLE_2RB) :: VFCA1   ! Fraction of calcite over dust 1st bin
  TYPE(VARIABLE_2RB) :: VFCA2   ! Fraction of calcite over dust 2nd bin
  TYPE(VARIABLE_2RB) :: VAERDEP   ! dust emission potential
  TYPE(VARIABLE_2RB) :: VAERLTS   ! dust lifting threshold speed
  TYPE(VARIABLE_2RB) :: VAERSCC   ! dust soil clay content
  TYPE(VARIABLE_2RB) :: VDSF   ! dust source function
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMFLXO   ! total chemistry flux (emissions + deposition)
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMWDFLX   ! wet deposition chemistry flux
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMDDFLX   ! dry deposition chemistry flux
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VCHEMDV   ! chemistry deposition velocity
  TYPE(VARIABLE_2RB) :: VNUDM   ! nudging mask
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VEMIS2D   ! 2D emission fields for composition
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VEMIS2DAUX   ! 2D emission auxiliary fields for composition

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VARSF_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VARSF
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VARSF
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VARSF
END TYPE SURFACE_VARIABLE_GROUP_VARSF

TYPE SURFACE_VARIABLE_GROUP_VCLIH
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VTCCH   ! total convective cloudiness
  TYPE(VARIABLE_2RB) :: VSCCH   ! convective cloud summit
  TYPE(VARIABLE_2RB) :: VBCCH   ! convective cloud base
  TYPE(VARIABLE_2RB) :: VPBLH   ! PBL height
  TYPE(VARIABLE_2RB) :: VSPSH   ! variable for prognostic convection scheme (ALARO)
  TYPE(VARIABLE_2RB) :: VQSH   ! surface moisture historic variable (used by TOUCANS)
  TYPE(VARIABLE_2RB) :: VPCL   ! 
  TYPE(VARIABLE_2RB) :: VPSL   ! 
  TYPE(VARIABLE_2RB) :: VPCN   ! 
  TYPE(VARIABLE_2RB) :: VPSN   ! 
  TYPE(VARIABLE_2RB) :: VEVA   ! 

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIH_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIH
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIH
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIH
END TYPE SURFACE_VARIABLE_GROUP_VCLIH

TYPE SURFACE_VARIABLE_GROUP_VCLIK
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VUDGRO   ! ud top position (accsu)

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIK_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIK
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIK
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIK
END TYPE SURFACE_VARIABLE_GROUP_VCLIK

TYPE SURFACE_VARIABLE_GROUP_VCLIP
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VTPC   ! climatological deep layer temperature
  TYPE(VARIABLE_2RB) :: VWPC   ! climatological deep layer moisture

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIP_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIP
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIP
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIP
END TYPE SURFACE_VARIABLE_GROUP_VCLIP

TYPE SURFACE_VARIABLE_GROUP_VCLIV
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VARG   ! silt percentage within soil
  TYPE(VARIABLE_2RB) :: VSAB   ! percentage of sand within the soil
  TYPE(VARIABLE_2RB) :: VD2   ! soil depth
  TYPE(VARIABLE_2RB) :: VIVEG   ! type of vegetation
  TYPE(VARIABLE_2RB) :: VRSMIN   ! stomatal minimum resistance
  TYPE(VARIABLE_2RB) :: VLAI   ! leaf area index
  TYPE(VARIABLE_2RB) :: VHV   ! resistance to evapotranspiration
  TYPE(VARIABLE_2RB) :: VZ0H   ! gravity * roughness length for heat
  TYPE(VARIABLE_2RB) :: VALS   ! albedo of bare ground
  TYPE(VARIABLE_2RB) :: VALV   ! albedo of vegetation

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIV_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIV
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIV
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIV
END TYPE SURFACE_VARIABLE_GROUP_VCLIV

TYPE SURFACE_VARIABLE_GROUP_VCLIA
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VSEA   ! aerosol sea
  TYPE(VARIABLE_2RB) :: VLAN   ! aerosol land
  TYPE(VARIABLE_2RB) :: VSOO   ! aerosol soot
  TYPE(VARIABLE_2RB) :: VDES   ! aerosol desert
  TYPE(VARIABLE_2RB) :: VSUL   ! aerosol sulfate
  TYPE(VARIABLE_2RB) :: VVOL   ! aerosol volcano

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIA_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIA
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIA
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIA
END TYPE SURFACE_VARIABLE_GROUP_VCLIA

TYPE SURFACE_VARIABLE_GROUP_VCLIN
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VTOP   ! index of convective cloud top
  TYPE(VARIABLE_2RB) :: VBAS   ! index of convective cloud base
  TYPE(VARIABLE_2RB) :: VACPR   ! averaged convective precipitaion rate
  TYPE(VARIABLE_2RB) :: VACCPR   ! accumulated total precipitaion for assimilation
  TYPE(VARIABLE_2RB) :: VACCPR5   ! accumulated total precipitaion for assimilation (trajectory)

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VCLIN_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIN
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIN
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIN
END TYPE SURFACE_VARIABLE_GROUP_VCLIN

TYPE SURFACE_VARIABLE_GROUP_VDIAGO2
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VOCDEP   ! bottom layer depth
  TYPE(VARIABLE_2RB) :: VUSTRC   ! taux clim.
  TYPE(VARIABLE_2RB) :: VVSTRC   ! tauy clim.

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VDIAGO2_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VDIAGO2
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VDIAGO2
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VDIAGO2
END TYPE SURFACE_VARIABLE_GROUP_VDIAGO2

TYPE SURFACE_VARIABLE_GROUP_VDIAGO3
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VDIFM   ! viscosity
  TYPE(VARIABLE_3RB) :: VDIFT   ! diff. coef. of temp
  TYPE(VARIABLE_3RB) :: VDIFS   ! diff. coef. of salinity
  TYPE(VARIABLE_3RB) :: VADVT   ! correction term for temp.
  TYPE(VARIABLE_3RB) :: VADVS   ! correction term for sal.
  TYPE(VARIABLE_3RB) :: VTRI0   ! coef. for solving matrix.
  TYPE(VARIABLE_3RB) :: VTRI1   ! coef. for solving matrix.
  TYPE(VARIABLE_3RB) :: VSWDK   ! radiation term
  TYPE(VARIABLE_3RB) :: VZO   ! depth of layer
  TYPE(VARIABLE_3RB) :: VHO   ! depth of interface layer
  TYPE(VARIABLE_3RB) :: VDO   ! layer thickness
  TYPE(VARIABLE_3RB) :: VHO_INV   ! 1 / YHO
  TYPE(VARIABLE_3RB) :: VUOC   ! U velocity clim.
  TYPE(VARIABLE_3RB) :: VVOC   ! V velocity clim.
  TYPE(VARIABLE_3RB) :: VOTKE   ! ocean turb. kin. energy

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VDIAGO3_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VDIAGO3
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VDIAGO3
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VDIAGO3
END TYPE SURFACE_VARIABLE_GROUP_VDIAGO3

TYPE SURFACE_VARIABLE_GROUP_VDIAG
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VLSP   ! Large scale precipitation
  TYPE(VARIABLE_2RB) :: VCP   ! Convective precipitation
  TYPE(VARIABLE_2RB) :: VSF   ! Snowfall
  TYPE(VARIABLE_2RB) :: VFZRA   ! Freezing rain
  TYPE(VARIABLE_2RB) :: VBLD   ! Boundary layer dissipation
  TYPE(VARIABLE_2RB) :: VSSHF   ! Surface sensible heat flux
  TYPE(VARIABLE_2RB) :: VSLHF   ! Surface latent heat flux
  TYPE(VARIABLE_2RB) :: VNEE   ! Surface net ecosystem exchange of CO2
  TYPE(VARIABLE_2RB) :: VGPP   ! Surface gross primary production of CO2
  TYPE(VARIABLE_2RB) :: VREC   ! Surface ecosystem respiration of CO2
  TYPE(VARIABLE_2RB) :: VMSL   ! Mean sea level pressure
  TYPE(VARIABLE_2RB) :: VSP   ! Surface pressure
  TYPE(VARIABLE_2RB) :: VTCC   ! Total cloud cover
  TYPE(VARIABLE_2RB) :: V10U   ! U-wind at 10 m
  TYPE(VARIABLE_2RB) :: V10V   ! V-wind at 10 m
  TYPE(VARIABLE_2RB) :: V2T   ! Temperature at 2 m
  TYPE(VARIABLE_2RB) :: V2D   ! Dewpoint temperature at 2 m
  TYPE(VARIABLE_2RB) :: V2SH   ! Specific humidity at 2 m
  TYPE(VARIABLE_2RB) :: VSSR   ! Surface solar radiation
  TYPE(VARIABLE_2RB) :: VSTR   ! Surface thermal radiation
  TYPE(VARIABLE_2RB) :: VTSR   ! Top solar radiation
  TYPE(VARIABLE_2RB) :: VTTR   ! Top thermal radiation
  TYPE(VARIABLE_2RB) :: VEWSS   ! Instantaneous surface U-wind stress
  TYPE(VARIABLE_2RB) :: VNSSS   ! Instantaneous surface V-wind stress
  TYPE(VARIABLE_2RB) :: VE   ! Water evaporation
  TYPE(VARIABLE_2RB) :: VPEV   ! Potential evaporation
  TYPE(VARIABLE_2RB) :: VCCC   ! Convective cloud cover
  TYPE(VARIABLE_2RB) :: VLCC   ! Low cloud cover
  TYPE(VARIABLE_2RB) :: VMCC   ! Medium cloud cover
  TYPE(VARIABLE_2RB) :: VHCC   ! High cloud cover
  TYPE(VARIABLE_2RB) :: VLGWS   ! Zonal gravity wave stress
  TYPE(VARIABLE_2RB) :: VMGWS   ! Meridian gravity wave stress
  TYPE(VARIABLE_2RB) :: VGWD   ! Gravity wave dissipation
  TYPE(VARIABLE_2RB) :: VMX2T   ! Maximum temperature at 2 m
  TYPE(VARIABLE_2RB) :: VMN2T   ! Minimum temperature at 2 m
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMX2T6   ! Bins for maximum temperature at 2 m since last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMN2T6   ! Bins for minimum temperature at 2 m since last 6 hours
  TYPE(VARIABLE_2RB) :: VRO   ! Runoff (total)
  TYPE(VARIABLE_2RB) :: VSRO   ! Runoff surface
  TYPE(VARIABLE_2RB) :: VSSRO   ! Runoff sub-surface
  TYPE(VARIABLE_2RB) :: VALB   ! (surface shortwave) albedo
  TYPE(VARIABLE_2RB) :: VIEWSS   ! Instantaneous surface zonal component of stress
  TYPE(VARIABLE_2RB) :: VINSSS   ! Instantaneous surface meridian component of stress
  TYPE(VARIABLE_2RB) :: VISSHF   ! Instantaneous surface heat flux
  TYPE(VARIABLE_2RB) :: VIE   ! Instantaneous surface moisture flux
  TYPE(VARIABLE_2RB) :: VINEE   ! Instantaneous net ecosystem exchange of CO2
  TYPE(VARIABLE_2RB) :: VIGPP   ! Instantaneous gross primary production of CO2
  TYPE(VARIABLE_2RB) :: VIREC   ! Instantaneous ecosystem respiration of CO2
  TYPE(VARIABLE_2RB) :: VICH4   ! Instantaneous wetland CH4 flux
  TYPE(VARIABLE_2RB) :: VCSF   ! Convective snow fall
  TYPE(VARIABLE_2RB) :: VLSSF   ! Large scale snowfall
  TYPE(VARIABLE_2RB) :: VMXTPR   ! Max precip rate since last post-processing
  TYPE(VARIABLE_2RB) :: VMNTPR   ! Min precip rate since last post-processing
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMXTPR6   ! Max precip rate in last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMNTPR6   ! Min precip rate in last 6 hours
  TYPE(VARIABLE_2RB) :: VTPR   ! Total precipitation rate
  TYPE(VARIABLE_2RB) :: VLSRR   ! Large scale rain rate
  TYPE(VARIABLE_2RB) :: VCRR   ! Convective rain rate
  TYPE(VARIABLE_2RB) :: VLSSFR   ! Large scale snowfall rate
  TYPE(VARIABLE_2RB) :: VCSFR   ! Convective snowfall rate
  TYPE(VARIABLE_2RB) :: VPTYPE   ! Precipitation type
  TYPE(VARIABLE_2RB) :: VILSPF   ! Large-scale precipitation fraction (inst.)
  TYPE(VARIABLE_2RB) :: VZ0F   ! Gravity * surface roughness length
  TYPE(VARIABLE_2RB) :: VLZ0H   ! Logarithm of z0 times heat flux
  TYPE(VARIABLE_2RB) :: VVIWVE   ! Vertical integral of eastward water vapour flux
  TYPE(VARIABLE_2RB) :: VVIWVN   ! Vertical integral of northward water vapour flux
  TYPE(VARIABLE_2RB) :: VTCW   ! Total water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCWV   ! Total water vapor content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCLW   ! Total liquid water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCIW   ! Total ice water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCRW   ! Total rain water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCSW   ! Total snow water content in a vertical column
  TYPE(VARIABLE_2RB) :: VTCSLW   ! Total supercooled liquid water content in a vertical column
  TYPE(VARIABLE_2RB) :: VSSRD   ! Downward surface solar radiation
  TYPE(VARIABLE_2RB) :: VSTRD   ! Downward surface thermic radiation
  TYPE(VARIABLE_2RB) :: VSSRDC   ! Clear-sky downward surface solar radiation
  TYPE(VARIABLE_2RB) :: VSTRDC   ! Claer-sky downward surface thermal radiation
  TYPE(VARIABLE_2RB) :: VBLH   ! Height of boundary layer
  TYPE(VARIABLE_2RB) :: VSUND   ! Sunshine duration
  TYPE(VARIABLE_2RB) :: VSPAR   ! Surface downward PARadiation
  TYPE(VARIABLE_2RB) :: VSUVB   ! Surface downward UV-B radiation
  TYPE(VARIABLE_2RB) :: VSFDIR   ! Surface total sky direct downward SW radiation
  TYPE(VARIABLE_2RB) :: VSCDIR   ! Surface clear-sky direct downward SW radiation
  TYPE(VARIABLE_2RB) :: VSDSRP   ! Surface total-sky direct beam downward SW radiation
  TYPE(VARIABLE_2RB) :: VCAPE   ! Conv.avail.potential energy (CAPE)
  TYPE(VARIABLE_2RB) :: VMUCAPE   ! Maximum unstable CAPE
  TYPE(VARIABLE_2RB) :: VMLCAPE50   ! CAPE from 50 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VMLCAPE100   ! CAPE from 100 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VPDEPL   ! Parcel departure level-pressure for MUCAPE
  TYPE(VARIABLE_2RB) :: VCAPES   ! CAPE-Shear
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMXCAP6   ! Bins for maximum CAPE in last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VMXCAPS6   ! Bins for maximum CAPE-Shear in last 6 hours
  TYPE(VARIABLE_2RB) :: VTROPOTP   ! Pressure of thermal Tropopause
  TYPE(VARIABLE_2RB) :: VTSRC   ! Top solar radiation clear sky
  TYPE(VARIABLE_2RB) :: VTTRC   ! Top thermal radiation clear sky
  TYPE(VARIABLE_2RB) :: VSSRC   ! Surface solar radiation clear sky
  TYPE(VARIABLE_2RB) :: VSTRC   ! Surface thermal radiation clear sky
  TYPE(VARIABLE_2RB) :: VES   ! Evaporation of snow
  TYPE(VARIABLE_2RB) :: VSMLT   ! Snow melt
  TYPE(VARIABLE_2RB) :: V10FG   ! Wind gust at 10 m (max since previous pp)
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: V10FG6   ! Bins for wind gust at 10 m (max since last 6 hours)
  TYPE(VARIABLE_2RB) :: V10FGCV   ! convective wind gust at 10m for current time level (m/s)
  TYPE(VARIABLE_2RB) :: VI10FG   ! Wind gust at 10 m ("instantaneous")
  TYPE(VARIABLE_2RB) :: VLSPF   ! Large scale precipitation fraction
  TYPE(VARIABLE_2RB) :: VTCO3   ! Total ozone content in a vertical column
  TYPE(VARIABLE_2RB) :: VVIMD   ! Vertically integrated mass divergence
  TYPE(VARIABLE_2RB) :: VSPARC   ! Surface clear-sky parallel radiation
  TYPE(VARIABLE_2RB) :: VSTINC   ! Top of atmosphere incident solar radiation
  TYPE(VARIABLE_2RB) :: VCBASE   ! Cloud base level
  TYPE(VARIABLE_2RB) :: V0DEGL   ! Zero deg. level
  TYPE(VARIABLE_2RB) :: VM10DEGL   ! -10 deg. level
  TYPE(VARIABLE_2RB) :: VVISIH   ! Horizontal visibility
  TYPE(VARIABLE_2RB) :: VCIN   ! CIN
  TYPE(VARIABLE_2RB) :: VMLCIN50   ! CIN from 50 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VMLCIN100   ! CIN from 100 hPa mixed-layer
  TYPE(VARIABLE_2RB) :: VKINDEX   ! Convective K-Index
  TYPE(VARIABLE_2RB) :: VTTINDEX   ! Convective TT-Index
  TYPE(VARIABLE_2RB) :: VCBASEA   ! Cloud base aviation
  TYPE(VARIABLE_2RB) :: VCTOPC   ! Cloud top convective
  TYPE(VARIABLE_2RB) :: VZTWETB0   ! Height of 0 deg wet bulb temperature
  TYPE(VARIABLE_2RB) :: VZTWETB1   ! Height of 1 deg wet bulb temperature
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VTCGHG   ! Total column greenhouse gases
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VTCCHEM   ! Total column chemistry
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:,:) :: VAERODIAG   ! Per-aerosol-type diagnostics
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:,:) :: VAERO_WVL_DIAG   ! Per-wavelength aerosol optical diagnostics
  TYPE(VARIABLE_2RB) :: V100U   ! 100m zonal wind
  TYPE(VARIABLE_2RB) :: V100V   ! 100m meridional wind
  TYPE(VARIABLE_2RB) :: V200U   ! 200m zonal wind
  TYPE(VARIABLE_2RB) :: V200V   ! 200m meridional wind
  TYPE(VARIABLE_2RB) :: VZUST   ! Friction velocity
  TYPE(VARIABLE_2RB) :: V10NU   ! 10m zonal neutral wind
  TYPE(VARIABLE_2RB) :: V10NV   ! 10m meridional neutral wind
  TYPE(VARIABLE_2RB) :: VDNDZN   ! Minimum vertical refractivity gradient
  TYPE(VARIABLE_2RB) :: VDNDZA   ! Mean vertical refractivity gradient
  TYPE(VARIABLE_2RB) :: VDCTB   ! Duct base height
  TYPE(VARIABLE_2RB) :: VTPLB   ! Trapping layer base height
  TYPE(VARIABLE_2RB) :: VTPLT   ! Trapping layer top height
  TYPE(VARIABLE_2RB) :: VODSS   ! optical depth sea salt aerosols
  TYPE(VARIABLE_2RB) :: VODDU   ! optical depth dust aerosols
  TYPE(VARIABLE_2RB) :: VODOM   ! optical depth organic m. aerosols
  TYPE(VARIABLE_2RB) :: VODBC   ! optical depth black C aerosols
  TYPE(VARIABLE_2RB) :: VODSU   ! optical depth sulphate aerosols
  TYPE(VARIABLE_2RB) :: VODNI   ! optical depth nitrate aerosols
  TYPE(VARIABLE_2RB) :: VODAM   ! optical depth ammonium aerosols
  TYPE(VARIABLE_2RB) :: VODSOA   ! optical depth secondary organic aerosols
  TYPE(VARIABLE_2RB) :: VODVFA   ! optical depth volcanic flying ash
  TYPE(VARIABLE_2RB) :: VODVSU   ! optical depth volcanic sulphate aerosols
  TYPE(VARIABLE_2RB) :: VODTOACC   ! optical depth total aerosol accumulated
  TYPE(VARIABLE_2RB) :: VAEPM1   ! particulate matter le 1 um
  TYPE(VARIABLE_2RB) :: VAEPM25   ! particulate matter le 2.5um
  TYPE(VARIABLE_2RB) :: VAEPM10   ! particulate matter le 10 um
  TYPE(VARIABLE_2RB) :: VUVBED   ! UV biologically effective dose
  TYPE(VARIABLE_2RB) :: VUVBEDCS   ! UV biologically effective dose clear sky
  TYPE(VARIABLE_2RB) :: VLITOTI   ! instantaneous total lightning flash density
  TYPE(VARIABLE_2RB) :: VLICGI   ! instantaneous cloud-to-ground lightning flash density
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VLITOTA6   ! Bins for averaged total lightning over last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:) :: VLICGA6   ! Bins for averaged cloud-to-ground lightning over last 6 hours
  TYPE(VARIABLE_2RB), ALLOCATABLE, DIMENSION(:,:) :: VPTYPEOCC6   ! Bins for accum freq of each precip type over last 6 hours

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VDIAG_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VDIAG
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VDIAG
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VDIAG
END TYPE SURFACE_VARIABLE_GROUP_VDIAG

TYPE SURFACE_VARIABLE_GROUP_SATSIM
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VCLBT   ! Cloudy brightness temperature
  TYPE(VARIABLE_3RB) :: VCSBT   ! Clear-sky brightness temperature

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SATSIM_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_SATSIM
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_SATSIM
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_SATSIM
END TYPE SURFACE_VARIABLE_GROUP_SATSIM

TYPE SURFACE_VARIABLE_GROUP_WAVES
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VCHAR   ! Charnock parameter as modified by the wave model.
  TYPE(VARIABLE_2RB) :: VCHARHQ   ! Charnock for heat and moisture from the wave model.
  TYPE(VARIABLE_2RB) :: VUSTOKES   ! U-component of the surface Stokes drift.
  TYPE(VARIABLE_2RB) :: VVSTOKES   ! V-component of the surface Stokes drift.
  TYPE(VARIABLE_2RB) :: VTAUOCX   ! U-component of the Momentum flux to ocean.
  TYPE(VARIABLE_2RB) :: VTAUOCY   ! V-component of the Momentum flux to ocean.
  TYPE(VARIABLE_2RB) :: VPHIOC   ! Energy flux to ocean.
  TYPE(VARIABLE_2RB) :: VWSEMEAN   ! Windsea variance.
  TYPE(VARIABLE_2RB) :: VWSFMEAN   ! Windsea mean frequency.

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_WAVES_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_WAVES
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_WAVES
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_WAVES
END TYPE SURFACE_VARIABLE_GROUP_WAVES

TYPE SURFACE_VARIABLE_GROUP_WAM
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VU10N   ! 10m neutral wind U-component passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VV10N   ! 10m neutral wind V-component passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VRHO   ! surface density passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VZIL   ! ZI/L passed to the wave model (used for gustiness in WAM).
  TYPE(VARIABLE_2RB) :: VCIF   ! Sea ice fraction passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VCLK   ! Lake cover passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VUCURW   ! Ocean current    U-component passed to the wave model (WAM).
  TYPE(VARIABLE_2RB) :: VVCURW   ! Ocean current    V-component passed to the wave model (WAM).

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_WAM_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_WAM
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_WAM
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_WAM
END TYPE SURFACE_VARIABLE_GROUP_WAM

TYPE SURFACE_VARIABLE_GROUP_PRECFRAC
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_PRECFRAC_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_PRECFRAC
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_PRECFRAC
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_PRECFRAC
END TYPE SURFACE_VARIABLE_GROUP_PRECFRAC

TYPE SURFACE_VARIABLE_GROUP_VEXTRA
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VEXTRA_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VEXTRA
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VEXTRA
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VEXTRA
END TYPE SURFACE_VARIABLE_GROUP_VEXTRA

TYPE SURFACE_VARIABLE_GROUP_VEXTRDI
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VXEDR   ! Eddy diffusivity rate

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VEXTRDI_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VEXTRDI
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VEXTRDI
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VEXTRDI
END TYPE SURFACE_VARIABLE_GROUP_VEXTRDI

TYPE SURFACE_VARIABLE_GROUP_VPRECIP
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VPRECIP   ! Diagnostic of precipitations type

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VPRECIP_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VPRECIP
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VPRECIP
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VPRECIP
END TYPE SURFACE_VARIABLE_GROUP_VPRECIP

TYPE SURFACE_VARIABLE_GROUP_VPRECIP2
  ! Diagnostic surface variable group
  TYPE(VARIABLE_3RB) :: VPRECIP2   ! Diagnostic of precipitations type

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_4RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VPRECIP2_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VPRECIP2
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VPRECIP2
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VPRECIP2
END TYPE SURFACE_VARIABLE_GROUP_VPRECIP2

TYPE SURFACE_VARIABLE_GROUP_VEXTR2
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VEXTR2_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VEXTR2
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VEXTR2
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VEXTR2
END TYPE SURFACE_VARIABLE_GROUP_VEXTR2

TYPE SURFACE_VARIABLE_GROUP_SFORC
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SFORC_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_SFORC
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_SFORC
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_SFORC
END TYPE SURFACE_VARIABLE_GROUP_SFORC

TYPE SURFACE_VARIABLE_GROUP_SFLUX
  ! Diagnostic surface variable group

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_SFLUX_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_SFLUX
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_SFLUX
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_SFLUX
END TYPE SURFACE_VARIABLE_GROUP_SFLUX

TYPE SURFACE_VARIABLE_GROUP_VO3ABC
  ! Diagnostic surface variable group
  TYPE(VARIABLE_2RB) :: VA   ! A climatological ozone profile
  TYPE(VARIABLE_2RB) :: VB   ! B climatological ozone profile
  TYPE(VARIABLE_2RB) :: VC   ! C climatological ozone profile

  ! Store a field encapsualting the entire group array for backward compatibility
  CLASS (FIELD_3RB), POINTER :: F_GROUP => NULL ()
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLE_GROUP_VO3ABC_FINAL 
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLE_GROUP_VO3ABC
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLE_GROUP_VO3ABC
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLE_GROUP_VO3ABC
END TYPE SURFACE_VARIABLE_GROUP_VO3ABC


TYPE SURFACE_VARIABLES
  ! Global variable and field storage for surface variables

  ! Prognostic variable groups
  TYPE(SURFACE_VARIABLE_GROUP_SOILB) :: GSP_SB
  TYPE(SURFACE_VARIABLE_GROUP_SNOWG) :: GSP_SG
  TYPE(SURFACE_VARIABLE_GROUP_LAKEB) :: GSP_SL
  TYPE(SURFACE_VARIABLE_GROUP_RESVR) :: GSP_RR
  TYPE(SURFACE_VARIABLE_GROUP_CLS) :: GSP_CL
  TYPE(SURFACE_VARIABLE_GROUP_OML) :: GSP_OM
  TYPE(SURFACE_VARIABLE_GROUP_EXTRP) :: GSP_EP
  TYPE(SURFACE_VARIABLE_GROUP_XTRP2) :: GSP_X2
  TYPE(SURFACE_VARIABLE_GROUP_CANRI) :: GSP_CI

  ! Diagnostic variable groups
  TYPE(SURFACE_VARIABLE_GROUP_VARSF) :: GSD_VF
  TYPE(SURFACE_VARIABLE_GROUP_VCLIH) :: GSD_VH
  TYPE(SURFACE_VARIABLE_GROUP_VCLIK) :: GSD_VK
  TYPE(SURFACE_VARIABLE_GROUP_VCLIP) :: GSD_VP
  TYPE(SURFACE_VARIABLE_GROUP_VCLIV) :: GSD_VV
  TYPE(SURFACE_VARIABLE_GROUP_VCLIA) :: GSD_VA
  TYPE(SURFACE_VARIABLE_GROUP_VCLIN) :: GSD_VN
  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO2) :: GSD_V2
  TYPE(SURFACE_VARIABLE_GROUP_VDIAGO3) :: GSD_V3
  TYPE(SURFACE_VARIABLE_GROUP_VDIAG) :: GSD_VD
  TYPE(SURFACE_VARIABLE_GROUP_SATSIM) :: GSD_SM
  TYPE(SURFACE_VARIABLE_GROUP_WAVES) :: GSD_WS
  TYPE(SURFACE_VARIABLE_GROUP_WAM) :: GSD_WW
  TYPE(SURFACE_VARIABLE_GROUP_PRECFRAC) :: GSD_PF
  TYPE(SURFACE_VARIABLE_GROUP_VEXTRA) :: GSD_XA
  TYPE(SURFACE_VARIABLE_GROUP_VEXTRDI) :: GSD_DI
  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP) :: GSD_XP
  TYPE(SURFACE_VARIABLE_GROUP_VPRECIP2) :: GSD_XP2
  TYPE(SURFACE_VARIABLE_GROUP_VEXTR2) :: GSD_X2
  TYPE(SURFACE_VARIABLE_GROUP_SFORC) :: GSD_SFO
  TYPE(SURFACE_VARIABLE_GROUP_SFLUX) :: GSD_SFL
  TYPE(SURFACE_VARIABLE_GROUP_VO3ABC) :: GSD_VC
CONTAINS
  PROCEDURE :: FINAL => SURFACE_VARIABLES_FINAL
PROCEDURE :: ACDC_COPY => ACDC_COPY_SURFACE_VARIABLES
PROCEDURE :: ACDC_HOST => ACDC_HOST_SURFACE_VARIABLES
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_SURFACE_VARIABLES
END TYPE SURFACE_VARIABLES

INTERFACE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_SOILB (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SOILB), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_SOILB (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SOILB), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_SOILB (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SOILB), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_SNOWG (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SNOWG), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_SNOWG (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SNOWG), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_SNOWG (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SNOWG), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_LAKEB (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_LAKEB), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_LAKEB (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_LAKEB), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_LAKEB (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_LAKEB), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_RESVR (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_RESVR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_RESVR (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_RESVR), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_RESVR (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_RESVR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_CLS (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_CLS), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_CLS (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_CLS), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_CLS (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_CLS), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_OML (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_OML), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_OML (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_OML), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_OML (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_OML), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_EXTRP (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_EXTRP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_EXTRP (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_EXTRP), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_EXTRP (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_EXTRP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_XTRP2 (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_XTRP2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_XTRP2 (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_XTRP2), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_XTRP2 (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_XTRP2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_CANRI (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_CANRI), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_CANRI (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_CANRI), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_CANRI (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_CANRI), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VARSF (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VARSF), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VARSF (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VARSF), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VARSF (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VARSF), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIH (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIH), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIH (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIH), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIH (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIH), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIK (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIK), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIK (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIK), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIK (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIK), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIP (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIP (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIP), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIP (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIV (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIV), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIV (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIV), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIV (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIV), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIA (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIA), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIA (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIA), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIA (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIA), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VCLIN (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIN), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VCLIN (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIN), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VCLIN (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VCLIN), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VDIAGO2 (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VDIAGO2 (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO2), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VDIAGO2 (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VDIAGO3 (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO3), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VDIAGO3 (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO3), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VDIAGO3 (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO3), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VDIAG (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAG), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VDIAG (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAG), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VDIAG (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VDIAG), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_SATSIM (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SATSIM), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_SATSIM (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SATSIM), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_SATSIM (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SATSIM), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_WAVES (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_WAVES), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_WAVES (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_WAVES), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_WAVES (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_WAVES), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_WAM (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_WAM), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_WAM (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_WAM), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_WAM (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_WAM), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_PRECFRAC (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_PRECFRAC), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_PRECFRAC (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_PRECFRAC), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_PRECFRAC (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_PRECFRAC), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VEXTRA (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTRA), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VEXTRA (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTRA), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VEXTRA (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTRA), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VEXTRDI (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTRDI), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VEXTRDI (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTRDI), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VEXTRDI (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTRDI), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VPRECIP (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VPRECIP (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VPRECIP (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VPRECIP2 (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VPRECIP2 (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP2), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VPRECIP2 (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VEXTR2 (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTR2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VEXTR2 (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTR2), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VEXTR2 (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VEXTR2), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_SFORC (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SFORC), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_SFORC (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SFORC), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_SFORC (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SFORC), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_SFLUX (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SFLUX), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_SFLUX (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SFLUX), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_SFLUX (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_SFLUX), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLE_GROUP_VO3ABC (SELF, LDCREATED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VO3ABC), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLE_GROUP_VO3ABC (SELF)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VO3ABC), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLE_GROUP_VO3ABC (SELF, LDDELETED, LDFIELDAPI)
USE FIELD_UTIL_MODULE
IMPLICIT NONE
CLASS (SURFACE_VARIABLE_GROUP_VO3ABC), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_COPY_SURFACE_VARIABLES (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (SURFACE_VARIABLES), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_SURFACE_VARIABLES (SELF)

IMPLICIT NONE
CLASS (SURFACE_VARIABLES), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_WIPE_SURFACE_VARIABLES (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (SURFACE_VARIABLES), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

CONTAINS

SUBROUTINE SURFACE_VARIABLE_GROUP_SOILB_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SOILB) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VT%FINAL
  CALL SELF%VQ%FINAL
  CALL SELF%VTL%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SNOWG_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SNOWG) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VF%FINAL
  CALL SELF%VA%FINAL
  CALL SELF%VR%FINAL
  CALL SELF%VT%FINAL
  CALL SELF%VW%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_LAKEB_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_LAKEB) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VLICT%FINAL
  CALL SELF%VLMLT%FINAL
  CALL SELF%VLTLT%FINAL
  CALL SELF%VLBLT%FINAL
  CALL SELF%VLSHF%FINAL
  CALL SELF%VLICD%FINAL
  CALL SELF%VLMLD%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_RESVR_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_RESVR) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VT%FINAL
  CALL SELF%VW%FINAL
  CALL SELF%VFC%FINAL
  CALL SELF%VIC%FINAL
  CALL SELF%VFP1%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_CLS_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_CLS) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VTCLS%FINAL
  CALL SELF%VHUCLS%FINAL
  CALL SELF%VUCLS%FINAL
  CALL SELF%VVCLS%FINAL
  CALL SELF%VNUCLS%FINAL
  CALL SELF%VNVCLS%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_OML_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_OML) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VTO%FINAL
  CALL SELF%VSO%FINAL
  CALL SELF%VUO%FINAL
  CALL SELF%VVO%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_EXTRP_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_EXTRP) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_XTRP2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_XTRP2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_CANRI_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_CANRI) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE


SUBROUTINE SURFACE_VARIABLE_GROUP_VARSF_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VARSF) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VZ0F%FINAL
  CALL SELF%VALBF%FINAL
  CALL SELF%VEMISF%FINAL
  CALL SELF%VGETRL%FINAL
  CALL SELF%VLSM%FINAL
  CALL SELF%VVEG%FINAL
  CALL SELF%VVRLAN%FINAL
  CALL SELF%VVRLDI%FINAL
  CALL SELF%VSIG%FINAL
  CALL SELF%VALBSF%FINAL
  CALL SELF%VLAN%FINAL
  CALL SELF%VSST%FINAL
  CALL SELF%VSSS%FINAL
  CALL SELF%VLZ0H%FINAL
  CALL SELF%VCVL%FINAL
  CALL SELF%VCO2TYP%FINAL
  CALL SELF%VCVH%FINAL
  CALL SELF%VCUR%FINAL
  CALL SELF%VTVL%FINAL
  CALL SELF%VTVH%FINAL
  CALL SELF%VLAIL%FINAL
  CALL SELF%VLAIH%FINAL
  CALL SELF%VFWET%FINAL
  CALL SELF%VSOTY%FINAL
  CALL SELF%VCLK%FINAL
  CALL SELF%VDL%FINAL
  CALL SELF%VCI%FINAL
  CALL SELF%VUCUR%FINAL
  CALL SELF%VVCUR%FINAL
  CALL SELF%VZ0RLF%FINAL
  CALL SELF%VCGPP%FINAL
  CALL SELF%VCREC%FINAL
  CALL SELF%VSDFOR%FINAL
  CALL SELF%VALUVP%FINAL
  CALL SELF%VALUVD%FINAL
  CALL SELF%VALNIP%FINAL
  CALL SELF%VALNID%FINAL
  CALL SELF%VFP1%FINAL
  CALL SELF%VSO2DD%FINAL
  CALL SELF%VDMSO%FINAL
  CALL SELF%VURBF%FINAL
  CALL SELF%VFCA1%FINAL
  CALL SELF%VFCA2%FINAL
  CALL SELF%VAERDEP%FINAL
  CALL SELF%VAERLTS%FINAL
  CALL SELF%VAERSCC%FINAL
  CALL SELF%VDSF%FINAL
  IF (ALLOCATED (SELF%VCHEMFLXO)) THEN
    DO I1 = 1, SIZE (SELF%VCHEMFLXO, 1)
      CALL SELF%VCHEMFLXO(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VCHEMWDFLX)) THEN
    DO I1 = 1, SIZE (SELF%VCHEMWDFLX, 1)
      CALL SELF%VCHEMWDFLX(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VCHEMDDFLX)) THEN
    DO I1 = 1, SIZE (SELF%VCHEMDDFLX, 1)
      CALL SELF%VCHEMDDFLX(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VCHEMDV)) THEN
    DO I1 = 1, SIZE (SELF%VCHEMDV, 1)
      CALL SELF%VCHEMDV(I1)%FINAL
    ENDDO
  ENDIF
  CALL SELF%VNUDM%FINAL
  IF (ALLOCATED (SELF%VEMIS2D)) THEN
    DO I1 = 1, SIZE (SELF%VEMIS2D, 1)
      CALL SELF%VEMIS2D(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VEMIS2DAUX)) THEN
    DO I1 = 1, SIZE (SELF%VEMIS2DAUX, 1)
      CALL SELF%VEMIS2DAUX(I1)%FINAL
    ENDDO
  ENDIF

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIH_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIH) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VTCCH%FINAL
  CALL SELF%VSCCH%FINAL
  CALL SELF%VBCCH%FINAL
  CALL SELF%VPBLH%FINAL
  CALL SELF%VSPSH%FINAL
  CALL SELF%VQSH%FINAL
  CALL SELF%VPCL%FINAL
  CALL SELF%VPSL%FINAL
  CALL SELF%VPCN%FINAL
  CALL SELF%VPSN%FINAL
  CALL SELF%VEVA%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIK_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIK) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VUDGRO%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIP_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIP) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VTPC%FINAL
  CALL SELF%VWPC%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIV_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIV) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VARG%FINAL
  CALL SELF%VSAB%FINAL
  CALL SELF%VD2%FINAL
  CALL SELF%VIVEG%FINAL
  CALL SELF%VRSMIN%FINAL
  CALL SELF%VLAI%FINAL
  CALL SELF%VHV%FINAL
  CALL SELF%VZ0H%FINAL
  CALL SELF%VALS%FINAL
  CALL SELF%VALV%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIA_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIA) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VSEA%FINAL
  CALL SELF%VLAN%FINAL
  CALL SELF%VSOO%FINAL
  CALL SELF%VDES%FINAL
  CALL SELF%VSUL%FINAL
  CALL SELF%VVOL%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VCLIN_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VCLIN) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VTOP%FINAL
  CALL SELF%VBAS%FINAL
  CALL SELF%VACPR%FINAL
  CALL SELF%VACCPR%FINAL
  CALL SELF%VACCPR5%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VDIAGO2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VOCDEP%FINAL
  CALL SELF%VUSTRC%FINAL
  CALL SELF%VVSTRC%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VDIAGO3_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VDIAGO3) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VDIFM%FINAL
  CALL SELF%VDIFT%FINAL
  CALL SELF%VDIFS%FINAL
  CALL SELF%VADVT%FINAL
  CALL SELF%VADVS%FINAL
  CALL SELF%VTRI0%FINAL
  CALL SELF%VTRI1%FINAL
  CALL SELF%VSWDK%FINAL
  CALL SELF%VZO%FINAL
  CALL SELF%VHO%FINAL
  CALL SELF%VDO%FINAL
  CALL SELF%VHO_INV%FINAL
  CALL SELF%VUOC%FINAL
  CALL SELF%VVOC%FINAL
  CALL SELF%VOTKE%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VDIAG_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VDIAG) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VLSP%FINAL
  CALL SELF%VCP%FINAL
  CALL SELF%VSF%FINAL
  CALL SELF%VFZRA%FINAL
  CALL SELF%VBLD%FINAL
  CALL SELF%VSSHF%FINAL
  CALL SELF%VSLHF%FINAL
  CALL SELF%VNEE%FINAL
  CALL SELF%VGPP%FINAL
  CALL SELF%VREC%FINAL
  CALL SELF%VMSL%FINAL
  CALL SELF%VSP%FINAL
  CALL SELF%VTCC%FINAL
  CALL SELF%V10U%FINAL
  CALL SELF%V10V%FINAL
  CALL SELF%V2T%FINAL
  CALL SELF%V2D%FINAL
  CALL SELF%V2SH%FINAL
  CALL SELF%VSSR%FINAL
  CALL SELF%VSTR%FINAL
  CALL SELF%VTSR%FINAL
  CALL SELF%VTTR%FINAL
  CALL SELF%VEWSS%FINAL
  CALL SELF%VNSSS%FINAL
  CALL SELF%VE%FINAL
  CALL SELF%VPEV%FINAL
  CALL SELF%VCCC%FINAL
  CALL SELF%VLCC%FINAL
  CALL SELF%VMCC%FINAL
  CALL SELF%VHCC%FINAL
  CALL SELF%VLGWS%FINAL
  CALL SELF%VMGWS%FINAL
  CALL SELF%VGWD%FINAL
  CALL SELF%VMX2T%FINAL
  CALL SELF%VMN2T%FINAL
  IF (ALLOCATED (SELF%VMX2T6)) THEN
    DO I1 = 1, SIZE (SELF%VMX2T6, 1)
      CALL SELF%VMX2T6(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VMN2T6)) THEN
    DO I1 = 1, SIZE (SELF%VMN2T6, 1)
      CALL SELF%VMN2T6(I1)%FINAL
    ENDDO
  ENDIF
  CALL SELF%VRO%FINAL
  CALL SELF%VSRO%FINAL
  CALL SELF%VSSRO%FINAL
  CALL SELF%VALB%FINAL
  CALL SELF%VIEWSS%FINAL
  CALL SELF%VINSSS%FINAL
  CALL SELF%VISSHF%FINAL
  CALL SELF%VIE%FINAL
  CALL SELF%VINEE%FINAL
  CALL SELF%VIGPP%FINAL
  CALL SELF%VIREC%FINAL
  CALL SELF%VICH4%FINAL
  CALL SELF%VCSF%FINAL
  CALL SELF%VLSSF%FINAL
  CALL SELF%VMXTPR%FINAL
  CALL SELF%VMNTPR%FINAL
  IF (ALLOCATED (SELF%VMXTPR6)) THEN
    DO I1 = 1, SIZE (SELF%VMXTPR6, 1)
      CALL SELF%VMXTPR6(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VMNTPR6)) THEN
    DO I1 = 1, SIZE (SELF%VMNTPR6, 1)
      CALL SELF%VMNTPR6(I1)%FINAL
    ENDDO
  ENDIF
  CALL SELF%VTPR%FINAL
  CALL SELF%VLSRR%FINAL
  CALL SELF%VCRR%FINAL
  CALL SELF%VLSSFR%FINAL
  CALL SELF%VCSFR%FINAL
  CALL SELF%VPTYPE%FINAL
  CALL SELF%VILSPF%FINAL
  CALL SELF%VZ0F%FINAL
  CALL SELF%VLZ0H%FINAL
  CALL SELF%VVIWVE%FINAL
  CALL SELF%VVIWVN%FINAL
  CALL SELF%VTCW%FINAL
  CALL SELF%VTCWV%FINAL
  CALL SELF%VTCLW%FINAL
  CALL SELF%VTCIW%FINAL
  CALL SELF%VTCRW%FINAL
  CALL SELF%VTCSW%FINAL
  CALL SELF%VTCSLW%FINAL
  CALL SELF%VSSRD%FINAL
  CALL SELF%VSTRD%FINAL
  CALL SELF%VSSRDC%FINAL
  CALL SELF%VSTRDC%FINAL
  CALL SELF%VBLH%FINAL
  CALL SELF%VSUND%FINAL
  CALL SELF%VSPAR%FINAL
  CALL SELF%VSUVB%FINAL
  CALL SELF%VSFDIR%FINAL
  CALL SELF%VSCDIR%FINAL
  CALL SELF%VSDSRP%FINAL
  CALL SELF%VCAPE%FINAL
  CALL SELF%VMUCAPE%FINAL
  CALL SELF%VMLCAPE50%FINAL
  CALL SELF%VMLCAPE100%FINAL
  CALL SELF%VPDEPL%FINAL
  CALL SELF%VCAPES%FINAL
  IF (ALLOCATED (SELF%VMXCAP6)) THEN
    DO I1 = 1, SIZE (SELF%VMXCAP6, 1)
      CALL SELF%VMXCAP6(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VMXCAPS6)) THEN
    DO I1 = 1, SIZE (SELF%VMXCAPS6, 1)
      CALL SELF%VMXCAPS6(I1)%FINAL
    ENDDO
  ENDIF
  CALL SELF%VTROPOTP%FINAL
  CALL SELF%VTSRC%FINAL
  CALL SELF%VTTRC%FINAL
  CALL SELF%VSSRC%FINAL
  CALL SELF%VSTRC%FINAL
  CALL SELF%VES%FINAL
  CALL SELF%VSMLT%FINAL
  CALL SELF%V10FG%FINAL
  IF (ALLOCATED (SELF%V10FG6)) THEN
    DO I1 = 1, SIZE (SELF%V10FG6, 1)
      CALL SELF%V10FG6(I1)%FINAL
    ENDDO
  ENDIF
  CALL SELF%V10FGCV%FINAL
  CALL SELF%VI10FG%FINAL
  CALL SELF%VLSPF%FINAL
  CALL SELF%VTCO3%FINAL
  CALL SELF%VVIMD%FINAL
  CALL SELF%VSPARC%FINAL
  CALL SELF%VSTINC%FINAL
  CALL SELF%VCBASE%FINAL
  CALL SELF%V0DEGL%FINAL
  CALL SELF%VM10DEGL%FINAL
  CALL SELF%VVISIH%FINAL
  CALL SELF%VCIN%FINAL
  CALL SELF%VMLCIN50%FINAL
  CALL SELF%VMLCIN100%FINAL
  CALL SELF%VKINDEX%FINAL
  CALL SELF%VTTINDEX%FINAL
  CALL SELF%VCBASEA%FINAL
  CALL SELF%VCTOPC%FINAL
  CALL SELF%VZTWETB0%FINAL
  CALL SELF%VZTWETB1%FINAL
  IF (ALLOCATED (SELF%VTCGHG)) THEN
    DO I1 = 1, SIZE (SELF%VTCGHG, 1)
      CALL SELF%VTCGHG(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VTCCHEM)) THEN
    DO I1 = 1, SIZE (SELF%VTCCHEM, 1)
      CALL SELF%VTCCHEM(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VAERODIAG)) THEN
    DO I2 = 1, SIZE (SELF%VAERODIAG, 2)
    DO I1 = 1, SIZE (SELF%VAERODIAG, 1)
      CALL SELF%VAERODIAG(I1,I2)%FINAL
    ENDDO
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VAERO_WVL_DIAG)) THEN
    DO I2 = 1, SIZE (SELF%VAERO_WVL_DIAG, 2)
    DO I1 = 1, SIZE (SELF%VAERO_WVL_DIAG, 1)
      CALL SELF%VAERO_WVL_DIAG(I1,I2)%FINAL
    ENDDO
    ENDDO
  ENDIF
  CALL SELF%V100U%FINAL
  CALL SELF%V100V%FINAL
  CALL SELF%V200U%FINAL
  CALL SELF%V200V%FINAL
  CALL SELF%VZUST%FINAL
  CALL SELF%V10NU%FINAL
  CALL SELF%V10NV%FINAL
  CALL SELF%VDNDZN%FINAL
  CALL SELF%VDNDZA%FINAL
  CALL SELF%VDCTB%FINAL
  CALL SELF%VTPLB%FINAL
  CALL SELF%VTPLT%FINAL
  CALL SELF%VODSS%FINAL
  CALL SELF%VODDU%FINAL
  CALL SELF%VODOM%FINAL
  CALL SELF%VODBC%FINAL
  CALL SELF%VODSU%FINAL
  CALL SELF%VODNI%FINAL
  CALL SELF%VODAM%FINAL
  CALL SELF%VODSOA%FINAL
  CALL SELF%VODVFA%FINAL
  CALL SELF%VODVSU%FINAL
  CALL SELF%VODTOACC%FINAL
  CALL SELF%VAEPM1%FINAL
  CALL SELF%VAEPM25%FINAL
  CALL SELF%VAEPM10%FINAL
  CALL SELF%VUVBED%FINAL
  CALL SELF%VUVBEDCS%FINAL
  CALL SELF%VLITOTI%FINAL
  CALL SELF%VLICGI%FINAL
  IF (ALLOCATED (SELF%VLITOTA6)) THEN
    DO I1 = 1, SIZE (SELF%VLITOTA6, 1)
      CALL SELF%VLITOTA6(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VLICGA6)) THEN
    DO I1 = 1, SIZE (SELF%VLICGA6, 1)
      CALL SELF%VLICGA6(I1)%FINAL
    ENDDO
  ENDIF
  IF (ALLOCATED (SELF%VPTYPEOCC6)) THEN
    DO I2 = 1, SIZE (SELF%VPTYPEOCC6, 2)
    DO I1 = 1, SIZE (SELF%VPTYPEOCC6, 1)
      CALL SELF%VPTYPEOCC6(I1,I2)%FINAL
    ENDDO
    ENDDO
  ENDIF

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SATSIM_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SATSIM) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VCLBT%FINAL
  CALL SELF%VCSBT%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_WAVES_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_WAVES) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VCHAR%FINAL
  CALL SELF%VCHARHQ%FINAL
  CALL SELF%VUSTOKES%FINAL
  CALL SELF%VVSTOKES%FINAL
  CALL SELF%VTAUOCX%FINAL
  CALL SELF%VTAUOCY%FINAL
  CALL SELF%VPHIOC%FINAL
  CALL SELF%VWSEMEAN%FINAL
  CALL SELF%VWSFMEAN%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_WAM_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_WAM) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VU10N%FINAL
  CALL SELF%VV10N%FINAL
  CALL SELF%VRHO%FINAL
  CALL SELF%VZIL%FINAL
  CALL SELF%VCIF%FINAL
  CALL SELF%VCLK%FINAL
  CALL SELF%VUCURW%FINAL
  CALL SELF%VVCURW%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_PRECFRAC_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_PRECFRAC) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VEXTRA_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VEXTRA) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VEXTRDI_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VEXTRDI) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VXEDR%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VPRECIP_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VPRECIP%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VPRECIP2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VPRECIP2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VPRECIP2%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VEXTR2_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VEXTR2) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SFORC_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SFORC) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_SFLUX_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_SFLUX) :: SELF

INTEGER (KIND=JPIM) :: I1, I2


  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE

SUBROUTINE SURFACE_VARIABLE_GROUP_VO3ABC_FINAL (SELF)
CLASS (SURFACE_VARIABLE_GROUP_VO3ABC) :: SELF

INTEGER (KIND=JPIM) :: I1, I2

  CALL SELF%VA%FINAL
  CALL SELF%VB%FINAL
  CALL SELF%VC%FINAL

  CALL FIELD_DELETE (SELF%F_GROUP)

END SUBROUTINE



SUBROUTINE SURFACE_VARIABLES_FINAL (SELF)
CLASS (SURFACE_VARIABLES) :: SELF

  CALL SELF%GSP_SB%FINAL
  CALL SELF%GSP_SG%FINAL
  CALL SELF%GSP_SL%FINAL
  CALL SELF%GSP_RR%FINAL
  CALL SELF%GSP_CL%FINAL
  CALL SELF%GSP_OM%FINAL
  CALL SELF%GSP_EP%FINAL
  CALL SELF%GSP_X2%FINAL
  CALL SELF%GSP_CI%FINAL

  CALL SELF%GSD_VF%FINAL
  CALL SELF%GSD_VH%FINAL
  CALL SELF%GSD_VK%FINAL
  CALL SELF%GSD_VP%FINAL
  CALL SELF%GSD_VV%FINAL
  CALL SELF%GSD_VA%FINAL
  CALL SELF%GSD_VN%FINAL
  CALL SELF%GSD_V2%FINAL
  CALL SELF%GSD_V3%FINAL
  CALL SELF%GSD_VD%FINAL
  CALL SELF%GSD_SM%FINAL
  CALL SELF%GSD_WS%FINAL
  CALL SELF%GSD_WW%FINAL
  CALL SELF%GSD_PF%FINAL
  CALL SELF%GSD_XA%FINAL
  CALL SELF%GSD_DI%FINAL
  CALL SELF%GSD_XP%FINAL
  CALL SELF%GSD_XP2%FINAL
  CALL SELF%GSD_X2%FINAL
  CALL SELF%GSD_SFO%FINAL
  CALL SELF%GSD_SFL%FINAL
  CALL SELF%GSD_VC%FINAL
 
END SUBROUTINE

END MODULE SURFACE_VARIABLES_MOD

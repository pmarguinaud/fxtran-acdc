MODULE SURFACE_FIELDS_MIX

!     Purpose.
!     --------

!      SURFACE_FIELDS_MIX contains data structures and manipulation routines
!      for the surface (physics) fields in the IFS            

!      This module is a mix of declarations, type definitions and 
!       subroutines linked with surface fields. There are four parts:
!       1/ Declaration of dimensions (including some parameter variables).
!       2/ Definition of types.
!       3/ Declarations:
!          Declaration of variables SP_[group], YSP_[group]D, YSP_[group]
!           (prognostic surface fields).
!          Declaration of variables SD_[group], YSD_[group]D, YSD_[group]
!           (diagnostic surface fields).
!       4/ Some routines linked to the surface data flow:
!          * INI_SFLP3: Initialize 3-D surface field group
!          * SETUP_SFLP3: Setup 3-D surface field
!          * FINALISE_SFLP3: Finalise 3-D surface field
!          * INI_SFLP2: Initialize 2-D surface field group
!          * SETUP_SFLP2: Setup 2-D surface field
!          * FINALISE_SFLP2: Finalise 2-D surface field
!          * GPPOPER: Operations on prognostic surface fields
!          * GPOPER: Operations on ALL surface groups
!          * GPOPER_2: Operations on 2-D surface groups
!          * GPOPER_3: Operations on 3-D surface groups
!          * SURF_STORE: Store all surface fields
!          * SURF_RESTORE: Restore all surface fields
!          * ALLO_SURF: Allocate surface field arrays

!     Author.
!     -------
!     Mats Hamrud(ECMWF)

!     Modifications.
!     --------------
!        Original : 2006-07-01
!        Modifications:
!        K. Yessad (25 Oct 2006): rephase ALARO0 contribution.
!        K. Yessad (26 Oct 2006): add missing comments.
!        G. Balsamo (14 Mar 2007): add soil type allocatable (SOTY).
!        Jean Bidlot (June 2007):  named pointer for fields to wave model.
!        S. Serrar (17 Jul 2007) methane surface fields pointers
!        Y. Takaya (?? Nov 2008): add ocean mixed layer model fields
!        A. Alias  (07 Aug 2009) move field Nudging mask from group VCLIA to VARSF
!        JJMorcrette 20091201 Total and clear-sky direct SW radiation flux at surface 
!        H. Hersbach (04-Dec-2009): 10m-neutral wind and friction velocity,
!                                   introduce YDUPD, SETPERTOVAL in GPOPEN
!        S. Boussetta/G.Balsamo (May 2009): add high/low vegetation LAI pointer (LAIH/LAIL)
!        Y. Takaya/P. de Rosnay May 2010: SSS for SMOS
!        R. Forbes (01-Mar-2010): Added TCRW,TCSW diagnostics
!        JJMorcrette 20100212 PP of CBASE, 0DEGL and VISIH
!        T. Wilhelmsson (25 Mar 2010) add 6 hourly min/max fields
!        A. Alias  (07 Aug 2009) move field Nudging mask from group VCLIA to VARSF
!        Y. Bouteloup (20 Oct 2010) Surface forcing for 1D model : SFORC
!        Y. Bouteloup (04 Jan 2011) Surface flux for EDKF and 1D model surface forcing : SFLUX
!        G.Balsamo/S.Boussetta (Apr 2011): add land carbon dioxide fluxes
!        H. Hersbach (01 April 2011): auxiliary diagnostic radiation fields
!        P. Marguinaud (07 November 2012): Fix uninitialized component
!        P. Bechtold (9 Aug 2011): add CIN, Convective Indices
!        P.Marguinaud (11 Sep 2012): Initialize TYPE_SURF_MTL_2D%CNAME (avoid valgrind warning)
!        M. Ahlgrimm 31 Oct 2011: clear-sky downward radiation at surface
!        L. Jones (25-Oct-2011): Created FINALISE_SFLP2/3, removing need for 
!                                pre-counting number of SETUP_SFLP2/3 calls
!        M. Fisher   7-March-2012 Use DEALLOCATE_IF_ASSOCIATED
!        JJMorcrette 20130213 PP optical depths GEMS/MACC aerosols
!        G. Balsamo  14-Jun-2013 Introduce lake buffer group SL=LAKEB
!        R. Forbes 01-March-2014 Add precip rates/type,TCSLW,I10FG,PEV
!        M. Ahlgrimm Apr 2014: Add precip fraction for DDH output
!        T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!        JJMorcrette 20130730 15-variable aerosol model + oceanic DMS
!        R. Forbes 10-Jan-2015 Add freezing rain FZRA
!        S. Rémy, 23/1/2015, injection height for biomass burning aerosol emissions
!        A. Agusti-Panareda (31 Oct 2013): add GPP/REC flux adjustment coefficient pointer (CGPP,CREC)
!        P. Bechtold 10-Nov-2015: add CBASEA, CTOPC, ZTWETB0,1
!        A. Bozzo Jan 2016 Add surface direct beam solar radiation
!        S. Rémy, 09/3/2016 add SO2 dry dep velocity pointer SO2DD
!        L. Gerard (Mar 2016) add VK group for deep convection prognostic fields
!        R. El Khatib 09-Sep-2016 Extend VCLIX
!        S. Rémy, 16/9/2016 add nitrate and ammonium optical depths
!        S. Rémy, 21/4/2017 add altitude of volcanoes
!        S. Rémy, 25/4/2017 add calcite fraction over dust 1 and 2
!        S. Rémy, 13/11/2017 SOA optical depth
!        S. Rémy, 16/11/2017 add dust source function
!        E.Dutra/G.Arduini, Jan 2018 change SNOWG to 3D and add new field for snow liquid water (YW)
!        B. Ingleby, 14-01-2019 add Y2Q - specific humidity at 2m
!        R. Hogan 14/01/2019 MODIS Albedo 2x3-components
!        H Petithomme, Oct-2019 add fields to vclih for water conservation
!          could we please avoid too long lines by not indenting? is there any common rule for this?
!-------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK
USE YOMDIM   , ONLY : TDIM

IMPLICIT NONE
SAVE

!     -------------------------------------------------------------------------

INTEGER(KIND=JPIM), PARAMETER :: JPMAXSFLDS=622 ! Max number of fields in individual group  !KPP
INTEGER(KIND=JPIM), PARAMETER :: JPMAXSTRAJ=100 ! Dimension of NSTRAJGRIB
INTEGER(KIND=JPIM) :: NOFFTRAJ                  ! Offset in surf trajectory
INTEGER(KIND=JPIM) :: NOFFTRAJ_CST              ! Offset in "constant" surf trajectory
INTEGER(KIND=JPIM) :: NSTRAJGRIB(JPMAXSTRAJ)    ! Used in trajectory setup
INTEGER(KIND=JPIM), PRIVATE :: NPTRSURF         ! Used by routine GPOPER

! General type defintions

TYPE TYPE_SURF_MTL
  INTEGER(KIND=JPIM) :: MP           ! Basic field pointer
  INTEGER(KIND=JPIM) :: MP0          ! Field pointer timelevel  0 (prognostic fields)
  INTEGER(KIND=JPIM) :: MP9          ! Field pointer timelevel -1 (prognostic fields)
  INTEGER(KIND=JPIM) :: MP1          ! Field pointer timelevel +1 (prognostic fields)
  INTEGER(KIND=JPIM) :: ITRAJ        !  0 not in trajectory (default)
                                     !  1 in trajectory
                                     !  2 in "constant" trajectory
  INTEGER(KIND=JPIM) :: IBUGFINDER   ! Debug use only. Can be removed if confident.
  LOGICAL            :: LSET         ! True if structure has been set up
END TYPE TYPE_SURF_MTL
! 2D surface field structure
TYPE,EXTENDS(TYPE_SURF_MTL) :: TYPE_SURF_MTL_2D
  INTEGER(KIND=JPIM) :: IGRBCODE     ! GRIB parameter code (default: -999)
  CHARACTER(LEN=16)  :: CNAME = '                '
                                     ! ARPEGE field name   (default: all spaces)

  REAL(KIND=JPRB)    :: REFVALI      ! Default value       (default: 0.0)
  INTEGER(KIND=JPIM) :: NREQIN       ! -1 - initial value from default (default)
                                     ! +1 - initial value from reading file
                                     !  0 - no initial value
END TYPE TYPE_SURF_MTL_2D

! 3D surface field structure
TYPE ,EXTENDS(TYPE_SURF_MTL) :: TYPE_SURF_MTL_3D
  INTEGER(KIND=JPIM), ALLOCATABLE :: IGRBCODE(:)  ! GRIB parameter code (default: -999)
  CHARACTER(LEN=16) , ALLOCATABLE :: CNAME(:)     ! ARPEGE field name   (default: all spaces)
  REAL(KIND=JPRB)   , ALLOCATABLE :: REFVALI(:)   ! Default value       (default: 0.0)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NREQIN(:)    ! -1 - initial value from default (default)
                                                  ! +1 - initial value from reading file
                                                  !  0 - no initial value
END TYPE TYPE_SURF_MTL_3D

! Descriptor pertaining to group
TYPE TYPE_SURF_GEN
  INTEGER(KIND=JPIM) :: NUMFLDS         ! Number of field in group
  INTEGER(KIND=JPIM) :: NDIM            ! Field dimension
  INTEGER(KIND=JPIM) :: NLEVS           ! Number of levels (for multi level groups)
  INTEGER(KIND=JPIM) :: IPTR            ! Internal use
  INTEGER(KIND=JPIM) :: IPTR5           ! Internal use
  INTEGER(KIND=JPIM) :: NDIM5           ! Dimension of trajectory array
  INTEGER(KIND=JPIM) :: NOFFTRAJ        ! Internal use
  INTEGER(KIND=JPIM) :: NOFFTRAJ_CST    ! Internal use
  CHARACTER(LEN=16)  :: CGRPNAME        ! Name of group (for prints)
  LOGICAL            :: L3D             ! TRUE if multi-level field (3-D)
  LOGICAL            :: LMTL            ! TRUE if prognostic field (multi time level)
  LOGICAL            :: FINALISED       ! TRUE if group finalised & no more fields allowed
END TYPE TYPE_SURF_GEN

! Type descriptor for derived type for communicating with GPOPER (see below)
TYPE TYPE_SFL_COMM
  INTEGER(KIND=JPIM) :: IGRBCODE
  LOGICAL            :: L_OK 
  CHARACTER(LEN=16)  :: CNAME
  INTEGER(KIND=JPIM) :: IFLDNUM
  REAL(KIND=JPRB)    :: VALUE
  INTEGER(KIND=JPIM) :: IPTRSURF
  INTEGER(KIND=JPIM) :: ICODES(JPMAXSFLDS)
  INTEGER(KIND=JPIM) :: ICOUNT
END TYPE TYPE_SFL_COMM

! Group specific type definitions: pronostic groups (SB, SG, RR, CL, OM, EP, X2, CI)

! * Group SB=SOILB: soil prognostic quantities for the different reservoirs
!    (four reservoirs at ECMWF, deep reservoir at METEO-FRANCE):
TYPE TYPE_SFL_SOILB
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YT => NULL()   ! temperature
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YQ => NULL()   ! liquid water content
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YTL=> NULL()   ! ice water content (for MF)
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YSB(:) => NULL()
END TYPE TYPE_SFL_SOILB

! * Group SG=SNOWG: surface snow prognostic quantities:
TYPE TYPE_SFL_SNOWG
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YF =>NULL()   ! content of surface snow
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YA =>NULL()   ! snow albedo
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YR =>NULL()   ! snow density
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YT =>NULL()   ! total albedo (diagnostic for MF for LVGSN)
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YW =>NULL()   ! Liquid water content 
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YSG(:) => NULL()
END TYPE TYPE_SFL_SNOWG

! * Group SL=LAKEB: Lake (FLAKE Model) prognostic quantities:
TYPE TYPE_SFL_LAKEB
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YLICT =>NULL() ! lake ice temperature
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YLMLT =>NULL() ! lake mixed-layer temperature
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YLTLT =>NULL() ! lake total layer temperature
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YLBLT =>NULL() ! lake bottom layer temperature
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YLSHF =>NULL() ! lake shape factor
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YLICD =>NULL() ! lake ice depth
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YLMLD =>NULL() ! lake mixed-layer depth
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YSL(:) => NULL()
END TYPE TYPE_SFL_LAKEB

! * Group RR=RESVR: surface prognostic quantities (ECMWF) or
!   surface + superficial reservoir prognostic quantities (MF):
!   Remark:
!    at ECMWF there are 4 soil reservoirs and there is a
!    clear distinction between the soil reservoirs (group SOILB)
!    and the surface (group RESVR);
!    at METEO-FRANCE there is a deep reservoir (group SOILB) and a
!    superficial reservoir (group RESVR):
!    - there is a skin surface temperature (Ts) which is the temperature at the
!      interface surface/superficial reservoir (and not two separate quantities
!      for superficial reservoir and surface)
!    - there is a skin surface water content (denoted by Wl) and a superficial
!      reservoir water content (denoted by Ws).
!    - there is a superficial reservoir ice content but no surface ice content.
!    (remark k.y.: it would have been more logical to use group name
!    RESVR for internal reservoirs and group name SOILB for surface!).
TYPE TYPE_SFL_RESVR
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YT  =>NULL()  ! skin temperature (Ts)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YW  =>NULL()  ! skin water content (Wskin) at ECMWF
                                                   ! superficial reservoir water content (Ws) at MF
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YFC =>NULL()  ! skin water content (Wl) at MF
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIC =>NULL()  ! superficial reservoir ice
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YFP1=>NULL()  ! interpolated Ts for 2nd part of 927-FULLPOS
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YRR(:) => NULL()
END TYPE TYPE_SFL_RESVR

! * Group CL=CLS: surface boundary layer prognostic quantities:
TYPE TYPE_SFL_CLS
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCLS =>NULL()  ! 2m temperature
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YHUCLS=>NULL()  ! 2m humidity
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YUCLS =>NULL()  ! 10m U-wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVCLS =>NULL()  ! 10m V-wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YNUCLS=>NULL()  ! 10m neutral U-wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YNVCLS=>NULL()  ! 10m neutral V-wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSICLS=>NULL()  ! sea.ice.cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCL(:) => NULL()
END TYPE TYPE_SFL_CLS

! * Group OM=OML: prognostic quantities for ocean mixed layer model (KPP/TKE):
TYPE TYPE_SFL_OML
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YTO=>NULL()     ! temperature
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YSO=>NULL()     ! salinity
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YUO=>NULL()     ! U velocity
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YVO=>NULL()     ! V velocity
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YOM(:) => NULL()
END TYPE TYPE_SFL_OML 

! * Group EP=EXTRP: extra 3-d prognostic fields:
TYPE TYPE_SFL_EXTRP
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YEP(:) => NULL()
END TYPE TYPE_SFL_EXTRP

! * Group X2=XTRP2: extra 2-d prognostic fields:
!   (is used for precipitation fields in CANARI)
TYPE TYPE_SFL_XTRP2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YX2(:) => NULL()
END TYPE TYPE_SFL_XTRP2

! * Group CI=CANRI: 2-d prognostic fields for CANARI:
TYPE TYPE_SFL_CANRI
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCI(:) => NULL()
END TYPE TYPE_SFL_CANRI

! Group specific type definitions: diagnostic groups 

! * Group VF=VARSF: climatological/geographical diagnostic fields:
TYPE TYPE_SFL_VARSF
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZ0F  =>NULL()  ! gravity * surface roughness length
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALBF =>NULL()  ! surface shortwave albedo
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YEMISF=>NULL()  ! surface longwave emissivity
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YGETRL=>NULL()  ! standard deviation of orography
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLSM  =>NULL()  ! land-sea mask
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVEG  =>NULL()  ! vegetation cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVRLAN=>NULL()  ! anisotropy of the sub-grid scale orography
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVRLDI=>NULL()  ! angle of the direction of orography with the x axis
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSIG  =>NULL()  ! characteristic orographic slope
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALBSF=>NULL()  ! soil shortwave albedo
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLAN  =>NULL()  ! fraction of land
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSST  =>NULL()  ! (open) sea surface temperature
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSSS  =>NULL()  ! sea surface salinity
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLZ0H =>NULL()  ! logarithm of roughness length for heat
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCVL  =>NULL()  ! low vegetation cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCVH  =>NULL()  ! high vegetation cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTVL  =>NULL()  ! low vegetation type
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTVH  =>NULL()  ! high vegetation type
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLAIL =>NULL()  ! low vegetation LAI
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLAIH =>NULL()  ! high vegetation LAI
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSOTY =>NULL()  ! soil type
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCLK  =>NULL()  ! lake cover 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YDL   =>NULL()  ! lake depth
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCI   =>NULL()  ! sea ice fraction
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YUCUR =>NULL()  ! U-component of the ocean current
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVCUR =>NULL()  ! V-component of the ocean current
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZ0RLF=>NULL()  ! gravity * vegetation roughness length
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCO2O =>NULL()  ! oceanic CO2 flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCO2B =>NULL()  ! biosphere CO2 flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCO2A =>NULL()  ! anthropogenic CO2 flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCO2F =>NULL()  ! CO2 fire emissions
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCGPP =>NULL()  ! GPP bias correction factor
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCREC =>NULL()  ! REC bias correction factor
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCH4AG=>NULL()  ! CH4 surface fluxes - aggregated field
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCH4F =>NULL()  ! CH4 fire emissions
  ! 4-component MODIS land albedo climatology
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSDFOR=>NULL()  ! SD filtered orography
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALUVP=>NULL()  ! MODIS-derived parallel albedo for shortwave radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALUVD=>NULL()  ! MODIS-derived diffuse albedo for shortwave radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALNIP=>NULL()  ! MODIS-derived parallel albedo for longwave radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALNID=>NULL()  ! MODIS-derived diffuse albedo for longwave radiation
  ! 6-component MODIS land albedo climatology
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALUVI=>NULL()  ! MODIS-derived parallel albedo for UV-visible radiation (iso.)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALNII=>NULL()  ! MODIS-derived parallel albedo for near-infrared radiation (iso.)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALUVV=>NULL()  ! MODIS-derived parallel albedo for UV-visible radiation (volu.)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALNIV=>NULL()  ! MODIS-derived parallel albedo for near-infrared radiation (volu.) 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALUVG=>NULL()  ! MODIS-derived parallel albedo for UV-visible radiation (geom.)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALNIG=>NULL()  ! MODIS-derived parallel albedo for near-infrared radiation (geom.)
  ! Indices to start/end albedo climatology coefficients currently in use
  INTEGER(KIND=JPIM)              :: IALSTART, IALEND
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YFP1  =>NULL()  ! surface orography in the 2nd part of FULLPOS-927
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YBCBF =>NULL()  ! black carbon biogenic
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YBCFF =>NULL()  ! black carbon fossil fuel
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YBCGF =>NULL()  ! black carbon GFED
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YOMBF =>NULL()  ! organic matter biogenic
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YOMFF =>NULL()  ! organic matter fossil fuel
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YOMGF =>NULL()  ! organic matter GFED
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YINJF =>NULL()  ! height of maximum injection for biomass burning emissions
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSO2L =>NULL()  ! sulphate low-level 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSO2H =>NULL()  ! sulphate higher-level 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSO2DD=>NULL()  ! sulphate dry dep velocity
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSOGF =>NULL()  ! sulphate GFED
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSOA  =>NULL()  ! secondary organic
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVOLC =>NULL()  ! volcanic continuous
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVOLE =>NULL()  ! volcanic explosive
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YDMSO =>NULL()  ! oceanic DMS
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSOACO=>NULL()  ! SOA from CO
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YURBF =>NULL()  ! Urban fraction
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVOLCALTI=>NULL()  ! Altitude of volcanoes
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YFCA1  =>NULL() ! Fraction of calcite over dust 1st bin
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YFCA2  =>NULL() ! Fraction of calcite over dust 2nd bin
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAERDEP=>NULL() ! dust emission potential 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAERLTS=>NULL() ! dust lifting threshold speed 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAERSCC=>NULL() ! dust soil clay content
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YDSF   =>NULL() ! dust source function
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCHEMFLX(:) =>NULL()! chemistry emissions input
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCHEMFLXO(:)=>NULL() ! total chemistry flux (emissions + deposition)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCHEMDV(:)=>NULL() ! chemistry deposition velocity
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YNUDM=>NULL()   ! nudging mask
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVF(:) => NULL()
END TYPE TYPE_SFL_VARSF

! * Group VP=VCLIP: deep soil diagnostic fields:
TYPE TYPE_SFL_VCLIP
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTPC=>NULL()    ! climatological deep layer temperature
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YWPC=>NULL()    ! climatological deep layer moisture
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVP(:) => NULL()
END TYPE TYPE_SFL_VCLIP

! * Group VV=VCLIV: vegetation diagnostic fields:
TYPE TYPE_SFL_VCLIV
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YARG  =>NULL()  ! silt percentage within soil
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSAB  =>NULL()  ! percentage of sand within the soil
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YD2   =>NULL()  ! soil depth
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIVEG =>NULL()  ! type of vegetation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YRSMIN=>NULL()  ! stomatal minimum resistance
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLAI  =>NULL()  ! leaf area index
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YHV   =>NULL()  ! resistance to evapotranspiration
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZ0H  =>NULL()  ! gravity * roughness length for heat
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALS  =>NULL()  ! albedo of bare ground
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALV  =>NULL()  ! albedo of vegetation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVV(:)=> NULL()
END TYPE TYPE_SFL_VCLIV

! * Group VN=VCLIN: cloudiness diagnostic predictors:
TYPE TYPE_SFL_VCLIN
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTOP   =>NULL() ! index of convective cloud top
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YBAS   =>NULL() ! index of convective cloud base
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YACPR  =>NULL() ! averaged convective precipitaion rate
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YACCPR =>NULL() ! accumulated total precipitaion for assimilation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YACCPR5=>NULL() ! accumulated total precipitaion for assimilation (trajectory)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVN(:) => NULL()
END TYPE TYPE_SFL_VCLIN

! * Group VH=VCLIH: convective cloud diagnostic fields:
TYPE TYPE_SFL_VCLIH
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCCH  => NULL() ! total convective cloudiness
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSCCH  => NULL() ! convective cloud summit
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YBCCH  => NULL() ! convective cloud base
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YPBLH  => NULL() ! PBL height
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSPSH  => NULL() ! variable for prognostic convection scheme (ALARO)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YQSH   => NULL() ! surface moisture historic variable (used by TOUCANS)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVH(:) => NULL()
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YPCL=>NULL()
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YPSL=>NULL()
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YPCN=>NULL()
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YPSN=>NULL()
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YEVA=>NULL()
END TYPE TYPE_SFL_VCLIH

! * Group VK=VCLIK: Convective cloud pseudo-historic fields:
TYPE TYPE_SFL_VCLIK
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YUDGRO => NULL() ! ud top position (accsu)
  TYPE(TYPE_SURF_MTL_2D),POINTER :: YVK(:) => NULL()
END TYPE TYPE_SFL_VCLIK

! * Group VA=VCLIA: aerosol diagnostic fields:
TYPE TYPE_SFL_VCLIA
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSEA=>NULL()    ! aerosol: sea
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLAN=>NULL()    ! aerosol: land
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSOO=>NULL()    ! aerosol: soot
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YDES=>NULL()    ! aerosol: desert
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSUL=>NULL()    ! aerosol: sulfate
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVOL=>NULL()    ! aerosol: volcano
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVA(:) => NULL()
END TYPE TYPE_SFL_VCLIA

! * Group VG=VCLIG: ice-coupler diagnostic fields:
!   ky: currently not used, missing setup in su_surf_flds.F90
TYPE TYPE_SFL_VCLIG
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YICFR=>NULL()   ! sea-ice fraction
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSOUP=>NULL()   ! upward solar flux over sea-ice
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIRUP=>NULL()   ! upward IR flux over sea-ice
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCHSS=>NULL()   ! sensible heat over sea-ice
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YEVAP=>NULL()   ! evaporation over sea-ice
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTAUX=>NULL()   ! U-component of stress over sea-ice
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTAUY=>NULL()   ! V-component of stress over sea-ice
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVG(:) => NULL()
END TYPE TYPE_SFL_VCLIG

! * Group VC=VO3ABC: A,B and C (Climatological ozone profiles) diagnostic fields:
TYPE TYPE_SFL_VO3ABC
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YA=>NULL()      ! A climatological ozone profile
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YB=>NULL()      ! B climatological ozone profile
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YC=>NULL()      ! C climatological ozone profile
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVC(:) => NULL()
END TYPE TYPE_SFL_VO3ABC

! * Group V2=VDIAGO2: 2-D climatological/diagnostic fields for an ocean mixed layer model (KPP):
TYPE TYPE_SFL_VDIAGO2
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YOCDEP=>NULL()  ! bottom layer depth
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YUSTRC=>NULL()  ! taux clim.
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YVSTRC=>NULL()  ! tauy clim.
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YV2(:) => NULL()
END TYPE TYPE_SFL_VDIAGO2 

! * Group V3=VDIAGO3: 3-D climatological/diagnostic fields for an ocean mixed layer model (KPP):
TYPE TYPE_SFL_VDIAGO3 
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YDIFM  =>NULL() ! viscosity
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YDIFT  =>NULL() ! diff. coef. of temp
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YDIFS  =>NULL() ! diff. coef. of salinity
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YADVT  =>NULL() ! correction term for temp.
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YADVS  =>NULL() ! correction term for sal.
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YTRI0  =>NULL() ! coef. for solving matrix.
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YTRI1  =>NULL() ! coef. for solving matrix.
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YSWDK  =>NULL() ! radiation term
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YZO    =>NULL() ! depth of layer
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YHO    =>NULL() ! depth of interface layer
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YDO    =>NULL() ! layer thickness
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YHO_INV=>NULL() ! 1 / YHO
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YUOC   =>NULL() ! U velocity clim.
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YVOC   =>NULL() ! V velocity clim.
  TYPE (TYPE_SURF_MTL_3D), POINTER :: YV3(:) => NULL()
END TYPE TYPE_SFL_VDIAGO3 

! * Group VD=VDIAG: (ECMWF) diagnostic fields:
TYPE TYPE_SFL_VDIAG
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLSP      =>NULL()  ! Large scale precipitation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCP       =>NULL()  ! Convective precipitation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSF       =>NULL()  ! Snowfall
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YFZRA     =>NULL()  ! Freezing rain
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YBLD      =>NULL()  ! Boundary layer dissipation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSSHF     =>NULL()  ! Surface sensible heat flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSLHF     =>NULL()  ! Surface latent heat flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YNEE      =>NULL()  ! Surface net ecosystem exchange of CO2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YGPP      =>NULL()  ! Surface gross primary production of CO2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YREC      =>NULL()  ! Surface ecosystem respiration of CO2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMSL      =>NULL()  ! Mean sea level pressure
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSP       =>NULL()  ! Surface pressure
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCC      =>NULL()  ! Total cloud cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y10U      =>NULL()  ! U-wind at 10 m
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y10V      =>NULL()  ! V-wind at 10 m
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y2T       =>NULL()  ! Temperature at 2 m
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y2D       =>NULL()  ! Dewpoint temperature at 2 m
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y2Q       =>NULL()  ! Specific humidity at 2 m
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSSR      =>NULL()  ! Surface solar radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSTR      =>NULL()  ! Surface thermal radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTSR      =>NULL()  ! Top solar radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTTR      =>NULL()  ! Top thermal radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YEWSS     =>NULL()  ! Instantaneous surface U-wind stress
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YNSSS     =>NULL()  ! Instantaneous surface V-wind stress
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YE        =>NULL()  ! Water evaporation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YPEV      =>NULL()  ! Potential evaporation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCCC      =>NULL()  ! Convective cloud cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLCC      =>NULL()  ! Low cloud cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMCC      =>NULL()  ! Medium cloud cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YHCC      =>NULL()  ! High cloud cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLGWS     =>NULL()  ! Zonal gravity wave stress
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMGWS     =>NULL()  ! Meridian gravity wave stress
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YGWD      =>NULL()  ! Gravity wave dissipation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMX2T     =>NULL()  ! Maximum temperature at 2 m
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMN2T     =>NULL()  ! Minimum temperature at 2 m
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMX2T6(:) =>NULL()  ! Bins for maximum temperature at 2 m since last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMN2T6(:) =>NULL()  ! Bins for minimum temperature at 2 m since last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YRO       =>NULL() ! Runoff (total)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSRO      =>NULL() ! Runoff surface
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSSRO     =>NULL() ! Runoff sub-surface
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YALB      =>NULL() ! (surface shortwave) albedo
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIEWSS    =>NULL() ! Instantaneous surface zonal component of stress
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YINSSS    =>NULL() ! Instantaneous surface meridian component of stress
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YISSHF    =>NULL() ! Instantaneous surface heat flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIE       =>NULL() ! Instantaneous surface moisture flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YINEE     =>NULL() ! Instantaneous net ecosystem exchange of CO2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIGPP     =>NULL() ! Instantaneous gross primary production of CO2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIREC     =>NULL() ! Instantaneous ecosystem respiration of CO2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCSF      =>NULL() ! Convective snow fall
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLSSF     =>NULL() ! Large scale snowfall
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMXTPR    =>NULL() ! Max precip rate since last post-processing
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMNTPR    =>NULL() ! Min precip rate since last post-processing
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMXTPR6(:)=>NULL() ! Max precip rate in last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMNTPR6(:)=>NULL() ! Min precip rate in last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTPR      =>NULL() ! Total precipitation rate 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLSRR     =>NULL() ! Large scale rain rate 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCRR      =>NULL() ! Convective rain rate 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLSSFR    =>NULL() ! Large scale snowfall rate 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCSFR     =>NULL() ! Convective snowfall rate 
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YPTYPE    =>NULL() ! Precipitation type
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YILSPF    =>NULL() ! Large-scale precipitation fraction (inst.)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZ0F      =>NULL() ! Gravity * surface roughness length
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLZ0H     =>NULL() ! Logarithm of z0 times heat flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVIWVE    =>NULL() ! Vertical integral of eastward water vapour flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVIWVN    =>NULL() ! Vertical integral of northward water vapour flux
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCW      =>NULL() ! Total water content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCWV     =>NULL() ! Total water vapor content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCLW     =>NULL() ! Total liquid water content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCIW     =>NULL() ! Total ice water content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCRW     =>NULL() ! Total rain water content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCSW     =>NULL() ! Total snow water content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCSLW    =>NULL() ! Total supercooled liquid water content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSSRD     =>NULL() ! Downward surface solar radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSTRD     =>NULL() ! Downward surface thermic radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSSRDC    =>NULL() ! Clear-sky downward surface solar radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSTRDC    =>NULL() ! Claer-sky downward surface thermal radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YBLH      =>NULL() ! Height of boundary layer
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSUND     =>NULL() ! Sunshine duration
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSPAR     =>NULL() ! Surface downward PARadiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSUVB     =>NULL() ! Surface downward UV-B radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSFDIR    =>NULL() ! Surface total sky direct downward SW radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSCDIR    =>NULL() ! Surface clear-sky direct downward SW radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSDSRP    =>NULL() ! Surface total-sky direct beam downward SW radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCAPE     =>NULL() ! Conv.avail.potential energy (CAPE)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCAPES    =>NULL() ! CAPE-Shear
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMXCAP6(:)=>NULL() ! Bins for maximum CAPE in last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YMXCAPS6(:)=>NULL()! Bins for maximum CAPE-Shear in last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTSRC     =>NULL() ! Top solar radiation clear sky
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTTRC     =>NULL() ! Top thermal radiation clear sky
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSSRC     =>NULL() ! Surface solar radiation clear sky
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSTRC     =>NULL() ! Surface thermal radiation clear sky
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YES       =>NULL() ! Evaporation of snow
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSMLT     =>NULL() ! Snow melt
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y10FG     =>NULL() ! Wind gust at 10 m (max since previous pp)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y10FG6(:) =>NULL() ! Bins for wind gust at 10 m (max since last 6 hours)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y10FGCV   =>NULL() ! convective wind gust at 10m for current time level (m/s)
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YI10FG    =>NULL() ! Wind gust at 10 m ("instantaneous")
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLSPF     =>NULL() ! Large scale precipitation fraction
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCO3     =>NULL() ! Total ozone content in a vertical column
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVIMD     =>NULL() ! Vertically integrated mass divergence
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSPARC    =>NULL() ! Surface clear-sky parallel radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSTINC    =>NULL() ! Top of atmosphere incident solar radiation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCBASE    =>NULL() ! Cloud base level
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y0DEGL    =>NULL() ! Zero deg. level
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVISIH    =>NULL() ! Horizontal visibility
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCIN      =>NULL() ! CIN
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YKINDEX   =>NULL() ! Convective K-Index
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTTINDEX  =>NULL() ! Convective TT-Index
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCBASEA   =>NULL() ! Cloud base aviation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCTOPC    =>NULL() ! Cloud top convective
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZTWETB0  =>NULL() ! Height of 0 deg wet bulb temperature
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZTWETB1  =>NULL() ! Height of 1 deg wet bulb temperature
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCGHG(:) =>NULL() ! Total column greenhouse gases
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTCCHEM(:)=>NULL() ! Total column chemistry
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAERODIAG(:,:)=>NULL()! Per-aerosol-type diagnostics
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAERO_WVL_DIAG(:,:)=>NULL()! Per-wavelength aerosol optical diagnostics
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y100U     =>NULL() ! 100m zonal wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y100V     =>NULL() ! 100m meridional wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y200U     =>NULL() ! 200m zonal wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y200V     =>NULL() ! 200m meridional wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZUST     =>NULL() ! Friction velocity
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y10NU     =>NULL() ! 10m zonal neutral wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: Y10NV     =>NULL() ! 10m meridional neutral wind
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YDNDZN    =>NULL() ! Minimum vertical refractivity gradient
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YDNDZA    =>NULL() ! Mean vertical refractivity gradient
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YDCTB     =>NULL() ! Duct base height
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTPLB     =>NULL() ! Trapping layer base height
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTPLT     =>NULL() ! Trapping layer top height
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODSS     =>NULL() ! optical depth sea salt aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODDU     =>NULL() ! optical depth dust aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODOM     =>NULL() ! optical depth organic m. aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODBC     =>NULL() ! optical depth black C aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODSU     =>NULL() ! optical depth sulphate aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODNI     =>NULL() ! optical depth nitrate aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODAM     =>NULL() ! optical depth ammonium aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODSOA    =>NULL() ! optical depth secondary organic aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODVFA    =>NULL() ! optical depth volcanic flying ash    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODVSU    =>NULL() ! optical depth volcanic sulphate aerosols    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YODTOACC  =>NULL() ! optical depth total aerosol accumulated
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAEPM1    =>NULL() ! particulate matter le 1 um    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAEPM25   =>NULL() ! particulate matter le 2.5um    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YAEPM10   =>NULL() ! particulate matter le 10 um    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YUVBED    =>NULL() ! UV biologically effective dose    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YUVBEDCS  =>NULL() ! UV biologically effective dose clear sky    
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLITOTI   =>NULL() ! instantaneous total lightning flash density
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLICGI    =>NULL() ! instantaneous cloud-to-ground lightning flash density
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLITOTA6(:)=>NULL()! Bins for averaged total lightning over last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLICGA6(:)=>NULL() ! Bins for averaged cloud-to-ground lightning over last 6 hours
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVD(:) => NULL()
END TYPE TYPE_SFL_VDIAG

! * Group SM=SATSIM: (ECMWF) simulated satellite images:
TYPE TYPE_SFL_SATSIM
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YCLBT=>NULL()      ! Cloudy brightness temperature
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YCSBT=>NULL()      ! Clear-sky brightness temperature
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YSM(:) => NULL()
END TYPE TYPE_SFL_SATSIM

! * Group WS=WAVES: surface prognostic quantities over sea (used by IFS):
TYPE TYPE_SFL_WAVES
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YWS(:) => NULL()
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCHAR =>NULL()     ! Charnock parameter as modified by the wave model.
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YUSTOKES=>NULL()   ! U-component of the surface Stokes drift.
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVSTOKES=>NULL()   ! V-component of the surface Stokes drift.
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YPHIOC=>NULL()     ! Energy flux to ocean.
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YPHIAW=>NULL()     ! Energy flux to ocean waves.
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTAUOC=>NULL()     ! Momentum flux to ocean.
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YEMEAN=>NULL()     ! Wave variance.
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YFMEAN=>NULL()     ! Wave mean frequency.
END TYPE TYPE_SFL_WAVES

! * Group WW=WAM: surface prognostic quantities over sea (used by WAM):
TYPE TYPE_SFL_WAM
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YWW(:) => NULL()
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YU10N =>NULL()     ! 10m neutral wind U-component passed to the wave model (WAM).
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YV10N =>NULL()     ! 10m neutral wind V-component passed to the wave model (WAM).
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YRHO  =>NULL()     ! surface density passed to the wave model (WAM).
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YZIL  =>NULL()     ! ZI/L passed to the wave model (used for gustiness in WAM).
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCIF  =>NULL()     ! Sea ice fraction passed to the wave model (WAM).
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YCLK  =>NULL()     ! Lake cover passed to the wave model (WAM).
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YUCURW=>NULL()     ! Ocean current    U-component passed to the wave model (WAM).
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVCURW=>NULL()     ! Ocean current    V-component passed to the wave model (WAM).
END TYPE TYPE_SFL_WAM

! * Group VX=VCLIX: auxilary climatological diagnostic fields:
TYPE TYPE_SFL_VCLIX
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YORO  =>NULL()  ! climatological surface geopotential
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTSC  =>NULL()  ! climatological surface temperature
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YPWS  =>NULL()  ! climatological surface max. prop. moisture
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YPWP  =>NULL()  ! climatological deep soil max. prop. moisture
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSNO  =>NULL()  ! climatological snow cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YTPC  =>NULL()  ! climatological deep soil temperature
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSAB  =>NULL()  ! climatologic percentage of sand within the soil
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YXD2  =>NULL()  ! climatologic soil depth
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLSM  =>NULL()  ! climatologic land sea mask
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YIVEG =>NULL()  ! climatologic type of vegetation
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YARG  =>NULL()  ! silt percentage within soil
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YRSMIN=>NULL()  ! climatologic stomatal minimum resistance
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YLAI  =>NULL()  ! leaf area index
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVEG  =>NULL()  ! vegetation cover
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YVX(:) => NULL()
END TYPE TYPE_SFL_VCLIX

! * Group XA=VEXTRA: extra 3-d diagnostic fields:
TYPE TYPE_SFL_VEXTRA
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YXA(:) => NULL()
END TYPE TYPE_SFL_VEXTRA

! * Group XA=VEXTRDI: targeted 3-d diagnostic fields:
TYPE TYPE_SFL_VEXTRDI
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YXA(:) => NULL()
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YXEDR=>NULL() ! Eddy diffusivity rate
END TYPE TYPE_SFL_VEXTRDI

! * Group XA=VPRECIP:  3-d diagnostic fields:
TYPE TYPE_SFL_VPRECIP
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YPRECIP ! Diagnostic of precipitations type
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YXP1(:) => NULL()
END TYPE TYPE_SFL_VPRECIP

TYPE TYPE_SFL_VPRECIP2
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YPRECIP2 ! Diagnostic of precipitations type
  TYPE(TYPE_SURF_MTL_3D), POINTER :: YXP2(:) => NULL()
END TYPE TYPE_SFL_VPRECIP2

! * Group X2=VEXTR2: extra 2-d diagnostic fields:
TYPE TYPE_SFL_VEXTR2
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YX2(:) => NULL()
END TYPE TYPE_SFL_VEXTR2

! * Group SFL:SFLUX Surface flux for EDKF 
TYPE TYPE_SFL_SFLUX
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSFL(:) => NULL()
END TYPE TYPE_SFL_SFLUX

! * Group SFO:SFORC Surface forcing for 1D model (MUSC)
TYPE TYPE_SFL_SFORC
  TYPE(TYPE_SURF_MTL_2D), POINTER :: YSFO(:) => NULL()
END TYPE TYPE_SFL_SFORC

! * Group OC=OCE: ocean model fields :
TYPE TYPE_SFL_OCE
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YSSTM =>NULL()  ! sst masked
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YCIM  =>NULL()  ! ci masked
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YUCURM=>NULL()  ! ucur masked
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YVCURM=>NULL()  ! vcur masked
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YICTH =>NULL()  ! ice thickness
  TYPE (TYPE_SURF_MTL_2D), POINTER :: Y20D  =>NULL()  ! 20 degree isotherm 
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YSSH  =>NULL()  ! sea level
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YMLD  =>NULL()  ! mixed layer depth
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YSSS  =>NULL()  ! sea surface salinity
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YTEM3 =>NULL()  ! pottemp avg 300m
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YSAL3 =>NULL()  ! salt avg 300m
  TYPE (TYPE_SURF_MTL_2D), POINTER :: YOC(:) => NULL()
END TYPE TYPE_SFL_OCE

! End of type definitions

TYPE :: TSURF
  
  INTEGER(KIND=JPIM) :: NSURF=0                   ! Number of surf var.
  INTEGER(KIND=JPIM) :: NSURFL=0                  ! Number of surf flds (fields*levels)
  INTEGER(KIND=JPIM) :: NDIMSURF=0                ! Total of surf var (includes timelevels etc)
  INTEGER(KIND=JPIM) :: NDIMSURFL=0               ! Total dimension of all surface variables
  INTEGER(KIND=JPIM) :: NPROGSURF=0               ! Number of prognostic surf var.
  INTEGER(KIND=JPIM) :: NPROGSURFL=0              ! Number of prognostic surf flds (fields*levels)
  
  REAL(KIND=JPRB), ALLOCATABLE :: STORE_ARRAY(:,:,:) ! Backup array for surf (see routine SURF_STORE )
  
  ! Information on status of tl perturbations
  TYPE(TYPE_SFL_COMM) :: YDUPD
  
  ! Data structures
  
  ! Prognostic (multi time level) fields (SB, SG, SL, RR, CL, OM, EP, X2, CI)
  
  ! Soilb
  REAL(KIND=JPRB), ALLOCATABLE :: SP_SB (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_SBD
  TYPE(TYPE_SFL_SOILB)         :: YSP_SB
  
  ! Snowg
  REAL(KIND=JPRB), ALLOCATABLE :: SP_SG (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_SGD
  TYPE(TYPE_SFL_SNOWG)         :: YSP_SG
  
  ! Lakeb
  REAL(KIND=JPRB),ALLOCATABLE :: SP_SL (:,:,:) 
  TYPE(TYPE_SURF_GEN)     :: YSP_SLD
  TYPE(TYPE_SFL_LAKEB)    :: YSP_SL
  
  ! Resvr
  REAL(KIND=JPRB), ALLOCATABLE :: SP_RR (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_RRD
  TYPE(TYPE_SFL_RESVR)         :: YSP_RR
  
  ! Cls
  REAL(KIND=JPRB), ALLOCATABLE :: SP_CL (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_CLD
  TYPE(TYPE_SFL_CLS)           :: YSP_CL
  
  ! Oml (used by ocean mixed layer model (KPP))
  REAL(KIND=JPRB), ALLOCATABLE :: SP_OM (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_OMD 
  TYPE(TYPE_SFL_OML)           :: YSP_OM
  
  ! Extrp
  REAL(KIND=JPRB), ALLOCATABLE :: SP_EP (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_EPD
  TYPE(TYPE_SFL_EXTRP)         :: YSP_EP 
  
  ! Xtrp2
  REAL(KIND=JPRB), ALLOCATABLE :: SP_X2 (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_X2D
  TYPE(TYPE_SFL_XTRP2)         :: YSP_X2
  
  ! Canri
  REAL(KIND=JPRB), ALLOCATABLE :: SP_CI (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSP_CID
  TYPE(TYPE_SFL_CANRI)         :: YSP_CI
  
  ! Diagnostic 3D ( 2D spatial and 1D time)
  
  !Vprecip
  REAL(KIND=JPRB), ALLOCATABLE :: SD_XP(:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_XPD
  TYPE(TYPE_SFL_VPRECIP)       :: YSD_XP
  
  REAL(KIND=JPRB), ALLOCATABLE :: SD_XP2(:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_XP2D
  TYPE(TYPE_SFL_VPRECIP2)      :: YSD_XP2
  
  ! Diagnostic (one time level) fields
  
  ! Varsf
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VF (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VFD
  TYPE(TYPE_SFL_VARSF)         :: YSD_VF
  
  ! Vclip
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VP (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VPD
  TYPE(TYPE_SFL_VCLIP)         :: YSD_VP
  
  ! Vcliv
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VV (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VVD
  TYPE(TYPE_SFL_VCLIV)         :: YSD_VV
  
  ! Vclin
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VN (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VND
  TYPE(TYPE_SFL_VCLIN)         :: YSD_VN
  
  ! Vclih
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VH (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VHD
  TYPE(TYPE_SFL_VCLIH)         :: YSD_VH
  
  ! Vclik
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VK (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VKD
  TYPE(TYPE_SFL_VCLIK)         :: YSD_VK
  
  ! Vclia
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VA (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VAD
  TYPE(TYPE_SFL_VCLIA)         :: YSD_VA
  
  ! Vclig
  ! currently nothing declared
  
  ! Vo3abc
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VC (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VCD
  TYPE(TYPE_SFL_VO3ABC)        :: YSD_VC
  
  ! Vdiago2 (used by ocean mixed layer model (KPP)) 
  REAL(KIND=JPRB), ALLOCATABLE :: SD_V2 (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_V2D 
  TYPE(TYPE_SFL_VDIAGO2)       :: YSD_V2 
  
  ! Vdiago3 (used by ocean mixed layer model (KPP))
  REAL(KIND=JPRB), ALLOCATABLE :: SD_V3 (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_V3D 
  TYPE(TYPE_SFL_VDIAGO3)       :: YSD_V3 
  
  ! Vdiag
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VD (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VDD
  TYPE(TYPE_SFL_VDIAG)         :: YSD_VD
  
  ! Satsim
  REAL(KIND=JPRB), ALLOCATABLE :: SD_SM (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_SMD
  TYPE(TYPE_SFL_SATSIM)        :: YSD_SM
  
  ! Waves (used by IFS)
  REAL(KIND=JPRB), ALLOCATABLE :: SD_WS (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_WSD
  TYPE(TYPE_SFL_WAVES)         :: YSD_WS
  
  ! Waves (used by WAM)
  REAL(KIND=JPRB), ALLOCATABLE :: SD_WW (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_WWD
  TYPE(TYPE_SFL_WAM)           :: YSD_WW
  
  ! Vclix
  REAL(KIND=JPRB), ALLOCATABLE :: SD_VX (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_VXD
  TYPE(TYPE_SFL_VCLIX)         :: YSD_VX
  
  ! Vextra
  REAL(KIND=JPRB), ALLOCATABLE :: SD_XA (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_XAD
  TYPE(TYPE_SFL_VEXTRA)        :: YSD_XA
  
  ! Phys diagnostics
  REAL(KIND=JPRB),ALLOCATABLE :: SD_DI (:,:,:,:)
  TYPE(TYPE_SURF_GEN)     :: YSD_DID
  TYPE(TYPE_SFL_VEXTRDI)   :: YSD_DI
  
  ! Vextra-radiation
  REAL(KIND=JPRB), ALLOCATABLE :: SD_XR (:,:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_XRD
  TYPE(TYPE_SFL_VEXTRA)        :: YSD_XR
  
  ! Vextr2
  REAL(KIND=JPRB), ALLOCATABLE :: SD_X2 (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_X2D
  TYPE(TYPE_SFL_VEXTR2)        :: YSD_X2
  
  ! SFLUX
  REAL(KIND=JPRB), ALLOCATABLE :: SD_SFL (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_SFLD
  TYPE(TYPE_SFL_SFLUX)         :: YSD_SFL
  
  ! SFORC
  REAL(KIND=JPRB), ALLOCATABLE :: SD_SFO (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_SFOD
  TYPE(TYPE_SFL_SFORC)         :: YSD_SFO
  
  ! Precip fraction
  REAL(KIND=JPRB),ALLOCATABLE :: SD_PF (:,:,:,:)
  TYPE(TYPE_SURF_GEN)     :: YSD_PFD
  TYPE(TYPE_SFL_VEXTRA)   :: YSD_PF
  
  ! Ocean fields from the ocean model
  REAL(KIND=JPRB), ALLOCATABLE :: SD_OC (:,:,:)
  TYPE(TYPE_SURF_GEN)          :: YSD_OCD 
  TYPE(TYPE_SFL_OCE)           :: YSD_OC
  
  ! Number of fields to be put in TRAJEC%SRFC
  INTEGER(KIND=JPIM) :: NGP5_oops
  ! Number of fields to be put in TRAJEC%CST
  INTEGER(KIND=JPIM) :: NTRAJ_CST_oops
  
END TYPE TSURF

!-------------------------------------------------------------------------


END MODULE SURFACE_FIELDS_MIX

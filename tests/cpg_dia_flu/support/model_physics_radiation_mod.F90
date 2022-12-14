MODULE MODEL_PHYSICS_RADIATION_MOD
  USE YOMRADF        , ONLY : TRADF
  USE YOERAD         , ONLY : TERAD
  USE YOEOVLP        , ONLY : TEOVLP
  USE YOENEUR        , ONLY : TENEUR
  USE YOELWRAD       , ONLY : TELWRAD
  USE YOEAERD        , ONLY : TEAERD
  USE YOEAERATM      , ONLY : TEAERATM
  USE YOE_UVRAD      , ONLY : TEUVRAD
  USE YOERDI         , ONLY : TERDI
  USE YOE_MCICA      , ONLY : TEMCICA
  USE YOMRCOEF       , ONLY : TRCOEF
  USE YOMTRC         , ONLY : TTRC
  USE YOMPRAD        , ONLY : RADIATION_GRID_STRUCT
  USE YOMGSGEOM      , ONLY : TGSGEOM
  USE YOERIP         , ONLY : TERIP
  USE RADIATION_SETUP, ONLY : TRADIATION
  USE EINT_MOD       , ONLY : SL_STRUCT
  IMPLICIT NONE

  TYPE MODEL_PHYSICS_RADIATION_TYPE

  TYPE(TRADF)                 :: YRRADF
  TYPE(TERAD)                 :: YRERAD     !! control options for radiation config
  TYPE(TEOVLP)                :: YREOVLP    !! vert distrib of cloud overlap param 
  TYPE(TENEUR)                :: YRENEUR    !! neuroflux LW radiation (neuroflux radiation == coolest name ever?)
  TYPE(TELWRAD)               :: YRELWRAD   !! cloud characteristics for LW radiation
  TYPE(TEAERD)                :: YREAERD    !! spectral distribution of aerosols 
  TYPE(TEAERATM)              :: YREAERATM  !! control parameters for atmos aerosols
  TYPE(TEUVRAD)               :: YREUVRAD   !! UV radiation coefs 
  TYPE(TERDI)                 :: YRERDI     !! coefs in radiation interface 
  TYPE(TEMCICA)               :: YREMCICA   !! cloud generator stuff
  TYPE(TRCOEF)                :: YRRCOEF    !! read & write radiation coefs 
  TYPE(TTRC)                  :: YRTRC      !! storage for solar optical depths, 
                                            !! perhaps only for MF phys ? 
  TYPE(RADIATION_GRID_STRUCT) :: RADGRID    !! 'new'(?) radiation grid stuff
  TYPE(TGSGEOM), POINTER      :: YRAD_GSGEOM(:) => NULL()
  TYPE(TERIP)                 :: YRERIP     !! TEMPORARY TREATMENT OF RADIATION TIME, SHOULD CHANGE AT CY45
  TYPE(TRADIATION)            :: YRADIATION !! ecRad configuration
! YRRI : model grid to radiation grid
  TYPE(SL_STRUCT)             :: YRRI
! YRRO : radiation grid to model grid
  TYPE(SL_STRUCT)             :: YRRO
  
    CONTAINS

     

  END TYPE MODEL_PHYSICS_RADIATION_TYPE

  !---------------------------------------------------------------------

  CONTAINS 

  

END MODULE MODEL_PHYSICS_RADIATION_MOD

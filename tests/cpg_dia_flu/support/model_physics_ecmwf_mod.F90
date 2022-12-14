MODULE MODEL_PHYSICS_ECMWF_MOD
  USE YOEPHY      , ONLY : TEPHY
  USE YOECLD      , ONLY : TECLD
  USE YOECLDP     , ONLY : TECLDP
  USE YOECND      , ONLY : TECND
  USE YOECUMF     , ONLY : TECUMF
  USE YOE_CUCONVCA, ONLY : TECUCONVCA
  USE YOEGWD      , ONLY : TEGWD
  USE YOEGWWMS    , ONLY : TEGWWMS
  USE YOETHF      , ONLY : TTHF
  IMPLICIT NONE

  TYPE MODEL_PHYSICS_ECMWF_TYPE

  TYPE(TEPHY)         :: YREPHY  
  ! cloudy stuff
  TYPE(TECLD)         :: YRECLD  !! explicit clouds (diagnostic)
  TYPE(TECLDP)        :: YRECLDP !! explicit clouds (prognostic)
  TYPE(TECND)         :: YRECND  !! moist processes
  ! convection
  TYPE(TECUMF)        :: YRECUMF !! cumulus mass flux
  TYPE(TECUCONVCA)    :: YRECUCONVCA !! cellular automaton
  ! gravity wave drag
  TYPE(TEGWD)         :: YREGWD   !! parameters
  TYPE(TEGWWMS)       :: YREGWWMS !! WarnerMcIntyre GW parameterization

  TYPE(TTHF)          :: YRTHF

  CONTAINS

   
 
END TYPE MODEL_PHYSICS_ECMWF_TYPE

  !---------------------------------------------------------------------

  CONTAINS 

  

END MODULE MODEL_PHYSICS_ECMWF_MOD

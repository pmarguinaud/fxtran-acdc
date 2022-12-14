
MODULE MODEL_GENERAL_CONF_MOD
  USE TYPE_GEOMETRY, ONLY : GEOMETRY
  USE YOMDIMF      , ONLY : TDIMF
  USE YOM_YGFL     , ONLY : TYPE_GFLD
  USE YOMRIP       , ONLY : TRIP
  USE TYPE_ACV     , ONLY : TACVDIM
  IMPLICIT NONE

  TYPE MODEL_GENERAL_CONF_TYPE

    TYPE(GEOMETRY), POINTER :: GEOM => NULL()

    TYPE(TDIMF)             :: YRDIMF                  !! number of fields
    TYPE(TYPE_GFLD)         :: YGFL                    !! gfl descriptors
    TYPE(TRIP)              :: YRRIP                   !! TEMPORARY TREATMENT OF TIME, SHOULD CHANGE AT CY45
    TYPE(TACVDIM)           :: YRDIMACV                !! ACV field

    LOGICAL                 :: LMODEL_WITH_SPECRT      !! LSPRT use in this Model object, handled in MODEL_CREATE, MODEL_INIT

    CONTAINS

     
 
  END TYPE MODEL_GENERAL_CONF_TYPE

  !---------------------------------------------------------------------

  CONTAINS 

  

END MODULE MODEL_GENERAL_CONF_MOD


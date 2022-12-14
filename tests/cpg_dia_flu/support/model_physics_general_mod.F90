MODULE MODEL_PHYSICS_GENERAL_MOD
  USE YOMDPHY , ONLY : TDPHY
  USE YOMSLPHY, ONLY : TSLPHY
  USE YOEVDF,   ONLY : TVDF
  IMPLICIT NONE
  
  TYPE MODEL_PHYSICS_GENERAL_TYPE

    TYPE(TDPHY)  :: YRDPHY   !! dimensions
    TYPE(TSLPHY) :: YRSLPHY  !! physics & dynamics interfacing
    TYPE(TVDF)   :: YRVDF
  
  CONTAINS
  
     

  END TYPE MODEL_PHYSICS_GENERAL_TYPE

  !---------------------------------------------------------------------

  CONTAINS 

  

END MODULE MODEL_PHYSICS_GENERAL_MOD

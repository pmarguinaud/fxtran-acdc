MODULE MODEL_CHEM_MOD
  USE YOMOZO  , ONLY : TOZO
  USE YOMCHEM , ONLY : TCHEM
  USE YOMCOMPO, ONLY : TCOMPO
  IMPLICIT NONE

  TYPE MODEL_CHEM_TYPE

    TYPE(TOZO)   :: YROZO
    TYPE(TCHEM)  :: YRCHEM
    TYPE(TCOMPO) :: YRCOMPO

    CONTAINS

     

  END TYPE MODEL_CHEM_TYPE

  !---------------------------------------------------------------------

  CONTAINS 

  

END MODULE MODEL_CHEM_MOD

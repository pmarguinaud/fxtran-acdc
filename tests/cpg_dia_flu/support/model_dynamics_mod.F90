MODULE MODEL_DYNAMICS_MOD
  USE YOMDYNA     , ONLY : TDYNA
  USE YOMDYN      , ONLY : TDYN
  USE YEMDYN      , ONLY : TEDYN
  USE SPNG_MOD    , ONLY : TSPNG
  USE PTRGPPC     , ONLY : TPTRGPPC
  USE INTDYNSL_MOD, ONLY : TLSCAW, TRSCAW, TSCO, TCCO 
  USE YOMSLREP    , ONLY : TSLREP
  USE PTRSLB1     , ONLY : TPTRSLB1
  USE PTRSLB2     , ONLY : TPTRSLB2
  USE PTRSLB15    , ONLY : TPTRSLB15
  USE YOMTNH      , ONLY : TTNH
  USE EINT_MOD    , ONLY : SL_STRUCT
  IMPLICIT NONE
    
  TYPE MODEL_DYNAMICS_TYPE
  ! general stuff
  TYPE(TDYNA)     :: YRDYNA
  TYPE(TDYN)      :: YRDYN
  TYPE(TEDYN)     :: YREDYN
  TYPE(TSPNG)     :: YRSPNG
  ! predictor-corrector
  TYPE(TPTRGPPC)  :: YRPTRGPPC
  ! semi-lag
  TYPE(TLSCAW)    :: YYTLSCAW, YYTLSCAWH  !! interp at 
  TYPE(TRSCAW)    :: YYTRSCAW, YYTRSCAWH  !! OBS location stuff
  TYPE(TSCO)      :: YYTSCO
  TYPE(TCCO)      :: YYTCCO
  TYPE(TSLREP)    :: YRSLREP
  TYPE(TPTRSLB1)  :: YRPTRSLB1
  TYPE(TPTRSLB2)  :: YRPTRSLB2
  TYPE(TPTRSLB15) :: YRPTRSLB15
  TYPE(TTNH)      :: YRTNH      !! traj for NH
 ! YRSL : semi-lagrangian advection, moved here from eint_mod
  TYPE(SL_STRUCT) :: YRSL  
! YRAD : semi-lagrangian advection (adjoint), moved here from eint_mod
  TYPE(SL_STRUCT) :: YRAD 

CONTAINS
  
    
 END TYPE MODEL_DYNAMICS_TYPE

  !---------------------------------------------------------------------

  CONTAINS 
END MODULE MODEL_DYNAMICS_MOD


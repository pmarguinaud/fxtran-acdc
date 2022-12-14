MODULE MODEL_PHYSICS_SIMPLINEAR_MOD
  USE YOEPHLI    , ONLY : TEPHLI
  USE YOMCUMFS   , ONLY : TCUMFS
  USE YOEGWDWMS  , ONLY : TEGWDWMS
  USE YOECUMF2   , ONLY : TECUMF2
  USE YOPHLC     , ONLY : TPHLC
  USE YOPHNC     , ONLY : TPHNC
  USE YOMNCL     , ONLY : TNCL
  USE YOMSRFTLAD , ONLY : TSRFTLAD
  USE YOMSPHYHIST, ONLY : SPHYS_HIST_TYPE
  IMPLICIT NONE

  TYPE MODEL_PHYSICS_SIMPLINEAR_TYPE

  TYPE(TEPHLI)   :: YREPHLI   !! constants for lin-phys
  TYPE(TCUMFS)   :: YRCUMFS   !! simplified convection scheme
  TYPE(TEGWDWMS) :: YREGWDWMS !! simplified non-oro g-w scheme
  TYPE(TECUMF2)  :: YRECUMF2  !! new linearized cumulus mass flux
  TYPE(TPHLC)    :: YRPHLC    !! simplified phys switches
  TYPE(TPHNC)    :: YRPHNC    !! switches for t-dt trajectory & phys
  TYPE(TNCL)     :: YRNCL     !! simplified cloud characteristics
  TYPE(TSRFTLAD) :: YRSRFTLAD !! lin phys skin temp
  TYPE(SPHYS_HIST_TYPE), POINTER :: GPHIST(:) => NULL()  !! trajectory “memory”
  !---------------------------------------------------------------------
  
CONTAINS
  
   
                                                       !! across timesteps
  END TYPE MODEL_PHYSICS_SIMPLINEAR_TYPE
  !======================================================================

  CONTAINS 

  

END MODULE MODEL_PHYSICS_SIMPLINEAR_MOD

MODULE PTRGFU_TYPE

!$ACDC methods --methods-list=size,wipe,copy,host --skip-components flu --field-api --field-api-class flu


USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------

!*    Pointers for cumulated fluxes diagnostics (CFU).


TYPE TCFUPTR
INTEGER(KIND=JPIM) :: MFCCQL        = 0 ! liquid condensation due to convection
INTEGER(KIND=JPIM) :: MFCCQN        = 0 ! solid condensation due to convection
INTEGER(KIND=JPIM) :: MFCHSP        = 0 ! heat in soil
INTEGER(KIND=JPIM) :: MFCL        = 0 ! latent heat
INTEGER(KIND=JPIM) :: MFCLL        = 0 ! liquid latent heat
INTEGER(KIND=JPIM) :: MFCLN        = 0 ! solid latent heat
INTEGER(KIND=JPIM) :: MFCSQL        = 0 ! large scale liquid condensation
INTEGER(KIND=JPIM) :: MFCSQN        = 0 ! large scale solid condensation
INTEGER(KIND=JPIM) :: MFCS        = 0 ! sensible heat
INTEGER(KIND=JPIM) :: MFDICQ        = 0 ! contribution of convection to q
INTEGER(KIND=JPIM) :: MFDICS        = 0 ! contribution of convection to (cp T)
INTEGER(KIND=JPIM) :: MFDISH        = 0 ! surface enthalpy (due to the dissipation of kinetic energy)
INTEGER(KIND=JPIM) :: MFDITQ        = 0 ! contribution of turbulence to T
INTEGER(KIND=JPIM) :: MFDITS        = 0 ! contribution of turbulence to (cp T)
INTEGER(KIND=JPIM) :: MFDUTP        = 0 ! filtered duration of total precipitations
INTEGER(KIND=JPIM) :: MFEVL        = 0 ! liquid evaporation
INTEGER(KIND=JPIM) :: MFEVN        = 0 ! snow evaporation
INTEGER(KIND=JPIM) :: MFEVV        = 0 ! evapotranspiration
INTEGER(KIND=JPIM) :: MFGEL        = 0 ! deep frost
INTEGER(KIND=JPIM) :: MFGELS        = 0 ! surface frost
INTEGER(KIND=JPIM) :: MFLASH        = 0 ! cumulative lighntning diagnotics
INTEGER(KIND=JPIM) :: MDIAMACCR        = 0 ! wet snow and ice diameter
INTEGER(KIND=JPIM) :: MLOADICE        = 0 ! accreted freezing rain
INTEGER(KIND=JPIM) :: MLOADSNOW        = 0 ! accreted wet snow mass
INTEGER(KIND=JPIM) :: MPCPWSNOW        = 0 ! wet snow precipitation
INTEGER(KIND=JPIM) :: MPCPICE        = 0 ! freezing rain precipitation
INTEGER(KIND=JPIM) :: MFLWSP        = 0 ! water in soil
INTEGER(KIND=JPIM) :: MFNEB        = 0 ! cloud cover
INTEGER(KIND=JPIM) :: MFNEBT        = 0 ! total cloudiness
INTEGER(KIND=JPIM) :: MFONTE        = 0 ! melt snow
INTEGER(KIND=JPIM) :: MFPLCG        = 0 ! convective graupel fall
INTEGER(KIND=JPIM) :: MFPLCH        = 0 ! convective hail fall
INTEGER(KIND=JPIM) :: MFPLCL        = 0 ! convective precipitation
INTEGER(KIND=JPIM) :: MFPLCN        = 0 ! convective snow fall
INTEGER(KIND=JPIM) :: MFPLSG        = 0 ! large scale graupel fall (stratiform)
INTEGER(KIND=JPIM) :: MFPLSH        = 0 ! large scale hail fall (stratiform)
INTEGER(KIND=JPIM) :: MFPLSL        = 0 ! large scale precipitation (stratiform)
INTEGER(KIND=JPIM) :: MFPLSN        = 0 ! large scale snow fall (stratiform)
INTEGER(KIND=JPIM) :: MFRSDNI        = 0 ! surface direct normal irradiance
INTEGER(KIND=JPIM) :: MFRSGNI        = 0 ! surface global normal irradiance
INTEGER(KIND=JPIM) :: MFRSOC0        = 0 ! top clear sky shortwave radiative
INTEGER(KIND=JPIM) :: MFRSOC1        = 0 ! surface clear sky shortwave radiative
INTEGER(KIND=JPIM) :: MFRSODS        = 0 ! surface down solar
INTEGER(KIND=JPIM) :: MFRSOLU        = 0 ! surface downward moon radiation
INTEGER(KIND=JPIM) :: MFRSOPS        = 0 ! surface parallel solar
INTEGER(KIND=JPIM) :: MFRSOPT        = 0 ! top parallel solar
INTEGER(KIND=JPIM) :: MFRSO        = 0 ! solar radiation
INTEGER(KIND=JPIM) :: MFRTHC0        = 0 ! top clear sky longwave radiative
INTEGER(KIND=JPIM) :: MFRTHC1        = 0 ! surface clear sky longwave radiative
INTEGER(KIND=JPIM) :: MFRTHDS        = 0 ! surface down thermic
INTEGER(KIND=JPIM) :: MFRTH        = 0 ! surface radiation
INTEGER(KIND=JPIM) :: MFTOPH        = 0 ! top mesospheric enthalpy (+ dissipation)
INTEGER(KIND=JPIM) :: MFTR        = 0 ! transpiration
INTEGER(KIND=JPIM) :: MNEBBAS        = 0 ! low cloud cover
INTEGER(KIND=JPIM) :: MNEBCON        = 0 ! convective cloud cover
INTEGER(KIND=JPIM) :: MNEBHAU        = 0 ! high cloud cover
INTEGER(KIND=JPIM) :: MNEBMOY        = 0 ! medium cloud cover
INTEGER(KIND=JPIM) :: MOZONT        = 0 ! total ozone
INTEGER(KIND=JPIM) :: MQICE        = 0 ! solid specific moisture
INTEGER(KIND=JPIM) :: MQLI        = 0 ! liquid specific moisture
INTEGER(KIND=JPIM) :: MQTOT        = 0 ! total precipitable water
INTEGER(KIND=JPIM) :: MRUISL        = 0 ! interception soil layer runoff
INTEGER(KIND=JPIM) :: MRUISP        = 0 ! deep soil runoff
INTEGER(KIND=JPIM) :: MRUISS        = 0 ! surface soil runoff
INTEGER(KIND=JPIM) :: MSNS        = 0 ! snow mass
INTEGER(KIND=JPIM) :: MSPRES        = 0 ! surface pressure
INTEGER(KIND=JPIM) :: MSTRCU        = 0 ! contribution of convection to U
INTEGER(KIND=JPIM) :: MSTRCV        = 0 ! contribution of convection to V
INTEGER(KIND=JPIM) :: MSTRDU        = 0 ! U-wind gravity wave stress
INTEGER(KIND=JPIM) :: MSTRDV        = 0 ! V-wind gravity wave stress
INTEGER(KIND=JPIM) :: MSTRTU        = 0 ! contribution of turbulence to U
INTEGER(KIND=JPIM) :: MSTRTV        = 0 ! contribution of turbulence to V
INTEGER(KIND=JPIM) :: MWS        = 0 ! soil moisture
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TCFUPTR
PROCEDURE :: ACDC_HOST => ACDC_HOST_TCFUPTR
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TCFUPTR
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TCFUPTR
END TYPE TCFUPTR

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TCFUPTR (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TCFUPTR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TCFUPTR (SELF)

IMPLICIT NONE
CLASS (TCFUPTR), TARGET :: SELF
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TCFUPTR (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TCFUPTR),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TCFUPTR (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TCFUPTR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE PTRGFU_TYPE

MODULE PTRXFU_TYPE

!$ACDC methods --methods-list=size,wipe,copy,host --skip-components flu --field-api --field-api-class flu


USE PARKIND1  ,ONLY : JPIM    

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------

!*    Pointers for instantaneous fluxes diagnostics (XFU).


TYPE TXFUPTR
INTEGER(KIND=JPIM) :: MXCAPE        = 0 ! CAPE (convective available potential energy)
INTEGER(KIND=JPIM) :: MXCLPH        = 0 ! height of the PBL out of the model
INTEGER(KIND=JPIM) :: MXCLPHMF        = 0 ! height of the PBL for minimum of buoyancy
INTEGER(KIND=JPIM) :: MXVMAXTH        = 0 ! diagnostic of maximal wind speed of the BL thermal
INTEGER(KIND=JPIM) :: MXCLWC2        = 0 ! maximum of CLWC
INTEGER(KIND=JPIM) :: MXCLWC        = 0 ! maximum of CLWC
INTEGER(KIND=JPIM) :: MXCTOP        = 0 ! pressure of top of deep convection
INTEGER(KIND=JPIM) :: MXDICQ        = 0 ! contribution of convection to q
INTEGER(KIND=JPIM) :: MXDICS        = 0 ! contribution of convection to (cp T)
INTEGER(KIND=JPIM) :: MXDITQ        = 0 ! contribution of turbulence to T
INTEGER(KIND=JPIM) :: MXDITS        = 0 ! contribution of turbulence to (cp T)
INTEGER(KIND=JPIM) :: MXFCHSP        = 0 ! heat flux in soil
INTEGER(KIND=JPIM) :: MXFEVL        = 0 ! liquid evaporation
INTEGER(KIND=JPIM) :: MXFEVV        = 0 ! evapotranspiration
INTEGER(KIND=JPIM) :: MXFLWSP        = 0 ! water flux in soil
INTEGER(KIND=JPIM) :: MXFONTE        = 0 ! melt snow
INTEGER(KIND=JPIM) :: MXFTR        = 0 ! transpiration
INTEGER(KIND=JPIM) :: MXHUN        = 0 ! minimum relative humidity at 2 meters (pbl)
INTEGER(KIND=JPIM) :: MXHUX        = 0 ! maximum relative humidity at 2 meters (pbl)
INTEGER(KIND=JPIM) :: MXMOCON        = 0 ! moisture convergence
INTEGER(KIND=JPIM) :: MXMRT        = 0 ! mean radiant temperature at 2 meters
INTEGER(KIND=JPIM) :: MXNBBAS        = 0 ! low cloud cover
INTEGER(KIND=JPIM) :: MXNBCON        = 0 ! convective cloud cover
INTEGER(KIND=JPIM) :: MXNBHAU        = 0 ! high cloud cover
INTEGER(KIND=JPIM) :: MXNBMOY        = 0 ! medium cloud cover
INTEGER(KIND=JPIM) :: MXNEB        = 0 ! cloud cover
INTEGER(KIND=JPIM) :: MXNEBT        = 0 ! total cloudiness
INTEGER(KIND=JPIM) :: MXNUCLS        = 0 ! U-component of neutral wind at 10 meters (pbl)
INTEGER(KIND=JPIM) :: MXNVCLS        = 0 ! V-component of neutral wind at 10 meters (pbl)
INTEGER(KIND=JPIM) :: MXPLCG        = 0 ! convective graupel fall
INTEGER(KIND=JPIM) :: MXPLCH        = 0 ! convective hail fall
INTEGER(KIND=JPIM) :: MXPLCL        = 0 ! convective precipitation
INTEGER(KIND=JPIM) :: MXPLCN        = 0 ! convective snow fall
INTEGER(KIND=JPIM) :: MXPLSG        = 0 ! large scale graupel fall (stratiform)
INTEGER(KIND=JPIM) :: MXPLSH        = 0 ! large scale hail fall (stratiform)
INTEGER(KIND=JPIM) :: MXPLSL        = 0 ! large scale precipitation (stratiform)
INTEGER(KIND=JPIM) :: MXPLSN        = 0 ! large scale snow fall (stratiform)
INTEGER(KIND=JPIM) :: MXPTYPE2        = 0 ! frequent precipitations type
INTEGER(KIND=JPIM) :: MXPTYPE        = 0 ! frequent precipitations type
INTEGER(KIND=JPIM) :: MXPTYPESEV2        = 0 ! severe precipitations type
INTEGER(KIND=JPIM) :: MXPTYPESEV        = 0 ! severe precipitations type
INTEGER(KIND=JPIM) :: MXQCLS        = 0 ! specific humidity at 2 meters (pbl)
INTEGER(KIND=JPIM) :: MXQICE        = 0 ! ice water
INTEGER(KIND=JPIM) :: MXQLI        = 0 ! liquid water
INTEGER(KIND=JPIM) :: MXRHCLS        = 0 ! relative humidity at 2 meters (pbl)
INTEGER(KIND=JPIM) :: MXRSO        = 0 ! solar radiation
INTEGER(KIND=JPIM) :: MXRTH        = 0 ! surface radiation
INTEGER(KIND=JPIM) :: MXRUISL        = 0 ! interception soil layer runoff
INTEGER(KIND=JPIM) :: MXRUISP        = 0 ! deep soil runoff
INTEGER(KIND=JPIM) :: MXRUISS        = 0 ! surface soil runoff
INTEGER(KIND=JPIM) :: MXSIC        = 0 ! 
INTEGER(KIND=JPIM) :: MXTCLS        = 0 ! temperature at 2 meters (pbl)
INTEGER(KIND=JPIM) :: MXTHW        = 0 ! theta prime_w surface
INTEGER(KIND=JPIM) :: MXTN        = 0 ! minimum temperature at 2 meters
INTEGER(KIND=JPIM) :: MXTPWCLS        = 0 ! wet bulb temperature at 2 meters (pbl)
INTEGER(KIND=JPIM) :: MXTRCU        = 0 ! contribution of convection to U
INTEGER(KIND=JPIM) :: MXTRCV        = 0 ! contribution of convection to V
INTEGER(KIND=JPIM) :: MXTRDU        = 0 ! U-wind gravity wave stress
INTEGER(KIND=JPIM) :: MXTRDV        = 0 ! V-wind gravity wave stress
INTEGER(KIND=JPIM) :: MXTRTU        = 0 ! contribution of turbulence to U
INTEGER(KIND=JPIM) :: MXTRTV        = 0 ! contribution of turbulence to V
INTEGER(KIND=JPIM) :: MXTX        = 0 ! maximum temperature at 2 meters
INTEGER(KIND=JPIM) :: MXUCLS        = 0 ! U-component of wind at 10 meters (pbl)
INTEGER(KIND=JPIM) :: MXUGST2        = 0 ! U-momentum of gusts2
INTEGER(KIND=JPIM) :: MXUGST        = 0 ! U-momentum of gusts out of the model
INTEGER(KIND=JPIM) :: MXVCLS        = 0 ! V-component of wind at 10 meters (pbl)
INTEGER(KIND=JPIM) :: MXVEIN        = 0 ! ventilation index in PBL
INTEGER(KIND=JPIM) :: MXVGST2        = 0 ! V-momentum of gusts2
INTEGER(KIND=JPIM) :: MXVGST        = 0 ! V-momentum of gusts out of the model
INTEGER(KIND=JPIM) :: MXVISICLD2        = 0 ! visibility due to water and/or ice cloud
INTEGER(KIND=JPIM) :: MXVISICLD        = 0 ! visibility due to water and/or ice cloud
INTEGER(KIND=JPIM) :: MXVISIHYD2        = 0 ! visibility due to precipitations
INTEGER(KIND=JPIM) :: MXVISIHYD        = 0 ! visibility due to precipitations
INTEGER(KIND=JPIM) :: MXXDIAGH        = 0 ! hail diagnostic
INTEGER(KIND=JPIM) :: MXSNWDE        = 0 ! snow depth diagnostic
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TXFUPTR
PROCEDURE :: ACDC_HOST => ACDC_HOST_TXFUPTR
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TXFUPTR
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TXFUPTR
END TYPE TXFUPTR

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TXFUPTR (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TXFUPTR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TXFUPTR (SELF)

IMPLICIT NONE
CLASS (TXFUPTR), TARGET :: SELF
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TXFUPTR (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TXFUPTR),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TXFUPTR (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TXFUPTR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE PTRXFU_TYPE

MODULE YOMSIMPHL

!$ACDC methods 


USE PARKIND1, ONLY : JPRB

IMPLICIT NONE

SAVE

TYPE TSIMPHL
!   -----------------------------------------------------------------

!*    with logical switches for simplified physical 
!           parametrization

!    LSIMPH   : switch for simplified physical parametrization 
!    LTRAJPS  : switch for write out and read the trajectory at 
!               t-dt for simplified physical computations in file
!    LTRAJPST : switch for write out and read the trajectory at 
!               t-dt for fluxes and tendencies in file
!    LSMOOTHD : smoothing some functions in direct computation
!    LSMOOTHA : smoothing some functions in TL and AD computations
!    LSMOOTHB : modifications in simplified physical parametrizations
!               to stabilize the TL and AD codes

!    LCVRASP  : key for calling deep convection of simplified
!               phys. parametrization (ACCONV)
!    LGWDSP   : key for calling the gravity wave drag of simpl.
!               phys. parametrization (ACDRAGL)
!    LGWDSP   : key for retrieving ZRAPP from nonlinear traj
!    LRAYSP   : key for calling radiation scheme of simplified
!               phys. parametrization (ACRADS)
!    LSTRASP  : key for calling stratiform precipitation of simpl.
!               phys. parametrization (ACQWLSR)
!    LSTRASPN : key for calling strat precip based on Smith scheme(ACLSPS)
!    LVDIFSP  : key for calling the vertical turbulent diffusion
!               of simpl. phys. param. (ACTSEC,ACDIFSP)
!    LVDIFSPNL: key for using Km/h from the non linear model as trajectory 
!               in the vertical turbulent diffusion TL and AD (ACTSECTL and ACTSECAD)
!    LRRMESSP : key for calling the mesospheric drag
!               of simpl. phys. param. (ACDRME)
!    LCLOUDS  : key for cloud parametrization
!    LPROCLDTL: q_l and q_i taken into account in trajectory for TL and AD
!    RHCRIT1S,RHCRIT2S: values for RHCRIT1/2 for strat precip(LSTRASPN)
!    LMICROTl : activation of autoconversion
!    LCVRASBM: Betts Miller convective scheme for simplified physics
!    TADJ: relaxation time in Betts-Miller scheme
!    LCONSENTH: enthalpy conservation activated for TL variables
!    LAPPROXCONV: approximate formulation for simplifying adjoint code of BM.



LOGICAL :: LSIMPH
LOGICAL :: LTRAJPS
LOGICAL :: LTRAJPST
LOGICAL :: LSMOOTHD
LOGICAL :: LSMOOTHA
LOGICAL :: LSMOOTHB
LOGICAL :: LCVRASP
LOGICAL :: LGWDSP
LOGICAL :: LRAYSP
LOGICAL :: LSTRASP
LOGICAL :: LVDIFSP
LOGICAL :: LVDIFSPNL
LOGICAL :: LRRMESSP
LOGICAL :: LCLOUDS
LOGICAL :: LGWDSPNL
LOGICAL :: LSTRASPN
LOGICAL :: LPROCLDTL
LOGICAL :: LMELTTL
LOGICAL :: LMELTNL
LOGICAL :: LMICROTL
LOGICAL :: LTRAJRAIN
LOGICAL :: LTRAJCOND
LOGICAL :: LNEBCVPPKF
LOGICAL :: LCOLLECTL
LOGICAL :: LEVAPTL
LOGICAL :: LSMOOTHEVP
LOGICAL :: LIGELREPRO
LOGICAL :: LCVRASBM,LCONSENTH,LAPPROXCONV
REAL(KIND=JPRB):: RHCRIT1S,RHCRIT2S,TADJ,RMINEVP,DELTAH
REAL(KIND=JPRB):: RMODULQCPROG

CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TSIMPHL
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TSIMPHL
PROCEDURE :: ACDC_HOST => ACDC_HOST_TSIMPHL
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TSIMPHL
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TSIMPHL
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TSIMPHL
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TSIMPHL
END TYPE TSIMPHL

!!TYPE(TSIMPHL), POINTER :: YRSIMPHL => NULL()

!   ----------------------------------------------------------------
INTERFACE

MODULE SUBROUTINE ACDC_COPY_TSIMPHL (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TSIMPHL), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TSIMPHL (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TSIMPHL), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TSIMPHL (SELF)

IMPLICIT NONE
CLASS (TSIMPHL), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TSIMPHL (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TSIMPHL), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TSIMPHL (SELF, KLUN)

IMPLICIT NONE
CLASS (TSIMPHL), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TSIMPHL (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TSIMPHL),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TSIMPHL (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TSIMPHL), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE YOMSIMPHL

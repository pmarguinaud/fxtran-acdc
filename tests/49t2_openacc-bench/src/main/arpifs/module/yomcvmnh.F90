MODULE YOMCVMNH

!$ACDC methods 


!Module to define the parameters of the KFB
!convection parametrization of MNH.
! Modified by E. Bazile 09.05.2007
!    parameters for KFB in namelist

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE MODD_NSV            , ONLY : NSV_T
USE MODD_CONVPAR        , ONLY : CONVPAR_T
USE MODD_CST            , ONLY : CST_T
USE MODD_CONVPAREXT     , ONLY : CONVPAREXT
USE MODD_CONVPAR_SHAL   , ONLY : CONVPAR_SHAL

IMPLICIT NONE

SAVE

TYPE TCVMNH
!--------------------------------------------

!LDEEP : Control Key to deep convection scheme
!LSHALLOW : Control key to shallow convection scheme
!LDIAGCONV : Control key to compute diagnostics on convection
!LSETTADJ : logical to set convective adjustment time by user
!LREFRESH_ALL ! refresh or not tendencies at every call
!LDOWN ! take or not convective downdrafts into account 

!OTADJD : user specified time step to call deep convection
!OTADJS : user specified time step to call shallow convection

!NSETENS : use of several calls to deep_conv (0 only one call)
!NIICE : USe of Ice in convection 1 yes 0 no

LOGICAL :: LDEEP, LSHALLOW, LDIAGCONV
LOGICAL :: LSETTADJ, LREFRESH_ALL, LDOWN, LSMOOTH
REAL(KIND=JPRB) :: OTADJD
REAL(KIND=JPRB) :: OTADJS
INTEGER(KIND=JPIM) :: NSETENS, NIICE
REAL(KIND=JPRB) :: XA25
REAL(KIND=JPRB) :: XCRAD
REAL(KIND=JPRB) :: XCDEPTH
REAL(KIND=JPRB) :: XCDEPTH_D
REAL(KIND=JPRB) :: XDTPERT
REAL(KIND=JPRB) :: XENTR
REAL(KIND=JPRB) :: XZLCL
REAL(KIND=JPRB) :: XZPBL
REAL(KIND=JPRB) :: XWTRIG
REAL(KIND=JPRB) :: XNHGAM
REAL(KIND=JPRB) :: XTFRZ1
REAL(KIND=JPRB) :: XTFRZ2 
REAL(KIND=JPRB) :: XSTABT
REAL(KIND=JPRB) :: XSTABC
REAL(KIND=JPRB) :: XAW
REAL(KIND=JPRB) :: XBW
REAL(KIND=JPRB) :: XATPERT
REAL(KIND=JPRB) :: XBTPERT

TYPE(CONVPAREXT)   :: YRCVPEXT
TYPE(CONVPAR_SHAL) :: YRCVP_SHAL
TYPE(CST_T)        :: YRCST_MNH
TYPE(NSV_T)        :: YRNSV
TYPE(CONVPAR_T)    :: YRCONVPAR

END TYPE TCVMNH

!!TYPE(TCVMNH), POINTER :: YRCVMNH => NULL()

END MODULE YOMCVMNH

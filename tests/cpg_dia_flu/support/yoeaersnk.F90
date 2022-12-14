MODULE YOEAERSNK

USE PARKIND1  ,ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERSNK* - CONTROL OPTIONS FOR AEROSOLS' SINKS
!     ------------------------------------------------------------------

TYPE :: TEAERSNK
INTEGER(KIND=JPIM):: NDRYDEPVEL_DYN
INTEGER(KIND=JPIM) :: NDRYDEP
REAL(KIND=JPRB) :: R_R, R_S
REAL(KIND=JPRB) :: RAERTS(21)
REAL(KIND=JPRB) :: RFRAER

REAL(KIND=JPRB) :: RRHTAB(12)
REAL(KIND=JPRB) :: RSSGROWTH_RHTAB(12)
REAL(KIND=JPRB) :: RSSDENS_RHTAB(12)
REAL(KIND=JPRB) :: RMMD_DD(9)
REAL(KIND=JPRB) :: RRHO_DD(9)
REAL(KIND=JPRB) :: RMMD_NI(2)
REAL(KIND=JPRB) :: RRHO_NI(2)
REAL(KIND=JPRB) :: RMMD_SS(9)
REAL(KIND=JPRB) :: RRHO_SS(9)
REAL(KIND=JPRB) :: RHO_WAT, RHO_ICE
REAL(KIND=JPRB) :: RHAMAKER

REAL(KIND=JPRB) :: RSO2CV1, RSO2CV2, RSUCV1 , RSUCV2 , RVSO2CV1, RVSO2CV2
!---------------------------------------------------------------------
CONTAINS
   
END TYPE TEAERSNK
!=====================================================================

TYPE(TEAERSNK), POINTER :: YREAERSNK => NULL()

!     ------------------------------------------------------------------
! NDRYDEP    : dry deposition 1: a la GEMS; 2: "exp" (a la D_GRG_4.6)
! R_R        : mean radius for rain drops (m)
! R_S        : mean radius for snow crystals (m)
! RAERTS     : inverse time scale (s-1) - used only in AER_LOSS{,_TL,_AD}, never called
! re-evaporation constants
! RFRAER     : for aerosols
! RRHTAB     : reference relative humidity in growth factor index for aerosol optical properties

! coefficient for parametrisation conversion SO2 to SO4
! RSO2CV1 and RSO2CV2 as used in aer_so2so4
! RSUCV1  and RSUCV2  possibly read in namelist naeaer 
!     -----------------------------------------------------------------

CONTAINS



END MODULE YOEAERSNK


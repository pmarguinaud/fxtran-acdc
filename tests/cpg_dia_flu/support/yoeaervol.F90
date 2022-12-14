MODULE YOEAERVOL

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERVOL* - CONTROL PARAMETERS FOR VOLCANIC AEROSOLS IN THE ATMOSPHERE
!     ------------------------------------------------------------------

TYPE :: TEAERVOL
INTEGER(KIND=JPIM) :: NAERVOLC
INTEGER(KIND=JPIM) :: NAERVOLE
INTEGER(KIND=JPIM) :: NINTERPT
INTEGER(KIND=JPIM) :: NTVOLC
INTEGER(KIND=JPIM) :: NVOLERUP
INTEGER(KIND=JPIM) :: NVOLHOMO
INTEGER(KIND=JPIM) :: NVOLOPTP
INTEGER(KIND=JPIM) :: NVOLERUZ(100)
INTEGER(KIND=JPIM) :: NVOLDATS(100)
INTEGER(KIND=JPIM) :: NVOLDATE(100)

REAL(KIND=JPRB) :: RAERVOLC(100,8)
REAL(KIND=JPRB) :: RAERVOLE(100,8)
REAL(KIND=JPRB) :: RVOLERUZ(100)
REAL(KIND=JPRB) :: RVSEDVOL, RVDEPVOL, RWDEPVOL
!---------------------------------------------------------------------
CONTAINS
   
END TYPE TEAERVOL
!=====================================================================

TYPE(TEAERVOL), POINTER :: YREAERVOL => NULL()

!     ------------------------------------------------------------------
! AERVOLC  parameters for continuous volcanoes
! AERVOLE  parameters for explosive volcanoes
! NAERVOL.. number of .. volcanoes
! ...VOL.(.. , 1) latitude
! ...VOL.(.. , 2) longitude  
! ...VOL.(.. , 3) mass of particulates (to go into FlyAsh) (kg)
! ...VOL.(.. , 4) mass of gases (to go into SO2) (kg)
! ...VOL.(.. , 5) base of drifting plume  (m)
! ...VOL.(.. , 6) top of drifting plume   (m)
! NINTERPT = 0 takes latest profile, = 1 interpolates between profiles  
! NTVOLC    total number of volcanoes considered (continuous + explosive)
! NVOLERUP = 0 no erupting volcano
! NVOLERUP = 1 using AEROCOM-type data for location and ejected mass
! NVOLERUP = 2 erupting volcano has some input as f(time/height)
! NVOLERUZ = 0 for erupting volcano, homogeneous distribution of mass over the vertical
! NVOLERUZ = 1 interpolating the "observed/retrieved" mass over the vertical
! RVOLERUZ     emission weighting factor when emission profile is known (empirical)
! NVOLHOMO = 0 profile information is used, = 1 vertically homogeneous distribution 
! NVOLOPTP     index for choosing the optical properties assigned to the ash
! NVOLOPTP = 1 opt.prop. as SO4
! NVOLOPTP = 2 opt.prop. as BC 
! NVOLOPTP = 3 opt.prop. as DU3
! NVOLDATS     yyyymmddhh date and start of eruption
! RVSEDVOL     gravitational sedimentation coefficient for volcanic component
! RVDEPVOL     dry deposition coefficient for volcanic component
! RWDEPVOL     wet deposition coefficient for volcanic component
!     ------------------------------------------------------------------

CONTAINS



END MODULE YOEAERVOL

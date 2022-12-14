MODULE YOMRIP 

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     --------------------------------------------------------------------------------
!     Date and timestep related variables.
!     Values may be different for the different models run under the OOPS layer.
!     In particuliar, we find there all date and timestep variables updated in UPDTIM.
!     --------------------------------------------------------------------------------

TYPE :: TRIP

!*    Numbering of timesteps

!     NSTART : first timestep of model
!     NSTOP  : last timestep of model
!     CSTOP  : character string defining stopping criteria for run

!     NFOST  : NUMBER OF FIRST-ORDER TIME STEPS AT THE START OF THE RUN 
!              IN ORDER TO AVOID TRAJECTORIES GOING OUT OF THE ATMOSPHERE.

!     ------------------------------------------------------------------

!*    Real time related variables (updated in UPDTIM)

!     NSTADD : NUMBER OF DAYS SINCE START OF THE MODEL
!     NSTASS : NUMBER OF SECONDS since start of model modulo(86400)
!     RSTATI : NUMBER OF SECONDS SINCE START OF THE MODEL
!     RTIMTR : ABSOLUTE TIME OF THE MODEL

!     RHGMT  : GMT TIME OF THE MODEL  (BETWEEN 0 AND 86400)
!     REQTIM : EQUATION OF TIME

!     ------------------------------------------------------------------

!*    Sun related variables (updated in UPDTIM)
!     RSOVR  : TRUE SOLAR TIME (GMT+EQUATION OF TIME)

!     RDEASO : DISTANCE EARTH-SUN
!     RDECLI : DECLINATION
!     RWSOVR : IN RADIANS, TRUE SOLAR TIME (GMT+EQUATION OF TIME)
!              THIS ANGLE IS ALSO PI - (LONGITUDE OF THE POINT
!              WHERE THE SUN IS AT ZENITH)
!     RIP0   : I0 WEIGHTED BY THE DISTANCE EARTH-SUN

!     RCODEC : COSINE OF THE DECLINATION
!     RSIDEC :   SINE OF THE DECLINATION

!     RCOVSR : COSINE OF TRUE SOLAR TIME
!     RSIVSR :   SINE OF TRUE SOLAR TIME

!     NEW VARIABLES FOR LMSE
!     RCODECN : COSINE OF THE DECLINATION FOR NEXT TIME STEP
!     RSIDECN :   SINE OF THE DECLINATION FOR NEXT TIME STEP

!     RCOVSRN : COSINE OF TRUE SOLAR TIME FOR NEXT TIME STEP
!     RSIVSRN :   SINE OF TRUE SOLAR TIME FOR NEXT TIME STEP

!     RCODECF : COSINE OF THE DECLINATION at the end of radiation step
!     RSIDECF :   SINE OF THE DECLINATION at the end of radiation step
!     RCOVSRF : COSINE OF TRUE SOLAR TIME at the end of radiation step
!     RSIVSRF :   SINE OF TRUE SOLAR TIME at the end of radiation step

!     ------------------------------------------------------------------

!*    Moon related variables (updated in UPDTIM)

!     RDECLU : LUNAR DECLINATION
!     RTMOLT : IN RADIANS, TRUE LUNAR TIME (GMT+EQUATION OF TIME)
!              THIS ANGLE IS ALSO PI - (LONGITUDE OF THE POINT
!              WHERE THE MOON IS AT ZENITH)
!     RIP0LU : LUNAR I0 (DOWNWARD TOA LUNAR FLUX)

!     RCODECLU : COSINE OF THE LUNAR DECLINATION
!     RSIDECLU :   SINE OF THE LUNAR DECLINATION

!     RCOVSRLU : COSINE OF TRUE LUNAR TIME
!     RSIVSRLU :   SINE OF TRUE LUNAR TIME

!     ------------------------------------------------------------------

!*    Time step related variables

!     TSTEP  : length of the timestep in seconds
!     TDT    : For leap-frog scheme: 2*TSTEP except at the first time step where it is TSTEP
!              For a two-time level scheme (semi-Lagrangian), TDT is always TSTEP.

!     RDTSA  : TDT  /RA
!     RDTSA2 : RDTSA**2
!     RDTS62 : RDTSA**2/6
!     RDTS22 : RDTSA**2/2

!     RTDT   : TDT

INTEGER(KIND=JPIM) :: NSTART
INTEGER(KIND=JPIM) :: NSTOP
CHARACTER(LEN=8)   :: CSTOP !! added olivier
INTEGER(KIND=JPIM) :: NSTADD
INTEGER(KIND=JPIM) :: NSTASS
INTEGER(KIND=JPIM) :: NFOST
REAL(KIND=JPRB) :: RSTATI
REAL(KIND=JPRB) :: RTIMTR
REAL(KIND=JPRB) :: RHGMT
REAL(KIND=JPRB) :: REQTIM
REAL(KIND=JPRB) :: RSOVR
REAL(KIND=JPRB) :: RDEASO
REAL(KIND=JPRB) :: RDECLI
REAL(KIND=JPRB) :: RWSOVR
REAL(KIND=JPRB) :: RIP0
REAL(KIND=JPRB) :: RCODEC
REAL(KIND=JPRB) :: RSIDEC
REAL(KIND=JPRB) :: RCOVSR
REAL(KIND=JPRB) :: RSIVSR
REAL(KIND=JPRB) :: RCODECN
REAL(KIND=JPRB) :: RSIDECN
REAL(KIND=JPRB) :: RCOVSRN
REAL(KIND=JPRB) :: RSIVSRN
REAL(KIND=JPRB) :: RCODECF
REAL(KIND=JPRB) :: RSIDECF
REAL(KIND=JPRB) :: RCOVSRF
REAL(KIND=JPRB) :: RSIVSRF
REAL(KIND=JPRB) :: TSTEP
REAL(KIND=JPRB) :: TDT
REAL(KIND=JPRB) :: RDTSA
REAL(KIND=JPRB) :: RDTSA2
REAL(KIND=JPRB) :: RDTS62
REAL(KIND=JPRB) :: RDTS22
REAL(KIND=JPRB) :: RTDT
REAL(KIND=JPRB) :: RDECLU
REAL(KIND=JPRB) :: RTMOLT
REAL(KIND=JPRB) :: RIP0LU
REAL(KIND=JPRB) :: RCODECLU
REAL(KIND=JPRB) :: RSIDECLU
REAL(KIND=JPRB) :: RCOVSRLU
REAL(KIND=JPRB) :: RSIVSRLU

CONTAINS
  
   

END TYPE TRIP

!     --------------------------------------------------------------------------------
CONTAINS 
  


!!TYPE(TRIP), POINTER :: YRRIP => NULL()

!     --------------------------------------------------------------------------------

END MODULE YOMRIP

MODULE YOE_CUCONVCA

! Modified : 
!    R. El Khatib 09-Mar-2012 Allocate RCUCONVCA/RNLCONVCA for safe bound checkings later
!    L. Bengtsson 05-Aug-2014 Correction if using with GOL
!    L. Gerard    31-Mar-2016 Add RCADELX
USE PARKIND1 , ONLY : JPIM, JPRB
USE RANDOM_NUMBERS_MIX, ONLY: RANDOMNUMBERSTREAM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------

!*    control parameters for cellular automaton for convection

!      yd_random_stream_CA: random number stream for CA update
!      NIJH         : Multiplicative for number of points in high resolution grid
!      NBmax        : maximum possible number of neighbours for every gridcell
!      RCADELX      : Length of individual CA cell (meters) to compute NIJH
!      RCUCONVCA    : Array for interaction with the physics
!      RNLCONVCA    : Array for interaction with the physics
!      RCAPECONVCA  : Array to store "good" CAPE between its calculation [MOD(KSTEP*TSPHY,3600._JPRB)]
!      NCELLCU      : Lat-lon grid for cellular automaton
!      NFERTCU      : Lat-lon grid for cellular automaton (fertile cell indicator)
!      NBLS         : neighbours for large scale grid (= model red gaussian grid)
!      NBSS         : neighbours for small scale grid (= NIJH*NIJH cells in every LS cell)
!      NRNBLS       : number of neighbours for large scale cells
!      NRNBSS       : number of neighbours for small scale cells
!      NBNS         : neighbour north/south or on same lat (1/-1/0)
!      NBEW         : neighbour east/west or on same long (1/-1/0)
!      RWASALIVE    : Array indicating grid-points where CA was alive before
!      RWGHTCU      : Weighted and smoothed CA pattern
!      LCUCONV_CA   : TRUE IF CELLULAR AUTOMATON USED IN CONVECTION
!      LCA_ADVECT   : switch for "kind of" semi-Lagrangian advection
!      LCA_GLOBAL   : switch for global CA instead ov "convective" CA
!      LCA_SMOOTH   : swith for smoothing CA on large scale grid
!      LCA_RANTROP  : switch for random coupling of CA to deep convection (concerns initialization)
!      LCA_TEST     : switch for initialize CA at single point
!      LCA_ADVTEST  : switch for advection test (CA only evolved first 5 steps)
!      NTESTPROC    : set on which processor the single point should lie
!      NTESTGP      : set which gridpoint the single point is on
!      NSPINUP      : set number of spin-up cycles for global pattern
!      CA_FORC      : switch to choose convective forcing
!      CA_WIND      : switch to choose CA-wind (real/idealized)
!      CA_PROB      : switch to choose probabilities
!      NLIVES       : switch to choose number of lives (scaled by CAPE in callpar)
!      NFERTYRS     : switch to choose max number of steps a cell can be fertile
!      NFRCASEED    : Frequency of seeding CA where physics diagnoses deep convection
!      RCA_SEEDPROB : Probability of random seeding for global CA

!      RPROB_SURVIVE: probabilities for cell survival
!      RPROB_BIRTH: probabilities for cell birth
!      RPROB_FUN_FERT_S: function for specifying probabilities for survival as function of upwind(uw)+downwind(dw) neighbours
!      RPROB_FUN_UW_DW_S: function for specifying probabilities for survival as function of upwind(uw)-downwind(dw) neighbours
!      RPROB_FUN_FERT_B: function for specifying probabilities for birth as function of upwind(uw)+downwind(dw) neighbours
!      RPROB_FUN_UW_DW_B: function for specifying probabilities for birth as function of upwind(uw)-downwind(dw) neighbours

!      SLLONG(YRSL%NASLB1)       : Array containing the longitude for each gridpoint in a semi/lagrangian buffer array
!      SLLAT(YRSL%NASLB1)        : Array containing the latitude for each gridpoint in a semi/lagrangian buffer array
!      SLDLONG(YRSL%NASLB1)      : Array containing the width of each gridbox in longitudinal direction 
!                             in a semi/lagrangian buffer array
!      SLDLAT(YRSL%NASLB1)       : Array containing the width of each gridbox in latitudinal direction 
!                             in a semi/lagrangian buffer array
!      SLDDLAT(YRSL%NASLB1)      : Array containing the difference of the gridbox's northern boundary latitude 
!                             and the latitude of the gp (necessary because latitudes are not uniformly distributed)

INTEGER(KIND=JPIM)            :: NIJH=4
INTEGER(KIND=JPIM), PARAMETER :: NBMAX=8

TYPE :: TECUCONVCA
TYPE (RANDOMNUMBERSTREAM)     :: YD_RANDOM_STREAM_CA
INTEGER(KIND=JPIM)            :: NLIVES
INTEGER(KIND=JPIM)            :: NFERTYRS
INTEGER(KIND=JPIM)            :: NSPINUP

INTEGER(KIND=JPIM),ALLOCATABLE:: NCELLCU(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NFERTCU(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NBLS(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NBSS(:,:,:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NRNBLS(:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NRNBSS(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NBEW(:,:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NBNS(:,:,:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RCUCONVCA(:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RNLCONVCA(:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RCAPECONVCA(:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RWASALIVE(:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RWGHTCU(:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RLATLONNBLS(:,:,:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RLATLONNBSS(:,:,:,:)

REAL(KIND=JPRB)               :: RPROB_SURVIVE(0:8,0:8)
REAL(KIND=JPRB)               :: RPROB_BIRTH(0:8,0:8)
REAL(KIND=JPRB)               :: RPROB_FUN_FERT_S(0:16)
REAL(KIND=JPRB)               :: RPROB_FUN_UW_DW_S(0:16)
REAL(KIND=JPRB)               :: RPROB_FUN_FERT_B(0:16)
REAL(KIND=JPRB)               :: RPROB_FUN_UW_DW_B(0:16)

REAL(KIND=JPRB)   ,ALLOCATABLE:: RLONDEP(:,:,:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: RLATDEP(:,:,:)

REAL(KIND=JPRB)               :: RCA_SEEDPROB
REAL(KIND=JPRB)               :: RCADELX

LOGICAL                       :: LCUCONV_CA
LOGICAL                       :: LCA_ADVECT
LOGICAL                       :: LCA_GLOBAL
LOGICAL                       :: LCA_SMOOTH
LOGICAL                       :: LCA_TEST,LCA_ADVTEST
LOGICAL                       :: LCA_RANTROP
LOGICAL                       :: LCA_NBDEBUG
LOGICAL                       :: LCA_EXTRACT
CHARACTER(LEN=10)             :: CA_FORC
CHARACTER(LEN=10)             :: CA_WIND
CHARACTER(LEN=10)             :: CA_PROB
INTEGER(KIND=JPIM)            :: NFRCASEED
INTEGER(KIND=JPIM)            :: NTESTPROC, NTESTGP
INTEGER(KIND=JPIM)            :: NDXUNREAL, NDYUNREAL

REAL(KIND=JPRB)   ,ALLOCATABLE:: SLLONG(:), SLLAT(:)
REAL(KIND=JPRB)   ,ALLOCATABLE:: SLDLONG(:), SLDLAT(:), SLDDLAT(:)
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TECUCONVCA


!     ------------------------------------------------------

CONTAINS



!     ------------------------------------------------------
!     CA-initialization
!       - read NAMELIST
!       - if CA active initialize constants and allocate arrays
!     ------------------------------------------------------



!     ------------------------------------------------------
!     prepare arrays containing latitude, longitude and 
!     gridbox size for every gridbox in SL-buffer
!     ------------------------------------------------------



!     ------------------------------------------------------
!     calculate neighbours on reduced gaussian grid
!     for each gridcell on this processor
!       - neighbour given in terms of index in the SL-buffer
!     ------------------------------------------------------



!     ------------------------------------------------------
!     initialize CA state
!       - set KCELL=KLIVES and KFERT=NFERTYRS where KINI == 1
!       - used for initialization at first timestep and for
!         seeding the CA with "new" cells later on
!     ------------------------------------------------------



!     ------------------------------------------------------
!     update CA
!       - evolve CA according to the probabilities set in the
!         initial setup
!       - advect CA if LD_ADVECT is set
!       - average to coarse CA grid
!     ------------------------------------------------------



!     ------------------------------------------------------
!     - average over NIJHxNIJH cell blocks, smooth and weight
!     ------------------------------------------------------



!     ------------------------------------------------------
!       apply smoothing
!     ------------------------------------------------------



!     ------------------------------------------------------
!     perform advection (using departure points from SL-advection)
!     ------------------------------------------------------


!     ------------------------------------------------------
!     binary 2D output for debuging
!     ------------------------------------------------------



END MODULE YOE_CUCONVCA

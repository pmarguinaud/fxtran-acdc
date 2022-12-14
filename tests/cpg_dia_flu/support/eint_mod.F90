MODULE EINT_MOD

!--------------------------------------------------------------------------
! EINT - Externalisable part of horizonal interpolators and halo management
!        used for example in the semi-Lagrangian scheme
!
!   Modifications.
!   --------------
!     M. Fisher   7-March-2012 Use DEALLOCATE_IF_ASSOCIATED
!     T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!     R. El Khatib 01-Jun-2022 Remove JPDUP
!--------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE
SAVE

PRIVATE
PUBLIC SL_STRUCT

TYPE SL_STRUCT

  ! CVER: two letter code describing the SL_STRUCT version
  !             'SL': SL advection
  !             'FP': Fullpos
  !             'OB': Observation
  !             'OA': Observation (adjoint)
  !             'RI': Radiation input
  !             'RO': Radiation output
  CHARACTER(LEN=2) :: CVER='XX'

  ! NSLGROUP: flag to allow different coding
  INTEGER(KIND=JPIM) :: NSLGROUP=0

  ! NSLSTA: interpolation buffer start position of grid columns.
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLSTA(:)   

  ! NSLONL: interpolation buffer number of grid column on latitudes.
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLONL(:)   

  ! NSLOFF: interpolation buffer offset to start of each row in.
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLOFF(:)   

  ! NSLPTSWEST: number of grid points on western side of core points
  !             only for LCOMPLAT=F
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLPTSWEST(:)   

  ! NSLPTSEAST: number of grid points on esatern side of core points
  !             only for LCOMPLAT=F
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLPTSEAST(:)   

  ! NSLEXT: pointer that makes sure addressing of points in the east-west
  !         extension zone is correct. It also handles the half latitude 
  !         shift of extension latitudes at the poles.
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLEXT(:,:) 

  ! LSLWHOLELAT: records whether complete latitude is present
  LOGICAL,ALLOCATABLE :: LCOMPLAT(:) 

  ! NLATGLO: global latitude
  INTEGER(KIND=JPIM),ALLOCATABLE :: NLATGLO(:) 

  ! DIST1GP: distance in kilometres of one grid point
  REAL(KIND=JPRB),ALLOCATABLE :: DIST1GP(:) 

  ! NSLSENDPOS: the addresses within the interpolation buffer of point
  !             sent from this PE (computed only if NPROC>1).
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLSENDPOS(:)

  ! NSLRECVPOS: the addresses within the interpolation buffer of point
  !             received on this PE (computed only if NPROC>1).
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLRECVPOS(:)

  ! NSLSENDPTR: allocatable to the first point for each of the PE's that has
  !             to receive interpolation halo-data from this.
  !             Used for addressing NSLSENDPOS(). 
  !             Computed only if NPROC > 1.
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLSENDPTR(:)

  ! NSLRECVPTR: pointer to the first point for each of the PE's that are
  !             sending interpolation halo-data to this PE.
  !             Used for addressing NSLRECVPOS(). 
  !             Computed only if NPROC > 1.
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLRECVPTR(:)


  ! NSLCOMM: list of the processors this processor has to communicate
  !          with (computed only if NPROC>1).
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLCOMM(:)  

  ! LSLCOMM: flag indicating whether this processor needs to communicate 
  !          with a specific processor
  LOGICAL           ,ALLOCATABLE :: LSLCOMM(:,:)

  ! NASLB1: local inner dimension of interpolation buffer.
  INTEGER(KIND=JPIM) :: NASLB1=0

  ! NASLB1_TRUE: local inner dimension of interpolation buffer,
  !              without padding for reducing memory conflicts
  INTEGER(KIND=JPIM) :: NASLB1_TRUE=0

  ! NSLPAD: number of pad words initialised to a huge number at either
  !         of side of the sl halo, used to trap halo problems.
  INTEGER(KIND=JPIM) :: NSLPAD=0

  ! LSLT_ARRAYS_INIT : true if on-demand per timestep masks are allocated
  LOGICAL            :: LSLT_ARRAYS_INIT=.FALSE.

  ! LSLONDEM: on-demand SL communications switch
  LOGICAL            :: LSLONDEM=.FALSE.

  ! LSLONDEM_ACTIVE: on-demand SL communications active switch
  LOGICAL            :: LSLONDEM_ACTIVE=.FALSE.

  ! NSAFEHALO: minimum of unused halo grid points on a latitude (West or East)
  ! over all latitudes of the halo
  INTEGER(KIND=JPIM) :: NUNUSEDHALO = 999999

  ! DISTSAFEHALO: minimum distance in km of unused halo grid points on a latitude
  ! (West or East) over all latitudes of the halo
  REAL(KIND=JPRB) :: DISTUNUSEDHALO = 999999.0_JPRB

  ! MASKS for on-demand SL comms
  INTEGER(KIND=JPIM),ALLOCATABLE :: MASK_SL1(:)   
  INTEGER(KIND=JPIM),ALLOCATABLE :: MASK_SL2(:)   
  INTEGER(KIND=JPIM),ALLOCATABLE :: MASK_SL2T(:,:)

  ! MASK for SL debugging (LSLDEBUG=T)
  INTEGER(KIND=JPIM),ALLOCATABLE :: MASK_SLD(:)   

  ! NSLPROCS: number of processors to communicate with.
  INTEGER(KIND=JPIM) :: NSLPROCS=0

  ! NSLRPT: dimension for NSLRECVPOS (the number of columns received
  !         from other PE's when computing the halo for interpolations).
  INTEGER(KIND=JPIM) :: NSLRPT=0

  ! NSLSPT: dimension for NSLSENDPOS (the number of columns sent
  !         to other PE's when computing the halo for interpolations).
  INTEGER(KIND=JPIM) :: NSLSPT=0

  ! NSLWIDEN,NSLWIDES,NSLWIDEE,NSLWIDEW: number of grid points required for halo
  !                                      (resp. north, south, east, west).
  INTEGER(KIND=JPIM) :: NSLWIDEN=0
  INTEGER(KIND=JPIM) :: NSLWIDES=0
  INTEGER(KIND=JPIM) :: NSLWIDEE=0
  INTEGER(KIND=JPIM) :: NSLWIDEW=0

  ! NSLWIDE number of grid points required for halo
  INTEGER(KIND=JPIM) :: NSLWIDE=0

  ! NMAP: temporary data structure only for SL adjoint reproducibility
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLMAP(:,:)     

  ! NSLCORE: pointer to this processors core region points within the
  !          interpolation buffer.
  INTEGER(KIND=JPIM),ALLOCATABLE :: NSLCORE(:)  

  ! LSLCORE: T if NSLCORE point, F if not
  LOGICAL,ALLOCATABLE            :: LSLCORE(:)  

  ! MASK_SLTOT: sum of sl buffer points used in trajectory calc.
  !             used to optimise ngptotad and thereby improve performance
  INTEGER(KIND=JPIM),ALLOCATABLE :: MASK_SLTOT(:)

  ! Copy of some variables to simplify the call tree for the SL interface
  INTEGER(KIND=JPIM) :: NDGLG=0
  INTEGER(KIND=JPIM) :: NDLON=0
  INTEGER(KIND=JPIM) :: NDGSAG=0
  INTEGER(KIND=JPIM) :: NDGENG=0
  INTEGER(KIND=JPIM) :: NDGSAL=0
  INTEGER(KIND=JPIM) :: NDGENL=0
  INTEGER(KIND=JPIM) :: NDGSAH=0
  INTEGER(KIND=JPIM) :: NDGENH=0
  INTEGER(KIND=JPIM) :: NGPTOT=0
  INTEGER(KIND=JPIM) :: NDGUXL=0
  INTEGER(KIND=JPIM) :: NDLUNG=0
  INTEGER(KIND=JPIM) :: NDLUXG=0
  INTEGER(KIND=JPIM) :: NDGUNG=0
  INTEGER(KIND=JPIM) :: NDGUXG=0
  INTEGER(KIND=JPIM) :: NDSUR1=0
  INTEGER(KIND=JPIM) :: NDLSUR=0
  INTEGER(KIND=JPIM) :: NDGSUR=0
  INTEGER(KIND=JPIM) :: NPTRFLOFF=0
  INTEGER(KIND=JPIM) :: NFRSTLOFF=0
  INTEGER(KIND=JPIM) :: MYFRSTACTLAT=0
  INTEGER(KIND=JPIM) :: MYLSTACTLAT=0
  INTEGER(KIND=JPIM),ALLOCATABLE :: NLOENG(:)


END TYPE SL_STRUCT


! structures used to compute and fill halo for interpolations
! YRSL : semi-lagrangian advection
!! TYPE(SL_STRUCT),POINTER :: YRSL !! moved to MODEL_DYNAMICS_TYPE
! YRAD : semi-lagrangian advection (adjoint)
!! TYPE(SL_STRUCT),POINTER :: YRAD !! moved to MODEL_DYNAMICS_TYPE
! YRRI : model grid to radiation grid
!! TYPE(SL_STRUCT),POINTER :: YRRI !! moved to MODEL_PHYSICS_RADIATION_TYPE
! YRRO : radiation grid to model grid
!! TYPE(SL_STRUCT),POINTER :: YRRO !! moved to MODEL_PHYSICS_RADIATION_TYPE

! ------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------
END MODULE EINT_MOD

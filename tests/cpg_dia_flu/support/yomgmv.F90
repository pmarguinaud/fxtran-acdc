MODULE YOMGMV

! Module containing t,t-dt,t+dt gridpoint arrays (apart from GFL) for dynamics
! and all "pointers" for accesssing elements in the arrays
! GMV and GMVS are permanently allocated, GMVT1 and GMVT1S temporary

!-------------------------------------------------------------------------

USE PARKIND1,  ONLY : JPIM, JPRB
USE TYPE_GMVS, ONLY : TYPE_T0, TYPE_T9, TYPE_T1, TYPE_PH9, TYPE_GP
USE YOMLUN,    ONLY : NULERR
IMPLICIT NONE
SAVE

TYPE :: TGMV

!-------------------------------------------------------------------------

REAL(KIND=JPRB), ALLOCATABLE :: GMV   (:,:,:,:) ! Multilevel fields at t and t-dt
REAL(KIND=JPRB), ALLOCATABLE :: GMVS  (:,:,:)   ! Single level fields at t snd t-dt
REAL(KIND=JPRB), ALLOCATABLE :: GMVT1 (:,:,:,:) ! Multilevel fields at t+dt
REAL(KIND=JPRB), ALLOCATABLE :: GMVT1S(:,:,:)   ! Single level fields at t+dt 

REAL(KIND=JPRB), ALLOCATABLE :: GMV5  (:,:,:,:)       ! Multilevel fields trajectory
REAL(KIND=JPRB), ALLOCATABLE :: GMV5S (:,:,:)         ! Single level fields trajectory 
! The following two fields are for 3-D FGAT (LIDMODEL)
REAL(KIND=JPRB), ALLOCATABLE :: GMV_DEPART  (:,:,:,:) ! Multilevel fields departure
REAL(KIND=JPRB), ALLOCATABLE :: GMVS_DEPART (:,:,:)   ! Single level fields departure

INTEGER(KIND=JPIM) :: NDIMGMV ! Third dim. of GMV "(NPROMA,NFLEVG,NDIMGMV,NGPBLKS)"
INTEGER(KIND=JPIM) :: NDIMGMVS ! Second dim. GMVS "(NPROMA,NDIMGMVS,NGPBLKS)"

!-------------------------------------------------------------------------

TYPE(TYPE_T0)  :: YT0  ! Pointers to time t quantities
TYPE(TYPE_T9)  :: YT9  ! Pointers to time t-dt quantities
TYPE(TYPE_T1)  :: YT1  ! Pointers to time t+dt quantities
TYPE(TYPE_PH9) :: YPH9 ! Pointers to physics time t-dt quantities
TYPE(TYPE_GP)  :: YGP  ! Pointers (offsets) for individual fields in GMV grid-point arrays

TYPE(TYPE_T0)  :: YT5  ! Pointers to trajectory quantities

!-------------------------------------------------------------------------

END TYPE TGMV

!-------------------------------------------------------------------------

CONTAINS



!-------------------------------------------------------------------------



!-------------------------------------------------------------------------



!-------------------------------------------------------------------------



!-------------------------------------------------------------------------




!-------------------------------------------------------------------------



!-------------------------------------------------------------------------



END MODULE YOMGMV


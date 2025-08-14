MODULE YOAIOP

!$ACDC methods 


USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     --------------------------------------------------------------------
!*    *YOAIOP* - reading aerosol inherent optical properties from file
!     L. Rontu 15 July 2018
!     --------------------------------------------------------------------

TYPE TAIOP

INTEGER(KIND=JPIM) :: NAEROMR, NWAVSW, NWAVLW, NRELHU

REAL(KIND=JPRB), ALLOCATABLE :: RSWAME(:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: RLWAME(:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: RSWAAS(:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: RLWAAS(:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: RSWASS(:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: RLWASS(:,:,:)
REAL(KIND=JPRB), ALLOCATABLE :: RSWWL(:), RLWWL(:), RSUNFR(:), RRH(:), RLWWEI(:)

END TYPE TAIOP

!     --------------------------------------------------------------------

END MODULE YOAIOP

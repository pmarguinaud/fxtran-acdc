MODULE YOMGEM

!$ACDC methods 


USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

TYPE :: TGEM

!*    * Number of grid points

!     NGPTOT   : Total number of grid columns on a PE
!     NGPTOT_CAP  : Size of grid points arrays for ALADIN
!     NGPTOTMX : Maximum number of grid columns on any of the PEs
!     NGPTOTG  : Total number of grid columns on the Globe
!     NGPTOTL(NPRGPNS,NPRGPEW)  : Total number of grid columns on on each PE

INTEGER(KIND=JPIM) :: NGPTOT
INTEGER(KIND=JPIM) :: NGPTOT_CAP
INTEGER(KIND=JPIM) :: NGPTOTMX
INTEGER(KIND=JPIM) :: NGPTOTG
INTEGER(KIND=JPIM), ALLOCATABLE :: NGPTOTL(:,:)

!     ------------------------------------------------------------------
!*    Other horizontal geometry quantities:

!     ------------------------------------------------------------------

! RDELXN  :    Horizontal mesh size at a location where the mapping factor is 1.
! SLHDP   :    Factor of the actual model resolution, i.e.
!              - for plane geometry: = sqrt( (ELX/NMSMAX)^2 + (ELY/NSMAX)^2 )
!              - for spherical geometry: = 2 Pi sqrt( a**2 / (NSMAX*(NSMAX+1)) )

REAL(KIND=JPRB) :: RDELXN
REAL(KIND=JPRB) :: SLHDP

!     ------------------------------------------------------------------

!*    * Defining the transformed sphere

!     RMUCEN : MU OF THE POLE OF STRETCHING
!     RLOCEN : LONGITUDE OF THE POLE OF STRETCHING
!     RSTRET : STRETCHING FACTOR
!     NSTTYP : 1 = POLE OF STRETCHING, POLE OF THE COLLOCATION GRID
!                 AT THE NORTHERN POLE OF THE REAL EARTH.
!              2 = THE POLE OF STRETCHING IS ANYWHERE ON THE REAL EARTH
!             AND ON THE EQUATOR OF THE COLLOCATION GRID ON THE MERIDIAN PI.
!                  THE EQUATOR OF THE COLLOCATION GRID IS TANGENT
!             TO A PARALLEL OF THE EARTH.

!     NHTYP  : 0 = regular grid
!            : 2 = number of points read on namelist namrgri

!     RNLGINC: increment to get non-linear grid

!     R4JP    inverse de delta(teta) approche a l'ordre 1
!     RC2P1   RSTRET*RSTRET+1.
!     RC2M1   RSTRET*RSTRET-1.
!     RCOR0   COMPONENT (0,0) OF CORIOLIS
!     RCOR1   COMPONENT (0,1) OF CORIOLIS

!     NLOEN(NDGSAG:NDGENG)  : number of active points on a parallel
!     NLOENG(NDGSAG:NDGENG) : global version of NLOEN
!     NMEN(NDGSAG:NDGENG)   : associated cut-off wave number
!     NMENG(NDGSAG:NDGENG)   : global version of NMEN
!     NDGLU(0:MAX(NSMAX,NMSMAX)) : number of active points in an hemisphere
!                          for a given wave number m
!     NSTAGP(NGPTOT)     : start position of latitude data for boundary fields
!     NTSTAGP(NGPTOT)    : start position of latitude data for boundary fields
!                          in C+I+E zone (ALADIN).

REAL(KIND=JPRB)    :: RMUCEN
REAL(KIND=JPRB)    :: RLOCEN
REAL(KIND=JPRB)    :: RSTRET
INTEGER(KIND=JPIM) :: NSTTYP
INTEGER(KIND=JPIM) :: NHTYP
REAL(KIND=JPRB)    :: RNLGINC
REAL(KIND=JPRB)    :: R4JP
REAL(KIND=JPRB)    :: RC2P1
REAL(KIND=JPRB)    :: RC2M1
REAL(KIND=JPRB)    :: RCOR0
REAL(KIND=JPRB)    :: RCOR1

INTEGER(KIND=JPIM), ALLOCATABLE :: NLOEN(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NLOENG(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NMEN(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NMENG(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NDGLU(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NSTAGP(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NTSTAGP(:)

CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TGEM
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TGEM
PROCEDURE :: ACDC_HOST => ACDC_HOST_TGEM
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TGEM
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TGEM
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TGEM
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TGEM
END TYPE TGEM


!     ------------------------------------------------------------------

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TGEM (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TGEM), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TGEM (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TGEM), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TGEM (SELF)

IMPLICIT NONE
CLASS (TGEM), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TGEM (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TGEM), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TGEM (SELF, KLUN)

IMPLICIT NONE
CLASS (TGEM), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TGEM (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TGEM),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TGEM (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TGEM), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE YOMGEM

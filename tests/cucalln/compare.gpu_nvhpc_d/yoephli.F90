MODULE YOEPHLI

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEPHLI* CONTAINS CONSTANTS FOR THE LINEARIZED PHYSICS
!     ------------------------------------------------------------------

TYPE :: TEPHLI
LOGICAL :: LTLEVOL
LOGICAL :: LPHYLIN
LOGICAL :: LENOPERT
LOGICAL :: LEPPCFLS
LOGICAL :: LRAISANEN
LOGICAL :: LOPPTWINS

REAL(KIND=JPRB) :: RLPTRC
REAL(KIND=JPRB) :: RLPAL1
REAL(KIND=JPRB) :: RLPAL2
REAL(KIND=JPRB) :: RLPBB
REAL(KIND=JPRB) :: RLPCC
REAL(KIND=JPRB) :: RLPDD
REAL(KIND=JPRB) :: RLPMIXL
REAL(KIND=JPRB) :: RLPBETA
REAL(KIND=JPRB) :: RLPDRAG
REAL(KIND=JPRB) :: RLPEVAP
REAL(KIND=JPRB) :: RLPP00
!----------------------------------------------------------------------------
END TYPE TEPHLI
!============================================================================

END MODULE YOEPHLI

MODULE YOMSLREP

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!     USED FOR BIT REPRODUCIBILITY OF SEMI-LAGRANGIAN ADJOINT (SLAD)

!     NADMAP     : USED FOR REMAPPING POINTS IN SL BUFFER SO THAT THEY
!                : ARE UNIQUE
!     NADCORE    : INNER HALO FOR ADJOINT INTERPOLATIONS
!                : ARE UNIQUE
!     LADCORE    : T - IF ONE OF THE NGPTOTAD CORE POINTS, F OTHERWISE
!     NGPTOTAD   : NUMBER OF GRID POINTS IN INNER HALO
!     RSASIGN    : USED FOR CORRECTING SYMMETRIC / ANTISYMMETRIC PROPERTIES
!                : (MIRRORED LATITUDES NEAR POLES)

TYPE :: TSLREP
INTEGER(KIND=JPIM) :: NGPTOTAD

INTEGER(KIND=JPIM),ALLOCATABLE :: NADMAP(:)
INTEGER(KIND=JPIM),ALLOCATABLE :: NADCORE(:)

LOGICAL,ALLOCATABLE :: LADCORE(:)

REAL(KIND=JPRB),ALLOCATABLE :: RSASIGN(:,:)
 
!     ------------------------------------------------------------------
CONTAINS
  
   

END TYPE TSLREP
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------


!!TYPE(TSLREP), POINTER :: YRSLREP => NULL()

CONTAINS 
  

!     ------------------------------------------------------------------
END MODULE YOMSLREP

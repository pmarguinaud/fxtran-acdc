MODULE YOMSWE

!$ACDC methods 


USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Shallow-water equation Williamson et al tests

! ALPHASWE  : Angle ALPHA of the axis of the experiment and the axis of the Earth
! GMUCENSWE : sine  of latitude of the axis of the experiment

TYPE TSWE

REAL(KIND=JPRB) :: ALPHASWE
REAL(KIND=JPRB) :: GMUCENSWE

END TYPE

!     ------------------------------------------------------------------
END MODULE YOMSWE

MODULE PTRSLB15

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     SEMI LAGRANGIAN BUFFER 15 (for adoint and tangent linear)
!     "O" stands for "origin point", "M" stands for medium point.
!     "NH" stands for "non hydrostatic", "SI" stands for "semi-implicit". 

!     * NFLDSLB15 : Number of fields in semi-lagrangian buffer 15 (TL and adjoint)
!     * RPARSL15  : parities for extrapolar extensions for interpol. buffers.
!     * MSLBUF15  : start pointer for interpolation buffer 15.

!     * MSLB1UR05 : as MSLB1UR0 but for trajectory (used in TL/AD)
!     * MSLB1VR05 : as MSLB1VR0 but for trajectory (used in TL/AD)
!     * MSLB1ZR05 : as MSLB1ZR0 but for trajectory (used in TL/AD)
!     * MSLB1WR05 : as MSLB1WR0 but for trajectory (used in TL/AD)
!     * MSLB1UR005 : as MSLB1UR00 but for trajectory (used in TL/AD)
!     * MSLB1VR005 : as MSLB1VR00 but for trajectory (used in TL/AD)
!     * MSLB1ZR005 : as MSLB1ZR00 but for trajectory (used in TL/AD)
!     * MSLB1WR005 : as MSLB1WR00 but for trajectory (used in TL/AD)
!     * MSLB1UR95 : as MSLB1UR9 but for trajectory (used in TL/AD)
!     * MSLB1VR95 : as MSLB1VR9 but for trajectory (used in TL/AD)
!     * MSLB1U05  : as MSLB1U0 but for trajectory (used in TL/AD)
!     * MSLB1V05  : as MSLB1V0 but for trajectory (used in TL/AD)
!     * MSLB1T05  : as MSLB1T0 but for trajectory (used in TL/AD)
!     * MSLB1C05  : as MSLB1C0 but for trajectory (used in TL/AD)
!     * MSLB1SP05 : as MSLB1SP0 but for trajectory (used in TL/AD)
!     * MSLB1U95  : as MSLB1U9 but for trajectory (used in TL/AD)
!     * MSLB1V95  : as MSLB1V9 but for trajectory (used in TL/AD)
!     * MSLB1T95  : as MSLB1T9 but for trajectory (used in TL/AD)
!     * MSLB1GFL95: as MSLB1GFL9 but for trajectory (used in TL/AD)
!     * MSLB1C95  : as MSLB1C9 but for trajectory (used in TL/AD)
!     * MSLB1SP95 : as MSLB1SP9 but for trajectory (used in TL/AD)

TYPE :: TPTRSLB15
INTEGER(KIND=JPIM) :: NFLDSLB15
REAL(KIND=JPRB),ALLOCATABLE:: RPARSL15(:)
INTEGER(KIND=JPIM) :: MSLBUF15
INTEGER(KIND=JPIM) :: MSLB1UR05
INTEGER(KIND=JPIM) :: MSLB1VR05
INTEGER(KIND=JPIM) :: MSLB1ZR05
INTEGER(KIND=JPIM) :: MSLB1WR05
INTEGER(KIND=JPIM) :: MSLB1UR005
INTEGER(KIND=JPIM) :: MSLB1VR005
INTEGER(KIND=JPIM) :: MSLB1ZR005
INTEGER(KIND=JPIM) :: MSLB1WR005
INTEGER(KIND=JPIM) :: MSLB1UR95
INTEGER(KIND=JPIM) :: MSLB1VR95
INTEGER(KIND=JPIM) :: MSLB1U05
INTEGER(KIND=JPIM) :: MSLB1V05
INTEGER(KIND=JPIM) :: MSLB1T05
INTEGER(KIND=JPIM) :: MSLB1C05
INTEGER(KIND=JPIM) :: MSLB1SP05
INTEGER(KIND=JPIM) :: MSLB1U95
INTEGER(KIND=JPIM) :: MSLB1V95
INTEGER(KIND=JPIM) :: MSLB1T95
INTEGER(KIND=JPIM) :: MSLB1GFL95
INTEGER(KIND=JPIM) :: MSLB1C95
INTEGER(KIND=JPIM) :: MSLB1SP95
!--------------------------------------------------------------
CONTAINS
  
   

END TYPE TPTRSLB15
!==============================================================

!!TYPE(TPTRSLB15), POINTER :: YRPTRSLB15 => NULL()

CONTAINS 
  


END MODULE PTRSLB15

MODULE YOE_MCICA

USE PARKIND1  ,ONLY : JPRD, JPIM, JPRB

IMPLICIT NONE

SAVE
!------------------------------------------------------------------------------

TYPE :: TEMCICA

REAL(KIND=JPRB) :: XCW(1000,140)
REAL(KIND=JPRD) :: XCW_D(1000,140)
INTEGER(KIND=JPIM) :: NMCI1, NMCI2
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TEMCICA
!============================================================================

!! TYPE(TEMCICA), POINTER :: YREMCICA => NULL()

!------------------------------------------------------------------------------
! Array (with associated dimension)s used in the Raisanen et al. (2004) cloud generator

! NMcI1 : INTEGER : 1st dimension of the XCW array 
!                   (= maximum number of increments in units of cumulative propability)
! NMcI2 : INTEGER : 2nd dimension of the XCW array (= number of g-points in RRTM_LW)
! XCW   : REAL    : resulting array, precomputed in su_mcica.F90
! XCW_D : REAL    : double prec array used for reading files
!------------------------------------------------------------------------------

CONTAINS



END MODULE YOE_MCICA

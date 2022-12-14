MODULE YOPHLC

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     -----------------------------------------------------------------
!*    ** *YOPHLC* - SWITCHES FOR SIMPLE PHYSICS
!     -----------------------------------------------------------------

!        * E.C.M.W.F. PHYSICS PACKAGE *

TYPE :: TPHLC
REAL(KIND=JPRB) :: ALPHA
REAL(KIND=JPRB) :: AH0
REAL(KIND=JPRB) :: USTARL
REAL(KIND=JPRB) :: USTARS
REAL(KIND=JPRB) :: ALANDZ0
REAL(KIND=JPRB) :: ASEAZ0
LOGICAL :: LSPHLC
LOGICAL :: LVDFLC
LOGICAL :: LSDRLC
LOGICAL :: LZMCON
LOGICAL :: LKEXP
LOGICAL :: LVDFDS
LOGICAL :: LSDRDS
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TPHLC
!============================================================================

!! TYPE(TPHLC), POINTER :: YRPHLC => NULL()

!     ------------------------------------------------------------------
!  NAME     TYPE     PURPOSE
!  ----  :  ----   : ---------------------------------------------------
! LSPHLC : LOGICAL : SWITCH TO TURN ON THE SIMPLE PHYSICS SCHEME
! LVDFLC : LOGICAL : TO TURN ON THE VERTICAL DIFFUSION ON U,V
! LVDFDS : LOGICAL : TO TURN ON THE VERT DIFF ALSO ON THE DRY STATIC ENERGY
! LSDRLC : LOGICAL : TO TURN ON THE SURFACE DRAG ON U,V
! LSDRDS : LOGICAL : TO TURN ON THE SURFACE DRAG ALSO ON THE DRY STATIC ENERGY
! LZMCON : LOGICAL : IF TRUE THE VERT DIFF COEFF K IS CONSTANT
! LKEXP  : LOGICAL : EXPONENTIAL APPROXIMATION OF THE CONSTANT K
! ALPHA  : REAL    : COEFFICIENT FOR THE IMPLICIT TIME STEP SCHEME
! AH0    : REAL    : REFERENCE HEIGHT FOR CONSTANT VERT DIFF (M)
! USTARL : REAL    : CONSTANT USTAR OVER LAND (M SEC-1)
! USTARS : REAL    : CONSTANT USTAR OVER SEA (M SEC-1)
! ALANDZ0: REAL    : CONSTANT Z0 OVER LAND (M)
! ASEAZ0 : REAL    : CONSTANT Z0 OVER SEA (M)
! NDIFLC : INTEGER : CONTROL DIFFERENT VERT DIFF SCHEMES
!                   - 1: CALL VDIFLCZ (DIRECT+TL+ADJ):
!                        X(T+1)-X(T-1)=K*X(T+1)
!                        K=CONSTANT
!                   - 2: CALL VDIFLCZ1 (DIRECT):
!                        X(T*)-X(T)=K(X(T))*(ALPHA*X(T*)+(1-ALPHA)*X(T))
!                        DX=X(T*)-X(T)
!                        X(T+1)=X(T-1)+2*DX
!     -----------------------------------------------------------------

CONTAINS



END MODULE YOPHLC

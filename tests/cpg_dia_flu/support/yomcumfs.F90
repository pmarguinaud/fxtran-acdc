MODULE YOMCUMFS

IMPLICIT NONE

SAVE

!       ----------------------------------------------------------------
!**    ** *YOMCUMFS* CONTAINS VARIABLES FOR SIMPLIFIED CONVECTION SCHEME 
!       ----------------------------------------------------------------

TYPE :: TCUMFS
LOGICAL :: LECUMFS
LOGICAL :: LREGCV
LOGICAL :: LMFCFL2_SHSTEP
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TCUMFS
!============================================================================

!!TYPE(TCUMFS), POINTER :: YRCUMFS => NULL()

!*    *YOMCUMFS* CONTAINS VARIABLES FOR SIMPLIFIED CONVECTION SCHEME 

!     P. LOPEZ      E.C.M.W.F.    09/01/2002

!     NAME            TYPE     DESCRIPTION
!     ----            ----     -----------

!***  LECUMFS         LOGICAL  SWITCH ON SIMPLIFIED CONVECTIVE MASS-FLUX SCHEME IN TRAJECTORY
!***  LREGCV          LOGICAL  SWITCH ON REGULARIZATIONS OF SIMPLIFIED CONVECTION SCHEME 
!***                           IN TANGENT-LINEAR AND ADJOINT CALCULATIONS
!***  LMFCFL2_SHSTEP  LOGICAL  SWITCH ON REDUCTION OF MASS FLUX CFL CRITERION FOR SHORT TIME STEPS 

!     ------------------------------------------------------------------
CONTAINS


END MODULE YOMCUMFS

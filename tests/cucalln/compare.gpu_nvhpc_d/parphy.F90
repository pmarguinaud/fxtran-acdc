MODULE PARPHY

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *PARPHY* CONTAINS CONSTANTS NEEDED BY *VDF....*
!     ------------------------------------------------------------------

REAL(KIND=JPRB),PARAMETER  :: RKAP=0.4_JPRB
REAL(KIND=JPRB) ,PARAMETER :: REPDU2=(0.1_JPRB)**2


!*     *PARPHY* CONTAINS CONSTANTS NEEDED BY *VDF....*
!     FOR THE COMPUTATION OF VERTICAL DIFFUSION

!     A.C.M. BELJAARS      E.C.M.W.F.    14/12/89

!     OBUKHOV-L UPDATE     ACMB          26/03/90.
!     LWDS-upate           A.Beljaars    Jan-2014   

!     NAME        TYPE     DESCRIPTION
!     ----        ----     -----------

!     *RKAP*      REAL     *VONKARMAN CONSTANT
!     *REPDU2*    REAL     *MINIMUM VELOCITY DIFFERENCE IN RI-NUMBER
!     ------------------------------------------------------------------
END MODULE PARPHY

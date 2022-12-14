MODULE YOEVDF

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEVDF* CONTAINS CONSTANTS NEEDED BY *VDF....*
!     ------------------------------------------------------------------
TYPE TVDF
REAL(KIND=JPRB) :: RLAM
REAL(KIND=JPRB) :: RVDIFTS
LOGICAL         :: LWDS 
REAL(KIND=JPRB) :: REPS1WDS 
REAL(KIND=JPRB) :: REPS2WDS 
REAL(KIND=JPRB) :: RETAWDS 
REAL(KIND=JPRB) :: RTOFDALPHA
INTEGER(KIND=JPIM) :: NSUBST
REAL(KIND=JPRB) :: RFAC_TWO_COEF
REAL(KIND=JPRB) :: RZC_H
REAL(KIND=JPRB) :: RZL_INF

!---------------------------------------------------------------------
CONTAINS
   
END TYPE TVDF
!*     *YOEVDF* CONTAINS CONSTANTS NEEDED BY *VDF....*
!     FOR THE COMPUTATION OF VERTICAL DIFFUSION

!     A.C.M. BELJAARS      E.C.M.W.F.    14/12/89

!     OBUKHOV-L UPDATE     ACMB          26/03/90.
!     LWDS-upate           A.Beljaars    Jan-2014   
!     HARMONIE-AROME upd   U. Andrae     Dec-2020

!     NAME        TYPE     DESCRIPTION
!     ----        ----     -----------

!     *RLAM*      REAL     *ASYMPTOTIC MIXING LENGTH FOR MOMENTUM
!     *RVDIFTS*   REAL     *FACTOR FOR TIME STEP WEIGHTING IN *VDF....*
!     *LWDS*      LOGICAL  .T. for Wood/Diamantakis/Staniforth scheme      
!     *REPS1WDS*  REAL     Epsilon1 in WDS       
!     *REPS2WDS*  REAL     Epsilon2 in WDS         
!     *RETAWDS*   REAL     Eta in WDS         
!     *NSUBST*    INTEGER  Number of substeps in VDF           
!     ------------------------------------------------------------------

 !---------------------------------------------------------------------

CONTAINS 
  


END MODULE YOEVDF

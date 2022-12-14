MODULE YOERDI

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     -----------------------------------------------------------------
!*    ** *YOERDI* - COEFFICIENTS WITHIN RADIATION INTERFACE
!     -----------------------------------------------------------------

TYPE :: TERDI
REAL(KIND=JPRB) :: RRAE
REAL(KIND=JPRB) :: RSUNDUR
REAL(KIND=JPRB) :: RCARDI
REAL(KIND=JPRB) :: RCH4
REAL(KIND=JPRB) :: RN2O
REAL(KIND=JPRB) :: RNO2
REAL(KIND=JPRB) :: RO3
REAL(KIND=JPRB) :: RCCL4
REAL(KIND=JPRB) :: RCFC11
REAL(KIND=JPRB) :: RCFC12
REAL(KIND=JPRB) :: RCFC22
REAL(KIND=JPRB) :: REPCLC
REAL(KIND=JPRB) :: REPH2O
REAL(KIND=JPRB) :: RCCO2, RCCH4, RCN2O, RCNO2, RCCFC11, RCCFC12, RCCFC22, RCCCL4
REAL(KIND=JPRB) :: RSOLINC
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TERDI
!============================================================================

!!TYPE(TERDI), POINTER :: YRERDI => NULL()

!        * E.C.M.W.F. PHYSICS PACKAGE *

!     Original  J.-J. MORCRETTE       E.C.M.W.F.      89/07/14
!     Modified  P. Viterbo    99/03/26    Surface tiling
!     Modified  P. Viterbo    24/05/2004  surf library
!     Modified JJMorcrette    2005/01/19  GHG and Solar constant variability

!  NAME     TYPE     PURPOSE
!  ----  :  ----   : ---------------------------------------------------
! RRAE   : EFFECT OF EARTH'S CURVATURE ON COSINE SOLAR ZENITH ANGLE
! RSUNDUR: MINIMUM DIRECT SOLAR FOR COMPUTING SOLAR DURATION
!
! RCARDI, RCH4, RN2O, RNO2, RO3, RCFC11, RCFC12, RCFC22, RCCL4
!    Mass mixing ratios of trace gases (CARDI=Carbon Dioxide), updated
!    each timestep in UPDRGAS (if LHGHG=TRUE).  These can be thought
!    of as annual-mean concentrations at the surface, and are used to
!    scale the monthly mean latitude-pressure climatologies
!
! RCCO2, RCCH4, RCN2O, RCCFC11, RCCFC12, RCCFC22, RCCCL4
!    If LHGHG=FALSE then instead we assume the gas concentrations are
!    constant with time and are taken from these *volume* mixing
!    ratios, which are set in SUECRAD and may be overridden in the
!    NAERAD namelist.
!
! REPCLC : SECURITY TO AVOID ZERO OR ONE CLOUD COVERS
! REPH2O : SECURITY TO AVOID WATER VAPOUR CONTENT IN A LAYER
!          TO BE MORE THAN THE RESPECTIVE VALUE AT SATURATION.
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------

CONTAINS



END MODULE YOERDI

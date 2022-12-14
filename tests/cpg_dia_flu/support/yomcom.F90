MODULE YOMCOM

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK

IMPLICIT NONE

SAVE

!-----------------------------------------------------------------------
!**** *SUMCC* * - MODULE INCLUDING VARIABLES FOR THE COM COUPLING

!     AUTHOR.
!     -------
!        Jean-Philippe Piedelievre CNRM/GMGEC/EAC

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 99-02-24
!       03-04-08  Philippe Piedelievre CNRM/GMGEC/EAC
!-----------------------------------------------------------------------

TYPE :: TCOM

! NVCOM     : number of parameters for the COM scheme.

INTEGER(KIND=JPIM) :: NVCOM

!*    DIAGNOSTICS POUR COUPLAGE COM

! OMLDTH    PROFONDEUR DE LA COUCHE DE MELANGE OCEANIQUE
! GTTLIN    GRADIENT THERMIQUE DE LA THERMOCLINE
! SSTPRE    SST DYNAMIQUE
! SSTMSK    MASQUE DE MIXAGE DYNAMIQUE-STATISTIQUE
! LOMLDTH   ??? (missing comment)

REAL(KIND=JPRB),ALLOCATABLE:: OMLDTH(:)
REAL(KIND=JPRB),ALLOCATABLE:: GTTLIN(:)
REAL(KIND=JPRB),ALLOCATABLE:: SSTPRE(:)
REAL(KIND=JPRB),ALLOCATABLE:: SSTMSK(:)

LOGICAL :: LOMLDTH

! TRAFLX         : correction flux for transports.
REAL(KIND=JPRB),ALLOCATABLE:: TRAFLX(:)
 !---------------------------------------------------------------------
CONTAINS
  
   
 
END TYPE TCOM
!======================================================================
!!TYPE(TCOM), POINTER :: YRCOM => NULL()

CONTAINS 
  


!     -----------------------------------------------------------------
END MODULE YOMCOM

MODULE YOMARPHY

USE PARKIND1, ONLY : JPIM

IMPLICIT NONE

SAVE

TYPE :: TARPHY
!*
!    -----------------------------------------------------------------

!    VARIABLES DE CONTROLE DE LA PHYSIQUE AROME :
!    CONTROL VARIABLES FOR THE AROME PHYSICS:

!    LMPA    : CLE GENERALE POUR LA PHYSIQUE AROME
!            : GLOBAL SWITCH FOR AROME PHYSICS
!    LMICRO  : CLE D'APPEL DE LA MICROPHYSIQUE AROME (ICE3/4 OR LIMA)
!              SWITCH FOR AROME MICROPHYSICS SCHEME (ICE3/4 OR LIMA)

!    LTURB   : CLE D'APPEL DU SCHEMA DE TURBULENCE
!              SWITCH FOR THE TURBULENCE SCHEME
!    LMSE    : CLE D'APPEL DU SHEMA DE SURFACE EXTERNALISE
!              SWITCH FOR THE EXTERNALIZED SURFACE SCHEME
!    LKFBCONV: Control key to KFB convection scheme (deep and/or shallow)
!    LKFBD   : Control key to KFB deep convection 
!    LKFBS   : Control key to KFB shallow convection 
!    LMFSHAL : Control key for Shallow Convection Mass Flux scheme (Pergaud et al, 2008?)
!    LUSECHEM: Contol key for calling the Gas Chemistry scheme
!    LINITCHEM: Contol key to initialise  Gas Chemistry concentration
!    LORILAM : Contol key for calling the Aerosol Chemistry scheme
!    LINITORILAM: Contol key to initialise  Aerosol Chemistry concentration
!    LRDUST   : Contol key for calling the desertic aerosols
!    LINITDUST: Contol key to initialise  Desertic aerosol concentration
!    LRDEPOS  : Contol key for calling aerosol scavenging
!    LRCO2    : Contol key for calling the CO2 gas concentration
!    LBUFLUX  : If TRUE fluxes are calculated in AROEND_BUDGET,
!               if FALSE, tendencies remain 
!   CCOUPLING : Type of SURFEX coupling. E - explicit, I - implicit 
!   NSURFEXCTL : disable surfex calculations & initialization (0 == disable
!   SURFEX, 1 == run SURFEX setup, 2 == run SURFEX setup + calculations,
!   3 = run SURFEX setup, calculations, use SURFEX output)
!   LSURFEX_CRITICAL : Run SURFEX calculations in a critical section
!   NSURFEX_ITER     : Run SURFEX calculations several times (test harness)
!   LSURFEX_KFROM    : Use TRUE/FALSE in GOTO_SURFEX from within OpenMP (debug
!   purposes)
!    LMDUST  : Contol key for calling the desertic aerosols IN ALADIN

LOGICAL :: LMPA
LOGICAL :: LMICRO

LOGICAL :: LTURB
LOGICAL :: LMSE
LOGICAL :: LKFBCONV
LOGICAL :: LKFBD,LKFBS
LOGICAL :: LMFSHAL
LOGICAL :: LUSECHEM
LOGICAL :: LORILAM
LOGICAL :: LRDUST
LOGICAL :: LRCO2
LOGICAL :: LINITCHEM
LOGICAL :: LINITORILAM
LOGICAL :: LINITDUST
LOGICAL :: LRDEPOS
LOGICAL :: LBUFLUX
CHARACTER(LEN=1) :: CCOUPLING
LOGICAL :: LMDUST
LOGICAL :: LSURFEX_CRITICAL
LOGICAL :: LSURFEX_KFROM
INTEGER(KIND=JPIM) :: NSURFEX_ITER

END TYPE TARPHY

!!TYPE(TARPHY), POINTER :: YRARPHY => NULL()

!    -------------------------------------------------------------------
END MODULE YOMARPHY


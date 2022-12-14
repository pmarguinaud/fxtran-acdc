MODULE YOMARPHY

USE PARKIND1, ONLY : JPIM

IMPLICIT NONE

SAVE

TYPE :: TARPHY
!*
!    -----------------------------------------------------------------

!    VARIABLES DE CONTROLE DE LA PHYSIQUE AROME :
!    CONTROL VARIABLES FOR THE AROME PHYSICS:

LOGICAL :: LMPA        ! Global switch for AROME physics coming from MesoNH (turbulence, microphysic, shallow-convection)
LOGICAL :: LMSE        ! Global switch for surfex use

LOGICAL :: LMICRO      ! Global switch for AROME microphysics 
LOGICAL :: LTURB       ! Global switch for AROME turbulence
LOGICAL :: LKFBCONV    ! Global switch for Kain-Fritch Convection
LOGICAL :: LKFBD       ! Control key to KFB deep convection
LOGICAL :: LKFBS       ! Control key to KFB shallow convection
LOGICAL :: LMFSHAL     ! Control key for Shallow Convection Mass Flux scheme (Pergaud et al, 2008)
LOGICAL :: LUSECHEM    ! Contol key for calling the Gas Chemistry scheme
LOGICAL :: LORILAM     ! Contol key for calling the Aerosol Chemistry scheme 
LOGICAL :: LRDUST      ! Contol key for calling the desertic aerosols scheme
LOGICAL :: LRCO2       ! Contol key for calling the CO2 gas concentration
LOGICAL :: LINITCHEM   ! Contol key to initialise  Gas Chemistry concentration
LOGICAL :: LINITORILAM ! Contol key to initialise  Aerosol Chemistry concentration
LOGICAL :: LINITDUST   ! Contol key to initialise  Desertic aerosol concentration
LOGICAL :: LRDEPOS     ! Contol key for calling aerosol scavenging
LOGICAL :: LBUFLUX     ! If TRUE fluxes are calculated in AROEND_BUDGET, if FALSE, tendencies remain
CHARACTER(LEN=1) :: CCOUPLING ! Type of SURFEX coupling. E - explicit, I - implicit
LOGICAL :: LMDUST      ! Contol key for calling the desertic aerosols IN ALADIN
LOGICAL :: LSURFEX_KFROM ! Use TRUE/FALSE in GOTO_SURFEX from within OpenMP (debug purposes)
LOGICAL :: LGRADHPHY
INTEGER(KIND=JPIM) :: NFLDCORE=0 ! Number of 3D fields in core array to compute gradients
INTEGER(KIND=JPIM) :: NGRADIENTS=0 ! Number of 3D gradients to compute from the fields in core

END TYPE TARPHY


!    -------------------------------------------------------------------
END MODULE YOMARPHY


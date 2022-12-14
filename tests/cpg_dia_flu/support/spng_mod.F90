!OCL  NOEVAL
MODULE SPNG_MOD

! Purpose :
! -------
!    Module for sponge applied at the top of the models.
!    This new sponge scheme can be used in 3D models.
!    - sponge is done in spectral space for GMV and spectral GFL.
!    - sponge is done in grid-point space for grid-point GFL.
!    Replaces the old sponge under NSPONGE=2

! Interface :
! ---------
!    Empty.

! External :  
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------

! Author :
! ------
!    K. YESSAD (CNRM/GMAP), after old sponge
!    Original : October 2011

! Modifications :
! -------------
!    T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!    K. Yessad (July 2014): Move some variables.
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK
USE YOMCST   , ONLY : RPI

IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITIONS
!            ----------------

! none for the time being.

!=============================================================================

!      2.    DECLARATIONS
!            ------------

! Namelist variables:
! * LNSPONGE : to switch on new sponge:
! * RSPONBT  : altitude of the bottom of the sponge layer.
! * RSPONTP  : altitude of the top of the sponge layer.
! * RSPONTAU : time scale of absorption.
! * RSPONGN  : minimum value for RSPONGF.
! Other variables:
! * RSPONGF  : sponge function at full levels.
! Remarks:
! * for RSPONBT,RSPONTP,RSPONTAU,RSPONGN,RSPONGF: index 1 for GMV, 2 for sp. GFL, 3 for gp. GFL.

TYPE :: TSPNG
LOGICAL :: LNSPONGE
REAL(KIND=JPRB), ALLOCATABLE :: RSPONGF(:,:)

END TYPE TSPNG

!!TYPE(TSPNG), POINTER :: YRSPNG => NULL()


END MODULE SPNG_MOD

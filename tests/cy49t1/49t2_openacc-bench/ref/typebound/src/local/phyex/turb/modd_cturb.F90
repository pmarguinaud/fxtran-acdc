!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!      #######################
          MODULE MODD_CTURB

!$ACDC methods 

!      #######################
!
!!****   *MODD_CTURB*  - declaration of the turbulent scheme constants
!!
!!     PURPOSE
!!     -------
!        The purpose of this declarative module is to declare the 
!      turbulence scheme constants.
!
!!
!!**   IMPLICIT ARGUMENTS
!!     ------------------
!!       NONE
!!
!!     REFERENCE
!!     ---------
!!       Book 2 of Meso-NH documentation (MODD_CTURB)
!!       Book 1 of Meso-NH documentation (Chapter Turbulence)
!!
!!     AUTHOR
!!     ------
!1       Joan Cuxart         * INM  and Meteo-France *
!!
!!     MODIFICATIONS
!!     -------------
!!       Original            08/08/94
!!     Nov 06, 2002 (V. Masson)  add XALPSBL and XASBL
!!     May 06                    Remove EPS
!!     Jan 2019     (Q. Rodier)  Remove XASBL
!!     Jan 2022     (Q. Rodier)  introduction of a strucuture
!----------------------------------------------------------------------------
!
!*       0. DECLARATIONS
!           ------------
!
IMPLICIT NONE
TYPE CSTURB_t
!
REAL :: XCMFS        ! constant for the momentum flux due to shear   
REAL :: XCMFB        ! constant for the momentum flux due to buoyancy
!
REAL :: XCPR2        ! second ct. for the turbulent Prandtl numbers
REAL :: XCPR3        ! third ct. for the turbulent Prandtl numbers
REAL :: XCPR4        ! fourth ct. for the turbulent Prandtl numbers
REAL :: XCPR5        ! fifth ct. for the turbulent Prandtl numbers
!
REAL :: XCET         ! constant into the transport term of the TKE eq.
!
REAL :: XCDP         ! ct. for the production term in the dissipation eq.
REAL :: XCDD         ! ct. for the destruction term in the dissipation eq.
REAL :: XCDT         ! ct. for the transport term in the dissipation eq.
!
REAL :: XRM17        ! Rodier et al 2017 constant in shear term for mixing length
!
REAL :: XLINF        ! to prevent division by zero in the BL algorithm
!
REAL :: XALPSBL      ! constant linking TKE and friction velocity in the SBL
!
REAL :: XCEP         ! Constant for wind pressure-correlations
REAL :: XA0          ! Constant a0 for wind pressure-correlations
REAL :: XA2          ! Constant a2 for wind pressure-correlations
REAL :: XA3          ! Constant a3 for wind pressure-correlations
REAL :: XA5          ! Constant a5 for temperature pressure-correlations
REAL :: XCTD         ! Constant for temperature and vapor dissipation
!
REAL :: XPHI_LIM     ! Threshold value for Phi3 and Psi3
REAL :: XSBL_O_BL    ! SBL height / BL height ratio
REAL :: XFTOP_O_FSURF! Fraction of surface (heat or momentum) flux used to define top of BL
!
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_CSTURB_T
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_CSTURB_T
PROCEDURE :: ACDC_HOST => ACDC_HOST_CSTURB_T
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_CSTURB_T
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_CSTURB_T
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_CSTURB_T
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_CSTURB_T
END TYPE CSTURB_t
!
TYPE(CSTURB_t), TARGET, SAVE :: CSTURB
!
REAL,POINTER :: XCMFS => NULL()
REAL,POINTER :: XCMFB => NULL()
!
REAL,POINTER :: XCPR2 => NULL()
REAL,POINTER :: XCPR3 => NULL()
REAL,POINTER :: XCPR4 => NULL()
REAL,POINTER :: XCPR5 => NULL()
!
REAL,POINTER :: XCET => NULL()
!
REAL,POINTER :: XCDP => NULL()
REAL,POINTER :: XCDD => NULL()
REAL,POINTER :: XCDT => NULL()
!
REAL,POINTER :: XRM17 => NULL()
!
REAL,POINTER :: XLINF => NULL()
!
REAL,POINTER :: XALPSBL => NULL()
!
REAL,POINTER :: XCEP => NULL()
REAL,POINTER :: XA0 => NULL()
REAL,POINTER :: XA2 => NULL()
REAL,POINTER :: XA3 => NULL()
REAL,POINTER :: XA5 => NULL()
REAL,POINTER :: XCTD => NULL()
!
REAL,POINTER :: XPHI_LIM => NULL()
REAL,POINTER :: XSBL_O_BL => NULL()
REAL,POINTER :: XFTOP_O_FSURF => NULL()

INTERFACE

MODULE SUBROUTINE ACDC_COPY_CSTURB_T (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (CSTURB_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_CSTURB_T (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (CSTURB_T), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_CSTURB_T (SELF)

IMPLICIT NONE
CLASS (CSTURB_T), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_CSTURB_T (SELF, KLUN)

IMPLICIT NONE
CLASS (CSTURB_T), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_CSTURB_T (SELF, KLUN)

IMPLICIT NONE
CLASS (CSTURB_T), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_CSTURB_T (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (CSTURB_T),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_CSTURB_T (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (CSTURB_T), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

CONTAINS
SUBROUTINE CTURB_ASSOCIATE()
IMPLICIT NONE
  XCMFS=>CSTURB%XCMFS
  XCMFB=>CSTURB%XCMFB
  !
  XCPR2=>CSTURB%XCPR2
  XCPR3=>CSTURB%XCPR3
  XCPR4=>CSTURB%XCPR4
  XCPR5=>CSTURB%XCPR5
  !
  XCET=>CSTURB%XCET
  !
  XCDP=>CSTURB%XCDP
  XCDD=>CSTURB%XCDD
  XCDT=>CSTURB%XCDT
  !
  XRM17=>CSTURB%XRM17
  !
  XLINF=>CSTURB%XLINF
  !
  XALPSBL=>CSTURB%XALPSBL
  !
  XCEP=>CSTURB%XCEP
  XA0=>CSTURB%XA0
  XA2=>CSTURB%XA2
  XA3=>CSTURB%XA3
  XA5=>CSTURB%XA5
  XCTD=>CSTURB%XCTD
  !
  XPHI_LIM=>CSTURB%XPHI_LIM
  XSBL_O_BL=>CSTURB%XSBL_O_BL
  XFTOP_O_FSURF=>CSTURB%XFTOP_O_FSURF
END SUBROUTINE CTURB_ASSOCIATE
END MODULE MODD_CTURB

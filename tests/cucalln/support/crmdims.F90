MODULE CRMDIMS

!************* LICENSE START *****************
!
! Copyright 2015 Marat Khairoutdinov 
! School of Marine & Atmospheric Sciences
! Stony Brook University
! This software is distributed under the terms of
! the Apache License version 2.0.
!
!************** LICENSE END ******************

USE PARKIND1  ,ONLY : JPIM, JPRB

IMPLICIT NONE

INTEGER(KIND=JPIM), PARAMETER :: YES3DVAL = 0
INTEGER(KIND=JPIM), PARAMETER :: NCYCLE_MAX = 12  ! maximum number of time-step subcycling
REAL(KIND=JPRB), PARAMETER :: W_LIMIT_CRM = 5.
REAL(KIND=JPRB), PARAMETER :: W_LIMIT_LES = 5.
REAL(KIND=JPRB), PARAMETER :: CRM_DX=4000., CRM_DY=CRM_DX, CRM_DT=20.
#ifdef SPCRM
  INTEGER(KIND=JPIM), PARAMETER :: CRM_NX=32, CRM_NY=1, CRM_NZ=108
  INTEGER(KIND=JPIM), PARAMETER :: CRMVARS = 7  ! number of prognostic CRM fields 
  INTEGER(KIND=JPIM), PARAMETER :: RADVARS = 3  ! number of saved CRM fields for radiation computations
  INTEGER(KIND=JPIM), PARAMETER :: CRMBUFSIZE = CRM_NX*CRM_NY*CRMVARS+RADVARS
#else
  ! minimum set to keep memory low
  INTEGER(KIND=JPIM), PARAMETER :: CRM_NX=1, CRM_NY=1, CRM_NZ=74
  INTEGER(KIND=JPIM), PARAMETER :: CRMVARS = 0  ! number of prognostic CRM fields 
  INTEGER(KIND=JPIM), PARAMETER :: RADVARS = 0  ! number of saved CRM fields for radiation computations
  INTEGER(KIND=JPIM), PARAMETER :: CRMBUFSIZE = CRM_NX*CRM_NY*CRMVARS+RADVARS
#endif

END MODULE CRMDIMS

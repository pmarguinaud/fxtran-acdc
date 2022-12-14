!
! Copyright 2011 ECMWF
!
! This software was developed at ECMWF for evaluation
! and may be used for academic and research purposes only.
! The software is provided as is without any warranty.
!
! This software can be used, copied and modified but not
! redistributed or sold. This notice must be reproduced
! on each copy made.
!

!> Handle geometry for the IFS model

MODULE GEOMETRY_MOD

USE PARKIND1 , ONLY : JPIM, JPRB
USE TYPE_GEOMETRY, ONLY : GEOMETRY
USE YOMLUN, ONLY : NULOUT

IMPLICIT NONE
PRIVATE

PUBLIC :: GEOMETRY

SAVE
INTEGER(KIND=JPIM), PRIVATE :: NUSEGEOM=0

! ------------------------------------------------------------------------------

CONTAINS

END MODULE GEOMETRY_MOD

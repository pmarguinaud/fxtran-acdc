MODULE YOEGWWMS

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE  

! YOEGWWMS --  parameters/switches for the Warner McIntyre GW parameterization

!-----------------------------------------------------------------

TYPE :: TEGWWMS
LOGICAL            :: LOZPR     !If .TRUE. then enhancement of mom. flux over tropics
LOGICAL            :: LGACALC   !If .TRUE. recalculate vertical coordinate stretch for equal ratio
LOGICAL            :: LGSATL    !If .TRUE. then spectrum is saturated up to m* at launch
LOGICAL            :: LGINDL    !If .TRUE. then saturation and launch spectrum is independent of launch location
INTEGER(KIND=JPIM) :: NLAUNCHL(3)!launch levels of gravity wave spectrum (Pa)
INTEGER(KIND=JPIM) :: NLAUNCHLEV!Number of independent launch heights to be treated
INTEGER(KIND=JPIM) :: NLAUNCH   !single launch level as used for simplified
INTEGER(KIND=JPIM) :: NSLOPE    !slope at small-m end of spectrum
INTEGER(KIND=JPIM) :: NGAUSS    !if LOZPR=TRUE and GGAUSS=2 then gaussian distribution of GFLUXLAUN based on GGAUSSA and GGAUSSB
                                !if LOZPR=TRUE and GGAUSS=1 then FLUXLAUN=GCOEFF*PPRECIP
REAL(KIND=JPRB)    :: GFLUXLAUNL(3)!total launch momentum flux (Pa) in each azimuth for all launch evels
REAL(KIND=JPRB)    :: GFLUXLAUN !total launch momentum flux (Pa) in each azimuth at NLAUNCH
REAL(KIND=JPRB)    :: GMSTAR_L(3)!m* (expressed as length, see Scinocca 2003)
REAL(KIND=JPRB)    :: GCSTAR    !C* (see McLandress and Scinocca 2005)
REAL (KIND=JPRB)   :: GPTWO     !2*p where p is the exponent of omega for the expression of the launch energy density
REAL(KIND=JPRB)    :: GTPHYGWWMS!Time frequency (s) of call of scheme

REAL(KIND=JPRB)    :: GGAUSSA   !gaussian distribution half-width (used if GGAUSS=1)
REAL(KIND=JPRB)    :: GGAUSSB(3)!height of gaussian distribution (ie amplification factor)
REAL(KIND=JPRB)    :: GCOEFF    !if GGAUSS=1 then FLUXLAUN=GCOEFF*PPRECIP
!---------------------------------------------------------------------
CONTAINS
   
END TYPE TEGWWMS

!!TYPE(TEGWWMS), POINTER :: YREGWWMS => NULL()
!---------------------------------------------------------------------

CONTAINS 
  


END MODULE YOEGWWMS

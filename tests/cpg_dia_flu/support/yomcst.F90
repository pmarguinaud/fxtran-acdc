MODULE YOMCST

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

TYPE TCST
!*    Common of physical constants
!     You will find the meanings in the annex 1 of the documentation
  
! A1.0 Fundamental constants
! * RPI          : number Pi
! * RCLUM        : light velocity
! * RHPLA        : Planck constant
! * RKBOL        : Bolzmann constant
! * RNAVO        : Avogadro number
  REAL(KIND=JPRB) :: RPI
  REAL(KIND=JPRB) :: RCLUM
  REAL(KIND=JPRB) :: RHPLA
  REAL(KIND=JPRB) :: RKBOL
  REAL(KIND=JPRB) :: RNAVO
  
! A1.1 Astronomical constants
! * RDAY         : duration of the solar day
! * RDAYI        : invariant time unit of 86400s
! * RHOUR        : duration of the solar hour 
! * REA          : astronomical unit (mean distance Earth-sun)
! * REPSM        : polar axis tilting angle
! * RSIYEA       : duration of the sideral year
! * RSIDAY       : duration of the sideral day
! * ROMEGA       : angular velocity of the Earth rotation
  REAL(KIND=JPRB) :: RDAY
  REAL(KIND=JPRB) :: RDAYI
  REAL(KIND=JPRB) :: RHOUR
  REAL(KIND=JPRB) :: REA
  REAL(KIND=JPRB) :: REPSM
  REAL(KIND=JPRB) :: RSIYEA
  REAL(KIND=JPRB) :: RSIDAY
  REAL(KIND=JPRB) :: ROMEGA
  
! A1.2 Geoide
! * RA           : Earth radius
! * RG           : gravity constant
! * R1SA         : 1/RA
! * DEG2RAD      : 180./RPI
  REAL(KIND=JPRB) :: RA
  REAL(KIND=JPRB) :: RG
  REAL(KIND=JPRB) :: R1SA
  REAL(KIND=JPRB) :: DEG2RAD
  
! A1.3 Radiation
! * RSIGMA       : Stefan-Bolzman constant
! * RI0          : solar constant
  REAL(KIND=JPRB) :: RSIGMA
  REAL(KIND=JPRB) :: RI0
  
! A1.4 Thermodynamic gas phase
! * R            : perfect gas constant
! * RMD          : dry air molar mass
! * RMV          : vapour water molar mass
! * RMO3         : ozone molar mass
! * RD           : R_dry (dry air constant)
! * RV           : R_vap (vapour water constant)
! * RCPD         : Cp_dry (dry air calorific capacity at constant pressure)
! * RCPV         : Cp_vap (vapour calorific capacity at constant pressure)
! * RCVD         : Cv_dry (dry air calorific capacity at constant volume)
! * RCVV         : Cv_vap (vapour calorific capacity at constant volume)
! * RKAPPA       : Kappa = R_dry/Cp_dry
! * RETV         : R_vap/R_dry - 1
! * RMCO2        : CO2 (carbon dioxide) molar mass
! * RMCH4        : CH4 (methane) molar mass
! * RMN2O        : N2O molar mass
! * RMCO         : CO (carbon monoxide) molar mass
! * RMHCHO       : HCHO molar mass
! * RMNO2        : NO2 (nitrogen dioxide) molar mass
! * RMSO2        : SO2 (sulfur dioxide) molar mass
! * RMSO4        : SO4 (sulphate) molar mass
! * RMCFC11      : CFC11 molar mass
! * RMCFC12      : CFC12 molar mass
! * RMHCFC12     : HCFC22 molar mass
! * RMCCL4       : CCl4 molar mass
  REAL(KIND=JPRB) :: R
  REAL(KIND=JPRB) :: RMD
  REAL(KIND=JPRB) :: RMV
  REAL(KIND=JPRB) :: RMO3
  REAL(KIND=JPRB) :: RD
  REAL(KIND=JPRB) :: RV
  REAL(KIND=JPRB) :: RCPD
  REAL(KIND=JPRB) :: RCPV
  REAL(KIND=JPRB) :: RCVD
  REAL(KIND=JPRB) :: RCVV
  REAL(KIND=JPRB) :: RKAPPA
  REAL(KIND=JPRB) :: RETV
  REAL(KIND=JPRB) :: RMCO2
  REAL(KIND=JPRB) :: RMCH4
  REAL(KIND=JPRB) :: RMN2O
  REAL(KIND=JPRB) :: RMCO
  REAL(KIND=JPRB) :: RMHCHO
  REAL(KIND=JPRB) :: RMNO2
  REAL(KIND=JPRB) :: RMSO2
  REAL(KIND=JPRB) :: RMSO4
  REAL(KIND=JPRB) :: RMCFC11
  REAL(KIND=JPRB) :: RMCFC12
  REAL(KIND=JPRB) :: RMHCFC22
  REAL(KIND=JPRB) :: RMCCL4
  
! A1.5,6 Thermodynamic liquid,solid phases
! * RCW          : Cw (calorific capacity of liquid water)
! * RCS          : Cs (calorific capacity of solid water)
  REAL(KIND=JPRB) :: RCW
  REAL(KIND=JPRB) :: RCS
  
! A1.7 Thermodynamic transition of phase
! * RATM         : pre_n = "normal" pressure
! * RTT          : Tt = temperature of water fusion at "pre_n"
! * RLVTT        : RLvTt = vaporisation latent heat at T=Tt
! * RLSTT        : RLsTt = sublimation latent heat at T=Tt
! * RLVZER       : RLv0 = vaporisation latent heat at T=0K
! * RLSZER       : RLs0 = sublimation latent heat at T=0K
! * RLMLT        : RLMlt = melting latent heat at T=Tt
! * RDT          : Tt - Tx(ew-ei)
  REAL(KIND=JPRB) :: RATM
  REAL(KIND=JPRB) :: RTT
  REAL(KIND=JPRB) :: RLVTT
  REAL(KIND=JPRB) :: RLSTT
  REAL(KIND=JPRB) :: RLVZER
  REAL(KIND=JPRB) :: RLSZER
  REAL(KIND=JPRB) :: RLMLT
  REAL(KIND=JPRB) :: RDT
  
! A1.8 Curve of saturation
! * RESTT        : es(Tt) = saturation vapour tension at T=Tt
! * RGAMW        : Rgamw = (Cw-Cp_vap)/R_vap
! * RBETW        : Rbetw = RLvTt/R_vap + Rgamw*Tt
! * RALPW        : Ralpw = log(es(Tt)) + Rbetw/Tt + Rgamw*log(Tt)
! * RGAMS        : Rgams = (Cs-Cp_vap)/R_vap
! * RBETS        : Rbets = RLsTt/R_vap + Rgams*Tt
! * RALPS        : Ralps = log(es(Tt)) + Rbets/Tt + Rgams*log(Tt)
! * RALPD        : Ralpd = Ralps - Ralpw
! * RBETD        : Rbetd = Rbets - Rbetw
! * RGAMD        : Rgamd = Rgams - Rgamw
  REAL(KIND=JPRB) :: RESTT
  REAL(KIND=JPRB) :: RGAMW
  REAL(KIND=JPRB) :: RBETW
  REAL(KIND=JPRB) :: RALPW
  REAL(KIND=JPRB) :: RGAMS
  REAL(KIND=JPRB) :: RBETS
  REAL(KIND=JPRB) :: RALPS
  REAL(KIND=JPRB) :: RALPD
  REAL(KIND=JPRB) :: RBETD
  REAL(KIND=JPRB) :: RGAMD
END TYPE TCST

!*    Common of physical constants
!     You will find the meanings in the annex 1 of the documentation

! A1.0 Fundamental constants
! * RPI          : number Pi
! * RCLUM        : light velocity
! * RHPLA        : Planck constant
! * RKBOL        : Bolzmann constant
! * RNAVO        : Avogadro number
REAL(KIND=JPRB),PROTECTED :: RPI
REAL(KIND=JPRB),PROTECTED :: RCLUM
REAL(KIND=JPRB),PROTECTED :: RHPLA
REAL(KIND=JPRB),PROTECTED :: RKBOL
REAL(KIND=JPRB),PROTECTED :: RNAVO

! A1.1 Astronomical constants
! * RDAY         : duration of the solar day
! * RDAYI        : invariant time unit of 86400s
! * RHOUR        : duration of the solar hour 
! * REA          : astronomical unit (mean distance Earth-sun)
! * REPSM        : polar axis tilting angle
! * RSIYEA       : duration of the sideral year
! * RSIDAY       : duration of the sideral day
! * ROMEGA       : angular velocity of the Earth rotation
REAL(KIND=JPRB),PROTECTED :: RDAY
REAL(KIND=JPRB),PROTECTED :: RDAYI
REAL(KIND=JPRB),PROTECTED :: RHOUR
REAL(KIND=JPRB),PROTECTED :: REA
REAL(KIND=JPRB),PROTECTED :: REPSM
REAL(KIND=JPRB),PROTECTED :: RSIYEA
REAL(KIND=JPRB),PROTECTED :: RSIDAY
REAL(KIND=JPRB),PROTECTED :: ROMEGA

! A1.2 Geoide
! * RA           : Earth radius
! * RG           : gravity constant
! * R1SA         : 1/RA
! * DEG2RAD      : 180./RPI
REAL(KIND=JPRB),PROTECTED :: RA
REAL(KIND=JPRB),PROTECTED :: RG
REAL(KIND=JPRB),PROTECTED :: R1SA
REAL(KIND=JPRB),PROTECTED :: DEG2RAD

! A1.3 Radiation
! * RSIGMA       : Stefan-Bolzman constant
! * RI0          : solar constant
REAL(KIND=JPRB),PROTECTED :: RSIGMA
REAL(KIND=JPRB),PROTECTED :: RI0

! A1.4 Thermodynamic gas phase
! * R            : perfect gas constant
! * RMD          : dry air molar mass
! * RMV          : vapour water molar mass
! * RMO3         : ozone molar mass
! * RD           : R_dry (dry air constant)
! * RV           : R_vap (vapour water constant)
! * RCPD         : Cp_dry (dry air calorific capacity at constant pressure)
! * RCPV         : Cp_vap (vapour calorific capacity at constant pressure)
! * RCVD         : Cv_dry (dry air calorific capacity at constant volume)
! * RCVV         : Cv_vap (vapour calorific capacity at constant volume)
! * RKAPPA       : Kappa = R_dry/Cp_dry
! * RETV         : R_vap/R_dry - 1
! * RMCO2        : CO2 (carbon dioxide) molar mass
! * RMCH4        : CH4 (methane) molar mass
! * RMN2O        : N2O molar mass
! * RMCO         : CO (carbon monoxide) molar mass
! * RMHCHO       : HCHO molar mass
! * RMNO2        : NO2 (nitrogen dioxide) molar mass
! * RMSO2        : SO2 (sulfur dioxide) molar mass
! * RMSO4        : SO4 (sulphate) molar mass
! * RMCFC11      : CFC11 molar mass
! * RMCFC12      : CFC12 molar mass
! * RMHCFC12     : HCFC22 molar mass
! * RMCCL4       : CCl4 molar mass
REAL(KIND=JPRB),PROTECTED :: R
REAL(KIND=JPRB),PROTECTED :: RMD
REAL(KIND=JPRB),PROTECTED :: RMV
REAL(KIND=JPRB),PROTECTED :: RMO3
REAL(KIND=JPRB),PROTECTED :: RD
REAL(KIND=JPRB),PROTECTED :: RV
REAL(KIND=JPRB),PROTECTED :: RCPD
REAL(KIND=JPRB),PROTECTED :: RCPV
REAL(KIND=JPRB),PROTECTED :: RCVD
REAL(KIND=JPRB),PROTECTED :: RCVV
REAL(KIND=JPRB),PROTECTED :: RKAPPA
REAL(KIND=JPRB),PROTECTED :: RETV
REAL(KIND=JPRB),PROTECTED :: RMCO2
REAL(KIND=JPRB),PROTECTED :: RMCH4
REAL(KIND=JPRB),PROTECTED :: RMN2O
REAL(KIND=JPRB),PROTECTED :: RMCO
REAL(KIND=JPRB),PROTECTED :: RMHCHO
REAL(KIND=JPRB),PROTECTED :: RMNO2
REAL(KIND=JPRB),PROTECTED :: RMSO2
REAL(KIND=JPRB),PROTECTED :: RMSO4
REAL(KIND=JPRB),PROTECTED :: RMCFC11
REAL(KIND=JPRB),PROTECTED :: RMCFC12
REAL(KIND=JPRB),PROTECTED :: RMHCFC22
REAL(KIND=JPRB),PROTECTED :: RMCCL4

! A1.5,6 Thermodynamic liquid,solid phases
! * RCW          : Cw (calorific capacity of liquid water)
! * RCS          : Cs (calorific capacity of solid water)
REAL(KIND=JPRB),PROTECTED :: RCW
REAL(KIND=JPRB),PROTECTED :: RCS

! A1.7 Thermodynamic transition of phase
! * RATM         : pre_n = "normal" pressure
! * RTT          : Tt = temperature of water fusion at "pre_n"
! * RLVTT        : RLvTt = vaporisation latent heat at T=Tt
! * RLSTT        : RLsTt = sublimation latent heat at T=Tt
! * RLVZER       : RLv0 = vaporisation latent heat at T=0K
! * RLSZER       : RLs0 = sublimation latent heat at T=0K
! * RLMLT        : RLMlt = melting latent heat at T=Tt
! * RDT          : Tt - Tx(ew-ei)
REAL(KIND=JPRB),PROTECTED :: RATM
REAL(KIND=JPRB),PROTECTED :: RTT
REAL(KIND=JPRB),PROTECTED :: RLVTT
REAL(KIND=JPRB),PROTECTED :: RLSTT
REAL(KIND=JPRB),PROTECTED :: RLVZER
REAL(KIND=JPRB),PROTECTED :: RLSZER
REAL(KIND=JPRB),PROTECTED :: RLMLT
REAL(KIND=JPRB),PROTECTED :: RDT

! A1.8 Curve of saturation
! * RESTT        : es(Tt) = saturation vapour tension at T=Tt
! * RGAMW        : Rgamw = (Cw-Cp_vap)/R_vap
! * RBETW        : Rbetw = RLvTt/R_vap + Rgamw*Tt
! * RALPW        : Ralpw = log(es(Tt)) + Rbetw/Tt + Rgamw*log(Tt)
! * RGAMS        : Rgams = (Cs-Cp_vap)/R_vap
! * RBETS        : Rbets = RLsTt/R_vap + Rgams*Tt
! * RALPS        : Ralps = log(es(Tt)) + Rbets/Tt + Rgams*log(Tt)
! * RALPD        : Ralpd = Ralps - Ralpw
! * RBETD        : Rbetd = Rbets - Rbetw
! * RGAMD        : Rgamd = Rgams - Rgamw
REAL(KIND=JPRB),PROTECTED :: RESTT
REAL(KIND=JPRB),PROTECTED :: RGAMW
REAL(KIND=JPRB),PROTECTED :: RBETW
REAL(KIND=JPRB),PROTECTED :: RALPW
REAL(KIND=JPRB),PROTECTED :: RGAMS
REAL(KIND=JPRB),PROTECTED :: RBETS
REAL(KIND=JPRB),PROTECTED :: RALPS
REAL(KIND=JPRB),PROTECTED :: RALPD
REAL(KIND=JPRB),PROTECTED :: RBETD
REAL(KIND=JPRB),PROTECTED :: RGAMD

! NaN value
CHARACTER(LEN=8), PARAMETER :: CSNAN = &
  & CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(244)//CHAR(127)
REAL(KIND=JPRB),PROTECTED :: RSNAN

LOGICAL,PROTECTED :: L_HAS_BEEN_SETUP=.FALSE.

TYPE (TCST), TARGET :: YRCST


END MODULE YOMCST

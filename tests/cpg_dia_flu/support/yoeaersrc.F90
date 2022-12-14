MODULE YOEAERSRC

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERSRC* - CONTROL OPTIONS FOR AEROSOLS' SOURCE
!     ------------------------------------------------------------------

TYPE :: TEAERSRC
INTEGER(KIND=JPIM) :: NDDUST, NSSALT, NAERWND, NDMSO, NPIST,NSO4SCHEME

LOGICAL :: LEPAERO, LAEROMIN, LOCNDMS

REAL(KIND=JPRB) :: RGELAV, RGEMUV, RDGLAV, RDGMUV
REAL(KIND=JPRB) :: RCLONV, RSLONV, RDCLONV, RDSLONV
REAL(KIND=JPRB) :: RLATVOL,RLONVOL
REAL(KIND=JPRB) :: RBCPHIL, RBCPHOB, ROMPHIL, ROMPHOB
REAL(KIND=JPRB) :: RFCTDU, RFCTSS, RFCTDUR, RFCTSSR
REAL(KIND=JPRB) :: RAERDUB, RDMSMIN
REAL(KIND=JPRB) :: RCODECA, RSIDECA, RCOVSRA, RSIVSRA

REAL(KIND=JPRB) :: RSSFLX(9)
REAL(KIND=JPRB) :: RDDUSRC(9)
!---------------------------------------------------------------------
CONTAINS
   
END TYPE TEAERSRC
!=====================================================================

TYPE(TEAERSRC), POINTER :: YREAERSRC => NULL()

!     ------------------------------------------------------------------
! RSSFLX     : sea salt flux for (9)3-size bins (in mg m-2 s-1) for a 1 m s-1
!              wind speed, at 10 m height and 80% RH 
! Rlat/lonVOL: LAT/LON of a possible volcanic eruption
! RxxPHIL, PHOB: fractions of BC and OM hydrophilic or hydrophobic
! NDDUST, NSSALT : various formulations for dust and sea-salt emissions (test only)
! NDMSO      : 0= no oceanic DMS, 1= parametrized, 2=from source file
! NPIST      : Index for treatment of piston velocity
! RAERDUB    : base value for dust emission
! NAERWND    : 0 = 10m-wind for both DU and SS
!            : 1 = gust for SS, 10m-wind for DU
!            : 2 = gust for DU, 10m-wind for SS
!            : 3 = gust for both DU and SS
! RFCTSS, -DU: factors depending on choice of NAERWND
!     ------------------------------------------------------------------

CONTAINS



END MODULE YOEAERSRC


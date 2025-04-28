MODULE YOMCAPE

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

TYPE TCAPE
!*
!     ------------------------------------------------------------------

!     VARIABLES TO CONTROL CAPE COMPUTATION IN FULLPOS:

!        NCAPEITER : NUMBER OF ITERATIONS IN THE NEWTON LOOPS. Default
!                    is the same as NBITER in YOMPHY.

!        NETAPES : NUMBER OF INTERMEDIATE-LAYERS USED FOR CALCULATION OF
!                  VERTICAL ASCENT BETWEEN TWO MODEL PRESSURE LEVELS.
!                  Default value is 2.

!        GCAPEPSD : CAPEs are calculated for the parcels released from
!                   all model levels where p > GCAPEPSD*ps.

!        NCAPEPSD : model-level of GCAPEPSD recalculated homogeneously

!        GCAPERET: FRACTION OF THE CONDENSATES WHICH IS RETAINED, 
!                  I.E. WHICH DOES NOT PRECIPITATE.
!            IF GCAPERET=1. ==> REVERSIBLE MOIST ASCENT.
!                      IT IS ASSUMED THAT ALL THE PARCEL'S CONDENSED
!                      WATER IS RETAINED, THUS CLOUD CONDENSATES 
!                      REDUCE THE BUOYANCY.
!            IF GCAPERET=0. ==> "IRREVERSIBLE" (PSEUDO-ADIABATIC) MOIST ASCENT.
!                       CLOUD CONDENSATES PRECIPITATE  INSTANTANEOUSLY
!                       AND THUS DO NOT AFFECT THE BUOYANCY.
!            GCAPERET CAN BE USED WITH VALUES BETWEEN 0. AND 1..
!            Default value is 0.
!-------------------------------------------------

INTEGER(KIND=JPIM) :: NCAPEITER
INTEGER(KIND=JPIM) :: NETAPES
INTEGER(KIND=JPIM) :: NCAPEPSD

REAL(KIND=JPRB) :: GCAPERET
REAL(KIND=JPRB) :: GCAPEPSD
REAL(KIND=JPRB) :: GMISCINV ! MISsing CIN Value. This value is used where CAPE=0, CIN cannot thus be defined.
REAL(KIND=JPRB) :: GCAPEMIN ! Minimal CAPE value under which LFC is not considered as found.
REAL(KIND=JPRB) :: GCINMAX ! Maximum value of CIN, to avoid compacting problems.

LOGICAL :: LADAE ! activates ADAptative Entrainment. Uniform entrainment if .false..
LOGICAL :: LMCAPEA ! activates Tiedtke-Bechtold Model-CAPE Algorithm to be that of CUASCN rather than CUBASEN.
!     ------------------------------------------------------------------

END TYPE

TYPE (TCAPE), TARGET :: YRCAPE

END MODULE YOMCAPE

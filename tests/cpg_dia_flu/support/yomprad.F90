MODULE YOMPRAD

USE PARKIND1 , ONLY : JPIM, JPRB, JPRD

IMPLICIT NONE

SAVE

!----------------------------------------------------------------
!*    new radiation grid data structures
!----------------------------------------------------------------

! NRESOL_ID : transformation package resolution identifier
! NSMAX     : spectral truncation
! NASM0     : address in a spectral array of (m, n=m)
! MYMS      : actual wave numbers handled by this processor
! NDGLG     : number of latitude rows
! NDLON     : length of a row of latitude near equator
! NDGSAL    : Local version of NDGSA , always 1
! NDGENL    : Number of latitude rows for which this process has grid points
! NDGSAH    : 1-NROWIDE
! NDGENH    : NDGENL+NROWIDE
! NDGSAG    : Global version of NDGSA
! NDGENG    : Global version of NDGEN
! NDSUR1    : over dimensioning of NDLON for technical reasons (at least 2)
! NDLSUR    : NDLON+NDSUR1
! NDGSUR    : number of additional rows at each pole for semi-lagrangian
!           : interpolation
! MYFRSTACTLAT : first actual lat on this PE in grid-point space,
!              : it is nfrstlat(my_region_ns)
! MYLSTACTLAT  : last actual lat on this PE in grid-point space,
!              : it is nlstlat(my_region_ns)
! NGPTOT    : number of local grid points
! NGPTOTG   : total number of grid points
! NRGRI     : number of grid points on a latitude row
! NLOENG    : as above but extended at poles for SL interpolation
! GELAM     : radiation grid geographic longitude "lambda"
! GELAT     : radiation grid geographic latitude "theta"
! GESLO     : sine of geographic longitude "sin(lambda)"
! GECLO     : cosine of geographic longitude "cos(lambda)
! GEMU      : sine of geographic latitude "sin(theta)"
! RMU       : mu              sin(theta)
! RSQM2     : SQRT(R1MU2)     cos(theta)
! RLATIG    : arcsin(mu)      theta  GLOBAL VIEW
! RLATI     : arcsin(mu)      theta
! RIPI      : bi-cubic interpolation coefficients

TYPE RADIATION_GRID_STRUCT
INTEGER(KIND=JPIM) :: NRESOL_ID, NGPTOT, NGPTOTG, &
 & NGPTOTMX, NSPEC2, NSMAX, &
 & NPTRFLOFF, NUMP, NDLON, &
 & NDGSAL, NDGENL, NDGSAH, NDGENH, &
 & NDGLG, NDGSAG, NDGENG, NDLSUR, &
 & NFRSTLOFF, NDSUR1,NDGSUR, &
 & MYFRSTACTLAT, MYLSTACTLAT  
INTEGER(KIND=JPIM), ALLOCATABLE, DIMENSION(:) :: NRGRI, NLOENG, NPTRFRSTLAT, NFRSTLAT, &
 & NLSTLAT, MYMS, NASM0  
INTEGER(KIND=JPIM), ALLOCATABLE, DIMENSION(:,:):: NSTA, NONL
REAL(KIND=JPRB),    ALLOCATABLE, DIMENSION(:)  :: GELAM, GELAT, GECLO, GESLO, &
 & RMU, RSQM2, RLATIG, RLATI
REAL(KIND=JPRD),    ALLOCATABLE, DIMENSION(:)  :: GEMU
REAL(KIND=JPRD),    ALLOCATABLE, DIMENSION(:,:):: RIPI
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE RADIATION_GRID_STRUCT
!============================================================================

CONTAINS



END MODULE YOMPRAD

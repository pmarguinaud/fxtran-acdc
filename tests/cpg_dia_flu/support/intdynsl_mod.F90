MODULE INTDYNSL_MOD

! Purpose :
! -------
!    To define and compute pointers and logical conditions used when
!    computing local quantities in the dynamics: SL scheme.
!    Allows to use some global structures under CALL_SL
!    (and also their TL and AD).

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
!    K. YESSAD (CNRM/GMAP)
!    Original : January 2011

! Modifications :
! -------------
!  K. YESSAD (Feb 2014): split into INTDYNSL_MOD
!  S. Malardel Nov 2013: pointers for COMAD weights
!  F. Vana  21-Nov-2017: Option LHOISLT
!  F. Vana  20-Feb-2019: Quintic vertical interpolation
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK
USE YOMDYNA  , ONLY : YRDYNA
USE YOMDYN   , ONLY : JPSLDIMK

IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITIONS
!            ----------------

!      1.11  Types TLSCAW and TRSCAW: pointers for interpolation weights computed in the SL scheme.

! Linear weights:
TYPE TLSCAW
INTEGER(KIND=JPIM) :: M_WDLO        ! distances for horizontal linear interpolations in longitude
INTEGER(KIND=JPIM) :: M_WDLAT       ! distance for horizontal linear interpolations in latitude
INTEGER(KIND=JPIM) :: M_WDVER       ! distance for vertical linear interpolation
INTEGER(KIND=JPIM) :: M_WDLOMAD     ! WDLO for LCOMADH
INTEGER(KIND=JPIM) :: M_WDLAMAD     ! WDLAT for LCOMADH
INTEGER(KIND=JPIM) :: M_WDVERMAD    ! WDVER for LCOMADV
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated

CONTAINS
  
 
    
END TYPE TLSCAW

! Other weights:
TYPE TRSCAW
INTEGER(KIND=JPIM) :: M_WCLO(JPSLDIMK)     ! weights for horizontal cubic interpolations in longitude
INTEGER(KIND=JPIM) :: M_WCLA(JPSLDIMK)     ! weights for horizontal cubic interpolations in latitude
INTEGER(KIND=JPIM) :: M_WVINTW             ! vertical cubic interpolation weights
INTEGER(KIND=JPIM) :: M_WCLOSLD(JPSLDIMK)  ! cf. WCLO but for SLHD case
INTEGER(KIND=JPIM) :: M_WCLASLD(JPSLDIMK)  ! cf. WCLA but for SLHD case
INTEGER(KIND=JPIM) :: M_WCLOSLT            ! cf. WCLO 
INTEGER(KIND=JPIM) :: M_WCLASLT            ! cf. WCLA 
INTEGER(KIND=JPIM) :: M_WVINTWSLD          ! cf. WVINTW but for SLHD case
INTEGER(KIND=JPIM) :: M_WVINTWSLT          ! cf. WVINTW 
INTEGER(KIND=JPIM) :: M_WCLOMAD(JPSLDIMK)  ! cf. WCLO but for COMAD case
INTEGER(KIND=JPIM) :: M_WCLAMAD(JPSLDIMK)  ! cf. WCLA but for COMAD case
INTEGER(KIND=JPIM) :: M_WVINTWMAD          ! cf. WVINTW but for COMAD case
INTEGER(KIND=JPIM) :: M_WVINTWS            ! vertical spline interpolation weights
INTEGER(KIND=JPIM) :: M_WVDERW             ! weights to compute vertical derivatives (Hermite cubic vertical interpolation)
INTEGER(KIND=JPIM) :: M_WHVW               ! Hermite vertical cubic interpolation weights
INTEGER(KIND=JPIM) :: M_CW                 ! C_k weights for the vertical WENO interpolation
INTEGER(KIND=JPIM) :: NDIM                 ! total number of fields allocated

CONTAINS
  
 

END TYPE TRSCAW

!      1.12  Types TSCO and TCCO: pointers for coordinates computed in the SL scheme.

TYPE TSCO
! spherical geometry:
!   cos(Longitude-Longitude(grid-point))*cos(Latitude) of the interpolation
!   point (geographical longitude and latitude).
! plane projection: x - coordinate (fractional system).
INTEGER(KIND=JPIM) :: M_COSCO
! spherical geometry:
!   sin(Longitude-Longitude(grid-point))*cos(Latitude) of the interpolation
!   point (geographical longitude and latitude).
! plane projection: y - coordinate (fractional system).
INTEGER(KIND=JPIM) :: M_SINCO
! sine of the interpolation point geographical latitude.
INTEGER(KIND=JPIM) :: M_SINLA
! cosine of the geographical angle between the interpolation point
! and the grid-point.
INTEGER(KIND=JPIM) :: M_COPHI
! total number of fields allocated.
INTEGER(KIND=JPIM) :: NDIM

CONTAINS
  
 

END TYPE TSCO

TYPE TCCO
INTEGER(KIND=JPIM) :: M_RLON ! computational sphere longitude of interpolation point
INTEGER(KIND=JPIM) :: M_RLAT ! computational sphere latitude of interpolation point
INTEGER(KIND=JPIM) :: M_RQX  ! first element of the wind displacement matrix (p,q)
INTEGER(KIND=JPIM) :: M_RQY  ! second element of the wind displacement matrix (p,q)
INTEGER(KIND=JPIM) :: NDIM   ! total number of fields allocated

CONTAINS
  
 

END TYPE TCCO

!=============================================================================

!      2.    DECLARATIONS
!            ------------

!      2.11  Types TLSCAW and TRSCAW.

!TYPE(TLSCAW), POINTER :: YYTLSCAW  => NULL()    ! at full levels
!TYPE(TLSCAW), POINTER :: YYTLSCAWH => NULL()    ! at half levels
!TYPE(TRSCAW), POINTER :: YYTRSCAW  => NULL()    ! at full levels
!TYPE(TRSCAW), POINTER :: YYTRSCAWH => NULL()    ! at half levels

!      2.12  Types TSCO and TCCO.

!TYPE(TSCO), POINTER :: YYTSCO => NULL()
!TYPE(TCCO), POINTER :: YYTCCO => NULL()

!=============================================================================

CONTAINS

!      3.    SET-UP

!      3.00  General set-up.




!      3.11  Set-up for types TLSCAW and TRSCAW.





!      3.12  Set-up for types TSCO and TCCO.





!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

END MODULE INTDYNSL_MOD

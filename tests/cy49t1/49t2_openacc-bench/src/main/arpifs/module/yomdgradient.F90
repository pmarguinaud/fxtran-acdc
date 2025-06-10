MODULE YOMDGRADIENT

!$ACDC methods 


USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
!
IMPLICIT NONE

TYPE TGRADIENT_PTR
  INTEGER :: M_DUDX      ! dudx_eta
  INTEGER :: M_DUDY      ! dudy_eta
  INTEGER :: M_DVDX      ! dvdx_eta
  INTEGER :: M_DVDY      ! dvdy_eta
  INTEGER :: M_DTDX      ! dtdx_eta
  INTEGER :: M_DTDY      ! dtdy_eta 
  INTEGER :: M_DQVDX     ! dqvdx_eta
  INTEGER :: M_DQVDY     ! dqvdy_eta
  INTEGER :: M_DQLDX     ! dqldx_eta
  INTEGER :: M_DQLDY     ! dqldy_eta
  INTEGER :: M_DQIDX     ! dqidx_eta
  INTEGER :: M_DQIDY     ! dqidy_eta
  INTEGER :: M_DQRDX     ! dqrdx_eta
  INTEGER :: M_DQRDY     ! dqrdy_eta
  INTEGER :: M_DQSDX     ! dqsdx_eta
  INTEGER :: M_DQSDY     ! dqsdy_eta
  INTEGER :: M_DQGDX     ! dqgdx_eta
  INTEGER :: M_DQGDY     ! dqgdy_eta
  INTEGER :: M_DU2DX2    ! du²dx²_eta 
  INTEGER :: M_DU2DY2    ! du²dy²_eta 
  INTEGER :: M_DV2DX2    ! dv²dx²_eta 
  INTEGER :: M_DV2DY2    ! dv²dy²_eta 
  INTEGER :: M_DUDXZ     ! dudx_z
  INTEGER :: M_DUDYZ     ! dudy_z
  INTEGER :: M_DVDXZ     ! dvdx_z
  INTEGER :: M_DVDYZ     ! dvdy_z
  INTEGER :: M_DWDXZ     ! dwdx_z
  INTEGER :: M_DWDYZ     ! dwdy_z
  INTEGER :: M_DRVDXZ    ! drvdx_z
  INTEGER :: M_DRVDYZ    ! drvdy_z
  INTEGER :: M_DRLDXZ    ! drldx_z
  INTEGER :: M_DRLDYZ    ! drldy_z
  INTEGER :: M_DRIDXZ    ! dridx_z
  INTEGER :: M_DRIDYZ    ! dridy_z
  INTEGER :: M_DTHETADXZ ! dthetadx_z
  INTEGER :: M_DTHETADYZ ! dthetady_z
  INTEGER :: NFLDCORE    ! computation array halo dimension  
  INTEGER :: NDIM
  INTEGER :: NDIMLEO
  INTEGER :: NDIMGOG
ENDTYPE
!
! ---------------------------------------------------------------------
!
TYPE TGRADIENT
  TYPE(TGRADIENT_PTR)  :: YPTR    ! horizontal gradients structure for the
ENDTYPE
!   
CONTAINS
!
!=================================================================
!
SUBROUTINE SUPTGRAD(LDLEO,LDGOG,YD)
! Sets-up pointers for TGRADIENT structure
! LLEO     : if Moeng Parametrisation (Leonard terms)
! LGOG     : if Goger Parametrisation 
! YD       : contains pointers.
!--------------------------------------------------------------------------
LOGICAL :: LDLEO
LOGICAL :: LDGOG
TYPE(TGRADIENT_PTR), INTENT(OUT)   :: YD
!--------------------------------------------------------------------------
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('YOMDGRADIENT:SUPTGRAD',0,ZHOOK_HANDLE)
!--------------------------------------------------------------------------
IF (LDGOG .AND. .NOT. LDLEO) THEN
    YD%M_DUDXZ= 1  ! dudx_z
    YD%M_DUDYZ= 2  ! dudy_z
    YD%M_DVDXZ= 3  ! dvdx_z
    YD%M_DVDYZ= 4  ! dvdy_z
    YD%M_DUDX = 5  ! dudx_eta
    YD%M_DUDY = 6  ! dudy_eta
    YD%M_DVDX = 7  ! dvdx_eta
    YD%M_DVDY  =8  ! dvdy_eta
    YD%NDIM    =8  ! ndim 
    YD%NDIMGOG =4  ! dim of Leonard Term used in the param
    YD%NFLDCORE=2  ! dim of the SL halo
ENDIF
IF (LDLEO .AND. .NOT. LDGOG) THEN
    YD%M_DWDXZ =1  ! dwdx_z
    YD%M_DWDYZ =2  ! dwdy_z
    YD%M_DRVDXZ=3  ! drvdx_z
    YD%M_DRVDYZ=4  ! drvdy_z
    YD%M_DRLDXZ=5  ! drldx_z
    YD%M_DRLDYZ=6  ! drldy_z
    YD%M_DRIDXZ=7  ! dridx_z
    YD%M_DRIDYZ=8  ! dridy_z
    YD%M_DTDX  =9  ! dtdx_eta
    YD%M_DTDY  =10 ! dtdy_eta 
    YD%M_DQVDX =11 ! dqvdx_eta
    YD%M_DQVDY =12 ! dqvdy_eta
    YD%M_DQLDX =13 ! dqldx_eta
    YD%M_DQLDY =14 ! dqldy_eta
    YD%M_DQIDX =15 ! dqidx_eta
    YD%M_DQIDY =16 ! dqidy_eta
    YD%M_DQRDX =17 ! dqrdx_eta
    YD%M_DQRDY =18 ! dqrdy_eta
    YD%M_DQSDX =19 ! dqsdx_eta
    YD%M_DQSDY =20 ! dqsdy_eta
    YD%M_DQGDX =21 ! dqgdx_eta
    YD%M_DQGDY =22 ! dqgdy_eta 
    YD%NDIM    =22 ! ndim 
    YD%NDIMLEO =8  ! dim of Leonard Term used in the param
    YD%NFLDCORE=7  ! dim of the SL halo
ENDIF
IF (LDGOG .AND. LDLEO) THEN
    YD%M_DUDX  =1  ! dudx_eta
    YD%M_DUDY  =2  ! dudy_eta
    YD%M_DVDX  =3  ! dvdx_eta
    YD%M_DVDY  =4  ! dvdy_eta
    YD%M_DTDX  =5  ! dtdx_eta
    YD%M_DTDY  =6  ! dtdy_eta 
    YD%M_DQVDX =7  ! dqvdx_eta
    YD%M_DQVDY =8  ! dqvdy_eta
    YD%M_DQLDX =9  ! dqldx_eta
    YD%M_DQLDY =10 ! dqldy_eta
    YD%M_DQIDX =11 ! dqidx_eta
    YD%M_DQIDY =12 ! dqidy_eta
    YD%M_DQRDX =13 ! dqrdx_eta
    YD%M_DQRDY =14 ! dqrdy_eta
    YD%M_DQSDX =15 ! dqsdx_eta
    YD%M_DQSDY =16 ! dqsdy_eta
    YD%M_DQGDX =17 ! dqgdx_eta
    YD%M_DQGDY =18 ! dqgdy_eta 
    YD%M_DWDXZ =19 ! dwdx_z
    YD%M_DWDYZ =20 ! dwdy_z
    YD%M_DRVDXZ=21 ! drvdx_z
    YD%M_DRVDYZ=22 ! drvdy_z
    YD%M_DRLDXZ=23 ! drldx_z
    YD%M_DRLDYZ=24 ! drldy_z
    YD%M_DRIDXZ=25 ! dridx_z
    YD%M_DRIDYZ=26 ! dridy_z
    YD%M_DUDXZ =27 ! dudx_z
    YD%M_DUDYZ =28 ! dudy_z
    YD%M_DVDXZ =29 ! dvdx_z
    YD%M_DVDYZ =30 ! dvdy_z
    YD%NDIM    =30 ! ndim 
    YD%NDIMLEO =8  ! dim of Leonard Term used in the param
    YD%NDIMGOG =4  ! dim of Leonard Term used in the param
    YD%NFLDCORE=9  ! dim of the SL halo
ENDIF

! -----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('YOMDGRADIENT:SUPTGRAD',1,ZHOOK_HANDLE)

END SUBROUTINE SUPTGRAD

ENDMODULE YOMDGRADIENT

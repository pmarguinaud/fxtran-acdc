SUBROUTINE SMPOS_PARALL(YDGEOMETRY,YDMODEL,KST,KEND,&
 & PGEMU,PGELAM,PSOLO)

!$ACDC singlecolumn 


!**** *SMPOS_PARALL*  - Sun and Moon POSitions : compute PARALLax & lunar obscuration of the Sun.

!     Purpose.
!     --------

!**   Interface.
!     ----------
!        *CALL* *SMPOS_PARALL*

!        Explicit arguments :
!        --------------------
!     Input:
!       PGEMU: sin(latitude)
!       PGELAM: longitude
!     Output:
!       PSOLO: SOLar Obscuration: fraction of the Sun disk surface, obscurred by the Moon.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------
!      2022-04-13 J.M. Piriou.

!     Modifications.
!     --------------
!     ------------------------------------------------------------------

USE TYPE_MODEL   , ONLY : MODEL
USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(MODEL)       ,INTENT(IN)    :: YDMODEL
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEMU(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGELAM(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSOLO(YDGEOMETRY%YRDIM%NPROMA)

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JROF

REAL(KIND=JPRB) :: ZCOSPHI,ZX_MC,ZY_MC,ZZ_MC,ZX_EST,ZY_EST,ZZ_EST,ZX_NOR
REAL(KIND=JPRB) :: ZY_NOR,ZZ_NOR,ZX_ZEN,ZY_ZEN,ZZ_ZEN,ZXM,ZYM,ZZM,ZNORME
REAL(KIND=JPRB) :: ZXS,ZYS,ZZS,ZCL,ZRADM,ZRADS,ZC,ZR,ZPLUS,ZMOINS &
  & ,ZDEGO_NONE,ZDEGO_TOTA,ZDEGO_ANNU,ZXO,ZTHETAM,ZTHETAS,ZYO &
  & ,ZDEGO_PART,ZBINPLUS,ZBINMOINSPOS,ZBINMOINSNEG,ZCRIT_NONE &
  & ,ZCRIT_TOTA,ZCRIT_ANNU,ZCRIT_PART,ZEPSCL

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SMPOS_PARALL',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDGEM=>YDGEOMETRY%YRGEM, YDMP=>YDGEOMETRY%YRMP, &
 &  YDDYN=>YDMODEL%YRML_DYN%YRDYN,YDPHY=>YDMODEL%YRML_PHY_MF%YRPHY,YDSIMPHL=>YDMODEL%YRML_PHY_MF%YRSIMPHL, &
 & YDRIP=>YDMODEL%YRML_GCONF%YRRIP,YDARPHY=>YDMODEL%YRML_PHY_MF%YRARPHY, &
  & YDERAD=>YDMODEL%YRML_PHY_RAD%YRERAD,YDERIP=>YDMODEL%YRML_PHY_RAD%YRERIP,YDPHY2=>YDMODEL%YRML_PHY_MF%YRPHY2, &
  & YDEPHY=>YDMODEL%YRML_PHY_EC%YREPHY, YDCST=>YDMODEL%YRCST)
ASSOCIATE(NPROMA=>YDDIM%NPROMA, NPROMM=>YDDIM%NPROMM, &
 & NFLEVG=>YDDIMV%NFLEVG, &
 & RWSOVR=>YDRIP%RWSOVR, &
 & RTMOLT=>YDRIP%RTMOLT, &
 & RDECLI=>YDRIP%RDECLI, &
 & RDECLU=>YDRIP%RDECLU, &
 & RDEASO=>YDRIP%RDEASO, &
 & RLUNDI=>YDRIP%RLUNDI, &
 & RA=>YDCST%RA, RPI=>YDCST%RPI)

ZEPSCL=1.E-4_JPRB
!     ------------------------------------------------------------------

!*       1.1   Astronomy.
DO JROF=KST,KEND

  !-------------------------------------------------
  ! Vector MC from observation point to Moon center. 
  ! In rectangular coordinates: ZX_MC, ZY_MC, ZZ_MC.
  !-------------------------------------------------
  ZCOSPHI=SQRT(MAX(0._JPRB,1._JPRB-PGEMU(JROF)**2))
  ZX_MC=RLUNDI*COS(RDECLU)*COS(RPI-RTMOLT) - RA*ZCOSPHI*COS(PGELAM(JROF))
  ZY_MC=RLUNDI*COS(RDECLU)*SIN(RPI-RTMOLT) - RA*ZCOSPHI*SIN(PGELAM(JROF))
  ZZ_MC=RLUNDI*SIN(RDECLU)             - RA*PGEMU(JROF)

  ! Unit vector eastward.
  ZX_EST=-SIN(PGELAM(JROF))
  ZY_EST=COS(PGELAM(JROF))
  ZZ_EST=0._JPRB

  ! Unit vector northward.
  ZX_NOR=-PGEMU(JROF)*COS(PGELAM(JROF))
  ZY_NOR=-PGEMU(JROF)*SIN(PGELAM(JROF))
  ZZ_NOR=ZCOSPHI

  ! Unit vector to zenith.
  ZX_ZEN=ZCOSPHI*COS(PGELAM(JROF))
  ZY_ZEN=ZCOSPHI*SIN(PGELAM(JROF))
  ZZ_ZEN=PGEMU(JROF)

  ! Scalar products between MC vector and each of the three unit vectors.
  ZXM=ZX_MC*ZX_EST+ZY_MC*ZY_EST+ZZ_MC*ZZ_EST
  ZYM=ZX_MC*ZX_NOR+ZY_MC*ZY_NOR+ZZ_MC*ZZ_NOR
  ZZM=ZX_MC*ZX_ZEN+ZY_MC*ZY_ZEN+ZZ_MC*ZZ_ZEN
  ZNORME=SQRT(ZXM*ZXM+ZYM*ZYM+ZZM*ZZM)
  ZXM=ZXM/ZNORME
  ZYM=ZYM/ZNORME
  ZZM=ZZM/ZNORME

  !-------------------------------------------------
  ! Vector MC from observation point to Sun center. 
  ! In rectangular coordinates: ZX_MC, ZY_MC, ZZ_MC.
  !-------------------------------------------------
  ZX_MC=RDEASO*COS(RDECLI)*COS(RPI-RWSOVR) - RA*ZCOSPHI*COS(PGELAM(JROF))
  ZY_MC=RDEASO*COS(RDECLI)*SIN(RPI-RWSOVR) - RA*ZCOSPHI*SIN(PGELAM(JROF))
  ZZ_MC=RDEASO*SIN(RDECLI)             - RA*PGEMU(JROF)

  ! Scalar products between MC vector and each of the three unit vectors.
  ZXS=ZX_MC*ZX_EST+ZY_MC*ZY_EST+ZZ_MC*ZZ_EST
  ZYS=ZX_MC*ZX_NOR+ZY_MC*ZY_NOR+ZZ_MC*ZZ_NOR
  ZZS=ZX_MC*ZX_ZEN+ZY_MC*ZY_ZEN+ZZ_MC*ZZ_ZEN
  ZNORME=SQRT(ZXS*ZXS+ZYS*ZYS+ZZS*ZZS)
  ZXS=ZXS/ZNORME
  ZYS=ZYS/ZNORME
  ZZS=ZZS/ZNORME

  !-------------------------------------------------
  ! ZCL : Angle between Sun and Moon, as seen from local observer (radians).
  !-------------------------------------------------
  ZCL=ACOS(MAX(-1._JPRB,MIN(1._JPRB,ZXS*ZXM+ZYS*ZYM+ZZS*ZZM)))

  !-------------------------------------------------
  ! Ratio between celestial body radius and its distance to Earth.
  ! This is the apparent radius of this celestial body.
  !-------------------------------------------------
  ZRADM=1.7374E6_JPRB/RLUNDI ! for Moon.
  ZRADS=6.943E8_JPRB/RDEASO ! for Sun.

  ! ZC: ratio (anglar distance between Moon and Sun centers)/(Sun apparent radius).
  ZC=ZCL/ZRADS

  ! ZR: ratio (Moon apparent radius)/(Sun apparent radius).
  ZR=ZRADM/ZRADS
  ZPLUS=ZC+ZR
  ZMOINS=ZC-ZR

  ! No intersection between the two circles (Moon & Sun).
  ZDEGO_NONE=0._JPRB

  ! Total eclipse of the Sun.
  ZDEGO_TOTA=1._JPRB

  ! Annular eclipse.
  ZDEGO_ANNU=ZR**2

  ! Partial eclipse.
  ZXO=(1._JPRB+ZC**2-ZR**2)/(2._JPRB*MAX(ZEPSCL,ZC))
  ZTHETAM=ACOS(MAX(-1._JPRB,MIN(1._JPRB,(ZC-ZXO)/ZR)))
  ZTHETAS=ACOS(MAX(-1._JPRB,MIN(1._JPRB,ZXO)))
  ZYO=SQRT(MAX(0._JPRB,-(ZMOINS**2-1._JPRB)*(ZPLUS**2-1._JPRB)))/(2._JPRB*MAX(ZEPSCL,ZC))
  ZDEGO_PART=(ZTHETAM*ZR**2-(ZC-ZXO)*ZYO + ZTHETAS-ZXO*ZYO)/RPI

  ! In order to avoid "if" statements, compute binary values.
  ZBINPLUS=MAX(0._JPRB,SIGN(1._JPRB,ZPLUS-1._JPRB)) ! 1. if c+r > 1., 0. else case.
  ZBINMOINSPOS=MAX(0._JPRB,SIGN(1._JPRB,ZMOINS-1._JPRB)) ! 1. if c-r > 1., 0. else case.
  ZBINMOINSNEG=MAX(0._JPRB,SIGN(1._JPRB,ZMOINS+1._JPRB)) ! 1. if c-r > -1., 0. else case.
  ZCRIT_NONE=ZBINPLUS*ZBINMOINSPOS ! 1. if no eclipse, 0. else case.
  ZCRIT_TOTA=ZBINPLUS*(1._JPRB-ZBINMOINSNEG) ! 1. if total eclipse of the Sun, 0. else case.
  ZCRIT_ANNU=(1._JPRB-ZBINPLUS)*ZBINMOINSNEG*(1._JPRB-ZBINMOINSPOS) ! 1. if annular eclipse, 0. else case.
  ZCRIT_PART=ZBINPLUS*ZBINMOINSNEG**(1._JPRB-ZBINMOINSPOS) ! 1. if partial eclipse, 0. else case.

  !-------------------------------------------------
  ! Final value of obscuration.
  !-------------------------------------------------
  PSOLO(JROF)=MAX(0._JPRB,MIN(1._JPRB,ZCRIT_NONE*ZDEGO_NONE+(1._JPRB-ZCRIT_NONE) &
    & *(ZCRIT_TOTA*ZDEGO_TOTA+(1._JPRB-ZCRIT_TOTA)*(ZCRIT_ANNU*ZDEGO_ANNU &
    & +(1._JPRB-ZCRIT_ANNU)*ZCRIT_PART*ZDEGO_PART))))
ENDDO

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SMPOS_PARALL',1,ZHOOK_HANDLE)
END SUBROUTINE SMPOS_PARALL

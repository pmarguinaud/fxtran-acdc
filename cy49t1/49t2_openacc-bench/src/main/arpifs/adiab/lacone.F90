SUBROUTINE LACONE(KPROMA,KST,KEND,KFLEV,KSTTYP,K2DINI,&
 & YDSWE,POMEGA,PDSTRET,PC2M1,PC2P1,PMUCEN,PRA,PI,&
 & PLON,PLAT,POMVR9L,POMVR9M)  

!$ACDC singlecolumn  --process-pointers


!**** *LACONE*  - Analytic computation of 2.Omega.Vec.r 

!     Purpose.
!     --------
!           Computes 2.Omega.Vec.r analytically at any point of the sphere,
!           not necessary a grid-point.

!**   Interface.
!     ----------
!        *CALL* *LACONE( ... )

!        Explicit arguments :
!        --------------------
!        INPUT :
!          KPROMA  - horizontal dimension.
!          KST  - first element of arrays where computations are performed.
!          KEND   - depth of work.
!          KFLEV   - vertical dimension.
!          KSTTYP  - 1: Not tilted pole;  2: Tilted pole.
!          POMEGA  - Omega.
!          PDSTRET - 2*c (where c is the stretching factor).
!          PC2M1   - c*c-1.
!          PC2P1   - c*c+1.
!          PMUCEN  - sinus of the geographical latitude of the 
!                    high resolution pole.
!          PRA     - Earth radius.
!          PI      - Number PI.
!          PLON    - computational sphere longitude. 
!          PLAT    - computational sphere latitude.

!        OUTPUT :
!          POMVR9L - zonal component of 2*Omega*Vec*r at the point of
!                    coordinates (PLON,PLAT).
!          POMVR9M - meridian component of 2*Omega*Vec*r at the point of
!                    coordinates (PLON,PLAT).

!        Implicit arguments :
!        --------------------
!        none.

!     Method.
!     -------
!        See documentation about semi-Lagrangian scheme.

!     Externals.
!     ----------
!        none.
!        Called by LAPINEB.

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        K. YESSAD (METEO-FRANCE/CNRM/GMAP).

!     Modifications.
!     --------------
!        Original: MARCH 1996.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        K. Yessad: Aug 2005 : code AD+TL for variable mesh.
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK
USE YOMSWE   , ONLY : TSWE

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTTYP 
INTEGER(KIND=JPIM),INTENT(IN)    :: K2DINI
TYPE(TSWE)        ,INTENT(IN)    :: YDSWE
REAL(KIND=JPRB)   ,INTENT(IN)    :: POMEGA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDSTRET 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC2M1 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC2P1 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMUCEN 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PI 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLON(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAT(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: POMVR9L(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: POMVR9M(KPROMA,KFLEV) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRB) :: Z2OMR, Z2OMR2C, ZA, ZB, ZCA, ZCOLA, ZCOLO,&
 & ZE, ZSA, ZSILA, ZSILO, ZSQM2CEN  
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LACONE',0,ZHOOK_HANDLE)

!     ------------------------------------------------------------------

!        1. COMPUTATION.
!        ---------------

IF (KSTTYP == 1 .AND. K2DINI /= 21) THEN

  Z2OMR2C=2.0_JPRB*POMEGA*PRA*PDSTRET

  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ZSILA=SIN(PLAT(JROF,JLEV))
      ZCOLA=SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSILA*ZSILA))
      ZA =1.0_JPRB/(PC2P1+PC2M1*ZSILA)
      POMVR9L(JROF,JLEV)=Z2OMR2C*ZCOLA*ZA
      POMVR9M(JROF,JLEV)=0.0_JPRB
    ENDDO
  ENDDO

ELSEIF (KSTTYP == 2 .OR. K2DINI == 21) THEN

  IF(K2DINI == 21) THEN
    ZSQM2CEN = SQRT(MAX(0.0_JPRB,1.0_JPRB - YDSWE%GMUCENSWE*YDSWE%GMUCENSWE))
  ELSE
    ZSQM2CEN = SQRT(MAX(0.0_JPRB,1.0_JPRB - PMUCEN*PMUCEN))
  ENDIF
  Z2OMR=2.0_JPRB*POMEGA*PRA

  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ZSILA=SIN(PLAT(JROF,JLEV))
      ZCOLA=SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSILA*ZSILA))
      ZSILO=SIN(PLON(JROF,JLEV))
      ZA =1.0_JPRB/(PC2P1+PC2M1*ZSILA)
      ZB =PC2P1*ZSILA+PC2M1
      IF(K2DINI == 21) THEN
        ZCOLO=COS(PLON(JROF,JLEV))
        ZCA=ZA*(PDSTRET*YDSWE%GMUCENSWE*ZCOLA-ZB*ZSQM2CEN*ZCOLO)
      ELSE
        ZE=SIGN(1.0_JPRB,MOD(PLON(JROF,JLEV)+1.5_JPRB*PI,2.0_JPRB*PI)-PI)
        ZCOLO=ZE*SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSILO*ZSILO))
        ZCA=ZA*(PDSTRET*PMUCEN*ZCOLA-ZB*ZSQM2CEN*ZCOLO)
      ENDIF
      ZSA=-ZSQM2CEN*ZSILO
      POMVR9L(JROF,JLEV)=Z2OMR*ZCA
      POMVR9M(JROF,JLEV)=-Z2OMR*ZSA
    ENDDO
  ENDDO

ENDIF

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LACONE',1,ZHOOK_HANDLE)
END SUBROUTINE LACONE


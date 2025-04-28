!-----------------------------------------------------------------------
SUBROUTINE ACMICRO( YDCST, YDML_PHY_MF,KIDIA, KFDIA, KLON, KTDIA, KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT -
 & PNEBST, PT  , PQL , PQI , PTS , PNEIJ , PLSM,&
 ! - OUTPUT -
 & PAUTOL, PAUTOI )

!**** *ACMICRO * - CALCULS D'AUTOCONVERSION.
!                  AUTOCONVERSION COMPUTATIONS.

!     Sujet.
!     ------

!**   Interface.
!     ----------
!        *CALL* *ACMICRO*

!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE./INPUT ARGUMENTS.
!     ------------------------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
!            : START OF HORIZONTAL LOOP 
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
!            : END OF HORIZONTAL LOOP
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
!            : HORIZONTAL DIMENSION  
! KTDIA      : INDICE DE DEPART DES BOUCLES VERTICALES. 
!            : START OF THE VERTICAL LOOP IN THE PHYSICS. 
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".
!            : END OF VERTICAL LOOP AND VERTICAL DIMENSION

! - 2D (1:KLEV)

! PNEBST     : NEBULOSITE PARTIELLE STRATIFORME.
!            : STRATIFORM FRACTIONAL CLOUDINESS.
! PT         : TEMPERATURE.
!            : TEMPERATURE.
! PQL        : CONTENU EN CONDENSAT NUAGEUX LIQUIDE.
!            : CLOUD WATER LIQUID.
! PQI        : CONTENU EN CONDENSAT NUAGEUX SOLIDE.
!            : CLOUD WATER SOLID.

! - 1D (1:KLON) .
                                                                                
! PTS        : TEMPERATURE DE SURFACE.
!            : SURFACE TEMPERATURE.
! PNEIJ      : PROPORTION DE LA MAILLE RECOUVERTE DE NEIGE.
!            : SNOW FRACTION.
! PLSM       : INDICE TERRE/MER.
!            : LAND/SEA MASK.
                                                                                
! -   ARGUMENTS EN SORTIE.
!     ---------------------------

! PAUTOL     : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE LIQ.
!            : GENERATION OF PRECIPITATION FROM LIQUID CLOUD WATER. 
! PAUTOI     : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE SOL.
!            : GENERATION OF PRECIPITATION FROM SOLID CLOUD WATER. 

!-----------------------------------------------------------------------

!     Auteur.
!     -------
!        99-10, Philippe Lopez
!               CNRM/GMME/RECYF

!     Modifications.
!     --------------
!      2002-01, P. Marquet Comments : 
!               here PAUTO is >0 when conversion occur, and the
!               contribution is "-PAUTO" in the "qc" equation.
!      2004-06, P. Marquet : introduce RAUTEFR for liquid->rain, with
!                            the old RAUTEFS valid for ice->snow
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!      2004-10, F. Bouyssel : Cleaning
!      2005-05, F. Bouyssel : More tunings in autoconvertion thresholds
!      2005-07, F. Bouyssel : Modification of ice autoconvertion thresholds
!                             over snow and sea ice (RQICRSN)
!      2006-01, F. Bouyssel : Remove ZSURF
!      2006-04, F. Bouyssel : Replace PQC by PQL,PQI
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.

!-----------------------------------------------------------------------

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1             , ONLY : JPIM     ,JPRB
USE YOMHOOK              , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST               , ONLY : TCST

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(IN):: YDML_PHY_MF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEBST(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT    (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL   (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI   (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS   (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEIJ (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM  (KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAUTOL(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAUTOI(KLON,KLEV) 

REAL(KIND=JPRB) :: ZQL,ZQI,ZCAUT,&
 & ZALPH,ZDUM,ZQCR,ZARG1,ZARG2,ZBETA,ZFACICE

REAL(KIND=JPRB) :: ZAUTOL(KLON,KLEV), ZAUTOI(KLON,KLEV) &
 & , ZDELT(KLON,KLEV) , ZEFFA(KLON,KLEV)  
INTEGER(KIND=JPIM) :: JLON,JLEV
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!     CHECK RELIABILITY OF INPUT ARGUMENTS.

IF (LHOOK) CALL DR_HOOK('ACMICRO',0,ZHOOK_HANDLE)
ASSOCIATE(RAUTEFR=>YDML_PHY_MF%YRPHY0%RAUTEFR, RAUTEFS=>YDML_PHY_MF%YRPHY0%RAUTEFS, &
 & RQICRT1=>YDML_PHY_MF%YRPHY0%RQICRT1, RQICRT2=>YDML_PHY_MF%YRPHY0%RQICRT2, RQICRSN=>YDML_PHY_MF%YRPHY0%RQICRSN, &
 & RQLCR=>YDML_PHY_MF%YRPHY0%RQLCR, RAUTSBET=>YDML_PHY_MF%YRPHY0%RAUTSBET, RQICRMIN=>YDML_PHY_MF%YRPHY0%RQICRMIN, &
 & RQICRMAX=>YDML_PHY_MF%YRPHY0%RQICRMAX, &
 & TMERGL=>YDML_PHY_MF%YRPHY1%TMERGL, &
 & RTT=>YDCST%RTT, &
 & TSPHY=>YDML_PHY_MF%YRPHY2%TSPHY)
!     ------------------------------------------------------------------

! Define threshold for ice autoconversion as a function of temperature
! --------------------------------------------------------------------
 
ZARG1 = 2.0_JPRB*RQICRMAX*(1.0_JPRB-0.999_JPRB)/(RQICRMAX-RQICRMIN)-1.0_JPRB
ZARG2 = 2.0_JPRB*(RQICRMAX-1.5_JPRB*RQICRMIN)/(RQICRMAX-RQICRMIN)-1.0_JPRB
ZARG1 = 0.5_JPRB*LOG(ABS((1.0_JPRB+ZARG1)/(1.0_JPRB-ZARG1)))
ZARG2 = 0.5_JPRB*LOG(ABS((1.0_JPRB+ZARG2)/(1.0_JPRB-ZARG2)))
ZALPH = (ZARG1 - ZARG2)/(RQICRT2-RQICRT1)
ZBETA = ZARG1 - RQICRT2 * ZALPH
 
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZDELT(JLON,JLEV) = PT(JLON,JLEV) - RTT

! Efficiency for ice conversion as a function of temperature.
! -----------------------------------------------------------
    ZEFFA(JLON,JLEV) = EXP(RAUTSBET*ZDELT(JLON,JLEV))

  ENDDO
ENDDO

! ---------------------------------------------------
! MICROPHYSICAL AUTOCONVERSION IN THE STRATIFORM CASE
! ---------------------------------------------------

DO JLEV=KTDIA,KLEV
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA

! Compute in-cloud values
! -----------------------

    ZQL = MAX(0.0_JPRB,PQL(JLON,JLEV)/PNEBST(JLON,JLEV))
    ZQI = MAX(0.0_JPRB,PQI(JLON,JLEV)/PNEBST(JLON,JLEV))

! AUTOCONVERSION OF CLOUD LIQUID WATER INTO RAIN
! ----------------------------------------------

    ZCAUT = RAUTEFR
    ZDUM = (1.0_JPRB-EXP(-ZCAUT*TSPHY)) * (ZQL-RQLCR) / TSPHY
    ZAUTOL(JLON,JLEV) = MAX(0.0_JPRB,ZDUM)

! AUTOCONVERSION OF CLOUD ICE INTO PRECIPITATING ICE
! --------------------------------------------------

    ZCAUT = RAUTEFS * ZEFFA(JLON,JLEV)
    ZQCR = RQICRMAX - (RQICRMAX - RQICRMIN) * 0.5_JPRB&
     & * (1.0_JPRB + TANH(ZALPH * ZDELT(JLON,JLEV) + ZBETA))
    ZFACICE = PLSM(JLON)*PNEIJ(JLON) + (1.0_JPRB-PLSM(JLON))&
     & * MAX(0.0_JPRB,SIGN(1.0_JPRB,TMERGL-PTS(JLON)))
    ZQCR = ZQCR * (1.0_JPRB-ZFACICE*(1.0_JPRB-RQICRSN))
    ZDUM = (1.0_JPRB-EXP(-ZCAUT*TSPHY)) * (ZQI-ZQCR) / TSPHY
    ZAUTOI(JLON,JLEV) = MAX(0.0_JPRB,ZDUM)
 
! TOTAL AUTOCONVERSION TERM
! -------------------------

    PAUTOL(JLON,JLEV) = ZAUTOL(JLON,JLEV) * PNEBST(JLON,JLEV)
    PAUTOI(JLON,JLEV) = ZAUTOI(JLON,JLEV) * PNEBST(JLON,JLEV)

  ENDDO 
ENDDO 

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACMICRO',1,ZHOOK_HANDLE)
END SUBROUTINE ACMICRO

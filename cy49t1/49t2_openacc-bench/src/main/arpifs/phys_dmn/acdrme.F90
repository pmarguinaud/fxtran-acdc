!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACDRME (YDCST,YDSTA,YDPHY2,YDTOPH,KIDIA,KFDIA,KLON,KTDIA,KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT  2D .
 & PCP,PDELP,PT,PQ,PU,PV,&
 ! - OUTPUT 2D .
 & PFRMH,PFRMQ,PSTRMU,PSTRMV)

!**** *ACDRME* FLUX MESOSPHERIQUE DE QUANTITE DE MOUVEMENT ET ENTHALPIE

!     Sujet.
!     ------
!     - ROUTINE DE CALCUL ACTIF .
!       CALCUL DU FLUX DE QUANTITE DE MOUVEMENT LIE A LA PRESENCE
!       D'UN FREINAGE DANS LA MESOSPHERE, DU FLUX D'ENTHALPIE
!       CORRESPONDANT A UN RAPPEL VERS LA TEMPERATURE DE
!       L'ATMOSPHERE STANDARD ET DU FLUX D'HUMIDITE
!       CORRESPONDANT A UN RAPPEL VERS UNE VALEUR DE
!       6 PPMV POUR SIMULER LA SOURCE DE VAPEUR D'EAU
!       PAR OXYDATION DU METHANE.
!     - COMPUTES THE MOMENTUM FLUX DUE TO A MESOSPHERIC DRAG, THE HEAT FLUX
!       CORRESPONDING TO A RELAXATION OF TEMPERATURE TOWARDS THE
!       STANDARD ATMOSPHERE.

!**   Interface.
!     ----------
!        *CALL* *ACDRME*

!-----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
!          "APLPAR" CODE.
!-----------------------------------------------------------------------

! -   ARGUMENTS D ENTREE.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

!       KIDIA: INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
!       KFDIA: INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
!       KLON : DIMENSION HORIZONTALE
!       KTDIA: DEBUT BOUCLE VERTICALE
!       KLEV : FIN BOUCLE VERTICALE ET DIMENSION VERTICALE

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (1:KLEV) .

!       PCP  : CHALEUR MASSIQUE A PRESSION CONSTANTE
!                   DE L'AIR
!       PDELP: EPAISSEUR EN PRESSION DE LA COUCHE
!       PT   : TEMPERATURE
!       PQ   : HUMIDITE SPECIFIQUE
!       PU   : VENT ZONAL
!       PV   : VENT MERIDIEN

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

!      PFRMH : FLUX D'ENTHALPIE DANS LA MESOSPHERE
!      PFRMQ : FLUX D'HUMIDITE DANS LA MESOSPHERE
!      PSTRMU: FLUX DE QUANTITE DE MVT EN U
!      PSTRMV: FLUX DE QUANTITE DE MVT EN V

!-----------------------------------------------------------------------

!     Externes.
!     ---------

!     Methode.
!     --------
!      Pour les variables U, V, T et Q nous avons la meme
!      formulation mathematique du probleme.
!      Elle se met sous la forme (pour un parametre a qq):

!     da/dt =  - drag / (1+alpha*drag*pdt) * (a(t-dt)-arel)

!        la valeur du parametre ZALPHA (alpha)

!               ZALPHA = 1     :   Implicite
!               ZALPHA = 0     :   Explicite

!     Method.
!     -------
!      The mathematical formulation for U, V, T and Q is the same.
!      For a parameter "a", it reads:

!     da/dt =  - drag / (1+alpha*drag*pdt) * (a(t-dt)-arel)

!        The value of ZALPHA (alpha) corresponds to:

!               ZALPHA = 1     :   Implicit scheme
!               ZALPHA = 0     :   Explicit scheme

!     Reference.
!    -----------

!     Auteur.
!    --------
!      A. Lasserre-Bigorry
!      Original : 91-06.

!     Modifications :
!    ----------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      05-09, A.ALias Introduction du rappel sur l'humidite (P.Simon)
!      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE YOMPHY2  , ONLY : TPHY2
USE YOMTOPH  , ONLY : TTOPH
USE YOMCST   , ONLY : TCST
USE YOMSTA   , ONLY : TSTA

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(TSTA)        ,INTENT(IN)    :: YDSTA
TYPE(TPHY2)       ,INTENT(IN)    :: YDPHY2
TYPE(TTOPH)       ,INTENT(IN)    :: YDTOPH
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFRMH(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFRMQ(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSTRMU(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSTRMV(KLON,0:KLEV) 

!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZTENU(KLON,KLEV),ZTENV(KLON,KLEV),ZTENT(KLON,KLEV)&
 & ,ZTENQ(KLON,KLEV)

INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZALPHA, ZRGI, ZUNMAL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACDRME',0,ZHOOK_HANDLE)
ASSOCIATE(TSPHY=>YDPHY2%TSPHY, &
 & RG=>YDCST%RG, &
 & RFMESOQ=>YDTOPH%RFMESOQ)
!-----------------------------------------------------------------------

!*
!     ------------------------------------------------------------------
!     I - CALCULS PRELIMINAIRES ET DIVERSES INITIALISATIONS

!         PRELIMINARY COMPUTATIONS AND VARIOUS INITIALIZATIONS

!     1.1 CALCULS DE PARAMETRES DERIVES ET D'UNE CONSTANTE DE SECURITE
!         AUXILLARY PARAMETERS AND SECURITY CONSTANT

ZRGI=1.0_JPRB/RG

!      POUR UN SCHEMA IMPLICITE ZALPHA = 1, EXPLICITE ZALPHA = 0
!      FOR AN IMPLICIT SCHEME ZALPHA = 1 , EXPLICIT SCHEME ZALPHA = 0

ZALPHA=1.0_JPRB
ZUNMAL=1.0_JPRB-ZALPHA

!     1.2 INITIALISATION DES PROFIL VERTICAUX DE RAPPEL
!         DE VENT, DE TEMPERATURE ET D'HUMIDITE
!         INITIALIZATION OF VERTICAL RELAXATION PROFILES
!         FOR WIND, TEMPERATURE AND HUMIDITY

! - TEMPORAIRE(S) 2D (1:KLEV).

!       ZTENU    : TENDANCE DE U
!                   TREND FOR U
!       ZTENV    : TENDANCE DE V
!                   TREND FOR V
!       ZTENT    : TENDANCE DE T
!                   TREND FOR T
!       ZTENQ    : TENDANCE DE Q
!                   TREND FOR Q

!     1.3 MISE A ZERO DES FLUX
!         FLUXES SET TO ZERO

DO JLEV=KTDIA-1,KLEV
  DO JLON=KIDIA,KFDIA
    PSTRMU(JLON,JLEV)=0.0_JPRB
    PSTRMV(JLON,JLEV)=0.0_JPRB
    PFRMH (JLON,JLEV)=0.0_JPRB
    PFRMQ (JLON,JLEV)=0.0_JPRB
  ENDDO
ENDDO

!*
!     ------------------------------------------------------------------
!     II - CALCUL DE LA TENDANCE MESOSPHERIQUE DE U, V, T ET Q

!          MESOSPHERIC TREND FOR U, V, T AND Q

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZTENU(JLON,JLEV)= - YDTOPH%RMESOU(JLEV)/&
     & (1.0_JPRB+YDTOPH%RMESOU(JLEV)*ZALPHA*TSPHY) * (PU(JLON,JLEV)-YDTOPH%RUREL(JLEV) )  
    ZTENV(JLON,JLEV)= - YDTOPH%RMESOU(JLEV)/&
     & (1.0_JPRB+YDTOPH%RMESOU(JLEV)*ZALPHA*TSPHY) * (PV(JLON,JLEV)-YDTOPH%RVREL(JLEV) )  
    ZTENT(JLON,JLEV)= - YDTOPH%RMESOT(JLEV)/&
     & (1.0_JPRB+YDTOPH%RMESOT(JLEV)*ZALPHA*TSPHY) * (PT(JLON,JLEV)-YDTOPH%RTREL(JLEV) )  
    ZTENQ(JLON,JLEV)= - YDTOPH%RMESOQ(JLEV)/&
     & (1.0_JPRB+YDTOPH%RMESOQ(JLEV)*ZALPHA*TSPHY) * (PQ(JLON,JLEV)-YDTOPH%RQREL(JLEV) )  
  ENDDO
ENDDO

!*
!     ------------------------------------------------------------------
!     III - CALCUL DU FLUX, PAR INTEGRATION DE LA TENDANCE DE BAS EN HAUT
!           LES FLUX SONT SUPPOSES NULS AU DERNIER NIVEAU (KLEV) DU MODELE

!           THE FLUXES ARE CALCULATED BY VERTICAL INTEGRATION (FROM THE BOTTOM)
!           OF THE TREND. THEY ARE ASSUMED TO BE ZERO AT THE LAST LEVEL (KLEV)

DO JLEV=KLEV,KTDIA,-1
  DO JLON=KIDIA,KFDIA
    PSTRMU(JLON,JLEV-1)=PSTRMU(JLON,JLEV)&
     & + ZRGI*ZTENU(JLON,JLEV)*PDELP(JLON,JLEV)  
    PSTRMV(JLON,JLEV-1)=PSTRMV(JLON,JLEV)&
     & + ZRGI*ZTENV(JLON,JLEV)*PDELP(JLON,JLEV)  
    PFRMH (JLON,JLEV-1)=PFRMH (JLON,JLEV)&
     & + ZRGI*ZTENT(JLON,JLEV)*PDELP(JLON,JLEV)&
     & *PCP(JLON,JLEV)  
    PFRMQ (JLON,JLEV-1)=PFRMQ (JLON,JLEV)&
     & + ZRGI*ZTENQ(JLON,JLEV)*PDELP(JLON,JLEV)  
  ENDDO
ENDDO

!-----------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACDRME',1,ZHOOK_HANDLE)
END SUBROUTINE ACDRME

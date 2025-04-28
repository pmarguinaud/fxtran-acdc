!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACSOLW ( YDPHY1,KIDIA,KFDIA,KLON,&
 !-----------------------------------------------------------------------
 ! - INPUT  1D
 & PARG,PD2,PLSM,PIVEG,PSAB,&
 ! - INPUT  LOGIQUE
 & LDHMT,&
 ! - OUTPUT 1D .
 & PWFC,PWPMX,PWSAT,PWSMX,PWWILT)

!**** *ACSOLW  * - DETERMINATION DES CONTENUS EN EAU CARACTERISTIQUES DU SOL

!     Sujet.
!     ------

!     - ROUTINE DE CALCUL INTERMEDIAIRE.
!       DETERMINATION DES CONTENUS EN EAU CARACTERISTIQUES DU SOL.

!    SUR MER      (PLSM=0. , PIVEG=NTVMER    )
!       wwilt=0.        wfc=1.        wsat=1.
!    SUR BANQUISE (PLSM=1. , PIVEG=NTVGLA*1.1) (MODE CLIMAT SEULEMENT)
!       wwilt=0.        wfc=1.        wsat=1.
!    SUR GLACIER  (PLSM=1. , PIVEG=NTVGLA    )
!       wwilt=f1(arg)   wfc=f2(arg)   wsat=wfc
!    SUR TERRE    (PLSM=1. , AUTRES PIVEG    )
!       wwilt=f1(arg)   wfc=f2(arg)   wsat=f3(sab)

!**   Interface.
!     ----------
!        *CALL* *ACSOLW*

!-----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
!          "APLPAR" CODE.
!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.

! - NOM DES CLES LOGIQUES

! LDHMT      : CALCULS REDUITS (POUR L'ANALYSE OU FULL-POS)
!              SEULS LES TABLEAUX (*) SONT UTILISES/CALCULES EN E/S

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 1D (GEOGRAPHIQUE) .

! PARG (*)   : POURCENTAGE D'ARGILE DANS LA MAILLE.
! PD2        : EPAISSEUR DU RESERVOIR PROFOND.
! PLSM (*)   : INDICE TERRE/MER.
! PIVEG      : TYPE DE SURFACE (MER, BANQUISE OU GLACIER, TERRE).
! PSAB       : POURCENTAGE DE SABLE DANS LA MAILLE.

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 1D (DIAGNOSTIQUE) .

! PWFC   (*) : TENEUR EN EAU A LA CAPACITE AUX CHAMPS.
! PWPMX      : TENEUR EN EAU MAXIMALE DU RESERVOIR PROFOND.
! PWSAT      : TENEUR EN EAU A LA SATURATION
! PWSMX      : TENEUR EN EAU MAXIMALE DU RESERVOIR DE SURFACE.
! PWWILT (*) : TENEUR EN EAU CORRESPONDANT AU POINT DE FLETRISSEMENT.

!-----------------------------------------------------------------------

! -   ARGUMENTS IMPLICITES.
!     ---------------------

! COMMON/YOMPHY1/

!-----------------------------------------------------------------------

!     Externes.
!     ---------

!     Methode.
!     --------

!     Auteur.
!     -------
!        99-02, D. Giard, d'apres ACSOL

!     Modifications.
!     --------------
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!        2011-06: M. Jerczynski - some cleaning to meet norms
!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMPHY1  , ONLY : TPHY1

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(TPHY1)       ,INTENT(IN)    :: YDPHY1
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PARG(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PD2(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PIVEG(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSAB(KLON) 
LOGICAL           ,INTENT(IN)    :: LDHMT 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWFC(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWPMX(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWSAT(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWSMX(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWWILT(KLON) 

!-----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLON

REAL(KIND=JPRB) :: ZARG, ZEPS, ZSAB
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACSOLW',0,ZHOOK_HANDLE)
ASSOCIATE(NTVGLA=>YDPHY1%NTVGLA, EWWILT=>YDPHY1%EWWILT, RD1=>YDPHY1%RD1, &
 & G1WSAT=>YDPHY1%G1WSAT, GCONV=>YDPHY1%GCONV, GWFC=>YDPHY1%GWFC, &
 & EWFC=>YDPHY1%EWFC, G2WSAT=>YDPHY1%G2WSAT, GWWILT=>YDPHY1%GWWILT)
!-----------------------------------------------------------------------

ZEPS=1.E-1_JPRB

!     ------------------------------------------------------------------
!     I - CALCULS REDUITS DANS LE CAS LDHMT=.TRUE.
!         ----------------------------------------
!         REDUCED COMPUTATIONS IN CASE LDHMT=.T.
!         --------------------------------------

! ** WFC , WWILT **

!DEC$ IVDEP
DO JLON=KIDIA,KFDIA
  IF ( PLSM(JLON) <= 0.5_JPRB ) THEN
    PWWILT(JLON) = 0.0_JPRB
    PWFC  (JLON) = 1.0_JPRB
  ELSE
    ZARG = MAX(ZEPS,PARG(JLON))
    PWWILT(JLON) = GWWILT*(ZARG**EWWILT)
    PWFC  (JLON) = GWFC*(ZARG**EWFC)
  ENDIF
ENDDO

IF ( .NOT. LDHMT ) THEN
  
!     ------------------------------------------------------------------
!     II - CALCUL DES AUTRES CHAMPS
!          ------------------------
!          COMPUTING OTHER FIELDS
!          ----------------------
  
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA
  
! ** CAS DE LA BANQUISE EN MODE CLIMAT - SEA-ICE - (CLIMAT) **
  
    IF ( NINT(10._JPRB*PIVEG(JLON)) == (10*NTVGLA+1) ) THEN
      PWWILT(JLON) = 0.0_JPRB
      PWFC  (JLON) = 1.0_JPRB
    ENDIF
  
! ** WSAT **
  
    IF ( PLSM(JLON) <= 0.5_JPRB .OR. NINT(PIVEG(JLON)) == NTVGLA ) THEN
      PWSAT(JLON) = PWFC(JLON)
    ELSE
      ZSAB = MAX(ZEPS,PSAB(JLON))
      PWSAT(JLON) = G1WSAT*ZSAB+G2WSAT
    ENDIF
  
! **  WSMX , WPMX **
  
    PWPMX(JLON) = PWSAT(JLON)*PD2(JLON)*GCONV
    PWSMX(JLON) = PWSAT(JLON)*RD1*GCONV
  
  ENDDO

ENDIF
  
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACSOLW',1,ZHOOK_HANDLE)

END SUBROUTINE ACSOLW

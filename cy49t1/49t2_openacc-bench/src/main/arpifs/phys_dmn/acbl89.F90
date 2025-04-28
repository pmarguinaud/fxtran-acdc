SUBROUTINE ACBL89  ( YDCST, YDPHY,YDPHY0,KIDIA,  KFDIA,    KLON,   KTDIAN, KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT  2D .
 & PAPHI,  PAPHIF,   PAPRS,  PAPRSF, PT,&
 & PECT,   PQV,      PQICE,  PQLI, PNLAB, PNLABCVP ,&
 ! - INPUT  1D .
 & PGZ0,   PTS,&
 ! - OUTPUT 2D .
 & PUSLE,  PLMECT,   PPHI3)

!**** *ACBL89 - CALCUL DES LONGUEURS DE MELANGE ET DE DISSIPATION,
!               D'APRES LA METHODE DE BOUGEAULT-LACARRERRE (1989),
!               MODIFIEE EN 2000 COMME DANS MESO-NH (LES EXPOSANTS
!               -2/3 ET -3/2), AVEC DES SECURITES POUR LE CLIMAT :    
!               L > MIN(RG*ALMAVE, VKARMN*ZPHIH). 

!     Sujet.
!     ------
!     - ROUTINE DE CALCUL ACTIF .
!       CALCUL DE LA LONGUEUR DE MELANGE "PLMECT=g*L_mix" ET DE 
!       L'INVERSE DE LA LONGUEUR DE DISSIPATION "PUSLE=1/(ALD*g*L_diss)"

!     - COMPUTATION OF THE MIXING LENGTH "PLMECT=g*L_mix"
!       AND THE DISSIPATIVE LENGTH "PUSLE=1/(ALD*g*L_diss)".

!**   Interface.
!     ----------
!        *CALL* *ACBL89*

!-----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
!          "APLPAR" CODE, EXCEPT FOR KTDIAN.
!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT.
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KTDIAN     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE + NEBULOSITE.
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PAPHI      : GEOPOTENTIEL AUX DEMI-NIVEAUX.
! PAPRS      : PRESSION AUX DEMI-NIVEAUX.

! - 2D (1:KLEV) .

! PAPHIF     : GEOPOTENTIEL AUX NIVEAUX DES COUCHES.
! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
! PT         : TEMPERATURE (APRES AJUSTEMENT CONVECTIF).
! PECT       : ENERGIE CINETIQUE TURBULENTE.
! PQV        : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PQICE      : HUMIDITE SPECIFIQUE SOLIDE.
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE.
! PNLAB      : Si 1 Presence d'un nuage Shallow
! PNLABCVP   : Si 1 Presence d'un nuage Deep

! - 1D (DIAGNOSTIQUE) .

! PGZ0       : G FOIS LA LONGUEUR DE RUGOSITE COURANTE.
! PTS        : TEMPERATURE DE SURFACE

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (1:KLEV) .

! PUSLE      : INVERSE DE LA LONGUEUR DE DISSIPATION MULTIPLIEE PAR G
! PLMECT     : UNE LONGUEUR DE MELANGE (FOIS G) POUR ACNEBR
! PPHI3      : LA FONCTION DE REDESLPERGER POUR K_T

!-----------------------------------------------------------------------

! -   ARGUMENTS IMPLICITES.
!     ---------------------

! COMMON/YOMCST /
! COMMON/YOMPHY0/

!-----------------------------------------------------------------------

!     Externes.
!     ---------

!     Methode.
!     --------

!     Auteur.
!     -------
!      2003-03, P. Marquet.
!              - - - - - - - - - - - - - - - - - - - - - - - - - - -
!               From the FIRST part of the old ACCOEFKE code,
!               written in 1993 in ARPEGE format by P. Lacarrere
!               from the old PERIDOT code, then tested by C. Bossuet
!               in 1997/98 using an Eulerian T42 Arpege GCM, then 
!               improved and tested by P. Marquet with the next 
!               versions of Arpege GCM (semi-lagrangian, SCM,
!               EUROCS Shallow convection case, with the use of
!               the new ideas coming from Meso-NH developments).
!              - - - - - - - - - - - - - - - - - - - - - - - - - - -

!     Modifications.
!     --------------
!      2004-06-03, P. Marquet : a limitation of ZG2L2SLD2 according to
!                  the use of ALMAVE>0, for which the minimal value of
!                  2.*ARSC1 must be granted in stable cases.
!                  If ALMAVE>0, then (ZGLMCBR)*2 is no longer equal to 
!                  2*e*Theta/[dTheta/dphi] and the MIN(_TWO_, ...) in
!                  factor of ARSC1 ensure the limit value of 2.*ARSC1,
!                  even if ZGLMCBR=ALMAVE>0.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!      2007-10-26, E. Bazile and S. Malardel : bugs correction for Lm_down
!                  ZDLUP1 replaced by ZDLDN1 and add deltaZ to Lm_down   
!      2008-07-18  Y. Bouteloup : 1) Modification of mixing length in case of 
!                                    shallow or deep convection cloud
!                                 2) : lup(j) = max(lup(j),lup(j+1)-delta_phi)
!                                 3) : ldn(j) = max(ldn(j),ldn(j-1)-delta_phi)
!      2008-10-03 E. Bazile : optimisation du calcul L=Lup**-2/3+Ldw**-2/3
!      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!      R. El Khatib 22-Jul-2014 Vectorizations
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!     R. El Khatib 06-Sep-2023 loop fusion
!-----------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY :  TCST  
USE YOMPHY   , ONLY : TPHY
USE YOMPHY0  , ONLY : TPHY0

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(TPHY)        ,INTENT(IN)    :: YDPHY
TYPE(TPHY0)       ,INTENT(IN)    :: YDPHY0
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAN 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNLAB(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNLABCVP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGZ0(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PUSLE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLMECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PPHI3(KLON,KLEV) 

!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZGDZF   (KLON,KLEV),ZGDZH  (KLON,KLEV)
REAL(KIND=JPRB) :: ZTHETAH (KLON,0:KLEV),ZTHETA (KLON,KLEV),ZDTHETA (KLON,KLEV)
REAL(KIND=JPRB) :: ZTHETAP (KLON,KLEV)

REAL(KIND=JPRB) :: ZEN(KLON),ZGZBOT(KLON),ZGZTOP(KLON),ZGZBOTCVP(KLON),ZGZTOPCVP(KLON)
REAL(KIND=JPRB) :: ZGLMUP (KLON,KLEV),ZGLMDN (KLON,KLEV),ZPHIH(KLON,KLEV)
REAL(KIND=JPRB) :: ZTESTSAVE(KLON)

! Tableaux pour les sorties sur listing (1D seulement)

INTEGER(KIND=JPIM) :: JJLEV, JLEV, JLON

REAL(KIND=JPRB) ::  ZDLDN,  ZDLDN1,  ZDLDN2,   &
 & ZDLUP,  ZDLUP1,  ZDLUP2,   ZDPHI, &
 & ZEPSX,  ZINCR,   ZG2L2SLD2, &
 & ZGLDIS, ZGLKARMN,ZGLMCBR, &
 & ZGLMINF,ZGLMIX,  ZPHI3MAX, &
 & ZTEST,   ZTEST0, &
 & ZTESTM, ZUSX,    ZX,       ZZDTHVL, ZZDTHVLP, &
 & ZZTHVL, ZZTHVLP, ZQCS, ZGZLCVPUP, ZGZLCVPDN, &
 & Z2SQRT2, ZLUP(KLON), ZLDN,ZLWK0,ZLWK1(KLON)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACBL89',0,ZHOOK_HANDLE)
ASSOCIATE(ALMAVX=>YDPHY0%ALMAVX, ACBRPHIM=>YDPHY0%ACBRPHIM, &
 & VKARMN=>YDPHY0%VKARMN, ALD=>YDPHY0%ALD, ECTMIN=>YDPHY0%ECTMIN, &
 & ALMAVE=>YDPHY0%ALMAVE, ARSC1=>YDPHY0%ARSC1, &
 & RATM=>YDCST%RATM, RETV=>YDCST%RETV, RG=>YDCST%RG, RKAPPA=>YDCST%RKAPPA, &
 & LECSHAL=>YDPHY%LECSHAL, LECDEEP=>YDPHY%LECDEEP)
!-----------------------------------------------------------------------

!*
!     ------------------------------------------------------------------
!     1 - CALCULS PRELIMINAIRES
!     ------------------------------------------------------------------

ZEPSX    = ECTMIN

!   UNE CONSTANTE POUR LA FONCTION "PHI3" (Cuxart Bougeault Redels.)
!   A CONSTANT FOR "PHI3" FUNCTION        (Quart. J. Roy. Met. Soc.)

ZPHI3MAX= (1.0_JPRB-ACBRPHIM)/ACBRPHIM

!   TABLEAUX DE TRAVAIL
!   WORK ARRAYS ONCE FOR ALL

DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZGDZH(JLON,JLEV) = PAPHIF(JLON,JLEV)  -PAPHI (JLON,JLEV)
  ENDDO ! JLON
ENDDO   ! JLEV
DO JLEV=KTDIAN+1,KLEV
  DO JLON=KIDIA,KFDIA
    ZGDZF(JLON,JLEV) = PAPHIF(JLON,JLEV-1)-PAPHIF(JLON,JLEV)
  ENDDO ! JLON
ENDDO   ! JLEV


!!  Calcul de ZGZTOP et ZGZBOT de la cvpp et de la cvp

ZGZTOP(:) = 0.0_JPRB
ZGZBOT(:) = 100000.0_JPRB

ZGZTOPCVP(:) = 0.0_JPRB
ZGZBOTCVP(:) = 100000.0_JPRB

DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
     ZGZTOP(JLON) = MAX(ZGZTOP(JLON),PAPHIF(JLON,JLEV)*PNLAB(JLON,JLEV))
     ZGZBOT(JLON) = (1.0_JPRB-PNLAB(JLON,JLEV))*ZGZBOT(JLON)&
    &             + PNLAB(JLON,JLEV)*MIN(ZGZBOT(JLON),PAPHIF(JLON,JLEV))
  ENDDO
ENDDO    

DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
     ZGZTOPCVP(JLON) = MAX(ZGZTOPCVP(JLON),PAPHIF(JLON,JLEV)*PNLABCVP(JLON,JLEV))
     ZGZBOTCVP(JLON) = (1.0_JPRB-PNLABCVP(JLON,JLEV))*ZGZBOTCVP(JLON)&
    &             + PNLABCVP(JLON,JLEV)*MIN(ZGZBOTCVP(JLON),PAPHIF(JLON,JLEV))
  ENDDO
ENDDO    

!*
!     ------------------------------------------------------------------
!     IV - DEFINITION DE TABLEAUX DE TRAVAIL POUR LES CALCULS A SUIVRE.
!          (CALCUL DES TEMPERATURES POTENTIELLES SECHES ET HUMIDES)
!     ------------------------------------------------------------------

! - - - - - - - - - - -
! CALCULS DE THETA (sec)
! - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZTHETA(JLON,JLEV)  = (PT(JLON,JLEV)*(RATM/PAPRSF(JLON,JLEV))**(RKAPPA)) &

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! CALCUL DE (THETA)vl = THETA * ( 1 + RETV*Qv - (Ql+Qi) )
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    & *(1.0_JPRB+RETV*PQV(JLON,JLEV)-(PQLI(JLON,JLEV)+PQICE(JLON,JLEV)))
  ENDDO
ENDDO

! - - - - - - - - - - - - - - - - -
! CALCULS DE (THETA)vl/Half-level
! - - - - - - - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZTHETAH(JLON,JLEV) = 0.5_JPRB*(ZTHETA(JLON,JLEV)+ZTHETA(JLON,JLEV+1))
  ENDDO
ENDDO
DO JLON=KIDIA,KFDIA
  ZTHETAH(JLON,KLEV)   = PTS(JLON)*(RATM/PAPRS(JLON,KLEV))**(RKAPPA)
ENDDO

! - - - - - - - - - - - -
! CALCULS DE d(THETA)vl
! - - - - - - - - - - - -
!DO JLON=KIDIA,KFDIA
!  ZDTHETA(JLON,KTDIAN) = ZTHETA(JLON,MAX(KTDIAN-1,1))&
!                    &   -ZTHETA(JLON,MAX(KTDIAN  ,1))
!ENDDO
DO JLEV=KTDIAN+1,KLEV
  DO JLON=KIDIA,KFDIA
    ZDTHETA(JLON,JLEV) = ZTHETA(JLON,JLEV-1)-ZTHETA(JLON,JLEV)
  ENDDO
ENDDO

!*
!     ------------------------------------------------------------------
!     V - CALCUL DES LONGUEURS DE MELANGE ET DE DISSIPATION.
!         ON ATTRIBUT A UNE PARTICULE L'ENERGIE CINETIQUE MOYENNE
!         DONT ELLE EST ISSUE, ON REGARDE SES DEPLACEMENTS MAXIMA
!         VERS LE HAUT "ZGLMUP" ET VERS LE BAS "ZGLMDN". ON FABRIQUERA
!         "PLMECT" ET "PUSLE" EN MOYENNANT CES DEUX QUANTITES.
!     ------------------------------------------------------------------

!         Pour ne pas ruiner les vectorisations on calcule les deux cas
!         ZEN(JLON)>ZINCR ==> ZDLUP1 (on atteint le niveau cible)
!         ZEN(JLON)<ZINCR ==> ZDLUP2 (on s'arrete entre 2 niveaux)
!         Et on utilise : ZTEST=1 pour garder ZDLUP1 et ZTEST=0
!         pour garder ZDLUP2.

!         De plus, il ne faut incrementer ZGLMUP(JLON) ou ZGLMDN(JLON)
!         que si ZEN(JLON) est positif au depart : ZTEST0=1., car
!         on est amene a continuer les boucles vers le haut et
!         vers le bas jusqu'a ce que TOUS les points ont atteint
!         leur seuil de flottabilite. ZEN(JLON) peut donc etre negatif
!         pour les premiers points a atteindre ce seuil...

!         Le calcul de ZDLDN2 est general sauf si ZUSX=1/ZX est proche
!         de 0 (si ZINCR -> 0). Dans ce cas, comme la fonction est
!         continue et egale a SQRT(PZEN(JLON)/ZINCR)*ZDLDN1 avec
!         0<PZEN(JLON)/ZINCR<1, il suffit de prendre une valeur
!         seuil ZEPSX pour la variable ZUSX afin d'assurer la
!         continuite de ZDLDN2(ZUSX,ZEN(JLON)/ZINCR).

!         Les divers ABS() qui interviennent dans les SQRT() ne
!         devraient pas etre necessaires, sinon pour remedier au
!         probleme de faire tous les calculs ZDLUP1/ZDLUP2 et
!         ZDLDN1/ZDLDN2 pour garantir la vectorisation, y compris
!         dans des cas ou les formules degenerent mal (mais
!         seules les formules pertinantes sont retenues ensuite
!         en fonction de ZTEST).

!     ------------------------------------------------------------------

ZQCS = 1.0E-6_JPRB

!     --------------------------------------
DO JLEV=KTDIAN,KLEV-1 ! BOUCLE GENERALE
!     --------------------------------------

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Pour les modifications "wet-BL89" de l'INM
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Calcul de (Theta)vl de la particule apres deplacement
  ! du demi-niveaux (JK) vers autres niveaux pleins, en
  ! conservant (Theta)l et Qtot au cours du deplacement.
  ! On a (THETA)vl = [ (THETA)l + Lv*THETA/Cp/T*(Ql+Qi) ]
  !                 *[ 1 + (1+RETV)*Qv - Qtot ]
  ! et ici : JLEV+1 varie entre KTDIAN+1 et KLEV (donc OK)
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  DO JJLEV=KTDIAN,KLEV
    DO JLON=KIDIA,KFDIA
      ZTHETAP(JLON,JJLEV) = ZTHETAH(JLON, JLEV)
    ENDDO ! JLON
  ENDDO ! JJLEV

  ! - - - - - - - - - - - - - - - - - - - - -
  ! BOUCLE VERS LE HAUT QUI CALCULE ZGLMUP :
  ! - - - - - - - - - - - - - - - - - - - - -

  ! L'energie (ZEN) et valeur initiale de "Lup"

  DO JLON=KIDIA,KFDIA
    ZEN   (JLON)=PECT(JLON,JLEV)
    ZGLMUP(JLON,JLEV)=0.0_JPRB
  ENDDO ! JLON

  ! Le passage du demi-niveau JLEV au niveau
  ! JLEV juste au dessus :

  DO JLON=KIDIA,KFDIA
    ZDLUP1 = ZGDZH(JLON,JLEV)
    ZINCR  =(ZTHETA (JLON,JLEV)-ZTHETAP(JLON,JLEV))&
     & /ZTHETAH(JLON,JLEV)/2.0_JPRB*ZDLUP1  
    ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
    ZTEST0 =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON))
    ZTEST  =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON)-ZINCR)
    ZTESTSAVE(JLON) = ZTEST
    ZDLUP2 =SQRT(ABS(ZEN(JLON)/ZINCR))*ZDLUP1
    ZDLUP  =ZTEST*ZDLUP1+(1.0_JPRB-ZTEST)*ZDLUP2
    ZGLMUP(JLON,JLEV)=ZGLMUP(JLON,JLEV)+ZDLUP*ZTEST0
    ZEN   (JLON)=ZEN  (JLON)-ZINCR*ZTEST0
  ENDDO
  ZTESTM=SUM(ZTESTSAVE(KIDIA:KFDIA))

  ! On boucle ensuite en passant du niveau JJLEV
  ! au niveau JJLEV-1 :

  DO JJLEV=JLEV,KTDIAN+1,-1
    IF (ZTESTM > 0.0_JPRB) THEN
      DO JLON=KIDIA,KFDIA
        ZDLUP1   = ZGDZF(JLON,JJLEV)
        ZZTHVL   =(ZTHETA (JLON,JJLEV)+ZTHETA (JLON,JJLEV-1))/2.0_JPRB
        ZZTHVLP  =(ZTHETAP(JLON,JJLEV)+ZTHETAP(JLON,JJLEV-1))/2.0_JPRB
        ZINCR    =(ZZTHVL-ZZTHVLP)/ZTHETAH(JLON,JLEV)*ZDLUP1
        ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
        ZTEST0   =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON))
        ZTEST    =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON)-ZINCR)
        ZTESTSAVE(JLON) = ZTEST
        ZZDTHVL  = ZTHETA (JLON,JJLEV-1)-ZTHETA (JLON,JJLEV)
        ZZDTHVLP = ZTHETAP(JLON,JJLEV-1)-ZTHETAP(JLON,JJLEV)
        ZX    =(ZZDTHVL-ZZDTHVLP)/ZTHETAH(JLON,JLEV)/ZINCR*ZDLUP1
        ZUSX  =1.0_JPRB/(SIGN(1.0_JPRB,ZX)*MAX(ZEPSX,ABS(ZX)))
        ZDLUP2=( -(ZUSX-0.5_JPRB) +SIGN(1.0_JPRB,ZX)*&
         & SQRT(ABS((ZUSX-0.5_JPRB)*(ZUSX-0.5_JPRB)&
         & +2.0_JPRB*ZUSX*ABS(ZEN(JLON)/ZINCR))) )*ZDLUP1  
        ZDLUP =ZTEST*ZDLUP1+(1.0_JPRB-ZTEST)*ZDLUP2
        ZGLMUP(JLON,JLEV)=ZGLMUP(JLON,JLEV)+ZDLUP*ZTEST0
        ZEN   (JLON)=ZEN  (JLON)-ZINCR*ZTEST0
      ENDDO
      ZTESTM=SUM(ZTESTSAVE(KIDIA:KFDIA))
    ENDIF
  ENDDO

  ! - - - - - - - - - - - - - - - - - - - -
  ! BOUCLE VERS LE BAS QUI CALCULE ZGLMDN :
  ! - - - - - - - - - - - - - - - - - - - -

  ! L'energie (ZEN) et valeur initiale de "Ldown"

  DO JLON=KIDIA,KFDIA
    ZEN   (JLON)=PECT(JLON,JLEV)
    ZGLMDN(JLON,JLEV)=0.0_JPRB
  ENDDO ! JLON

  ! Le passage du demi-niveau JLEV au niveau
  ! JLEV+1 juste au dessous :

  DO JLON=KIDIA,KFDIA
    ZDLDN1 = ZGDZF  (JLON,JLEV+1)-ZGDZH (JLON,JLEV)
    ZINCR  =(ZTHETAP(JLON,JLEV+1)-ZTHETA(JLON,JLEV+1))&
     & /ZTHETAH(JLON,JLEV)/2.0_JPRB*ZDLDN1  
    ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
    ZTEST0 =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON))
    ZTEST  =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON)-ZINCR)
    ZTESTSAVE(JLON) = ZTEST
    ZDLDN2 =SQRT(ABS(ZEN(JLON)/ZINCR))*ZDLDN1
    ZDLDN  =ZTEST*ZDLDN1+(1.0_JPRB-ZTEST)*ZDLDN2
    ZGLMDN(JLON,JLEV)=ZGLMDN(JLON,JLEV)+ZDLDN*ZTEST0
    ZEN   (JLON)=ZEN  (JLON)-ZINCR*ZTEST0
  ENDDO
  ZTESTM=SUM(ZTESTSAVE(KIDIA:KFDIA))

  ! On boucle ensuite en passant du niveau JJLEV
  ! au niveau JJLEV+1 :
  DO JJLEV=JLEV+1,KLEV-1
    IF (ZTESTM > 0.0_JPRB) THEN
      ZTESTM=0.0_JPRB
      DO JLON=KIDIA,KFDIA
        ZDLDN1   = ZGDZF(JLON,JJLEV+1)
        ZZTHVL   =(ZTHETA (JLON,JJLEV)+ZTHETA (JLON,JJLEV+1))/2.0_JPRB
        ZZTHVLP  =(ZTHETAP(JLON,JJLEV)+ZTHETAP(JLON,JJLEV+1))/2.0_JPRB
        ZINCR    =(ZZTHVLP-ZZTHVL)/ZTHETAH(JLON,JLEV)*ZDLDN1
        ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
        ZTEST0   =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON))
        ZTEST    =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON)-ZINCR)
        ZTESTSAVE(JLON) = ZTEST
        ZZDTHVL  = ZTHETA (JLON,JJLEV)-ZTHETA (JLON,JJLEV+1)
        ZZDTHVLP = ZTHETAP(JLON,JJLEV)-ZTHETAP(JLON,JJLEV+1)
        ZX    =(ZZDTHVL-ZZDTHVLP)/ZTHETAH(JLON,JLEV)/ZINCR*ZDLDN1
        ZUSX  =1.0_JPRB/(SIGN(1.0_JPRB,ZX)*MAX(ZEPSX,ABS(ZX)))
        ZDLDN2=( -(ZUSX-0.5_JPRB) +SIGN(1.0_JPRB,ZX)*&
         & SQRT(ABS((ZUSX-0.5_JPRB)*(ZUSX-0.5_JPRB)&
         & +2.0_JPRB*ZUSX*ABS(ZEN(JLON)/ZINCR))) )*ZDLDN1  
        ZDLDN=ZTEST*ZDLDN1+(1.0_JPRB-ZTEST)*ZDLDN2
        ZGLMDN(JLON,JLEV)=ZGLMDN(JLON,JLEV)+ZDLDN*ZTEST0
        ZEN   (JLON)=ZEN  (JLON)-ZINCR*ZTEST0
      ENDDO
      ZTESTM=SUM(ZTESTSAVE(KIDIA:KFDIA))
    ENDIF
  ENDDO

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! CALCUL EFFECTIF DES LONGUEURS DE MELANGE : "Lmel" et "Ldiss"
  ! ET CALCUL DES COEFFICIENTS DE MELANGE "PKUROV" ET "PKTROV".
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! ON PREVOIT UNE LIMITATION INFERIEURE (PAS SUPERIEURE CAR LA
  ! LOI POUR FAIBLES "z" EST PLUTOT EN "2.8*z" QU'EN "0.4*z") :
  !    -> ALMAVE       :  DANS L'ATMOSPHERE LIBRE
  !    -> KARMANN*"z"  :  DANS LA CLP
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  
  DO JLON=KIDIA,KFDIA

    ! On limite Ldown par la hauteur :

    ZPHIH(JLON,JLEV)   =PAPHI(JLON,JLEV)-PAPHI(JLON,KLEV)+PGZ0(JLON)
    ZTEST0   =0.5_JPRB+SIGN(0.5_JPRB,ZEN(JLON))
    ZGLMDN(JLON,JLEV)=ZGLMDN(JLON,JLEV)+ZTEST0*(PAPHIF(JLON,KLEV)-PAPHI(JLON,KLEV))

    ZGLMDN(JLON,JLEV) =MIN(ZPHIH(JLON,JLEV), ZGLMDN(JLON,JLEV))
    
    ! On majore par la hauteur de cvpp et de cvp
    
    IF (LECSHAL) THEN
      ZGLMUP(JLON,JLEV) = MAX(ZGLMUP(JLON,JLEV),PNLAB(JLON,JLEV)*(ZGZTOP(JLON)-PAPHI(JLON,JLEV)))
      ZGLMDN(JLON,JLEV) = MAX(ZGLMDN(JLON,JLEV),PNLAB(JLON,JLEV)*(PAPHI(JLON,JLEV)-ZGZBOT(JLON)))
    ENDIF
    IF (LECDEEP) THEN  
      ZGZLCVPUP = MIN(RG*ALMAVX,ZGZTOPCVP(JLON)-PAPHI(JLON,JLEV))
      ZGZLCVPDN = MIN(RG*ALMAVX,PAPHI(JLON,JLEV)-ZGZBOTCVP(JLON))
      ZGLMUP(JLON,JLEV) = MAX(ZGLMUP(JLON,JLEV),PNLABCVP(JLON,JLEV)*ZGZLCVPUP)
      ZGLMDN(JLON,JLEV) = MAX(ZGLMDN(JLON,JLEV),PNLABCVP(JLON,JLEV)*ZGZLCVPDN)
    ENDIF  

  ENDDO  ! jlon  
ENDDO ! Jlev    


!     --------------------------------------
DO JLEV=KLEV-2,KTDIAN,-1 ! BOUCLE GENERALE Nr 2
!     --------------------------------------
  DO JLON=KIDIA,KFDIA

    !  On veut que ca monte et descende au moins a la meme hauteur !!
    
    ZGLMUP(JLON,JLEV) = MAX(ZGLMUP(JLON,JLEV),ZGLMUP(JLON,JLEV+1)&
    &  + PAPHI(JLON,JLEV+1) - PAPHI(JLON,JLEV))

  ENDDO  ! jlon  
ENDDO ! Jlev    

!     --------------------------------------
DO JLEV=KTDIAN+1,KLEV-1 ! BOUCLE GENERALE Nr 3
!     --------------------------------------
  DO JLON=KIDIA,KFDIA

    ZGLMDN(JLON,JLEV) = MAX(ZGLMDN(JLON,JLEV),ZGLMDN(JLON,JLEV-1)&
    &  + PAPHI(JLON,JLEV)   - PAPHI(JLON,JLEV-1))

  ENDDO  ! jlon  
ENDDO ! Jlev    

Z2SQRT2=2._JPRB*SQRT(2._JPRB)

!     --------------------------------------
DO JLEV=KTDIAN,KLEV-1 ! BOUCLE GENERALE Nr 4
!     --------------------------------------
  DO JLON=KIDIA,KFDIA
    
    ! La "nouvelle" longueur de melange de meso-NH 
    ! (devant approcher "2.8*g*z" pres de la surface) :

     ZLUP(JLON)=MAX(ZGLMUP(JLON,JLEV),1.E-10_JPRB)
     ZLDN=MAX(ZGLMDN(JLON,JLEV),1.E-10_JPRB)
     ZLWK0=ZLUP(JLON)/ZLDN
     ZLWK1(JLON)=1._JPRB+ZLWK0**(2.0_JPRB/3._JPRB)

  ENDDO ! JLON

!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA

     ZGLMCBR=Z2SQRT2*ZLUP(JLON)/(ZLWK1(JLON)*SQRT(ZLWK1(JLON)))

    ! La limitation : L > LINF = MIN( 0.4*(G*z) , ALMAVE )

    ZGLKARMN= VKARMN*ZPHIH(JLON,JLEV)
    ZGLMINF = MIN(RG*ALMAVE, ZGLKARMN)

    ZGLMIX   = MAX(ZGLMINF, ZGLMCBR)
    ZGLDIS   = MAX(ZGLMINF, ZGLMCBR)

    ! Les deux longueurs : (1) de melange ; (2) de dissipation

    PLMECT(JLON,JLEV)=ZGLMIX
    PUSLE (JLON,JLEV)=1.0_JPRB/(ALD*ZGLDIS)

    ! La fonction "PHI3" de Redeslperger ; neutre=1./(1.+2.*ARSC1)

    ZDPHI    =ZGDZF(JLON,JLEV+1)
    ZG2L2SLD2=MAX( ZPHI3MAX, ARSC1* MIN( 2.0_JPRB, ZGLMCBR*ZGLMCBR&
     & *ZDTHETA(JLON,JLEV+1)/ZDPHI&
     & /PECT(JLON,JLEV)/ZTHETAH(JLON,JLEV) )&
     & )  
    PPHI3 (JLON,JLEV)=1.0_JPRB/(1.0_JPRB+ZG2L2SLD2)

  ENDDO ! JLON

!     ------------------------------
ENDDO ! JLEV (BOUCLE GENERALE)
!     ------------------------------

!*
!     --------------------------------------------------------------------
!     VI  - AU DERNIER NIVEAU : CALCULS DE LA LONGEUR DE MELANGE "PLMECT"
!           DE L'INVERSE DE LA LONGUEUR DE DISSIPATION "PUSLE" ET DE LA
!           FONCTION "PPHI3" DE REDELSPERGER.
!     --------------------------------------------------------------------

!DEC$ IVDEP
DO JLON=KIDIA,KFDIA
  PLMECT(JLON,KLEV)= 0.5_JPRB*PLMECT(JLON,KLEV-1)
  PUSLE (JLON,KLEV)= 1.0_JPRB/(ALD*0.5_JPRB*PLMECT(JLON,KLEV-1))
  PPHI3 (JLON,KLEV)= PPHI3(JLON,KLEV-1)
ENDDO ! JLON
!*
!     ------------------------------------------------------------------

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACBL89',1,ZHOOK_HANDLE)
END SUBROUTINE ACBL89

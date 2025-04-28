SUBROUTINE ACTURB  ( YDCST, YDPHY,YDPHY0,KIDIA,  KFDIA,  KLON,   KTDIAT, KTDIAN, KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT  2D .
 & PAPHI,  PAPHIF, PAPRS,  PAPRSF, PR,     PT,&
 & PU, PV, PECT,   PQV,    LDCONV,  PLSCPE,&
 & PLMECT, PPHI3,&
 ! - INPUT  1D .
 & PCD,    PCH,    PGZ0,   PTS,    PQS,&
 ! - INPUT/OUTPUT  2D .
 & PQICE,  PQLI,&
 ! - OUTPUT 2D .
 & PKTROV,PKQROV,PKQLROV, PKUROV, PNBVNO,&
 & PPRODTH,PNEBS,  PQCS,   PL3F2,&
 ! - OUTPUT 1D .
 & PGKCLS, PECTCLS)

!**** *ACTURB - CALCUL DES COEFFICIENTS D'ECHANGE VERTICAL TURBULENT ET
!               DE LA PRODUCTION THERMIQUE HUMIDE <w'(THETA)vl'> (QUI
!               DEPEND DE LA FONCTION STATISTIQUE ASYMETRIQUE F2 DE
!               BOUGEAULT, PLUS L'AJOUT DU TERME "LAMBDA3" DE BECHTOLD).
!               CALCULS DES NEBULOSITE ET EAU CONDENSEE STRATIFORMES,
!               QUI DEPENDENT DES FONCTIONS STATISTIQUES ASYMETRIQUES
!               F0 ET F1 DE BOUGEAULT. CES FONCTIONS (F0,F1,F2) SONT
!               TABULEES, COMME DANS MESO-NH.

!     Sujet.
!     ------
!     - ROUTINE DE CALCUL ACTIF .
!       CALCUL DES COEFFICIENTS D'ECHANGES VERTICAUX TURBULENTS (DIMEN-
!       ON (DP/(G*DT)) ET DE LA STABILITE STATIQUE (DIMENSION (U/DP)**2)

!     - COMPUTATION OF VERTICAL TURBULENT EXCHANGE COEFFICIENTS
!       (DIMENSION (DP/(G*DT)) AND OF STATIC STABILITY (DIMENSION
!       (U/DP)**2) .

!**   Interface.
!     ----------
!        *CALL* *ACTURB*

!-----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
!          "APLPAR" CODE, EXCEPT FOR KTDIAT AND KTDIAN.
!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT.
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KTDIAT     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE.
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
! PR         : CONSTANTE DES GAZ POUR L'AIR.
! PT         : TEMPERATURE (APRES AJUSTEMENT CONVECTIF).
! PU         : COMPOSANTE EN X DU VENT.
! PV         : COMPOSANTE EN Y DU VENT.
! PECT       : ENERGIE CINETIQUE TURBULENTE.
! PQV        : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! LDCONV     : INDICE DE CONVECTION
! PLSCPE     : RAPPORT EFECTIF DES L ET CP EN CONDENSATION/EVAPORATION.
! PQICE      : HUMIDITE SPECIFIQUE  SOLIDE "PRONOSTIQUE".
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE "PRONOSTIQUE".
! PLMECT     : UNE LONGUEUR DE MELANGE (FOIS G) POUR ACNEBR

! - 1D (DIAGNOSTIQUE) .

! PCD        : COEFFICIENT D'ECHANGE EN SURFACE POUR U ET V
! PCH        : COEFFICIENT D'ECHANGE EN SURFACE POUR T ET Q
! PGZ0       : G FOIS LA LONGUEUR DE RUGOSITE COURANTE.
! PTS        : TEMPERATURE DE SURFACE
! PQS        : HUMIDITE SPECIFIQUE DE SURFACE.

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PKTROV     : COEFFICIENT D'ECHANGE VERTICAL DE T EN KG/(M*M*S).
! PKQROV     : COEFFICIENT D'ECHANGE VERTICAL DE Q EN KG/(M*M*S).
! PKQLROV    : COEFFICIENT D'ECHANGE VERTICAL DE QL EN KG/(M*M*S).
! PKUROV     : COEFFICIENT D'ECHANGE VERTICAL DE U ET V EN KG/(M*M*S).
!              !! PKUROV et PKTROV : egaux a g*K*P/(R*T*d(Phi))
! PNBVNO     : CARRE DE FR. BRUNT-VAISALA DIVISEE PAR G FOIS LA DENSITE.

! - 2D (1:KLEV) .

! PQICE      : HUMIDITE SPECIFIQUE  SOLIDE "RADIATIVE".
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE "RADIATIVE".
! PPRODTH    : LA PRODUCTION THERMIQUE : +(g/T)*(w'X') avec X=(THETA)vl
! PNEBS      : NEBULOSITE PARTIELLE STRATIFORME.
! PQCS       : EAU CONDENSEE STRATIFORME.
! PL3F3      : PRODUIT DES FONCTIONS "LAMBDA3" ET "F2" DE BECHTOLD ET DE
!              BOUGEAULT, INTERVENANT DANS LA PONDERATION DES PARTIES
!              "AIR SEC" ET "AIR SATURE" (PRODUCTION THERMIQUE, FLUX
!              TURBULENTS "HUMIDES", etc ...)

! - 1D (KLON) .

! PGKCLS     : COEFFICIENT D'ECHANGE A LA SURFACE !! en (m/s)**3 (=g*K)
! PECTCLS    : ENERGIE CINETIQUE TURBULENTE A LA SURFACE.

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
!      2002-03, P. Marquet.
!              - - - - - - - - - - - - - - - - - - - - - - - - - - -
!               From the LAST part of the old ACCOEFKE code,
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
!      2003-11-05, P. Marquet : LLIMQ1 switch
!      2004-03-19, P. Marquet : PQLI and PQICE = prognostic as input,
!                                              = statiform  as output.
!      2005-02-02, P. Marquet : Top PBL Entrainment (if LPBLE), from
!                  the ideas tested in ACCOFGY (H. Grenier, F. Gueremy)
!      2005-02-18, P. Marquet : ZECTBLK in limited by PECTCLS.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!      2007-05-09, E. Bazile : ZEPSIG and no cloud with ZSTAB=0 at the surface.
!      2007-05-09, Y. Bouteloup : AGRE2 for LPBLE.
!      2008-02-18, P. Marquet : set ILEVT to ILEVBI(JLON)
!                               and no more  ILEVBI(JLON)-1
!      2008-02-21, Y. Bouteloup : LECTREP + LECTQ1
!      2008-03-19, P. Marquet : change the definition of ZTHETAVL
!                  (see page 360 and the appendix-B in Grenier
!                   and Bretherton, MWR 2001)
!      2008-04-25, E. Bazile and P. Marquet : Correction for the AGRE2 term
!                  with  A1*[1+A2*L*qc/(cp*d(theta_vl))] instead of
!                   A1*[1+A2/d(theta_vl)], with "L*qc/cp" missing...)
!      2008-10-06, E. Bazile : Computation of a 'unified' PBL height for
!                  the TKEcls, the top-entrainment and the diagnostic
!      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!      2011-06: M. Jerczynski - some cleaning to meet norms
!      2012-01-28, E. Bazile Correction for ZDTL and new option  LECTFL0
!      2016-10-04, P. Marguinaud Port to single precision
!      2018-09-19, R. Roehrig: contribution from climate model (from JF Guérémy and D. StMartin)
!                        - case AGREF<0 for top-entrainment
!                        - LDISTUR: discretization option + reproduce climate results
!                        - LDIFCEXP: correction of the T turb coef
!                        - LCVTURB: minimum of turbulence if convection and
!                                   sursaturation above 650hPa
!                        - limitation for condensed water (lower than total water)
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!-----------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB      ,JPRD
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY :  TCST
USE YOMPHY0  , ONLY : TPHY0
USE YOMPHY   , ONLY : TPHY
USE YOMLSFORC, ONLY : LMUSCLFA,NMUSCLFA

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(TPHY)        ,INTENT(IN)    :: YDPHY
TYPE(TPHY0)       ,INTENT(IN)    :: YDPHY0
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAT
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAN
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV)
LOGICAL           ,INTENT(IN)    :: LDCONV(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSCPE(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLMECT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PPHI3(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCD(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGZ0(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS(KLON)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQICE(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQLI(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKTROV(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKQROV(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKQLROV(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKUROV(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNBVNO(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PPRODTH(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNEBS(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQCS(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PL3F2(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGKCLS(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PECTCLS(KLON)

!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZSTAB (KLON), ZRS(KLON),ZBLH(KLON)
REAL(KIND=JPRB) :: ZRTV   (KLON,KLEV)
REAL(KIND=JPRB) :: ZGDZF   (KLON,KLEV),ZZ     (KLON,KLEV)
REAL(KIND=JPRB) :: ZTHETA  (KLON,KLEV)
REAL(KIND=JPRB) :: ZTHETALF(KLON,KLEV),ZLOCPEXF(KLON,KLEV)
REAL(KIND=JPRB) :: ZTHETAVL(KLON,KLEV)

REAL(KIND=JPRB) :: ZLMECTF(KLON,KLEV),ZECTF  (KLON,KLEV)
REAL(KIND=JPRB) :: ZGKTF  (KLON,KLEV)
REAL(KIND=JPRB) :: ZGKTH  (KLON,0:KLEV), ZGKUH  (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZLM  (KLON,0:KLEV) 

REAL(KIND=JPRB) :: ZUSTAR(KLON),ZWSTAR(KLON)

!- - - - - - - - - - - - - - - -
! For the Top-PBL Entrainment :
!- - - - - - - - - - - - - - - -
REAL(KIND=JPRB) :: ZECTINT(KLON)     ! ect moyenne de la cla (sans surf)
REAL(KIND=JPRB) :: ZQCINT (KLON)     ! qc_cloud moyen de la cla (sans surf)
INTEGER(KIND=JPIM) :: ICM(KLON,KLEV)    ! indice de couche melangee
INTEGER(KIND=JPIM) :: ILEVBI(KLON)      ! niveau de la base de l'inversion
REAL(KIND=JPRB) :: ZBI, ZDEN, ZECTBLK, ZNUM, ZLINV, ZQCBLK
REAL(KIND=JPRB) :: ZGKENT
INTEGER(KIND=JPIM) :: ICLA, ILEVM1, ILEVT

INTEGER(KIND=JPIM) :: IHCLPMAX, IHCLPMIN, IJLEVM1, IJLEVP1,&
 & INIV, INQ1, JLEV, JLON

LOGICAL :: LLIMQ1

REAL(KIND=JPRB) ::  Z2B, Z3B, Z3BCF, ZA, ZAA, ZCE1, ZCIS, ZCK, ZCTO, &
 & ZDD, ZDELTQF, ZDELTQF1, ZDELTQF2, ZDELTQH, &
 & ZDI, ZDIFFC, ZDIFFH, ZDLEWF, ZDLEWF1, ZDLEWF2, &
 & ZDPHI, ZDPHI0, ZDQLST, ZDQW, ZDS, &
 & ZDSTA, ZDT, ZDTETA, ZDTL, ZDU2, &
 & ZECTH, ZEPDELT, ZEPNEBS, ZECTBLH,&
 & ZEPS, ZEPS1, ZEPSQ, ZEPSQ1, ZEPSV, &
 & ZEW, ZEW1, ZEW2, ZFACT, ZGALP2, &
 & ZGLMT2, ZGLMU2, ZGLT, ZGLTZ, ZGLU, ZGLUZ, &
 & ZGZ, ZH, ZH1, ZH2, ZIGMAS, ZIGMAS2, &
 & ZINC, ZIS, ZLCPM1, ZLCPP1, ZLMECT, ZLOI, ZLOS, &
 & ZLSCPEF, ZLSCPEF1, ZLSCPEF2, ZLSCPEH, ZMAXQ1, &
 & ZMODU, ZNEBLOW, ZPHI3MIN, &
 & ZPHMAX, ZPHMIN, ZPREF, ZPRODC, ZPRODH, &
 & ZQ11, ZQ1MAX, ZQ1MIN, ZQC, ZQLF, ZQCF1, ZQCF2, ZQLH, &
 & ZQLM1, ZQLP1, ZQSATF, ZQSATF1, ZQSATF2, ZQSLTLF, &
 & ZQSLTLF1, ZQSLTLF2, &
 & ZQWF, ZQWF1, ZQWF2, ZQWH, ZRESUL, ZRH, ZRHOH, &
 & ZRIC, ZRICHF, ZRIH, ZRLTLU, ZROSDPHI, ZRQZERO, &
 & ZRTI, ZSIGMAS, ZSIGMAS2, ZSRC, ZSTA, ZSURSAT, &
 & ZTETA, ZTF, ZTF1, ZTF2, ZTH, ZTHETAOT, &
 & ZTLF, ZTLF1, ZTLF2, ZU, ZUSTAR2, &
 & ZWDIFF, ZWQW, ZWTL, ZZETF, ZZF0, ZZF1, &
 & ZZKTH, ZZLMF, ZZN1D, &
 & ZZQC, ZZRT, ZZT, ZL3F2, ZILIMQ1, ZEPSIG, &
 & ZHTOP, ZHBOT, ZSIGCR, ZPRETURB
REAL(KIND=JPRB) ::  ZPLS, ZDELTA, ZGAUSS,ZQV
REAL(KIND=JPRB) :: ZQSLTLH(KLON,KLEV),ZAH(KLON,KLEV)
REAL(KIND=JPRB) :: ZZDPHI,ZZRTV, ZDTETL, ZDIFTQL, ZDQLI,ZEPS3,ZEPS2
REAL(KIND=JPRB) :: ZDIFTTET, ZDTETI,ZDQI,ZDIFTQ,ZGKQH,ZGKQLH,ZGKTAH
REAL(KIND=JPRB) :: ZBIC, ZBICX, ZKROVN
REAL(KIND=JPRB) :: ZTLH,ZHH,ZEWH,ZQSATH,ZDLEWH

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------
!     INTRODUCTION DE FONCTIONS.

!     FUNCTIONS THERMODYNAMIQUES DE BASE
#include "fcttrm.func.h"
#include "wrscmr.intfb.h"

!-----------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('ACTURB',0,ZHOOK_HANDLE)
ASSOCIATE(UPRETMIN=>YDPHY0%UPRETMIN, VKARMN=>YDPHY0%VKARMN, &
 & UPRETMAX=>YDPHY0%UPRETMAX, ARSB2=>YDPHY0%ARSB2, ECTMIN=>YDPHY0%ECTMIN, &
 & AKN=>YDPHY0%AKN, ALPHAT=>YDPHY0%ALPHAT, AGRE1=>YDPHY0%AGRE1, &
 & AGRE2=>YDPHY0%AGRE2, AJBUMIN=>YDPHY0%AJBUMIN, ALMAV=>YDPHY0%ALMAV, &
 & AECLS4=>YDPHY0%AECLS4, TURB=>YDPHY0%TURB, STTBMIN=>YDPHY0%STTBMIN, &
 & AECLS3=>YDPHY0%AECLS3, RCOFLM=>YDPHY0%RCOFLM, GALP=>YDPHY0%GALP, &
 & UDECT=>YDPHY0%UDECT, USHEARM=>YDPHY0%USHEARM, UCWSTAR=>YDPHY0%UCWSTAR, &
 & ARSC1=>YDPHY0%ARSC1, AGREF=>YDPHY0%AGREF, EDB=>YDPHY0%EDB, EDC=>YDPHY0%EDC, &
 & RICRET=>YDPHY0%RICRET, EDD=>YDPHY0%EDD, USURIC=>YDPHY0%USURIC, &
 & GCVTURB=>YDPHY0%GCVTURB, &
 & RATM=>YDCST%RATM, RCPD=>YDCST%RCPD, RD=>YDCST%RD, RDT=>YDCST%RDT, RETV=>YDCST%RETV, &
 & RG=>YDCST%RG, RKAPPA=>YDCST%RKAPPA, RPI=>YDCST%RPI, RTT=>YDCST%RTT, RV=>YDCST%RV, &
 & LPBLE=>YDPHY%LPBLE, LNEIGE=>YDPHY%LNEIGE, LECTQ1=>YDPHY%LECTQ1, &
 & LECTFL0=>YDPHY%LECTFL0, LNEBECT=>YDPHY%LNEBECT, LECTREP=>YDPHY%LECTREP, &
 & LDISTUR=>YDPHY%LDISTUR, LDIFCEXP=>YDPHY%LDIFCEXP, LCVTURB=>YDPHY%LCVTURB,&
 & LNEBRIC=>YDPHY%LNEBRIC)
!-----------------------------------------------------------------------

!*
!     ------------------------------------------------------------------
!     0 - CALCULS PRELIMINAIRES
!     ------------------------------------------------------------------

!     ZCE1     : FACTEUR DE DECROISSANCE DE L'E.C.T.
!     ZPHMAX   : PRESSION PAR DEFAUT AU SOMMET DE LA COUCHE LIMITE, PMIN
!     ZPHMIN   : PRESSION PAR DEFAUT A LA BASE DE LA COUCHE LIMITE, PMAX
!     IHCLPMAX : NIVEAU MAXIMUM DE LA COUCHE LIMITE
!     IHCLPMIN : NIVEAU MINIMUN DE LA COUCHE LIMITE
!     ZEPS     : VALEUR MINIMALE DE L'E.C.T.
!     ZEPS1    : VALEUR MINIMALE DU CISAILLEMENT DU VENT
!     ZLMIN    : VALEUR MINIMALE POUR ZLMUP ET ZLMDN

ZCE1     = UDECT
ZPHMAX   = UPRETMIN
ZPHMIN   = UPRETMAX
IHCLPMAX = KTDIAN
IHCLPMIN = KLEV-2
ZEPS     = ECTMIN
ZEPS1    = USHEARM
ZEPS2   = 1.E+04_JPRB
ZEPS3   = 1.E-12_JPRB

PKTROV(:,:)=0.0_JPRB
PKUROV(:,:)=0.0_JPRB
PPRODTH(:,:)=0.0_JPRB

! ZSTAB      : INDICE DE STABILITE A LA SURFACE (1 SI STABLE, 0 SINON).
! ZRS        : CONSTANTE DES GAZ PARFAITS EN SURFACE.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Compute the value for ZRS, ZSTAB (from ACHMT usually...
! but not available if LMSE and no call to ACHMT)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLON=KIDIA,KFDIA
  ZRS  (JLON) = RD + (RV-RD)*PQS(JLON)
  ZDPHI0      = PAPHIF(JLON,KLEV)-PAPHI(JLON,KLEV)
  ZRTI        = 2.0_JPRB/(PR(JLON,KLEV)*PT(JLON,KLEV)&
             &            +RKAPPA*ZDPHI0&
             &            +ZRS(JLON)*PTS(JLON))
  ZSTA        = ZDPHI0*( PR(JLON,KLEV)*PT(JLON,KLEV)&
             &          +RKAPPA*ZDPHI0&
             &          -ZRS(JLON)*PTS(JLON) )*ZRTI
  ZSTAB(JLON) = MAX(0.0_JPRB,SIGN(1.0_JPRB,ZSTA))
ENDDO

!   CONSTANTES DE SECURITE ET DE PARAMETRISATION. (Schema stat.)
!   SECURITY AND PARAMETRIZATION CONSTANTS.       (Stat. Scheme)

ZEPSQ   = 1.E-10_JPRB
IF (JPRB == JPRD) THEN
  ZEPNEBS = 1.E-12_JPRB
ELSE
  ZEPNEBS = 1.E-06_JPRB
ENDIF
ZEPDELT = 1.E-12_JPRB
ZEPSV   = 1.E-10_JPRB
ZEPSIG  = 1.E-10_JPRB

ZMAXQ1  = 20._JPRB
ZEPSQ1  = 1.E-6_JPRB
LLIMQ1  = .TRUE.

IF (LLIMQ1) THEN
  ZILIMQ1 = 1.0_JPRB
ELSE
  ZILIMQ1 = 0.0_JPRB
ENDIF

ZSIGCR = GCVTURB
ZPRETURB = 65000._JPRB

ZGAUSS=1.0_JPRB/(2.0_JPRB*RDT**2)

!   TABLEAUX DE TRAVAIL
!   WORK ARRAYS ONCE FOR ALL

DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZZ(JLON,JLEV)    = PAPHIF(JLON,JLEV)  -PAPHI (JLON,KLEV)
    ZRTV (JLON,JLEV) = PR(JLON,JLEV)*PT(JLON,JLEV)
  ENDDO ! JLON
ENDDO   ! JLEV
DO JLEV=KTDIAN+1,KLEV
  DO JLON=KIDIA,KFDIA
    ZGDZF(JLON,JLEV) = PAPHIF(JLON,JLEV-1)-PAPHIF(JLON,JLEV)
  ENDDO ! JLON
ENDDO   ! JLEV

!*
!     ------------------------------------------------------------------
!     I - ACCOEFK SIMPLIFIE AU DESSUS DE KTDIAN.

!     CALCULS DE PARAMETRES AUXILIAIRES ET DE CONSTANTES
!     DE SECURITE (POUR LE CARRE DU CISAILLEMENT DU VENT).

!     COMPUTATION OF DERIVED PARAMETERS AND SECURITY
!     CONSTANTS (FOR THE SQUARE OF THE WIND SHEAR).
!     ------------------------------------------------------------------

Z2B=2.0_JPRB*EDB
Z3B=3._JPRB*EDB
Z3BCF=EDB*EDC*VKARMN**2/SQRT(3._JPRB)
ZRLTLU=SQRT(1.5_JPRB*EDD)

ZGLU=RG*ALMAV
ZGLT=ZGLU*ZRLTLU

!     BOUCLE PASSIVE SUR LES NIVEAUX VERTICAUX.
!     PASSIVE LOOP ON VERTICAL LEVELS.

DO JLEV=KTDIAT,KTDIAN-1

!       CALCULS PROPREMENT DITS.

  ZGLUZ=ZGLU
  ZGLMU2=ZGLUZ**2
  ZGLTZ=ZGLT
  ZGLMT2=ZGLTZ**2

!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA

!         PROFIL DE LONGUEUR DE MELANGE CONSTANT
!         CONSTANT MIXING LENGTH PROFILE

    ZGZ=PAPHI(JLON,JLEV)-PAPHI(JLON,KLEV)+PGZ0(JLON)
    ZCK=Z3BCF*(ZGLTZ/(VKARMN*ZGZ))**2
    ZDPHI0=PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1)

!         CISAILLEMENT DE VENT.
!         WIND SHEAR.

    ZCIS=MAX(ZEPS1,(PU(JLON,JLEV)-PU(JLON,JLEV+1))**2&
     & +(PV(JLON,JLEV)-PV(JLON,JLEV+1))**2)
    ZU=SQRT(ZCIS)

!         PRECALCUL DE STABILITE.
!         PRELIMINARY STABILITY COMPUTATION.

    ZDTETA=PR(JLON,JLEV)  *PT(JLON,JLEV)&
     & -PR(JLON,JLEV+1)*PT(JLON,JLEV+1)+RKAPPA*ZDPHI0

!         CALCUL DE STABILITE.
!         STABILITY COMPUTATION.

    ZRTI=2.0_JPRB/(PR(JLON,JLEV)  *PT(JLON,JLEV)&
     & +PR(JLON,JLEV+1)*PT(JLON,JLEV+1))
    ZSTA=ZDPHI0*ZDTETA*ZRTI
    ZSTA=ZSTA/(1.0_JPRB+MAX(0.0_JPRB,ZSTA)*USURIC/ZCIS)
    ZIS=MAX(0.0_JPRB,SIGN(1.0_JPRB,ZSTA))

!         CALCULS COMMUNS POUR QUANTITE DE MOUVEMENT ET ENERGIE.
!         COMMON COMPUTATIONS FOR MOMENTUM AND ENERGY.

    ZDS=SQRT(ZCIS+EDD*ABS(ZSTA))
    ZDI=1.0_JPRB/(ZU+ZCK*SQRT(ABS(ZSTA)))

!         CALCULS POUR LES COMPOSANTES DU VENT.
!         COMPUTATIONS FOR THE WIND COMPONENTS.

    ZLOS=ZCIS*ZDS/(ZU*ZDS+Z2B*ABS(ZSTA))
    ZLOI=ZU-Z2B*ZSTA*ZDI
    PKUROV(JLON,JLEV)=(ZLOI+ZIS*(ZLOS-ZLOI))*&
     & ZGLMU2*PAPRS(JLON,JLEV)*ZRTI/ZDPHI0**2

!         CALCULS POUR LA TEMPERATURE ET L'HUMIDITE.
!         COMPUTATIONS FOR TEMPERATURE AND HUMIDITY.

    ZLOS=ZCIS**2/(ZU*ZCIS+Z3B*ABS(ZSTA)*ZDS)
    ZLOI=ZU-Z3B*ZSTA*ZDI
    PKTROV(JLON,JLEV)=(ZLOI+ZIS*(ZLOS-ZLOI))*&
     & ZGLMT2*PAPRS(JLON,JLEV)*ZRTI/ZDPHI0**2
    PKQROV(JLON,JLEV)=PKTROV(JLON,JLEV)
    PKQLROV(JLON,JLEV)=0.0_JPRB

!         CALCUL DE UN SUR G FOIS LA FREQUENCE DE BRUNT-VAISALA
!         DIVISEE PAR  LA DENSITE LE TOUT AU CARRE.
!         COMPUTATION OF ONE OVER G TIME THE BRUNT-VAISALA FREQUENCY
!         DIVIDED BY DENSITY THE WHOLE BEING SQUARED.

    PNBVNO(JLON,JLEV)=ZSTA/(PAPRS(JLON,JLEV)*ZRTI*ZDPHI0)**2

  ENDDO
ENDDO

!     FIN DE ACCOEFK SIMPLIFIE


!     CALCUL DE LA HAUTEUR DE COUCHE LIMITE:
!          PREMIER NIVEAU EN PARTANT DE LA SURFACE OU LA TKE <0.01
!          SI LPBLE (Top. Entr. ) ou TKECLS (not LECTREP)

ZBLH(:) =0._JPRB
IF (LPBLE.OR.(.NOT.LECTREP)) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - -
! Compute the INDEX array ILEVBI from the lowest
! half level (KLEV-1) to the "Top-PBL" half level :
!- - - - - - - - - - - - - - - - - - - - - - - - - -
! ILEVBI(JLON) = 1 from KLEV-1 to the "Top-PBL"
! ILEVBI(JLON) = 0 above the "Top-PBL" half level
!- - - - - - - - - - - - - - - - - - - - - - - - - -
! the case "MAX(JLEV, ILEVBI(JLON))" avoid the
! detection of the other "PBL" located above
! the first one close to the ground.
!- - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLON=KIDIA,KFDIA
   ILEVBI(JLON)=0
ENDDO
ICM(:,:)=0
ZECTBLH=0.01_JPRB
DO JLEV=KTDIAN,KLEV
   DO JLON=KIDIA,KFDIA
     ICM(JLON,JLEV) = INT(MAX(0.0_JPRB,SIGN(1.0_JPRB, PECT(JLON,JLEV)-ZECTBLH)))
   ENDDO
ENDDO
DO JLEV=KLEV,KTDIAN,-1
   DO JLON=KIDIA,KFDIA
     ILEVM1=MAX(KTDIAN, JLEV-1)
     IF ( (ICM(JLON, JLEV ) == 1).AND.(ICM(JLON,ILEVM1) == 0) ) THEN
        ILEVBI(JLON) = MAX(JLEV, ILEVBI(JLON))
     ENDIF
   ENDDO
ENDDO
DO JLON=KIDIA,KFDIA
   ILEVBI(JLON)=ILEVBI(JLON)*MAX(ICM(JLON, KLEV),ICM(JLON, KLEV-1))
   IF ((ICM(JLON, KLEV ) == 0).AND.(ILEVBI(JLON) == 0) ) ILEVBI(JLON)=KLEV
ENDDO
DO JLON=KIDIA,KFDIA
   IF ((ILEVBI(JLON) > 1).AND.(ILEVBI(JLON) < KLEV)) THEN
      ZHBOT=(PAPHI(JLON,ILEVBI(JLON))-PAPHI(JLON,KLEV))/RG
      ZHTOP=(PAPHI(JLON,ILEVBI(JLON)-1)-PAPHI(JLON,KLEV))/RG
      ZBLH(JLON)=ZHBOT+(ZHTOP - ZHBOT)/&
     & (PECT(JLON,ILEVBI(JLON)-1) - PECT(JLON,ILEVBI(JLON)))*&
     & (ZECTBLH - PECT(JLON,ILEVBI(JLON)))
   ELSEIF (ILEVBI(JLON) == KLEV) THEN
      ZBLH(JLON)=(PAPHIF(JLON,KLEV)-PAPHI(JLON,KLEV))/RG
   ELSEIF (ILEVBI(JLON) == 0) THEN
      ZBLH(JLON)=(PAPHI(JLON,KTDIAN)-PAPHI(JLON,KLEV))/RG
   ELSE
      ZBLH(JLON)=(PAPHI(JLON,ILEVBI(JLON))-PAPHI(JLON,KLEV))/RG
   ENDIF
ENDDO
ENDIF

IF (.NOT. LECTREP) THEN

!*
!     ------------------------------------------------------------------
!     III - CALCUL DE L'ENERGIE CINETIQUE TURBULENTE DANS LA COUCHE
!           LIMITE DE SURFACE (PECTCLS).
!     ------------------------------------------------------------------

!DEC$ IVDEP
   DO JLON=KIDIA,KFDIA

     ZMODU          = SQRT( PU(JLON,KLEV)**2+ PV(JLON,KLEV)**2 )
     ZUSTAR2        = PCD(JLON)*ZMODU*ZMODU
     ZUSTAR(JLON)   = SQRT(ZUSTAR2)
     ZTETA          = ZRTV(JLON,KLEV)+RKAPPA*ZZ(JLON,KLEV)-ZRS(JLON)*PTS(JLON)
     ZRQZERO        = MAX(ZEPS,-PCH(JLON)*ZTETA*ZMODU)
     ZWSTAR(JLON)   = (RG*ZBLH(JLON)*ZRQZERO/ZRTV(JLON,KLEV))**UCWSTAR
     PECTCLS(JLON)  = MAX( ECTMIN, AECLS3*ZUSTAR(JLON)**2&
      & +AECLS4*ZWSTAR(JLON)**2&
      & *(1.0_JPRB-ZSTAB(JLON)) )

   ENDDO ! JLON

ELSE
   DO JLON=KIDIA,KFDIA
      PECTCLS(JLON)  = PECT(JLON,KLEV-1)
   ENDDO
ENDIF ! LECREP
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
    ZPREF              = PAPRSF(JLON,JLEV)
    ZTHETA(JLON,JLEV)  = PT(JLON,JLEV)*(RATM/ZPREF)**(RKAPPA)
  ENDDO
ENDDO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! CALCUL DE (THETA)l = THETA * [1-Lv*(Ql+Qi)/(Cp*T)]
!           (THETA)l = THETA - ZLOCPEXF*(Ql+Qi)
! AVEC      ZLOCPEXF = (Lv/Cp)*(THETA/T)
! ET DONC   ZLOCPEXF =  Lv/Cp/(T/THETA)
! LA FONCTION D'EXNER ETANT : PI=T/THETA
! CALCUL DE  ZCOEFJF = [Lv(Tl)*qsat(Tl)]/[Rv*Tl*(Theta)l]
! - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZZT                 = PT  (JLON,JLEV)
    ZQC                 = PQLI(JLON,JLEV) +PQICE(JLON,JLEV)
    ZTHETAOT            = ZTHETA(JLON,JLEV)/ZZT
    ZLOCPEXF(JLON,JLEV) = PLSCPE(JLON,JLEV)*ZTHETAOT
    ZTHETALF(JLON,JLEV) = ZTHETA(JLON,JLEV)-ZQC*ZLOCPEXF(JLON,JLEV)
  ENDDO
ENDDO
!!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      si : (THETA)l  =  THETA * [ 1 - L*(Ql+Qi)/(Cp*T) ]
! CALCUL DE (THETA)vl = (THETA)l * [ 1 + RETV*(Ql+Qi) ]
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZQV               =   PQV (JLON,JLEV)
    ZQC               =   PQLI(JLON,JLEV)+PQICE(JLON,JLEV)
    ! ZTHETAVL(JLON,JLEV) = ZTHETALF(JLON,JLEV)*(1.0_JPRB+RETV*ZQC)
    ! ancien calcul
    ZTHETAVL(JLON,JLEV) = ZTHETA(JLON,JLEV)*(1.0_JPRB+RETV*ZQV-ZQC)
  ENDDO
ENDDO

!*
!     ------------------------------------------------------------------
!     V - CALCUL DES COEFFICIENTS DE MELANGE "PKUROV" ET "PKTROV".
!     ------------------------------------------------------------------
ZGKUH(:,:)=1.E-14_JPRB
ZGKTH(:,:)=1.E-14_JPRB
DO JLEV=KTDIAN,KLEV-1
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA

    ZDPHI    =ZGDZF(JLON,JLEV+1)
    ZZRT     =0.5_JPRB*(ZRTV(JLON,JLEV)+ZRTV(JLON,JLEV+1))
    ZROSDPHI =PAPRS(JLON,JLEV)/(ZDPHI*ZZRT)

    ZGKUH(JLON,JLEV)=AKN*PLMECT(JLON,JLEV)*SQRT(PECT(JLON,JLEV))
    ZGKTH(JLON,JLEV)=ZGKUH(JLON,JLEV)*PPHI3(JLON,JLEV)*ALPHAT

    PKTROV(JLON,JLEV)=ZGKTH(JLON,JLEV)*ZROSDPHI
    PKUROV(JLON,JLEV)=ZGKUH(JLON,JLEV)*ZROSDPHI
    PKQROV(JLON,JLEV)=PKTROV(JLON,JLEV)
    PKQLROV(JLON,JLEV)=0.0_JPRB

  ENDDO ! JLON
ENDDO ! JLEV

!*
!     ------------------------------------------------------------------
!     VI - AU DERNIER DEMI-NIVEAU (C'EST LE SOL): CALCULS POUR ACEVOLET
!          (EQUATION D'EVOLUTION DE L'ECT) DU COEFFICIENT DE MELANGE
!          PGKCLS=g*KUCLS ET DE LA LONGEUR DE MELANGE PLMECT (A KLEV).
!     ------------------------------------------------------------------

!DEC$ IVDEP
DO JLON=KIDIA,KFDIA
  IF (LECTFL0) THEN
     PGKCLS(JLON) = 0._JPRB
     ZGKTH (JLON,KLEV)= 0._JPRB
     ZGKUH (JLON,KLEV)= 0._JPRB
  ELSE
     PGKCLS(JLON)     =AKN*PLMECT(JLON,KLEV)*SQRT(PECTCLS(JLON))
     ZGKTH (JLON,KLEV)=PGKCLS(JLON)*PPHI3(JLON,KLEV-1)*ALPHAT
     ZGKUH (JLON,KLEV)=PGKCLS(JLON)
  ENDIF
ENDDO ! JLON

IF(LMUSCLFA) THEN
   CALL WRSCMR(NMUSCLFA,'ZKH',ZGKTH/RG,KLON,KLEV+1)
   CALL WRSCMR(NMUSCLFA,'ZKM',ZGKUH/RG,KLON,KLEV+1)
   ZLM(:,:)=PLMECT(:,:)/RG
   CALL WRSCMR(NMUSCLFA,'ZLM',ZLM,KLON,KLEV+1)
ENDIF   

!*
!     ------------------------------------------------------------------
!     VII - AUX DEMI-NIVEAUX : CALCUL DE PNBVNO (->GRAVITY WAVE DRAG).
!           > CISAILLEMENT DE VENT (ZCIS), CALCULS DE STABILITE (ZDTETA
!           > ZSTA), CALCUL DE "UN SUR G FOIS LA FREQUENCE DE BRUNT
!           > VAISALA DIVISEE PAR LA DENSITE", LE TOUT AU CARRE (PNBVNO)
!     ------------------------------------------------------------------

DO JLEV=KTDIAN,KLEV-1
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA
    ZDPHI =PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1)
    ZCIS  =MAX(ZEPS1,(PU(JLON,JLEV)-PU(JLON,JLEV+1))**2&
     & +(PV(JLON,JLEV)-PV(JLON,JLEV+1))**2)
    ZDTETA=ZRTV(JLON,JLEV)-ZRTV(JLON,JLEV+1)+RKAPPA*ZDPHI
    ZZRT  =2.0_JPRB/(ZRTV(JLON,JLEV)+ZRTV(JLON,JLEV+1))
    ZSTA  =ZDPHI*ZDTETA*ZZRT
    ZSTA  =ZSTA/(1.0_JPRB+MAX(0.0_JPRB,ZSTA)*USURIC/ZCIS)
    PNBVNO(JLON,JLEV)=ZSTA/(PAPRS(JLON,JLEV)*ZZRT*ZDPHI)**2
  ENDDO ! JLON
ENDDO ! JLEV

!*
!     ------------------------------------------------------------------
!     VIII(a) - CALCUL DE "PPRODTH" SUR LES DEMI-NIVEAUX.
!     ------------------------------------------------------------------
!            - LA PRODUCTION THERMIQUE "PPRODTH" EST CALCULEE COMME
!            LA CORRELATION (g/T)*(w'X'), OU ON UTILISE LA VARIABLE
!            "X=(THETA)vl" QUI CORRESPOND A "T(1+0.608Qv-Ql)". LA
!            PRODUCTION THERMIQUE EST OBTENUE PAR UNE PONDERATION PAR
!            "L3*F2" D'UN TERME "SEC" ET D'UN TERME "HUMIDE"
!                       -------------------------------------------
!                        PPRODTH = ZPRODH + (LAMBDA_3*F_2)* ZPRODC.
!                       -------------------------------------------
!     ------------------------------------------------------------------

! - - - - - - - - - - - - - - - - - - - - - - - - - - -
! ** DEBUT DES BOUCLES VERTICALES ET HORIZONTALES   **
! ** START OF VERTICAL AND HORIZONTAL NESTED LOOPS  **
! - - - - - - - - - - - - - - - - - - - - - - - - - - -

DO JLEV=KTDIAN,KLEV-1
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.1 - VARIABLES AUXILIAIRES ET DE TRAVAIL (demi-niveaux).
!                - AUXILIARY VARIABLES AND WORK VALUES (half levels).
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZTF1     = PT (JLON,JLEV  )
    ZTF2     = PT (JLON,JLEV+1)
    ZQWF1    = PQV(JLON,JLEV  ) +PQLI(JLON,JLEV  ) +PQICE(JLON,JLEV  )
    ZQWF2    = PQV(JLON,JLEV+1) +PQLI(JLON,JLEV+1) +PQICE(JLON,JLEV+1)
    ZQWF1    = MAX(ABS(ZQWF1),ZEPSQ)
    ZQWF2    = MAX(ABS(ZQWF2),ZEPSQ)
    ZLSCPEF1 = PLSCPE(JLON,JLEV  )
    ZLSCPEF2 = PLSCPE(JLON,JLEV+1)
    ZQCF1    = PQLI(JLON,JLEV  )+PQICE(JLON,JLEV  )
    ZQCF2    = PQLI(JLON,JLEV+1)+PQICE(JLON,JLEV+1)
    ZTLF1    = ZTF1-ZLSCPEF1*ZQCF1
    ZTLF2    = ZTF2-ZLSCPEF2*ZQCF2

    ZRH     = (PR(JLON,JLEV)+PR(JLON,JLEV+1))/2.0_JPRB
    ZQWH    = (ZQWF1   +ZQWF2   )/2.0_JPRB
    ZTH     = (ZTF1    +ZTF2    )/2.0_JPRB
    ZQLH    = (ZQCF1   +ZQCF2   )/2.0_JPRB
    ZLSCPEH = (ZLSCPEF1+ZLSCPEF2)/2.0_JPRB

    IF (LDISTUR) THEN
      ZTLH     = (ZTLF1    +ZTLF2    )/2.0_JPRB
      ZHH      = MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTLH))
      ZEWH     = FOEW(ZTLH,ZHH)/PAPRSF(JLON,JLEV)
      ZQSATH   = FOQS(ZEWH)
      ZDLEWH   = FODLEW(ZTLH,ZHH)
      ZQSLTLH(JLON,JLEV) = FDQW(ZEWH,ZDLEWH)
      ZDELTQH = ZQWH-ZQSATH
      ZDELTQH = SIGN(MAX(ABS(ZDELTQH),ZEPDELT),ZDELTQH)
    ELSE
      ZH1      = MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTLF1))
      ZEW1     = FOEW(ZTLF1,ZH1)/PAPRSF(JLON,JLEV)
      ZQSATF1  = FOQS(ZEW1)
      ZDLEWF1  = FODLEW(ZTLF1,ZH1)
      ZQSLTLF1 = FDQW (ZEW1, ZDLEWF1)
      ZDELTQF1 = ZQWF1-ZQSATF1
      ZDELTQF1 = SIGN(MAX(ABS(ZDELTQF1),ZEPDELT),ZDELTQF1)

      ZH2      = MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTLF2))
      ZEW2     = FOEW(ZTLF2,ZH2)/PAPRSF(JLON,JLEV+1)
      ZQSATF2  = FOQS(ZEW2)
      ZDLEWF2  = FODLEW(ZTLF2,ZH2)
      ZQSLTLF2 = FDQW (ZEW2, ZDLEWF2)  
      ZDELTQF2 = ZQWF2-ZQSATF2
      ZDELTQF2 = SIGN(MAX(ABS(ZDELTQF2),ZEPDELT),ZDELTQF2)

      ZQSLTLH(JLON,JLEV) = (ZQSLTLF1+ZQSLTLF2)/2.0_JPRB
      ZDELTQH = (ZDELTQF1+ZDELTQF2)/2.0_JPRB
    ENDIF

    ZAA  = 1.0_JPRB/(1.0_JPRB+ZLSCPEH*ZQSLTLH(JLON,JLEV))
    ZAH(JLON,JLEV) = ZAA

    ZDD  = ZLSCPEH-(1.0_JPRB+RETV)*ZTH
    ZCTO = RETV*ZTH

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.2 - CALCUL DES GRADIENTS VERTICAUX    D/DZ = RG * D/DPHI
!                - COMPUTATION OF VERTICAL GRADIENTS D/DZ = RG * D/DPHI
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZDPHI  = PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1)
    ZDQW   = PQV   (JLON,JLEV)-PQV   (JLON,JLEV+1)&
     & +  PQLI  (JLON,JLEV)-PQLI  (JLON,JLEV+1)&
     & +  PQICE (JLON,JLEV)-PQICE (JLON,JLEV+1)
    ZDT    = PT    (JLON,JLEV)-PT    (JLON,JLEV+1)

    IF (LDISTUR) THEN
!   Ancien calcul
      ZDQLST = ZQCF1*ZLSCPEF1/ZTF1-ZQCF2*ZLSCPEF2/ZTF2
      ZDSTA  = ZDT+ZDPHI*RKAPPA/ZRH
      ZDTL   = ZDSTA*(1.0_JPRB-ZLSCPEH*ZQLH/ZTH)-ZTH*ZDQLST
    ELSE
!    
      ZDTL   = (ZTHETALF(JLON,JLEV)-ZTHETALF(JLON,JLEV+1))*ZTH /&
            & (ZTHETALF(JLON,JLEV)+ZTHETALF(JLON,JLEV+1)) * 2._JPRB
    ENDIF

    ZDIFFH = ZDTL +  ZCTO  *ZDQW
    ZDIFFC = ZDQW - ZQSLTLH(JLON,JLEV)*ZDTL

!         Attention, ici : ZZKTH=RG*KTH/ZDPHI
    ZRHOH  = PAPRS (JLON,JLEV)/ZRH/ZTH
    ZZKTH  = PKTROV(JLON,JLEV)/ZRHOH

!         - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.3 - CALCUL DE SIGMAQ ET DE SIGMAQL=Q/STTBMIN
!                - COMPUTATION OF SIGMAQ AND SIGMAQL=Q/STTBMIN
!         - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZECTH  = MAX(ECTMIN,PECT(JLON,JLEV))
    ZLMECT = PLMECT(JLON,JLEV)
    ZWQW   = -ZZKTH*ZDQW
    ZWTL   = -ZZKTH*ZDTL
    ZWDIFF = ZWQW-ZQSLTLH(JLON,JLEV)*ZWTL

!         - - - - - - - - - - - - - - - - - - - - - -
!         VIII.4 - CALCUL DE SIGMA_S, PUIS DE Q11 :
!         - - - - - - - - - - - - - - - - - - - - - -
    ZIGMAS2 = -ZAA*ZAA*ARSB2*ZLMECT/4._JPRB/SQRT(ZECTH)*ZWDIFF*ZDIFFC/ZDPHI
    ZIGMAS  = MAX(ZEPSIG,SQRT(ABS(ZIGMAS2)))
    ZQ11    = ZAA*ZDELTQH/(2*ZIGMAS)

    IF (LCVTURB .AND. ZDELTQH > 0._JPRB .AND. PAPRSF(JLON,JLEV) < ZPRETURB .AND. &
     & LDCONV(JLON,JLEV)) THEN
      ZIGMAS = MAX(ZSIGCR, ZIGMAS)
      ZQ11 = ZAA*ZDELTQH/(2.0_JPRB*ZIGMAS)
    ENDIF

!         - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.5 - CALCUL DE Q1MAX (LIMITATION SUR LA LONGUEUR
!                  MELANGE ET, EN FAIT, ICI SUR "PHI3") :
!         - - - - - - - - - - - - - - - - - - - - - - - - - - -

    IF (LECTQ1) THEN
       ZGALP2   = GALP*GALP/TURB
       ZPHI3MIN = 1.0_JPRB/(1.0_JPRB+ARSC1*ZGALP2)
       ZQ1MAX   = ZDELTQH*ZDPHI/ZLMECT&
        & /SQRT(ARSB2*AKN*ALPHAT*ZPHI3MIN)&
        & /MAX(ABS(ZDIFFC),ZEPSQ1)
       ZQ1MAX   = SIGN(MAX(ABS(ZQ1MAX),ZEPSQ1),ZQ1MAX)
       ZQ1MAX   = SIGN(MIN(ABS(ZQ1MAX),ZMAXQ1),ZQ1MAX)
       ZQ1MAX   = ZILIMQ1*ZQ1MAX -(1.0_JPRB-ZILIMQ1)*ZMAXQ1

!         - - - - - - - - - - - - - - - - - - - - -
!         VIII.6 - CALCUL DE Q1MIN (LIMITATION DES
!                  VALEURS NEGATIVES D'HUMIDITE) :
!         - - - - - - - - - - - - - - - - - - - - -

       ZQ1MIN = STTBMIN*ZDELTQH*ABS(ZDQW)/(ZQWH*MAX(ABS(ZDIFFC),ZEPSQ1))
       ZQ1MIN = SIGN( MAX(ABS(ZQ1MIN),ZEPSQ1),ZQ1MIN)
       ZQ1MIN = ZILIMQ1*ZQ1MIN +(1.0_JPRB-ZILIMQ1)*ZEPSQ1

!         - - - - - - - - - - - - - - - - - - - - -
!         VIII.7 - LIMITATIONS (EN MODULE) DE Q1
!                  PAR ABS(Q1MIN) ET ABS(Q1MAX) :
!         - - - - - - - - - - - - - - - - - - - - -

       ZQ11   = SIGN(MAX(ABS(ZQ11),ABS(ZQ1MIN)),ZQ11)
       ZQ11   = SIGN(MIN(ABS(ZQ11),ABS(ZQ1MAX)),ZQ11)

!         - - - - - - - - - - - - - - - - - - -
!         VIII.8 - CALCUL DU NOUVEAU SIGMAS ET
!                  SECURITE PAR "ZEPNEBS" :
!         - - - - - - - - - - - - - - - - - - -

      ZIGMAS = ZAA*ZDELTQH/(2.0_JPRB*ZQ11)
      ZIGMAS = MAX(ZEPSIG,ZIGMAS)

    ENDIF   ! LECTQ1

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.9 - CALCUL DE ZPRODH, ZPRODC, PUIS DE LA PRODUCTION
!                  THERMIQUE : PPRODTH = ZPRODH + (LAMBDA_3*F_2)*ZPRODC
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZPRODH = -RG*ZZKTH*ZDIFFH/ZTH
    ZPRODC = -RG*ZZKTH*ZDIFFC*ZAA*ZDD/ZTH

    INQ1   = MIN( MAX(-22,FLOOR(2*ZQ11) ), 10)
    ZINC   = 2.0_JPRB*ZQ11 - INQ1
    ZSRC   = (1.0_JPRB-ZINC)*YDPHY0%RSRC1D(INQ1)+ZINC*YDPHY0%RSRC1D(INQ1+1)
    ZL3F2  = MIN(1.0_JPRB,ZSRC)* MIN( MAX(1.0_JPRB, 1.0_JPRB-ZQ11), 3._JPRB )

    PL3F2  (JLON,JLEV) = ZL3F2

    PPRODTH(JLON,JLEV) = ZPRODH+ ZL3F2*ZPRODC

  ENDDO !  JLON=KIDIA, KFDIA
ENDDO   !  JLEV=KTDIAN,KLEV-1
DO JLON=KIDIA,KFDIA
   PPRODTH(JLON,KLEV)=PPRODTH(JLON,KLEV-1)
ENDDO

! - - - - - - - - - - - - - - - - - - - - - - - - -
! ** FIN DES BOUCLES VERTICALES ET HORIZONTALES  **
! ** END OF VERTICAL AND HORIZONTAL NESTED LOOPS **
! - - - - - - - - - - - - - - - - - - - - - - - - -

!*
!     ------------------------------------------------------------------
!     VIII(b) - CALCUL DES COEFFICIENTS AU NIVEAU DE L'ENTRAINEMENT EN
!             SOMMET DE COUCHE LIMITE, DEFINIE PAR LE DERNIER NIVEAU
!             OU, PARTANT DU SOL, ON EST ENCORE EN COUCHE INSTABLE,
!             AU SENS OU LE RICHARDSON DEPASSE POUR LA PREMIERE FOIS
!             LE SEUIL "AGRERICR".
!     ------------------------------------------------------------------
!----------------
 IF (LPBLE) THEN
!----------------

  !- - - - - - - - - - - - -
  ! ICM(KLEV)=1 if ZSTAB=0 (instable):
  ! Entrainement si instable en surface
  !- - - - - - - - - - - - -

  DO JLON=KIDIA,KFDIA
    ICM(JLON,KLEV)=INT(1.0_JPRB-ZSTAB(JLON))
    ILEVBI(JLON)=ILEVBI(JLON)*ICM(JLON,KLEV)
  ENDDO

  !- - - - - - - - - - - -
  ! Set to 0.0 the PBL integral
  ! of the TKE (ZECTINT) and of the
  ! cloud water content (ZQCINT) :
  !- - - - - - - - - - - -

  DO JLON=KIDIA,KFDIA
    ZECTINT(JLON)=0.0_JPRB
    ZQCINT (JLON)=0.0_JPRB
  ENDDO


  !- - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Compute the integral from the half-level
  ! KLEV-1 to the "top-PBL" for the TKE, from
  ! the full level KLEV to the "top-PBL" for
  ! the Cloud Water Content.
  !- - - - - - - - - - - - - - - - - - - - - - - - - -
  ! ICLA=1 in the layer from KLEV to ILEVBI,
  ! with ICLA=0 above)
  !- - - - - - - - - - - - - - - - - - - - - - - - - -
  DO JLEV=KLEV-1,KTDIAN,-1
    DO JLON=KIDIA,KFDIA
      ICLA=MAX(0,ISIGN(1, JLEV-ILEVBI(JLON)))
      ZECTINT(JLON) = ZECTINT(JLON) +&
         &   ICLA*PECT(JLON,JLEV)&
         & *(PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1))
      ZQCINT(JLON)  = ZQCINT (JLON) +&
         &   ICLA*(PQLI(JLON,JLEV+1)+PQICE(JLON,JLEV+1))&
         & *(PAPHI(JLON,JLEV)-PAPHI(JLON,JLEV+1))
    ENDDO
  ENDDO ! JLEV=KLEV-1,KTDIAN,-1

!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA
    IF (ILEVBI(JLON) > 0) THEN

      !- - - - - - - - - - - - - - - - - -
      ! Set the entrainment level ILEVT :
      !- - - - - - - - - - - - - - - - - -
      ILEVT=MIN(ILEVBI(JLON), KLEV-1)
      !- - - - - - - - - - - - - - - - - - - - -
      ! Compute the (bulk) average of TKE
      ! (Grenier 2002 EUROCS meeting Utrecht) :
      !- - - - - - - - - - - - - - - - - - - - -

      ZNUM    = ZECTINT(JLON) + PECTCLS(JLON)*&
       &        (PAPHIF(JLON,KLEV)-PAPHI(JLON,KLEV))
      ZDEN    = PAPHIF(JLON,ILEVT)-PAPHI(JLON,KLEV)
      IF (AGREF < 0._JPRB) THEN
        ZECTBLK = ZNUM/ZDEN
      ELSE
        ZECTBLK = MIN(PECTCLS(JLON), ZNUM/ZDEN)
      ENDIF
      !- - - - - - - - - - - - - - - - - - - - -
      ! Compute the (bulk) average of Q_cloud
      ! (E. Bazile, 2008) :
      !- - - - - - - - - - - - - - - - - - - - -

      ZDEN    = PAPHI(JLON,ILEVT)-PAPHI(JLON,KLEV)
      ZQCBLK  = ZQCINT(JLON)/ZDEN

      !- - - - - - - - - - - - - - - - - -
      ! Compute the square of the "moist"
      ! Brunt-Vaisalla frequency :
      ! N**2=ZBI=(g/THETA)*d(THETA_vl)/d(z)
      !      ZBI=-g*(PROD_THER)/(g*KT)
      !   AJBUMIN=min_value[d(Theta)/Theta]
      !- - - - - - - - - - - - - - - - - -

      ZDPHI = PAPHIF(JLON,ILEVT)-PAPHIF(JLON,ILEVT+1)
      ZBI   = -RG*PPRODTH(JLON,ILEVT)/ZGKTH(JLON,ILEVT)
      ZBIC  = RG*RG*AJBUMIN/ZDPHI

      !- - - - - - - - - - - - - - - - - - - - - -
      ! Compute the "Entrainment Master Length" :
      !- - - - - - - - - - - - - - - - - - - - - -
      ! A "master" value (Grenier 2002 EUROCS meeting Utrecht) :
      ! RCOFLM = 0.085 in (Grenier 2002)
      !
!     ZLINV=PLMECT(JLON,ILEVT)/RG   ! just above the inversion
!     ZLINV=PLMECT(JLON,ILEVT+1)/RG ! just below the inversion
      ZGZ=PAPHI(JLON,ILEVT)-PAPHI(JLON,KLEV)+PGZ0(JLON)
      ZLINV=RCOFLM*ZGZ/RG

      !- - - - - - - - - - - - - - - - - - - - -
      ! Compute the entrainement coefficient
      ! at the inversion level / see Grenier
      ! and Bretherton, MWR, 2001 = GB01 and
      ! Grenier 2002 (EUROCS meeting Utrecht)
      !- - - - - - - - - - - - - - - - - - - - -
      ! A = A1*[1+A2*Af*L*qc/(cp*d(theta_vl))]
      ! in GB01 :  A1=0.16 ; A2=15. ; Af=0.8
      !- - - - - - - - - - - - - - - - - - - - -
      ! !    AJBUMIN=min_value[d(Theta)/Theta]
      ! ! => Theta*AJBUMIN=min_value[d(Theta)]
      !- - - - - - - - - - - - - - - - - - - - -
      IF (AGREF < 0._JPRB) THEN
        ZBICX=ZBIC*1.05_JPRB
        ZA = -AGREF*AGRE1*(1._JPRB-(1._JPRB/AGREF+1._JPRB)&
         &*SIN(RPI/2._JPRB*MAX(0._JPRB,MIN(1._JPRB,((ZBICX-ZBI)&
         &/(ZBICX-ZBIC)))))**2._JPRB)
        ZQCBLK  =PQLI(JLON,ILEVT+1)+PQICE(JLON,ILEVT+1)
        ZA = ZA *( 1._JPRB +  &
           &  AGRE2*PLSCPE(JLON,ILEVT)*ZQCBLK &
           &  /MAX(ZTHETAVL(JLON,ILEVT)-ZTHETAVL(JLON,ILEVT+1) &
           &      ,AJBUMIN*ZTHETA(JLON,ILEVT)) )
      ELSE
        ZA = AGRE1*( 1._JPRB +&
           &  AGRE2*AGREF*PLSCPE(JLON,ILEVT)*ZQCBLK&
           &  /MAX(ZTHETAVL(JLON,ILEVT)-ZTHETAVL(JLON,ILEVT+1)&
           &      ,AJBUMIN*ZTHETA(JLON,ILEVT))&
           &      )
      ENDIF  ! (AGREF < 0.)

      !- - - - - - - - - - - - - - - - - - - -
      ! L'action sur le coefficient d'echange
      ! g*Kinv = A *(e)**3/2 *g/(L_inv*ZBI)
      !- - - - - - - - - - - - - - - - - - - -
      ! N**2=ZBI=(g/THETA)*d(THETA_vl)/d(z)
      !      ZBI=-g*(PROD_THER)/(g*KT)
      !- - - - - - - - - - - - - - - - - - - -

      ZBI = MAX( ZBI, ZBIC )
      ZGKENT = MAX( ZA*ZECTBLK*SQRT(ZECTBLK)*RG/ZLINV/ZBI ,&
     &              ZGKTH(JLON,ILEVT))

      !- - - - - - - - - - - - - - -
      ! "g*Ku" is equal to "g*KT" :
      !- - - - - - - - - - - - - - -

      ZGKTH(JLON,ILEVT) = ZGKENT
      ZGKUH(JLON,ILEVT) = ZGKENT

      ZDPHI    = PAPHIF(JLON,ILEVT)-PAPHIF(JLON,ILEVT+1)
      ZZRT     = 0.5_JPRB*(ZRTV(JLON,ILEVT)+ZRTV(JLON,ILEVT+1))
      ZROSDPHI = PAPRS(JLON,ILEVT)/(ZDPHI*ZZRT)

      PKTROV(JLON,ILEVT)=ZGKTH(JLON,ILEVT)*ZROSDPHI
      PKUROV(JLON,ILEVT)=ZGKUH(JLON,ILEVT)*ZROSDPHI

    ENDIF

  ENDDO
!------
 ENDIF ! LPBLE
!------

!*
!     ------------------------------------------------------------------
!     VIII(c) - CALCUL DES COEFFICIENTS D'ECHANGE NORMALISES.

!          COMPUTATION OF THE NORMALIZED VERTICAL TURBULENT EXCHANGE
!          COEFFICIENTS.

IF (LDIFCEXP) THEN

DO JLEV=KTDIAN,KLEV-1
  DO JLON=KIDIA,KFDIA

    ZZDPHI=PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1)
    ZZRTV=0.5_JPRB*(PR(JLON,JLEV)*PT(JLON,JLEV)+ &
     &PR(JLON,JLEV+1)*PT(JLON,JLEV+1))
    ZROSDPHI=PAPRS(JLON,JLEV)/(ZZDPHI*ZZRTV)

    ZDQW=PQV(JLON,JLEV)-PQV(JLON,JLEV+1)+PQLI(JLON,JLEV)-PQLI(JLON,JLEV+1)+ &
     &PQICE(JLON,JLEV)-PQICE(JLON,JLEV+1)
    ZDTL=PT(JLON,JLEV)-PT(JLON,JLEV+1) &
     &-PLSCPE(JLON,JLEV)*(PQLI(JLON,JLEV)+PQICE(JLON,JLEV)) &
     &+PLSCPE(JLON,JLEV+1)*(PQLI(JLON,JLEV+1)+PQICE(JLON,JLEV+1))
    ZDTETL=ZDTL+ZZDPHI/RCPD
    ZDIFTQL=ZAH(JLON,JLEV)*PL3F2(JLON,JLEV)*ZGKTH(JLON,JLEV)/ZZDPHI &
     &*(ZDQW-ZQSLTLH(JLON,JLEV)*ZDTETL)
    ZDQLI=PQLI(JLON,JLEV)-PQLI(JLON,JLEV+1)+PQICE(JLON,JLEV)-PQICE(JLON,JLEV+1)
    ZDQLI=MAX(ZEPS3,ABS(ZDQLI))*SIGN(1._JPRB,ZDQLI)
    ZGKQLH=MIN(ZEPS2,MAX(ZEPS3,ZDIFTQL*ZZDPHI/(RG*ZDQLI)))*RG

    ZDIFTQL=ZGKQLH*ZDQLI/ZZDPHI
    ZLSCPEH=0.5_JPRB*(PLSCPE(JLON,JLEV)+PLSCPE(JLON,JLEV+1))
    ZDIFTTET=ZGKTH(JLON,JLEV)*ZDTETL/ZZDPHI+ZLSCPEH*ZDIFTQL
    ZDTETI=PT(JLON,JLEV)-PT(JLON,JLEV+1)+ZZDPHI/RCPD
    ZDTETI=MAX(ZEPS3,ABS(ZDTETI))*SIGN(1._JPRB,ZDTETI)

    ZDQI=PQV(JLON,JLEV)-PQV(JLON,JLEV+1)
    ZDQI=MAX(ZEPS3,ABS(ZDQI))*SIGN(1._JPRB,ZDQI)
    ZDIFTQ=ZGKTH(JLON,JLEV)*ZDQW/ZZDPHI&
     &-ZDIFTQL

    ZGKTAH=MIN(ZEPS2,MAX(ZEPS3,ZDIFTTET*ZZDPHI/(RG*ZDTETI)))*RG
    ZGKQH=MIN(ZEPS2,MAX(ZEPS3,ZDIFTQ*ZZDPHI/(RG*ZDQI)))*RG

    PKTROV(JLON,JLEV)=ZGKTAH*ZROSDPHI
    PKQLROV(JLON,JLEV)=ZGKQLH*ZROSDPHI
    PKQROV(JLON,JLEV)=ZGKQH*ZROSDPHI
  ENDDO
ENDDO ! JLEV=KTDIAN,KLEV-1

DO JLEV=KTDIAN,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZKROVN=5.E-3_JPRB
    ZKROVN=MAX(0.0_JPRB,(-ZKROVN/200._JPRB)*(PAPHI(JLON,JLEV)-PAPHI(JLON,KLEV))/RG+ZKROVN)
    PKTROV(JLON,JLEV)=MAX(ZKROVN,PKTROV(JLON,JLEV))
    PKQROV(JLON,JLEV)=MAX(ZKROVN,PKQROV(JLON,JLEV))
    PKQLROV(JLON,JLEV)=MAX(ZKROVN,PKQLROV(JLON,JLEV))
    PKUROV(JLON,JLEV)=MAX(ZKROVN,PKUROV(JLON,JLEV))
  ENDDO
ENDDO ! JLEV=KTDIAN,KLEV-1

ENDIF ! LDIFCEXP

!*
!     ------------------------------------------------------------------
!     IX - CALCUL DE "Q1" ET DE "SIGMAS" SUR LES NIVEAUX DU MODELE.
!        > POUR CALCULS DE "PQCS" ET "PNEBS" (SUR LES "FULL-LEVELS" =
!        > SUR LES NIVEAUX DU MODELE).
!     ------------------------------------------------------------------
IF (LNEBECT) THEN
!*
!     ------------------------------------------------------------------
!     VI-BIS - ON PASSE SUR LES NIVEAUX PLEINS, CAR ON A FAIT LES
!            > CALCULS AUX DEMI-NIVEAUX POUR L'ECT (PKTROV, PECT,
!            > PLMECT), ALORS QU'IL FAUT DES CALCULS SUR LES NIVEAUX
!            > PLEIN POUR TROUVER "Q1" ET LES (PQLI,PQICE,PNEBS).
!     ------------------------------------------------------------------

DO JLON=KIDIA,KFDIA
  ZGKTF  (JLON,KTDIAN)=ZGKTH (JLON,KTDIAN)
  ZECTF  (JLON,KTDIAN)=PECT  (JLON,KTDIAN)
  ZLMECTF(JLON,KTDIAN)=PLMECT(JLON,KTDIAN)
ENDDO
DO JLEV=KTDIAN+1,KLEV
  DO JLON=KIDIA,KFDIA
    ZGKTF  (JLON,JLEV)=(ZGKTH  (JLON,JLEV)+ZGKTH  (JLON,JLEV-1))/2.0_JPRB
    ZECTF  (JLON,JLEV)=(PECT   (JLON,JLEV)+PECT   (JLON,JLEV-1))/2.0_JPRB
    ZLMECTF(JLON,JLEV)=(PLMECT (JLON,JLEV)+PLMECT (JLON,JLEV-1))/2.0_JPRB
  ENDDO
ENDDO

!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     ** DEBUT DES BOUCLES VERTICALES ET HORIZONTALES   **
!     ** START OF VERTICAL AND HORIZONTAL NESTED LOOPS  **
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLEV=KLEV,KTDIAN,-1
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA

!         - - - - - - - - - - - - - - - - - - - -
!         * VARIABLES AUXILIAIRES ET DE TRAVAIL.
!         * WORK ARRAYS AND VARIABLES
!         - - - - - - - - - - - - - - - - - - - -

    ZTF     = PT(JLON,JLEV)
    ZQWF    = PQV(JLON,JLEV)+PQLI(JLON,JLEV)+PQICE(JLON,JLEV)
    ZQWF    = MAX(ABS(ZQWF),ZEPSQ)
    ZLSCPEF = PLSCPE(JLON,JLEV)
    ZQLF    = PQLI(JLON,JLEV)+PQICE(JLON,JLEV)
    ZTLF    = ZTF-ZLSCPEF*ZQLF

    ZH       = MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTLF))
    ZEW      = FOEW(ZTLF,ZH)/PAPRSF(JLON,JLEV)
    ZQSATF   = FOQS(ZEW)
    ZDLEWF   = FODLEW(ZTLF,ZH)
    ZQSLTLF  = FDQW (ZEW, ZDLEWF) 
    ZDELTQF  = ZQWF-ZQSATF
    ZDELTQF  = SIGN(MAX(ABS(ZDELTQF),ZEPDELT),ZDELTQF)

    ZAA      = 1.0_JPRB/(1.0_JPRB+ZLSCPEF*ZQSLTLF)
    ZDD      = ZLSCPEF-(1.0_JPRB+RETV)*ZTF
    ZCTO     = RETV*ZTF

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         * CALCUL DES GRADIENTS VERTICAUX     DA/DZ = RG * DA/DPHI.
!         * COMPUTATION OF VERTICAL GRADIENTS  DA/DZ = RG * DA/DPHI.
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         * INDICE INIV TESTANT LE PLUS BAS NIVEAU DU MODELE
!         * 1 <= JLEV <= KLEV-1      ---> INIV=1
!         * JLEV = KLEV              ---> INIV=0
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    INIV    = MAX(SIGN(1,KLEV-JLEV-1), 0) ! It is =1 if JLEV<KLEV ; =0 if JLEV=KLEV
    IJLEVM1 = MAX(KTDIAN, JLEV-1)
    IJLEVP1 = MIN(KLEV,   JLEV+1)

!   INIV    = _ONE_                       ! =1 => the soil values are never used

    ZDPHI  = PAPHIF(JLON,IJLEVM1)-PAPHIF(JLON,IJLEVP1) *REAL(INIV,  JPRB)&
     & -PAPHI (JLON,KLEV)    *REAL(1-INIV,JPRB)
    ZDQW   = PQV   (JLON,IJLEVM1)-PQV   (JLON,IJLEVP1) *REAL(INIV,  JPRB)&
     & -PQS   (JLON)         *REAL(1-INIV,JPRB)&
     & +PQLI(JLON,IJLEVM1)+PQICE(JLON,IJLEVM1)&
     & -PQLI(JLON,IJLEVP1)-PQICE(JLON,IJLEVP1)
    ZDT    = PT    (JLON,IJLEVM1)-PT    (JLON,IJLEVP1) *REAL(INIV,  JPRB)&
     & -PTS   (JLON)         *REAL(1-INIV,JPRB)
    ZQLM1  = PQLI  (JLON,IJLEVM1)+PQICE (JLON,IJLEVM1)
    ZQLP1  = PQLI  (JLON,IJLEVP1)+PQICE (JLON,IJLEVP1)
    ZLCPM1 = PLSCPE(JLON,IJLEVM1)
    ZLCPP1 = PLSCPE(JLON,IJLEVP1)
    ZDQLST = ZQLM1*ZLCPM1/PT(JLON,IJLEVM1)&
     & -ZQLP1*ZLCPP1/PT(JLON,IJLEVP1)*REAL(INIV,JPRB)
    ZDSTA  = ZDT   + ZDPHI*RKAPPA/PR(JLON,JLEV)
    ZDTL   = ZDSTA *(1.0_JPRB-ZLSCPEF*ZQLF/ZTF)-ZTF*ZDQLST

    ZDIFFH = ZDTL + ZCTO   *ZDQW
    ZDIFFC = ZDQW - ZQSLTLF*ZDTL

    ZDU2   =  (PU(JLON,IJLEVM1) -PU(JLON,IJLEVP1)*REAL(INIV,JPRB))**2&
     & +(PV(JLON,IJLEVM1) -PV(JLON,IJLEVP1)*REAL(INIV,JPRB))**2
    ZDU2   =  MAX(ABS(ZDU2),ZEPSV)

    ZZETF  =  MAX(ZEPNEBS,ZECTF(JLON,JLEV))
    ZZLMF  =  ZLMECTF(JLON,JLEV)
    ZWQW   = -ZGKTF(JLON,JLEV)*ZDQW/ZDPHI
    ZWTL   = -ZGKTF(JLON,JLEV)*ZDTL/ZDPHI
    ZWDIFF =  ZWQW-ZQSLTLF*ZWTL

!         - - - - - - - - - - - - - - - - - -
!         * CALCUL DE SIGMA_S, PUIS DE Q11 :
!         - - - - - - - - - - - - - - - - - -

    ZSIGMAS2 = -ZAA*ZAA*ARSB2*ZZLMF/4._JPRB/SQRT(ZZETF)*ZWDIFF*ZDIFFC/ZDPHI
    ZSIGMAS  = MAX(ZEPSIG,SQRT(ABS(ZSIGMAS2)))
    ZQ11     = ZAA*ZDELTQF/(2*ZSIGMAS)

    IF (LCVTURB .AND. ZDELTQF > 0._JPRB .AND. PAPRSF(JLON,JLEV) < ZPRETURB .AND. &
     & LDCONV(JLON,JLEV)) THEN
      ZSIGMAS = MAX(ZSIGCR, ZSIGMAS)
      ZQ11 = ZAA*ZDELTQF/(2.0_JPRB*ZSIGMAS)
    ENDIF

!         - - - - - - - - - - - - - - - - - -
!         * CALCUL DE Q1MAX (LIMITATION SUR
!         * LA LONGUEUR DE MELANGE) :
!         - - - - - - - - - - - - - - - - - -

    IF (LECTQ1) THEN

      ZGALP2   = GALP*GALP/TURB
      ZPHI3MIN = 1.0_JPRB/(1.0_JPRB+ARSC1*ZGALP2)
      ZQ1MAX   = ZDELTQF*ZDPHI/ZZLMF&
       & /SQRT(ARSB2*AKN*ALPHAT*ZPHI3MIN)&
       & /MAX(ABS(ZDIFFC),ZEPSQ1)
      ZQ1MAX   = SIGN(MAX(ABS(ZQ1MAX),ZEPSQ1),ZQ1MAX)
      ZQ1MAX   = SIGN(MIN(ABS(ZQ1MAX),ZMAXQ1),ZQ1MAX)
      ZQ1MAX   = ZILIMQ1*ZQ1MAX -(1.0_JPRB-ZILIMQ1)*ZMAXQ1

!         - - - - - - - - - - - - - - - - - -
!         * CALCUL DE Q1MIN (LIMITATION DES
!         * VALEURS NEGATIVES D'HUMIDITE) :
!         - - - - - - - - - - - - - - - - - -

      ZQ1MIN   = STTBMIN*ZDELTQF*ABS(ZDQW)/(ZQWF*MAX(ABS(ZDIFFC),ZEPSQ1))
      ZQ1MIN   = SIGN( MAX(ABS(ZQ1MIN),ZEPSQ1),ZQ1MIN)
      ZQ1MIN   = ZILIMQ1*ZQ1MIN +(1.0_JPRB-ZILIMQ1)*ZEPSQ1

!         - - - - - - - - - - - - - - - - -
!         * LIMITATIONS (EN MODULE) DE Q1
!         * PAR ABS(Q1MIN) ET ABS(Q1MAX) :
!         - - - - - - - - - - - - - - - - -

      ZQ11     = SIGN( MAX(ABS(ZQ11),ABS(ZQ1MIN)), ZQ11)
      ZQ11     = SIGN( MIN(ABS(ZQ11),ABS(ZQ1MAX)), ZQ11)

!         - - - - - - - - - - - - - - -
!         * CALCUL DU NOUVEAU SIGMAS ET
!         * SECURITE PAR "ZEPNEBS" :
!         - - - - - - - - - - - - - - -

      ZSIGMAS   = MAX(ZEPNEBS, ZAA*ZDELTQF/(2.0_JPRB*ZQ11) )

    ENDIF  ! LECTQ1

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         * CALCUL DU NOMBRE DE RIDCHARDSON : RI = (RI)h + F2*L3*(RI)c
!         * SI : ZDSSCP=d(T+Phi/Cp) ET ZDQW=d(qw)
!         *    ZRIH = ( ZDSSCP +  ZCTO  *ZDQW   )*(ZDPHI/PT/DU2)
!         *    ZRIC = ( ZDQW   - ZQSLTLF*ZDSSCP )*(ZDPHI/PT/DU2)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZFACT  = ZDPHI/ZTF/ZDU2

    ZRIH   = ZDIFFH*ZFACT
    ZRIC   = ZDIFFC*ZFACT*ZAA*ZDD

    INQ1   = MIN( MAX(-22,FLOOR(2*ZQ11) ), 10)
    ZINC   = 2.0_JPRB*ZQ11 - INQ1
    ZSRC   = (1.0_JPRB-ZINC)*YDPHY0%RSRC1D(INQ1)+ZINC*YDPHY0%RSRC1D(INQ1+1)
    ZL3F2  = MIN(1.0_JPRB,ZSRC)* MIN( MAX(1.0_JPRB, 1.0_JPRB-ZQ11), 3._JPRB )

    ZRICHF = ZRIH + ZL3F2*ZRIC

!         --------------------------------------------------------------
!         *  CALCUL DE LA NEBULOSITE ET DE LA QUANTITE D'EAU LIQUIDE
!         *  DANS LE CAS DE NUAGES STRATIFORMES. SI RI<RICRET ON UTILISE
!         *  LES FONCTIONS "FOF0" ET "FOF1", SINON ON UTILISE LE SIGNE
!         *  DE "ZDELTQ" DANS LES CAS TRES STABLES (RI>RICRET).
!         *  (SECURITES A "ZEPNEBS" ET "1-ZEPNEBS" POUR PNEBS ET
!         *   ANNULATION DE PQCS SI PNEBS<ZEPNEBS).
!         --------------------------------------------------------------

!         - - - - - - - - - - - - - - -
!         Test du regime "instable" :  (=1 si ZRICHF<RICRET ; =0 sinon)
!         - - - - - - - - - - - - - - -
    IF (LNEBRIC) THEN
      ZRESUL=MAX(0.0_JPRB,SIGN(1.0_JPRB, RICRET-ZRICHF ))
    ELSE
      ZRESUL=1.0_JPRB
    ENDIF

!         - - - - - - - - - - - - - - -
!         Test de la "sursaturation" : (=1 si ZDELTQF>0     ; =0 sinon)
!         - - - - - - - - - - - - - - -
    ZSURSAT=MAX(0.0_JPRB,SIGN(1.0_JPRB,ZDELTQF))

!         - - - - - - - - - -
!         Calculs de PNEBS :
!         - - - - - - - - - -
    INQ1   = MIN( MAX(-22,FLOOR(2*ZQ11) ), 10)
    ZINC   = 2.0_JPRB*ZQ11 - INQ1
    ZZN1D  = (1.0_JPRB-ZINC)*YDPHY0%RN1D(INQ1)+ZINC*YDPHY0%RN1D(INQ1+1)
    ZZF0   = MIN(1.0_JPRB,ZZN1D)
    PNEBS(JLON,JLEV) = ZZF0*ZRESUL + ZSURSAT*(1.0_JPRB-ZRESUL)
    IF (LCVTURB .AND. ZDELTQF > 0._JPRB .AND. PAPRSF(JLON,JLEV) < ZPRETURB .AND. &
     & LDCONV(JLON,JLEV)) THEN
      PNEBS(JLON,JLEV) = ZZF0
    ENDIF

!         - - - - - - - - - - - - - - - - - - - - - - - -
!         On limite PNEBS entre ZEPNEBS et 1.-ZEPNEBS :
!         - - - - - - - - - - - - - - - - - - - - - - - -
    PNEBS(JLON,JLEV) = MAX( ZEPNEBS ,MIN(PNEBS(JLON,JLEV), 1.0_JPRB-ZEPNEBS) )

!         - - - - - - - - - -
!         Calculs de PQCS :
!         - - - - - - - - - -
    INQ1  = MIN( MAX(-22,FLOOR(2*ZQ11) ), 10)
    ZINC  = 2.0_JPRB*ZQ11 - INQ1
    ZZF1  = (1.0_JPRB-ZINC)*YDPHY0%RRC1D(INQ1)+ZINC*YDPHY0%RRC1D(INQ1+1)
    ZZQC  = 2.0_JPRB*ZSIGMAS*ZZF1 *ZRESUL&
     & + ABS(ZAA*ZDELTQF)*ZSURSAT *(1.0_JPRB-ZRESUL)
    IF (LCVTURB .AND. ZDELTQF > 0._JPRB .AND. PAPRSF(JLON,JLEV) < ZPRETURB .AND. &
     & LDCONV(JLON,JLEV)) THEN
      ZZQC = 2.0_JPRB*ZSIGMAS*ZZF1
    ENDIF
    ZZQC  = ZZQC/(1.0_JPRB+ZZQC)
    PQCS(JLON,JLEV) = MIN(ZZQC,PQV(JLON,JLEV)+PQICE(JLON,JLEV)+PQLI(JLON,JLEV))

!         - - - - - - - - - - - - - - - - - - - - -
!         Annulation de PQCS si PNEBS < ZEPNEBS : (alors ZNEBLOW=1)
!         - - - - - - - - - - - - - - - - - - - - -
    ZNEBLOW=MAX(0.0_JPRB,SIGN(1.0_JPRB, ZEPNEBS-PNEBS(JLON,JLEV) ))
    PQCS(JLON,JLEV) = PQCS(JLON,JLEV)*(1.0_JPRB-ZNEBLOW)

!         - - - - - - - - - - - - - - - - - - - - - - - - - -
!         Calcul de de PQLI et PQICE en fonction de "LNEIGE"
!         - - - - - - - - - - - - - - - - - - - - - - - - - -
    IF (LNEIGE) THEN
      ZDELTA=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-PT(JLON,JLEV)))
      ZPLS=ZDELTA*(1.0_JPRB-EXP(-(RTT-PT(JLON,JLEV))**2*ZGAUSS))
      PQLI (JLON,JLEV)=PQCS(JLON,JLEV)*(1.0_JPRB-ZPLS)
      PQICE(JLON,JLEV)=PQCS(JLON,JLEV)*ZPLS
    ELSE
      PQLI (JLON,JLEV)=PQCS(JLON,JLEV)
      PQICE(JLON,JLEV)=0.0_JPRB
    ENDIF

  ENDDO
ENDDO

!         - - - - - - - - - - - - - - - - - - - - -
!          Annulation de PQCS et PNEBS si ZSTAB=0 (instable en surface)
!           pour le dernier niveau (le plus bas)
!         - - - - - - - - - - - - - - - - - - - - -

DO JLON=KIDIA,KFDIA
   PQCS (JLON,KLEV) = PQCS (JLON,KLEV)*ZSTAB(JLON)
   PNEBS(JLON,KLEV) = PNEBS(JLON,KLEV)*ZSTAB(JLON)
   PNEBS(JLON,KLEV) = MAX( ZEPNEBS ,MIN(PNEBS(JLON,KLEV), 1.0_JPRB-ZEPNEBS) )
ENDDO

ELSE
   PNEBS(:,:) = ZEPNEBS
   PQCS(:,:)  = 0.0_JPRB
ENDIF ! KEY LNEBECT

!         - - - - - - - - - - - - - - - - - - - -
!         * VARIABLES AUXILIAIRES ET DE TRAVAIL.
!         * WORK ARRAYS AND VARIABLES
!         - - - - - - - - - - - - - - - - - - - -

!     - - - - - - - - - - - - - - - - - - - - - - - - -
!     ** FIN DES BOUCLES VERTICALES ET HORIZONTALES  **
!     ** END OF VERTICAL AND HORIZONTAL NESTED LOOPS **
!     - - - - - - - - - - - - - - - - - - - - - - - - -

!*
!     ------------------------------------------------------------------

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACTURB',1,ZHOOK_HANDLE)
END SUBROUTINE ACTURB

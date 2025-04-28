SUBROUTINE ACTKE ( YDCST, YDLDDH, YDMDDH, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV,      &
& PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP, PR, PT, PU, PV, PQ, PQICONV,  PQLCONV, PLSCPE, PCD, PCH, PGZ0, &
& PTS, PQS, PQICE, PQLI, PECT, PPRODTH,  PNLAB, PNLABCVP, PKTROV, PKQROV, PKQLROV, PKUROV, PXTROV,    &
& PXUROV, PNBVNO,  PNEBS, PQCS, PNEBS0, PQCS0, PCOEFN, PFECT, PFECTI, PECT1, PTPRDY, PEDR,  YDDDH)

!**** *ACTKE * - SCHEMA DE TURBULENCE TKE

!     Sujet.
!     ------
!     - APPEL DES SOUS-PROGRAMMES ACBL89, ACTURB, ACEVOLET

!**   Interface.
!     ----------
!        *CALL* *ACTKE*

!-----------------------------------------------------------------------

!     Auteur.
!     -------
!       04-11, Francois Bouyssel

!   Modifications.
!   --------------
!      2006-04-11 E. Bazile : Ajout de CDLOCK.
!      2006-05-04 E. Bazile : Possibilite de passer l'ECT sur Full-level 
!                             (LECTFL) pour l'advection.
!      2007-05-10 E. Bazile : PXTXX en sortie, PQC0, PNEBS0 pour rayt.
!      2008-04-28 E. Bazile : Introduction of PPROTH and removal PRS, PSTAB
!      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!      2009-11-10 E. Bazile : Input of PDELP for ACEVOLET.
!      2010-12-01 E. Bazile : Output for AROCLDIA of TKE(t+dt)
!      2011-10-01 E. Bazile : TKE fluxes for DDH and use of HL2FL and FL2HL
!      2014-10-14 E. Bazile : EDR : Output similar to TKE dissipation (>0)
!      2015-03-11 J.M Piriou: fix bug in case of LFLEXDIA=T: introduce ZDIAG array.
!      2016-10-04 P. Marguinaud: Port to single precision
!      2018-09-19 R. Roehrig: Add convection index LLCONV and PKQROV/PKQLROV (from JF GuÃÂÃÂ©rÃÂÃÂ©my)
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!-----------------------------------------------------------------------


! -   ARGUMENTS D'ENTREE.
!   -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT.
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KTDIAT     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE.
! KTDIAN     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE + NEBULOSITE.
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".

! - 2D (0:KLEV) .

! PAPHI      : GEOPOTENTIEL AUX DEMI-NIVEAUX.
! PAPRS      : PRESSION AUX DEMI-NIVEAUX.
! PPRODTH    : PRODUCTION THERMIQUE DU A AU SCHEMA SHALLOW.

! - 2D (1:KLEV) .

! PAPHIF     : GEOPOTENTIEL AUX NIVEAUX DES COUCHES.
! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
! PDELP      : EPAISSEUR EN PRESSION DE LA COUCHE.
! PR         : CONSTANTE DES GAZ POUR L'AIR.
! PT         : TEMPERATURE (APRES AJUSTEMENT CONVECTIF).
! PU         : COMPOSANTE EN X DU VENT.
! PV         : COMPOSANTE EN Y DU VENT.
! PQ         : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PQICONV    : EAU LIQUIDE CONVECTIVE.
! PQLCONV    : EAU SOLIDE CONVECTIVE.
! PLSCPE     : RAPPORT EFECTIF DES L ET CP EN CONDENSATION/EVAPORATION.
! PECT       : ENERGIE CINETIQUE TURBULENTE.

! - 1D (DIAGNOSTIQUE) .

! PCD        : COEFFICIENT D'ECHANGE EN SURFACE POUR U ET V
! PCH        : COEFFICIENT D'ECHANGE EN SURFACE POUR T ET Q
! PGZ0       : G FOIS LA LONGUEUR DE RUGOSITE COURANTE.
! PTS        : TEMPERATURE DE SURFACE
! PQS        : HUMIDITE SPECIFIQUE DE SURFACE.

!-----------------------------------------------------------------------

! -   ARGUMENTS EN ENTREE/SORTIE.
!     ---------------------------

! - 2D (1:KLEV) .

! PQICE      : HUMIDITE SPECIFIQUE  SOLIDE "PRONOSTIQUE".
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE "PRONOSTIQUE".
! PNLAB      : Si 1 Presence d'un nuage Shallow (used in ACBL89)
! PNLABCVP   : Si 1 Presence d'un nuage Deep    (used in ACBL89)


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
! PXTROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PKTROV.
! PXUROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PKUROV.
! PNBVNO     : CARRE DE FR. BRUNT-VAISALA DIVISEE PAR G FOIS LA DENSITE.

! - 2D (1:KLEV) .

! PNEBS      : NEBULOSITE PARTIELLE STRATIFORME.
! PQCS       : EAU CONDENSEE STRATIFORME.
! PNEBS      : NEBULOSITE PARTIELLE STRATIFORME APRES AJUSTEMENT.
!            : STRATIFORM FRACTIONAL CLOUDINESS APRES AJUSTEMENT.
! PQCS0      : CONTENU "STRATIFORME" EN CONDENSAT NUAGEUX POUR RAYT.
!            : STRATIRORM CLOUD WATER (LIQUID + SOLID) FOR RADIATION.
! PNEBS0     : NEBULOSITE PARTIELLE STRATIFORME POUR RAYT.
! PCOEFN     : COEFFICIENT STATISTIQUE POUR LES FLUX D'EAUX CONDENSEES.
! PTPRDY     : tendance de TKE due a la production dynamique.
! PEDR       : EDR tendance de TKE due a la dissipation (calcul specifique)

!-----------------------------------------------------------------------

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE YOMMDDH              , ONLY : TMDDH
USE PARKIND1             , ONLY : JPIM,    JPRB
USE YOMHOOK              , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST               , ONLY : TCST
USE YOMLSFORC            , ONLY : LMUSCLFA,NMUSCLFA
USE YOMLDDH              , ONLY : TLDDH
USE DDH_MIX              , ONLY : ADD_FIELD_3D, TYP_DDH, NEW_ADD_FIELD_3D 

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)                  ,INTENT(IN)     :: YDCST
TYPE(TLDDH)                 ,INTENT(IN)     :: YDLDDH
TYPE(TMDDH)                 ,INTENT(IN)     :: YDMDDH
TYPE(MODEL_PHYSICS_MF_TYPE) ,INTENT(IN)     :: YDML_PHY_MF
INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KIDIA 
INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KFDIA 
INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KLON 
INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KTDIAT 
INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KTDIAN 
INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KLEV 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPHI(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPHIF(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PR(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PT(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PU(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PV(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PQ(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PQICONV(KLON,KLEV)
REAL(KIND=JPRB)             ,INTENT(IN)     :: PQLCONV(KLON,KLEV)
REAL(KIND=JPRB)             ,INTENT(IN)     :: PLSCPE(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PCD(KLON) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PCH(KLON) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PGZ0(KLON) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PTS(KLON) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PQS(KLON) 
REAL(KIND=JPRB)             ,INTENT(INOUT)  :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(INOUT)  :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PECT(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PPRODTH(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PNLAB(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(IN)     :: PNLABCVP(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PKTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PKQROV(KLON,0:KLEV)
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PKQLROV(KLON,0:KLEV)
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PKUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PXTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PXUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PNBVNO(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PNEBS(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PQCS(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PNEBS0(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PQCS0(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PCOEFN(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PFECT(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PFECTI(KLON,0:KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PECT1(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PTPRDY(KLON,KLEV) 
REAL(KIND=JPRB)             ,INTENT(OUT)    :: PEDR(KLON,KLEV) 
TYPE(TYP_DDH)               ,INTENT(INOUT)  :: YDDDH

!-----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JLON
! Attention tableau pour acturb, acbl89, et acevolet de dim KLEV mais sur 
! les 1/2 niveaux
REAL(KIND=JPRB) :: ZUSLE  (KLON,KLEV), ZLMECT(KLON,KLEV), ZPHI3(KLON,KLEV)
REAL(KIND=JPRB) :: ZPRODTH(KLON,KLEV),ZPRDY(KLON,KLEV)
REAL(KIND=JPRB) :: ZDIAG(KLON,KLEV)
REAL(KIND=JPRB) :: ZDIFF(KLON,KLEV),ZDISS(KLON,KLEV)
REAL(KIND=JPRB) :: ZECT(KLON,KLEV), ZECT1(KLON,KLEV)
REAL(KIND=JPRB) :: ZDELPSG(KLON,KLEV)
!--------------------------------------------------------------------------
REAL(KIND=JPRB) :: ZDET(KLON,KLEV)
REAL(KIND=JPRB) :: ZKCLS(KLON), ZECTCLS(KLON)
REAL(KIND=JPRB) :: ZTABHL(KLON,0:KLEV),ZTABFL(KLON,KLEV),ZDIFFAR(KLON,KLEV)
! For DDH and MUSC and in future in CPTEND_NEW ???
REAL(KIND=JPRB) :: ZFPRTH(KLON,0:KLEV),ZFPRDY(KLON,0:KLEV),&
    & ZFDISS(KLON,0:KLEV),ZFDIFF(KLON,0:KLEV),ZFCORTKE(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZTPRTH(KLON,KLEV),&
    & ZTDISS(KLON,KLEV),ZTDIFF(KLON,KLEV),ZTCORTKE(KLON,KLEV),ZEDR(KLON,KLEV)
REAL(KIND=JPRB) :: ZEPSQ
LOGICAL         :: LLCONV(KLON,KLEV)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

#include "acbl89.intfb.h"
#include "acturb.intfb.h"
#include "acevolet.intfb.h"
#include "hl2fl.intfb.h"
#include "fl2hl.intfb.h"
#include "wrscmr.intfb.h"
#include "aclender.intfb.h"

!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACTKE',0,ZHOOK_HANDLE)
ASSOCIATE(RPRTH=>YDML_PHY_MF%YRPHY0%RPRTH, NLEND=>YDML_PHY_MF%YRPHY%NLEND, ECTMIN=>YDML_PHY_MF%YRPHY0%ECTMIN, &
 & TSPHY=>YDML_PHY_MF%YRPHY2%TSPHY, &
 & LECTFL=>YDML_PHY_MF%YRPHY%LECTFL, &
 & RG=>YDCST%RG, &
 & LFLEXDIA=>YDLDDH%LFLEXDIA,LDDH_OMP=>YDLDDH%LDDH_OMP)
!-----------------------------------------------------------------------
ZEPSQ   = 1.E-10_JPRB 
ZTDIFF(:,:)  =0.0_JPRB
PEDR(:,:)    =0.0_JPRB
ZTDISS(:,:)  =0.0_JPRB
ZTPRTH(:,:)  =0.0_JPRB
PTPRDY(:,:)  =0.0_JPRB
ZTCORTKE(:,:)=0.0_JPRB
ZFDIFF(:,:)  =0.0_JPRB
ZFDISS(:,:)  =0.0_JPRB
ZFPRTH(:,:)  =0.0_JPRB
ZFPRDY(:,:)  =0.0_JPRB
ZFCORTKE(:,:)=0.0_JPRB
ZDET(:,:)=0.0_JPRB
ZDELPSG(:,:)=0.0_JPRB
ZDIAG(:,:)=0.0_JPRB
!     ------------------------------------------------------------------
! 0.   Passage eventuel sur les demi-niveaux dans le cas ou LECTFL=.TRUE.
!      en effet en sortie d'ACTKE on a passe l'ECT sur les niveaux pleins pour 
!      l'advecter.
ZDIFFAR(:,:)=0._JPRB
IF (LECTFL) THEN
! PECT en entree sur les FL donc passage sur les HL pour les calculs physiques 
   CALL FL2HL ( KIDIA, KFDIA, KLON, 1, KLEV,&
          & PAPRS, PAPRSF, PECT, ZECT, 1)
   IF (LMUSCLFA) THEN
      ZTABFL(:,:)=0._JPRB
      CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
          & PAPRS, PAPRSF, ZECT, 1, ZTABFL)
      DO JLEV = 1, KLEV
      DO JLON = KIDIA,KFDIA
         ZDIFFAR(JLON,JLEV)=(ZTABFL(JLON,JLEV)-PECT(JLON,JLEV))/TSPHY
      ENDDO
      ENDDO 
   ENDIF
ELSE
!  ECT toujours sur les demi-niveaux
   DO JLEV = 1, KLEV
      DO JLON = KIDIA,KFDIA
         ZECT(JLON,JLEV)=PECT(JLON,JLEV)
      ENDDO
   ENDDO 
ENDIF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.   Trois parties : ACBL89 (longueur de melange de
!      Bougeault-Lacarerre-1989) + ACTURB (calculs de
!      l'ect et de ses evolutions) + ACEVOLET (evolution
!      proprement dite de l'ect + calcul des flux
!      des tendances physiques pour CPTEND). 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DO JLEV = 1, KLEV
  DO JLON = KIDIA,KFDIA
    ZUSLE  (JLON,JLEV) = 0.0_JPRB
    ZLMECT (JLON,JLEV) = 0.0_JPRB
    ZPHI3  (JLON,JLEV) = 0.0_JPRB
  ENDDO
ENDDO

DO JLON = KIDIA,KFDIA
  ZECTCLS (JLON) = 0.0_JPRB
  ZKCLS   (JLON) = 0.0_JPRB
ENDDO

DO JLEV=KTDIAT,KLEV
  DO JLON=KIDIA,KFDIA
    ZECT(JLON,JLEV) = MAX( ECTMIN, ZECT(JLON,JLEV) )
    ZDELPSG(JLON,JLEV)=PDELP(JLON,JLEV)/RG
  ENDDO
ENDDO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.1   Calculs : des longueurs de melange (ZLMECT, ZUSLE)
!       de l'ect en surface (ZECTCLS)
!       de la production thermique (ZPRODTH)
!       des coefficients d'echange (PKTROV,PKUROV, ZKCLS)
!       de la frequence de Brunt-Vaisala (PNBVNO)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     
IF (NLEND == 0) THEN
          CALL ACBL89   (YDCST,  YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0, KIDIA, KFDIA, KLON, KTDIAN, KLEV,&
              & PAPHI, PAPHIF, PAPRS, PAPRSF, PT,&
              & ZECT, PQ, PQICE, PQLI, PNLAB, PNLABCVP,&
              & PGZ0, PTS,&
              & ZUSLE, ZLMECT, ZPHI3)

ELSEIF (NLEND>0) THEN
    CALL ACLENDER  (YDCST,  YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0, KIDIA,  KFDIA,    KLON,   KTDIAN, KLEV,&
                  & PAPHI,  PAPHIF,   PAPRS,  PAPRSF, PT,&
                  & PQ,      PQICE,  PQLI, PNLAB, PNLABCVP,&
                  & PLSCPE,  PU,     PV,   ZECT,  PGZ0,&
                  & ZUSLE,ZLMECT,ZPHI3)
ENDIF

CALL ACTURB   (YDCST, YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0, KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV,&
              & PAPHI, PAPHIF, PAPRS, PAPRSF, PR, PT,&
              & PU, PV, ZECT, PQ, LLCONV, PLSCPE,&
              & ZLMECT, ZPHI3,&
              & PCD, PCH, PGZ0, PTS, PQS, &
              & PQICE, PQLI,&
              & PKTROV, PKQROV, PKQLROV, PKUROV, PNBVNO,&
              & ZPRODTH, PNEBS, PQCS, PCOEFN,&
              & ZKCLS, ZECTCLS )

PNEBS0(:,:)=PNEBS(:,:)
PQCS0(:,:)=PQCS(:,:)

PXTROV(:,:)=1.0_JPRB
PXUROV(:,:)=1.0_JPRB

DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      ZPRODTH(JLON,JLEV)=ZPRODTH(JLON,JLEV)+ &
          &              RPRTH*MAX(0._JPRB,PPRODTH(JLON,JLEV))
   ENDDO
ENDDO
!- - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.2 Calcul d'evolution de l'ECT
!- - - - - - - - - - - - - - - - - - - - - - - - - -
CALL ACEVOLET (YDCST, YDML_PHY_MF,KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV, &
              & PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP, ZECT, &
              & PKUROV, PR, PT, PU, PV, ZUSLE, ZPRODTH,&
              & ZKCLS, ZECTCLS,&
              & ZECT1, ZPRDY, ZDIFF, ZDISS)

!     ------------------------------------------------------------------
! 2.   Passage eventuel sur les niveaux pleins dans le cas ou LECTFL=.TRUE.
!      pour l'advecter.
PECT1(:,:)=ECTMIN
! Calcul de l'EDR avec protection de la valeur min de L a 0.01
DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      ZEDR(JLON,JLEV)= MIN(100._JPRB,ZUSLE(JLON,JLEV)*RG)* &
           & (0.5_JPRB*(ZECT1(JLON,JLEV)+PECT(JLON,JLEV)))**1.5
   ENDDO
ENDDO
IF (LECTFL) THEN
! Passage de ZECT1 sur les niveaux pleins pour calculer la tendance sur les FL
! puis le flux  
   CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
          & PAPRS, PAPRSF, ZECT1, 1, PECT1) 
   DO JLEV = KTDIAT, KLEV
      DO JLON = KIDIA,KFDIA
         ZDET(JLON,JLEV)=(PECT1(JLON,JLEV)-PECT(JLON,JLEV))/TSPHY
      ENDDO
   ENDDO
   ! Production dynamique
   CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
      & PAPRS, PAPRSF, ZPRDY, 1, PTPRDY) 
   ! EDR
   CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
      & PAPRS, PAPRSF, ZEDR, 1, PEDR) 
   IF (LMUSCLFA.OR.LFLEXDIA) THEN 
      ! Dissipation
      CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
          & PAPRS, PAPRSF, ZDISS, 1, ZTDISS) 
      ! Terme de diffusion
      CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
          & PAPRS, PAPRSF, ZDIFF, 1, ZTDIFF) 
      ! Production dynamique
      CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
         & PAPRS, PAPRSF, ZPRDY, 1, PTPRDY) 
      ! Production thermique
      CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
         & PAPRS, PAPRSF, ZPRODTH, 1, ZTPRTH) 
      ! Dissipation
      CALL HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,&
         & PAPRS, PAPRSF, ZDISS, 1, ZTDISS) 
      DO JLEV = KTDIAT, KLEV
      DO JLON = KIDIA,KFDIA
         ZTCORTKE(JLON,JLEV)=ZDET(JLON,JLEV)-ZTPRTH(JLON,JLEV) &
           & -PTPRDY(JLON,JLEV)-ZTDIFF(JLON,JLEV)-ZTDISS(JLON,JLEV)
         ZTDIFF(JLON,JLEV)=ZTDIFF(JLON,JLEV)+ZTCORTKE(JLON,JLEV)
      ENDDO
      ENDDO
      IF (LMUSCLFA) CALL WRSCMR(NMUSCLFA,'PECT1',PECT1,KLON,KLEV)
      IF (LMUSCLFA) CALL WRSCMR(NMUSCLFA,'ZDECT',ZDET,KLON,KLEV)
      IF (LMUSCLFA) CALL WRSCMR(NMUSCLFA,'ZDIFF',ZTDIFF,KLON,KLEV)
      IF (LMUSCLFA) CALL WRSCMR(NMUSCLFA,'ZDIFFAR',ZDIFFAR,KLON,KLEV)
      IF (LMUSCLFA) CALL WRSCMR(NMUSCLFA,'ZPRDY',PTPRDY,KLON,KLEV)
      IF (LMUSCLFA) CALL WRSCMR(NMUSCLFA,'ZPRTH',ZTPRTH,KLON,KLEV)
      IF (LMUSCLFA) CALL WRSCMR(NMUSCLFA,'ZDISS',ZTDISS,KLON,KLEV)
      IF (LFLEXDIA) THEN
         ZTABHL(:,:)=0._JPRB
         DO JLEV = KTDIAT, KLEV
         DO JLON = KIDIA,KFDIA
            ZTPRTH(JLON,JLEV)=ZTPRTH(JLON,JLEV)*ZDELPSG(JLON,JLEV)
            ZDIAG(JLON,JLEV)=PTPRDY(JLON,JLEV)*ZDELPSG(JLON,JLEV)
            ZTDISS(JLON,JLEV)=ZTDISS(JLON,JLEV)*ZDELPSG(JLON,JLEV)
            ZTDIFF(JLON,JLEV)=ZTDIFF(JLON,JLEV)*ZDELPSG(JLON,JLEV)
         ENDDO
         ENDDO
         IF (LDDH_OMP) THEN
           CALL NEW_ADD_FIELD_3D(YDMDDH,ZTPRTH,'TKEPRTH',YDDDH)
           CALL NEW_ADD_FIELD_3D(YDMDDH,ZDIAG,'TKEPRDY',YDDDH)
           CALL NEW_ADD_FIELD_3D(YDMDDH,ZTDISS,'TKEDISS',YDDDH)
           CALL NEW_ADD_FIELD_3D(YDMDDH,ZTDIFF,'TKEDIFF',YDDDH)
         ELSE
           CALL ADD_FIELD_3D(YDLDDH,ZTPRTH,'TKEPRTH','T','ARP',.TRUE.,.TRUE.)
           CALL ADD_FIELD_3D(YDLDDH,ZDIAG,'TKEPRDY','T','ARP',.TRUE.,.TRUE.)
           CALL ADD_FIELD_3D(YDLDDH,ZTDISS,'TKEDISS','T','ARP',.TRUE.,.TRUE.)
           CALL ADD_FIELD_3D(YDLDDH,ZTDIFF,'TKEDIFF','T','ARP',.TRUE.,.TRUE.)
         ENDIF 
      ENDIF
   ENDIF
ELSE
!  ECT toujours sur les demi-niveaux
   DO JLEV = KTDIAT, KLEV
!DEC$ IVDEP
      DO JLON = KIDIA,KFDIA
         ZDET(JLON,JLEV) =(ZECT1(JLON,JLEV)-PECT(JLON,JLEV))/TSPHY
         PECT1(JLON,JLEV)=ZECT1(JLON,JLEV)
         PTPRDY(JLON,JLEV)=ZPRDY(JLON,JLEV)
         PEDR(JLON,JLEV)=ZEDR(JLON,JLEV)
      ENDDO
   ENDDO 
   IF (LMUSCLFA) THEN
      ZTABHL(:,:)=0._JPRB
      ZTABHL(:,1:)= PECT1(:,:)
      CALL WRSCMR(NMUSCLFA,'PECT1',ZTABHL,KLON,KLEV+1)
      ZTABHL(:,1:)= ZDET(:,:)
      CALL WRSCMR(NMUSCLFA,'ZDECT',ZTABHL,KLON,KLEV+1)
      ZTABHL(:,1:)= ZPRDY(:,:)
      CALL WRSCMR(NMUSCLFA,'ZPRDY',ZTABHL,KLON,KLEV+1)
      ZTABHL(:,1:)= ZPRODTH(:,:)
      CALL WRSCMR(NMUSCLFA,'ZPRTH',ZTABHL,KLON,KLEV+1)
      ZTABHL(:,1:)= ZDISS(:,:)
      CALL WRSCMR(NMUSCLFA,'ZDISS',ZTABHL,KLON,KLEV+1)
      DO JLEV = KTDIAT, KLEV
      DO JLON = KIDIA,KFDIA
         ZTCORTKE(JLON,JLEV)=ZDET(JLON,JLEV)-ZPRODTH(JLON,JLEV)&
           & -ZPRDY(JLON,JLEV)-ZDIFF(JLON,JLEV)-ZDISS(JLON,JLEV)
         ZDIFF(JLON,JLEV)=ZDIFF(JLON,JLEV)+ZTCORTKE(JLON,JLEV)
      ENDDO
      ENDDO
      ZTABHL(:,1:)= ZDIFF(:,:)
      CALL WRSCMR(NMUSCLFA,'ZDIFF',ZTABHL,KLON,KLEV+1)
   ENDIF
ENDIF
IF (LFLEXDIA) THEN
   ZTABFL(:,:)=0._JPRB
   ZTABFL(KIDIA:KFDIA,KTDIAT:KLEV)=PDELP(KIDIA:KFDIA,KTDIAT:KLEV) &
         & *PECT1(KIDIA:KFDIA,KTDIAT:KLEV)
   IF(LDDH_OMP) THEN
     CALL NEW_ADD_FIELD_3D(YDMDDH,ZTABFL,'VKE',YDDDH)
    ELSE
     CALL ADD_FIELD_3D(YDLDDH,ZTABFL,'VKE','V','ARP',.TRUE.,.TRUE.)
   ENDIF
ENDIF
! CALCUL DU FLUX PAR INTEGRATION DE LA TENDANCE DE HAUTS EN BAS
!         LES FLUX SONT SUPPOSES NULS AU PREMIER NIVEAU (KTDIA) DE
!         CALCUL (FLUX AU NIVEAU DU MODELE).
DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      PFECT(JLON,JLEV)=PFECT(JLON,JLEV-1)- ZDET(JLON,JLEV) &
           & * ZDELPSG(JLON,JLEV)
      PFECTI(JLON,JLEV)=ZDET(JLON,JLEV)
   ENDDO
ENDDO
!-----------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACTKE',1,ZHOOK_HANDLE)
END SUBROUTINE ACTKE


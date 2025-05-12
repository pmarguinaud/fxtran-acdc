SUBROUTINE ACTKE2 (YDCST, YDML_PHY_MF,KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV,&
            & PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP, PR, PT,&
            & PU, PV, &
            & PECT, PPRODTH, PKUROV, &
            & PUSLE,PKCLS,PECTCLS, &
            & PFECT , PECT1 , PTPRDY, PEDR)

!$ACDC singlecolumn 


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
!      2018-09-19 R. Roehrig: Add convection index LLCONV and PKQROV/PKQLROV (from JF Guérémy)
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


!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PKTROV     : COEFFICIENT D'ECHANGE VERTICAL DE T ET Q EN KG/(M*M*S).
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
USE PARKIND1             , ONLY : JPIM,    JPRB
USE YOMHOOK              , ONLY : LHOOK, DR_HOOK, JPHOOK

USE YOMCST               , ONLY : TCST

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(IN):: YDML_PHY_MF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAT 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PPRODTH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PECT1(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFECT(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTPRDY(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PEDR(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUSLE(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKCLS(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECTCLS(KLON)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PKUROV(KLON,0:KLEV)
!-----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JLON
! Attention tableau pour acturb, acbl89, et acevolet de dim KLEV mais sur 
! les 1/2 niveaux
REAL(KIND=JPRB) :: ZPRDY(KLON,KLEV)
REAL(KIND=JPRB) :: ZDIFF(KLON,KLEV),ZDISS(KLON,KLEV)
REAL(KIND=JPRB) :: ZECT(KLON,KLEV), ZECT1(KLON,KLEV)
REAL(KIND=JPRB) :: ZDELPSG(KLON,KLEV)
!--------------------------------------------------------------------------
REAL(KIND=JPRB) :: ZDET(KLON,KLEV)
! For DDH and MUSC and in future in CPTEND_NEW ???
REAL(KIND=JPRB) :: ZEDR(KLON,KLEV)

REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

#include "acevolet.intfb.h"
#include "hl2fl.intfb.h"
#include "fl2hl.intfb.h"

!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACTKE2',0,ZHOOK_HANDLE)
ASSOCIATE(RPRTH=>YDML_PHY_MF%YRPHY0%RPRTH, NLEND=>YDML_PHY_MF%YRPHY%NLEND, ECTMIN=>YDML_PHY_MF%YRPHY0%ECTMIN, &
 & TSPHY=>YDML_PHY_MF%YRPHY2%TSPHY, &
 & RG=>YDCST%RG, &
 & LECTFL=>YDML_PHY_MF%YRPHY%LECTFL)
!-----------------------------------------------------------------------
PEDR(:,:)    =0.0_JPRB
PTPRDY(:,:)  =0.0_JPRB
ZDET(:,:)=0.0_JPRB
ZDELPSG(:,:)=0.0_JPRB
!     ------------------------------------------------------------------
! 0.   Passage eventuel sur les demi-niveaux dans le cas ou LECTFL=.TRUE.
!      en effet en sortie d'ACTKE on a passe l'ECT sur les niveaux pleins pour 
!      l'advecter.
IF (LECTFL) THEN
! PECT en entree sur les FL donc passage sur les HL pour les calculs physiques 
   CALL FL2HL ( KIDIA, KFDIA, KLON, 1, KLEV,&
          & PAPRS, PAPRSF, PECT, ZECT, 1)
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


DO JLEV=KTDIAT,KLEV
  DO JLON=KIDIA,KFDIA
    ZECT(JLON,JLEV) = MAX( ECTMIN, ZECT(JLON,JLEV) )
    ZDELPSG(JLON,JLEV)=PDELP(JLON,JLEV)/RG
  ENDDO
ENDDO
     
!- - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.2 Calcul d'evolution de l'ECT
!- - - - - - - - - - - - - - - - - - - - - - - - - -
CALL ACEVOLET (YDCST, YDML_PHY_MF,KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV, &
              & PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP, ZECT, &
              & PKUROV, PR, PT, PU, PV, PUSLE, PPRODTH,&
              & PKCLS, PECTCLS,&
              & ZECT1, ZPRDY, ZDIFF, ZDISS)

!     ------------------------------------------------------------------
! 2.   Passage eventuel sur les niveaux pleins dans le cas ou LECTFL=.TRUE.
!      pour l'advecter.
PECT1(:,:)=ECTMIN
! Calcul de l'EDR avec protection de la valeur min de L a 0.01
DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      ZEDR(JLON,JLEV)= MIN(100._JPRB,PUSLE(JLON,JLEV)*RG)* &
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
ELSE
!  ECT toujours sur les demi-niveaux
   DO JLEV = KTDIAT, KLEV
      DO JLON = KIDIA,KFDIA
         ZDET(JLON,JLEV) =(ZECT1(JLON,JLEV)-PECT(JLON,JLEV))/TSPHY
         PECT1(JLON,JLEV)=ZECT1(JLON,JLEV)
         PTPRDY(JLON,JLEV)=ZPRDY(JLON,JLEV)
         PEDR(JLON,JLEV)=ZEDR(JLON,JLEV)
      ENDDO
   ENDDO 
ENDIF
! CALCUL DU FLUX PAR INTEGRATION DE LA TENDANCE DE HAUTS EN BAS
!         LES FLUX SONT SUPPOSES NULS AU PREMIER NIVEAU (KTDIA) DE
!         CALCUL (FLUX AU NIVEAU DU MODELE).
DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      PFECT(JLON,JLEV)=PFECT(JLON,JLEV-1)- ZDET(JLON,JLEV) &
           & * ZDELPSG(JLON,JLEV)
   ENDDO
ENDDO
!-----------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACTKE2',1,ZHOOK_HANDLE)
END SUBROUTINE ACTKE2

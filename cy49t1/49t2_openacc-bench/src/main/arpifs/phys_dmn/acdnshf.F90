SUBROUTINE ACDNSHF(YDCST, YDPHY,YDPHY1,KIDIA, KFDIA, KLON, &
 & KTDIA, KLEV, &
 !---------------------------------------------------------------------
 ! - INPUT .
 & PEMIS, PLSM, PNEIJ, PQ, PQS, PTS, PCHROV, PDQSTS, &
 ! - OUTPUT .
 & PDERNSHF)

!**** ACDNSHF ** -

!     Purpose.
!     --------
!           COMPUTATION OF THE DERIVATIVE OF NON SOLAR SURFACE FLUXES.

!**   Interface.
!     ----------
!        *CALL* *ACDNSHF*

!        Explicit arguments :
!        --------------------
!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
! -   INPUT ARGUMENTS.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.
! - DIMENSIONS.

! KIDIA, KFDIA : BORNES BOUCLES HORIZONTALES   (IST,IEND DANS CPG).
! KIDIA, KFDIA : START/END OF HORIZONTAL LOOP  (IST,IEND IN *CPG*).
! KLON : DIMENSION HORIZONTALE                 (NPROMA DANS CPG).
! KLON : HORIZONTAL DIMENSION                  (NPROMA IN *CPG*).
! KLON: IDEM BUT FOR ARRAYS USED SOLELY BY DMN PHYSICS
! KTDIA : DEBUT BOUCLE VERTICALE DANS LA PHYSIQUE.
! KTDIA : START OF THE VERTICAL LOOP IN THE PHYSICS (IF SOME LEVELS ARE
!                     SKIPPED AT THE TOP OF THE MODEL).
! KLEV : FIN BOUCLE VERTICE ET DIMENSION VERTICALE (NFLEVG DANS CPG).
! KLEV : END OF VERTICAL LOOP AND VERTICAL DIMENSION(NFLEVG IN *CPG*).
! KGL1, KGL2 : BORNES BANDES LATITUDES CONCATENEES DANS BOUCLE (1,KLON)
! KGL1, KGL2 : START/END CONCATENATED LATITUDES IN VECTOR OF KLON LENGTH
! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .
! PQ         : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PQ         : SPECIFIC HUMIDITY OF WATER VAPOUR.
! - 1D (PROGNOSTIQUE) .
! - 1D (PROGNOSTIC QUANTITIES) .
! PLSM       : INDICE TERRE/MER.
! PLSM       : LAND/SEA MASK.
! PTS        : TEMPERATURE DE SURFACE.
! PTS        : SURFACE LAYER TEMPERATURE.
! PEMIS      : EMISSIVITE DE SURFACE COURANTE.
! PEMIS      : MODEL SURFACE LONGWAVE EMISSIVITY.
! PNEIJ      : PROPORTION DE SOL ENNEIGE.
! PNEIJ      : FRACTION OF SOIL COVERED BY SNOW.
! PQS        : HUMIDITE SPECIFIQUE DE SURFACE.
! PQS        : SPECIFIC HUMIDITY AT SURFACE LEVEL.
! PCHROV     : PCH RENORME EN DENSITE FOIS VITESSE.
! PDQSTS     : DERIVEE DE PQSATS PAR RAPPORT A LA TEMPERATURE.
!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
! -   OUTPUT ARGUMENTS.
!     --------------------
! PDERNSHF    : DERIVEE DES FLUX NON SOLAIRES PAR RAPPORT A LA TEMPERATURE

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        Coupled run CA2 of CERFACS (Laurent Terray)

!     Author.
!     -------
!        JF Royer

!     Modifications.
!     --------------
!        Original : 99-07-16
!        Modified : 01-12-07 P. Marquet - Modifications for Climat-V4
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMPHY   , ONLY : TPHY
USE YOMCST   , ONLY :  TCST  
USE YOMPHY1  , ONLY : TPHY1

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(TPHY)        ,INTENT(IN)    :: YDPHY
TYPE(TPHY1)       ,INTENT(IN)    :: YDPHY1
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEMIS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEIJ(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCHROV(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDQSTS(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDERNSHF(KLON) 

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZZDLS(KLON), ZZDLLW(KLON), ZZDLLS(KLON),ZZDRCN(KLON), ZZPN(KLON)

INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZCPVMD, ZCPVMS, ZCPVMW
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "fcttrm.func.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACDNSHF',0,ZHOOK_HANDLE)
ASSOCIATE(TMERGL=>YDPHY1%TMERGL, &
 & RCPD=>YDCST%RCPD, RCPV=>YDCST%RCPV, RCS=>YDCST%RCS, RCW=>YDCST%RCW, RSIGMA=>YDCST%RSIGMA, &
 & LNEIGE=>YDPHY%LNEIGE)
!     ------------------------------------------------------------------

!* ca2 PB 22/11/96 Calcul de dQ/dT

!  Constantes auxiliaires.

ZCPVMD=RCPV-RCPD
ZCPVMW=RCPV-RCW
ZCPVMS=RCPV-RCS

!  Calcul de la proportion de la surface evaporante en phase glace

DO JLEV=KTDIA,KLEV
  DO  JLON = KIDIA,KFDIA
    IF (LNEIGE) THEN
      ZZPN(JLON)=PLSM(JLON)*PNEIJ(JLON)+(1.0_JPRB-PLSM(JLON))&
       & * MAX(0.0_JPRB,SIGN(1.0_JPRB,TMERGL-PTS(JLON)))  
    ELSE
      ZZPN(JLON)=0.0_JPRB
    ENDIF
  ENDDO

!  Boucle de calcul des derivees des composantes du flux non solaire
!  Remarques :

!    - Coefficient d'echange ce = ch
!    - On considere dch/dT et drau/dT = 0

  DO  JLON=KIDIA,KFDIA

! Derivee du flux de chaleur sensible
    ZZDLS(JLON)=PCHROV(JLON)*&
     & (RCPD + ZCPVMD*PQS(JLON) +&
     & ZCPVMD*PDQSTS(JLON)*PTS(JLON))  

! Derivee du flux de chaleur latente sur eau
    ZZDLLW(JLON)= (1.0_JPRB-ZZPN(JLON))*PCHROV(JLON)*&
     & ((PQS(JLON)-PQ(JLON,KLEV))*ZCPVMW +&
     & FOLH(PTS(JLON),0.0_JPRB)*PDQSTS(JLON))  

! Derivee du flux de chaleur latente sur glace
    ZZDLLS(JLON)= PCHROV(JLON)*ZZPN(JLON)*&
     & ((PQS(JLON)-PQ(JLON,KLEV))*ZCPVMS +&
     & FOLH(PTS(JLON),1.0_JPRB)*PDQSTS(JLON))  

! Derivee du flux de rayonnement thermique
    ZZDRCN(JLON)=PEMIS(JLON)*4._JPRB*RSIGMA*&
     & PTS(JLON)*PTS(JLON)*PTS(JLON)  

! Derivee du flux non solaire total
    PDERNSHF(JLON) =&
     & -(ZZDLS(JLON)+ZZDLLW(JLON)+ZZDLLS(JLON)+ZZDRCN(JLON))  

  ENDDO
ENDDO

!     ------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACDNSHF',1,ZHOOK_HANDLE)
END SUBROUTINE ACDNSHF

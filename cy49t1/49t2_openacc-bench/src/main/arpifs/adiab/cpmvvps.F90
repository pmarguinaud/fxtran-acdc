!OCL  NOEVAL
SUBROUTINE CPMVVPS(YDCST, YDVAB,KLON,KIDIA,KFDIA,KFLEV,PDT,&
 !  Variables 2D Input
 & PFP,&
 !  Variables 1D Input
 & PRES, PFEVL, PFEVN,&
 !  Variables 2D In/Out
 & PEVEL, PSDVBC, PSPT1)

!     ------------------------------------------------------------------
!     MODIFICATION DE LA VITESSE VERTICALE ET DE LA PRESSION DE SURFACE
!     DANS LE CAS NDPSFI = 1.
!     ------------------------------------------------------------------

!      VOIR DOCUMENTATION, INTERFACE PHYSICO-DYNAMIQUE
!                            -----------------

!     ARGUMENTS D ENTREE.
!     ------------------.
!       KLON   : DIMENSION HORIZONTALE.
!       KIDIA  : DEBUT DE LA BOUCLE HORIZONTALE.
!       KFDIA : BORNE HORIZONTALE DES CALCULS.
!       KFLEV : DIMENSION ET BORNE VERTICALE.
!       PDT   : Delta t (SL2TL or first timestep) or 2 Delta t.

! --- INPUT 2D.
!     --------.
!       PFP : FLUX TOTAL DE PRECIPITATIONS LIQUIDES ET NEIGEUSES.

! --- INPUT 1D.
!     --------.
!       PRES : PRESSION DE SURFACE A L'INSTANT OU EST CALCULEE LA PHYSIQUE.
!              Surface pressure at the same instant as non lagged physics.
!       PFEVL, PFEVN ( IDEM) : FLUX D'EVAPORATION.

!     ARGUMENTS IMPLICITES
!     --------------------
!       CONSTANTES UNIVERSELLES = COMMON /YOMCST/: RG
!       DECOUPAGE VERTICAL      = YOMGEM: YRVAB%YRVAB%VBH

!     SORTIES
!     -------
!       PEVEL (KLON,0:KFLEV) : VITESSE VERTICALE GENERALISEE.
!       PSDVBC (IDEM) : Integral of divergence term, including
!                       the "lrubc" and "delta m=1" contributions
!                       of (etadot DP/DETA), but not the
!                       "delta m=1" physics.
!       PSPT1 (KLON) : surface pressure or log of pressure buffer.

!     AUTEUR : E.BAZILE  JUIN 93.
!     ------

!     INSPIRE DE CPATY ECRIT EN SON TEMPS PAR A.JOLY

!     Modifications:
!     --------------
!      K. Yessad (Dec 2008): remove dummy CDLOCK and useless dummy arg
!      K. Yessad (Jan 2011): remove useless calculations.
!      K. Yessad (Feb 2018): remove deep-layer formulations.
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!    ------------------------------------------------------------------

USE YOMVERT  , ONLY : TVAB
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST   , ONLY : TCST

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(TVAB)        ,INTENT(IN)    :: YDVAB
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDT 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFP(KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRES(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFEVL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFEVN(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVEL(KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSDVBC(KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSPT1(KLON) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZFE(KLON,0:KFLEV)
REAL(KIND=JPRB) :: ZEVELS(KLON)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CPMVVPS',0,ZHOOK_HANDLE)
ASSOCIATE(RG=>YDCST%RG)
!     ------------------------------------------------------------------

!     ------------------------------------------------------------------
!     1. - MODIFY :
!          * The divergence vertical integral term.
!          * etadot (d prehyd/d eta).
!     ------------------------------------------------------------------

! * ZFE : FLUX D EVAPORATION TOTAL
DO JLON = KIDIA, KFDIA
  ZFE(JLON,KFLEV)=PFEVL(JLON)+PFEVN(JLON)
  ZEVELS(JLON)=ZFE(JLON,KFLEV)+PFP(JLON,KFLEV)
ENDDO
ZFE(KIDIA:KFDIA,0:KFLEV-1)=0.0_JPRB

! * 1.1 Surface/bottom values:

DO JLON = KIDIA, KFDIA
  PEVEL(JLON,KFLEV) = PEVEL(JLON,KFLEV) + RG*ZEVELS(JLON)
  PSDVBC(JLON,KFLEV) = PSDVBC(JLON,KFLEV) + RG*ZEVELS(JLON)
ENDDO

! * 1.2 Other levels:

DO JLEV = 1, KFLEV-1
  DO JLON = KIDIA, KFDIA
    PEVEL(JLON,JLEV) = PEVEL(JLON,JLEV) + RG*( YDVAB%VBH(JLEV)* ZEVELS(JLON) )  
    PSDVBC(JLON,JLEV)= PSDVBC(JLON,JLEV) + RG*( YDVAB%VBH(JLEV)* ZEVELS(JLON) )
  ENDDO
ENDDO

!    ------------------------------------------------------------------
!     2. - ADD PHYSICS TO PSPT1.
!    ------------------------------------------------------------------

DO JLON = KIDIA, KFDIA
  PSPT1(JLON)=PSPT1(JLON)-PDT*RG*(PFP(JLON,KFLEV)+ZFE(JLON,KFLEV))/PRES(JLON)  
ENDDO

!     ------------------------------------------------------------------

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('CPMVVPS',1,ZHOOK_HANDLE)
END SUBROUTINE CPMVVPS

!----------------------------------------------------------------------
SUBROUTINE CPWTS (YDCST, YDCPG_OPTS, YDMCC,YDPHY,YDPHY1,KLON, KIDIA, KFDIA, KCSS, PDT,&
 !----------------------------------------------------------------------
 ! - INPUT 1D
 & PTDTS, PTDTP, PTDWS, PTDWSI,&
 & PTDWP, PTDWPI, PTDWL, PTDSNS,&
 & PTDALBNS, PTDRHONS,&
 & PTPCLI, PWPCLI, PLSM, PIVEG,&
 ! - IN/OUT 1D
 & PTS1, PTP1, PWS1, PWSI1, PWP1, PWPI1, PWL1, PSNS1,&
 & PALBNS1, PRHONS1 )  

!$ACDC singlecolumn 


!DEC$ NOOPTIMIZE

!****
!     ------------------------------------------------------------------

!     EVOLUTION DES VARIABLES SOLS
!     ------------------------------------------------------------------

!     ARGUMENTS D ENTREE
!     ------------------
!       KLON   : DIMENSION HORIZONTALE
!       KIDIA  : BORNE INITIALE HORIZONTALE DES CALCULS
!       KFDIA  : BORNE FINALE HORIZONTALE DES CALCULS
!       KCSS   : NBRE DE COUCHES DANS LE SOL PROFOND
!       PDT    : PAS DE TEMPS EFFECTIF

! --- INPUT 1D
!     --------
!       PTDTS   (KLON): TENDANCE PHYSIQUE - TEMPERATURE DE SURFACE
!       PTDTP(KLON,KCSS):  "     "        - TEMPERATURE PROFONDE
!       PTDWS   (KLON):    "     "        - RESERVOIR D'EAU SUPERFICIEL
!       PTDWSI  (KLON):    "     "        - RESERVOIR D'EAU SUPERFICIEL GELEE
!                                       SI LSOLV ET LFGELS
!       PTDWP   (KLON):    "     "        - RESERVOIR D'EAU PROFOND
!       PTDWPI  (KLON):    "     "        - RESERVOIR D'EAU PROFOND GELEE
!                                       SI LSOLV ET LFGEL
!       PTDWL   (KLON):    "     "        - RESERVOIR D'EAU D'INTERCEPTION
!                                       SI LSOLV
!       PTDSNS  (KLON):    "     "        - RESERVOIR DE NEIGE
!                                       SI LNEIGE
!       PTDALBNS(KLON):    "     "        - ALBEDO DE LA NEIGE
!       PTDRHONS(KLON):    "     "        - DENSITE DE LA NEIGE
!                                       SI LSNV (ET LNEIGE)
!       PTPCLI  (KLON): TEMPERATURE CLIMATOLOGIQUE DU SOL PROFOND
!       PWPCLI  (KLON): RESERVOIR EAU CLIMATOLOGIQUE DU SOL PROFOND
!       PLSM    (KLON): INDICATEUR TERRE (1.)/ MER (0.)
!       PIVEG   (KLON): INDICE DE VEGETATION DOMINANTE (LSOLV)

!     ARGUMENTS IMPLICITES
!     --------------------
!       INDICATEURS LOGIQUES    = COMMON /YOMPHY /: LSOLV,LNEIGE,LSNV,LFGEL,
!                                                   LFGELS
!       CONSTANTES UNIVERSELLES = COMMON /YOMCST /: RTT
!       PROPRIETES DU SOL       = COMMON /YOMPHY1/: OMTPRO,OMWPRO,TMERGL
!       CLIMAT                  = COMMON /YOMMCC/ : LMCCO2
!       ACTION ON FIELDS        = YSD_VP%Y(field)%LSET

!     SORTIES
!     -------
!       PTS1, PTP1, PWS1, PWSI1, PWP1, PWPI1, PWL1, PSNS1, PALBNS1, PRHONS1:
!       CHAMPS DE SURFACE A FAIRE EVOLUER

!     IMPROVISE PAR
!     ------------- ERIC BAZILE 92/11 INSPIRE DE CPSP

! --- MODIFICATIONS.
!     --------------
!      R. El Khatib : 01-08-07 Pruning options
!      Modified 02-09-03 by E. Bazile    Schema de neige LVGSN.
!      Modified 06-09-10 by A. Alias     relaxation reservoir profond (Michel Deque)
!      R. El Khatib : 09-Mar-2012 Cleaning for the sake of bounds checking controls
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!     ------------------------------------------------------------------


USE PARKIND1         , ONLY : JPIM, JPRB
USE YOMHOOK          , ONLY : DR_HOOK, JPHOOK, LHOOK
USE CPG_OPTS_TYPE_MOD, ONLY : CPG_OPTS_TYPE

USE YOMCST           , ONLY : TCST
USE YOMPHY           , ONLY : TPHY
USE YOMPHY1          , ONLY : TPHY1
USE YOMMCC           , ONLY : TMCC

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)          ,INTENT(IN)     :: YDCST
TYPE(CPG_OPTS_TYPE) ,INTENT(IN)     :: YDCPG_OPTS
TYPE(TMCC)          ,INTENT(IN)     :: YDMCC
TYPE(TPHY)          ,INTENT(IN)     :: YDPHY
TYPE(TPHY1)         ,INTENT(IN)     :: YDPHY1
INTEGER(KIND=JPIM)  ,INTENT(IN)     :: KLON 
INTEGER(KIND=JPIM)  ,INTENT(IN)     :: KIDIA 
INTEGER(KIND=JPIM)  ,INTENT(IN)     :: KFDIA 
INTEGER(KIND=JPIM)  ,INTENT(IN)     :: KCSS 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PDT 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDTS(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDTP(KLON,KCSS) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDWS(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDWSI(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDWP(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDWPI(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDWL(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDSNS(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDALBNS(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTDRHONS(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PTPCLI(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PWPCLI(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PLSM(KLON) 
REAL(KIND=JPRB)     ,INTENT(IN)     :: PIVEG(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PTS1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PTP1(KLON,KCSS) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PWS1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PWSI1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PWP1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PWPI1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PWL1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PSNS1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PALBNS1(KLON) 
REAL(KIND=JPRB)     ,INTENT(INOUT)  :: PRHONS1(KLON) 
!     ------------------------------------------------------------------
INTEGER(KIND=JPIM) :: ITVGLAM, JCSS, JLON

REAL(KIND=JPRB) :: ZEPSN
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CPWTS',0,ZHOOK_HANDLE)
ASSOCIATE(NTVGLA=>YDPHY1%NTVGLA, OMTPRO=>YDPHY1%OMTPRO, OMWPRO=>YDPHY1%OMWPRO, &
 & ALBMIN=>YDPHY1%ALBMIN, RHOMIN=>YDPHY1%RHOMIN, RHOMAX=>YDPHY1%RHOMAX, &
 & ALBMAX=>YDPHY1%ALBMAX, &
 & RTT=>YDCST%RTT, YSD_VP=>YDCPG_OPTS%YRSURF_OPTS%YSD_VP, &
 & LSOLV=>YDPHY%LSOLV, LVGSN=>YDPHY%LVGSN, LNEIGE=>YDPHY%LNEIGE, &
 & LSNV=>YDPHY%LSNV, LFGEL=>YDPHY%LFGEL, &
 & LMCC02=>YDMCC%LMCC02)
!*    ------------------------------------------------------------------

!     1.- CALCUL DES EVOLUTIONS

ZEPSN=1.E-3_JPRB
DO JCSS = 1,KCSS
  DO JLON = KIDIA,KFDIA

!        TEMPERATURE PROFONDE
!        --------------------
    PTP1(JLON,JCSS) = PTP1(JLON,JCSS) + PDT * PTDTP(JLON,JCSS)
  ENDDO
ENDDO

!DEC$ IVDEP
DO JLON = KIDIA,KFDIA

!        TEMPERATURE DE SURFACE
!        ----------------------
  PTS1(JLON) = PTS1(JLON) + PDT * PTDTS(JLON)

!        TEMPERATURE PROFONDE
!        --------------------
  IF (YSD_VP%YTPC%LSET) THEN
    PTP1(JLON,KCSS) = PTP1(JLON,KCSS) +&
     & PDT * PLSM(JLON) * OMTPRO * (PTPCLI(JLON)-PTP1(JLON,KCSS))  
  ENDIF

!        RESERVOIR SUPERFICIEL
!        ---------------------
  PWS1(JLON) = PWS1(JLON) + PDT * PTDWS(JLON)
  PWS1(JLON) = MAX(0.0_JPRB,PWS1(JLON))

ENDDO

IF (LSOLV) THEN
!DEC$ IVDEP
  DO JLON = KIDIA,KFDIA

!        RESERVOIR D'INTERCEPTION
!        ------------------------
    PWL1(JLON) = PWL1(JLON) + PDT * PTDWL(JLON)
    PWL1(JLON) = MAX(0.0_JPRB,PWL1(JLON))

!        RESERVOIR PROFOND
!        -----------------
    PWP1(JLON) = PWP1(JLON) + PDT * PTDWP(JLON)
    IF(YSD_VP%YWPC%LSET) THEN
      PWP1(JLON) = PWP1(JLON) +&
       &PDT * PLSM(JLON) * OMWPRO * (PWPCLI(JLON)-PWP1(JLON))
    ENDIF
    PWP1(JLON) = MAX(0.0_JPRB,PWP1(JLON))

!        RESERVOIR PROFOND GELE
!        ----------------------
    IF (LFGEL) THEN
      PWPI1(JLON) = PWPI1(JLON) + PDT * PTDWPI(JLON)
      PWPI1(JLON) = MAX(0.0_JPRB,PWPI1(JLON))
      PWSI1(JLON) = PWSI1(JLON) + PDT * PTDWSI(JLON)
      PWSI1(JLON) = MAX(0.0_JPRB,PWSI1(JLON))
    ENDIF
  ENDDO
ELSE
!DEC$ IVDEP
  DO JLON = KIDIA,KFDIA

!        RESERVOIR PROFOND
!        -----------------
    PWP1(JLON) = PWP1(JLON) + PDT * PTDWP(JLON)
    IF (YSD_VP%YWPC%LSET) THEN
      PWP1(JLON) = PWP1(JLON) +&
       & PDT * PLSM(JLON) * OMWPRO * (PWPCLI(JLON)-PWP1(JLON))  
    ENDIF
  ENDDO
ENDIF

!        VARIABLES NEIGE EVENTUELLES
!        ---------------------------
IF ( LNEIGE ) THEN
!DEC$ IVDEP
  DO JLON = KIDIA,KFDIA
    PSNS1(JLON) = PSNS1(JLON) + PDT * PTDSNS(JLON)
    IF (LSOLV) THEN
      IF (NINT(PIVEG(JLON)) == NTVGLA) THEN
        PSNS1(JLON) = MAX(ZEPSN,PSNS1(JLON))
      ENDIF
    ENDIF
    PSNS1(JLON) = MAX(0.0_JPRB,PSNS1(JLON))
    IF (LSNV.OR.LVGSN) THEN
      IF (PSNS1(JLON) > 0.0_JPRB) THEN
        PALBNS1(JLON) = PALBNS1(JLON)+PTDALBNS(JLON)*PDT
        PALBNS1(JLON) = MIN(MAX(PALBNS1(JLON),ALBMIN),ALBMAX)
      ELSE
        PALBNS1(JLON) = ALBMAX
      ENDIF
      IF (PSNS1(JLON) > 0) THEN
        PRHONS1(JLON) = PRHONS1(JLON)+PTDRHONS(JLON)*PDT
        PRHONS1(JLON) = MIN(MAX(PRHONS1(JLON),RHOMIN),RHOMAX)
      ELSE
        PRHONS1(JLON) = RHOMIN
      ENDIF
    ENDIF
  ENDDO
ENDIF

!      PSEUDO FONTE BANQUISE

IF (LMCC02.AND. LSOLV) THEN
  ITVGLAM=10*NTVGLA+1
  DO JCSS = 1,KCSS
    DO JLON = KIDIA,KFDIA
      IF (NINT(10._JPRB*PIVEG(JLON)) == ITVGLAM) THEN
!               ON SEA-ICE THE TEMPERATURE CANNOT EXCEED 273.16 K
!               OTHERWISE THE ICE MELTS TO MAINTAIN IT
        PTP1(JLON,JCSS) = MIN(PTP1(JLON,JCSS),RTT)
      ENDIF
    ENDDO
  ENDDO
  DO JLON = KIDIA,KFDIA
    IF (NINT(10._JPRB*PIVEG(JLON)) == ITVGLAM) THEN
!               ON SEA-ICE THE TEMPERATURE CANNOT EXCEED 273.16 K
!               OTHERWISE THE ICE MELTS TO MAINTAIN IT
      PTS1(JLON) = MIN(PTS1(JLON),RTT)
    ENDIF
  ENDDO
ENDIF

!     ------------------------------------------------------------------

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('CPWTS',1,ZHOOK_HANDLE)
END SUBROUTINE CPWTS


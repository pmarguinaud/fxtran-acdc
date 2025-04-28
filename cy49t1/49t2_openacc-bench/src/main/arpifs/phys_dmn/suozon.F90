!OPTIONS XOPT(NOEVAL)
SUBROUTINE SUOZON ( KIDIA,KFDIA,KLON,KLEV,PROFO3,LDQINT,PRESI,PRDELP,LD_LO3ABC,PVO3A,PVO3B,PVO3C)

!     ROUTINE DE CALCUL ACTIF .
!**** *SUOZON* INITIALISATION DE PROFILS VERTICAUX D'OZONE

!     But.   INITIALISATION DE UN OU PLUSIEURS PROFILS VERTICAUX D'OZONE
!     ----   A PARTIR D'UN PROFIL CLIMATOLOGIQUE,
!           SOIT EN QUANTITE INTEGREE DOUBLE AU DESSUS D'UNE COUCHE (LDQINT=.TRU
!           SOIT EN RAPPORT DE MELANGE (LDQINT=.FALSE.)

!**   Interface.
!     ----------
!        *CALL* *SUOZON ( KIDIA,KFDIA,KLON,KLEV,
!    S                    PROFO3,LDQINT,PRESI,PRDELP)

!        Arguments explicites :
!        ----------------------

!       KIDIA    : DEBUT DES BOUCLES HORIZONTALES
!                  BEGINNING OF HORIZONTAL LOOPS
!       KFDIA    : FIN DES BOUCLES HORIZONTALES
!                  END OF HORIZONTAL LOOPS
!       KLON      : DIMENSION HORIZONTALE                       (input)
!       KLEV      : DIMENSION VERTICALE                         (input)
!       PROFO3   : PROFIL VERTICAL D'OZONE
!              (0): VALEUR AU-DESSUS DU MODELE                  (output)
!       LDQINT     : INDICATEUR DU TYPE DE PROFIL
!                   .TRUE.  QUANTITE TOTALE AU DESSUS D'UNE COUCHE x2
!                   .FALSE. RAPPORT DE MELANGE MASSIQUE         (input)
!       PRESI    : PRESSION DE L'INTER-COUCHE                  (input)
!       PRDELP    : INVERSE DE L'EPAISSEUR EN PRESSION
!                   DE LA COUCHE                                (input)
!       LO3ABC    : Switch to use climatological profile        (input)
!       PVO3ABC   : Climatological coef                         (input)

!        Arguments implicites :
!        ----------------------
!       Les points de grille du modele.

!     Methode.
!     --------
!         L'ozone est une fonction de la pression P

!        |0                a
!        | qO3 dp =  --------------
!        |P           1 + (b/P)**3/2

!           a = ZQO31
!           b = ZQO32

!     On peut donc approximer le rapport de melange d'ozone

!                        1    |Pj
!              QO3 =  ------- |   qO3 dp
!                      Pi-Pj  |Pi

!     Reference.
!    -----------
!         AUCUNE

!     Auteur.
!    --------
!         A. Lasserre-Bigorry

!     Modifications :
!    ----------------
!         Original : 91-07-22
!         M. Deque 91-09-25
!         Y. Bouteloup 02-03-30  : Use of climatological profiles
!        M.Hamrud      01-Oct-2003 CY28 Cleaning

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PROFO3(KLON,0:KLEV) 
LOGICAL           ,INTENT(IN)    :: LDQINT 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRESI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP(KLON,KLEV) 
LOGICAL           ,INTENT(IN)    :: LD_LO3ABC 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVO3A(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVO3B(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVO3C(KLON) 
INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZEPSO, ZP, ZQO3A, ZQO3B, ZQO3C, ZQO31, ZQO32
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!*
!     ------------------------------------------------------------------
!     1. CALCULS PRELIMINAIRES ET DIVERSES INITIALISATIONS

IF (LHOOK) CALL DR_HOOK('SUOZON',0,ZHOOK_HANDLE)
ZEPSO=1.E-03_JPRB
ZQO31=0.6012E-01_JPRB
ZQO32=0.3166E+04_JPRB
ZQO32=SQRT(ZQO32*ZQO32*ZQO32)

!*
!     ------------------------------------------------------------------
!     2.  INITIALISATION DE L'OZONE

DO JLEV=0,KLEV
  IF (LD_LO3ABC) THEN
    DO JLON=KIDIA,KFDIA
      ZQO3A=PVO3A(JLON)
      ZQO3B=PVO3B(JLON)
      ZQO3C=PVO3C(JLON)/2._JPRB
      ZP=MAX(ZEPSO,PRESI(JLON,JLEV))
      PROFO3(JLON,JLEV)=(2.0_JPRB*ZQO3A)/( 1.0_JPRB + EXP(ZQO3C*LOG(ZQO3B/ZP)) )
    ENDDO
  ELSE
    DO JLON=KIDIA,KFDIA
      ZP=MAX(ZEPSO,PRESI(JLON,JLEV))
      PROFO3(JLON,JLEV)=(2.0_JPRB*ZQO31)/( 1.0_JPRB + ZQO32/SQRT(ZP*ZP*ZP) )
    ENDDO
  ENDIF  
ENDDO
IF (.NOT.LDQINT) THEN
  DO JLEV=KLEV,1,-1
    DO JLON=KIDIA,KFDIA
      PROFO3(JLON,JLEV)=(PROFO3(JLON,JLEV)-PROFO3(JLON,JLEV-1))&
       & *PRDELP(JLON,JLEV)*0.5_JPRB  
    ENDDO
  ENDDO
  DO JLON=KIDIA,KFDIA
    PROFO3(JLON,0)=PROFO3(JLON,0)/MAX(ZEPSO,PRESI(JLON,0))*0.5_JPRB
  ENDDO
ENDIF

IF (LHOOK) CALL DR_HOOK('SUOZON',1,ZHOOK_HANDLE)
END SUBROUTINE SUOZON

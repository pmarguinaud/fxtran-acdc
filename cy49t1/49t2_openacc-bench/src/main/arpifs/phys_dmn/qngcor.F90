SUBROUTINE QNGCOR ( YDCST, YDPHY2,KIDIA, KFDIA, KLON, KTDIA, KLEV,&
 & PQ, PQL, PQI, PRDELP,&
 & PDIFCQ , PDIFCQI, PDIFCQL, PDIFTQ , PDIFTQI, PDIFTQL,&
 & PFESL  , PFESN  , PFECL  , PFECN  ,&
 & PFASL  , PFASN  , PFACL  , PFACN  ,&
 & PFCCQL , PFCCQN , PFCSQL , PFCSQN , PFCQNG )  

!--------------------------------------------------------------------
!     CORRECTING FOR NEGATIVE SPECIFIC HUMIDITY
!--------------------------------------------------------------------

!     INPUT ARGUMENTS
!     ---------------

! - PHYSIC DIMENSIONING PARAMETERS.

! KIDIA      : START OF HORIZONTAL LOOP 
! KFDIA      : END OF HORIZONTAL LOOP
! KLON       : HORIZONTAL DIMENSION
! KTDIA      : START OF THE VERTICAL LOOP IN MICROPHYSICAL ADJUSTMENT
! KLEV       : END OF VERTICAL LOOP AND VERTICAL DIMENSION

! --- INPUT 2D (1:KLEV)
!     -----------------

! PQ         : SPECIFIC HUMIDITY OF WATER VAPOUR.
! PQL        : SPECIFIC HUMIDITY OF LIQUID CONDENSATED WATER.
! PQI        : SPECIFIC HUMIDITY OF SOLID CONDENSATED WATER.
! PRDELP     : INVERSE OF LAYER THICKNESS IN PRESSURE UNITS.

! --- INPUT 2D (0:KLEV)
!     -----------------

! PDIFCQ     : convective flux of specific humidity (not rain/snow).
! PDIFCQI    : convective flux of solid water (not rain/snow).
! PDIFCQL    : convective flux of liquid water (not rain/snow).
! PDIFTQ     : turbulent flux (inc. "q" negative) of specific humidity.
! PDIFTQI    : turbulent flux (inc. "q" negative) of solid water.
! PDIFTQL    : turbulent flux (inc. "q" negative) of liquid water.
! PFESL      : resolved precipitation flux due to evaporation.
! PFESN      : resolved precipitation flux due to sublimation.
! PFECL      : convective precipitation flux due to evaporation.
! PFECN      : convective precipitation flux due to sublimation.
! PFASL      : flux of liquid resol. precipitation: the generation term.
! PFASN      : flux of solid resolved precipitation: the generation term.
! PFACL      : flux of liquid conv. precipitation: the generation term.
! PFACN      : flux of solid conv. precipitation: the generation term.
! PFCCQL     : convective condensation flux for liquid water.
! PFCCQN     : convective condensation flux for ice.

!     OUTPUT ARGUMENTS
!     ----------------

! --- INPUT/OUTPUT 2D (0:KLEV)
!     ------------------------

! PFCSQL     : stratiform condensation flux for liquid water.
! PFCSQN     : stratiform condensation flux for ice.
! PFCQNG     : pseudo-flux of water to correct for Q<0.

! --- AUTEUR.
!     ------.
!     04/05/99 Philippe LOPEZ.

!     Modifications.
!     --------------
!        M.Hamrud      01-Oct-2003 CY28 Cleaning

!     2004-01-28, P. Marquet : change sign of the five
!               input flux PFPEVP in ADVPRC => the sign
!               is changed in the ZTENDQ equation.  
!               Also Complete the name description of all 
!               input/output variables
!     2004-10-06, F. Bouyssel : cleaning
!     2005-06-20, F. Bouyssel : correction of negative ql, qi
!     2006-01-25, F. Bouyssel : introduction of PFPEVPL, PFPEVPN
!     2008-10-01, F. Bouyssel : new fluxes and adaptation to cptend_new
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.

!---------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMPHY2   , ONLY : TPHY2
USE YOMCST    , ONLY : TCST 

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(TPHY2)       ,INTENT(IN)    :: YDPHY2
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ     (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL    (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI    (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDIFCQ (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDIFCQI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDIFCQL(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDIFTQ (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDIFTQI(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDIFTQL(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFESL  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFESN  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFECL  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFECN  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFASL  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFASN  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFACL  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFACN  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFCCQL (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFCCQN (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCSQL (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCSQN (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCQNG (KLON,0:KLEV) 

REAL(KIND=JPRB) :: ZGSDPDT(KLON,KLEV),ZFCQNG(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZQL(KLON,KLEV),ZQI(KLON,KLEV)
REAL(KIND=JPRB) :: ZQLCORP(KLON),ZQICORP(KLON)
REAL(KIND=JPRB) :: ZQ,ZQCORP
INTEGER(KIND=JPIM) :: JLON,JLEV
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!  ------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('QNGCOR',0,ZHOOK_HANDLE)
ASSOCIATE(TSPHY=>YDPHY2%TSPHY, RG=>YDCST%RG)
DO JLON = KIDIA, KFDIA
  ZQLCORP(JLON) = 0.0_JPRB
  ZQICORP(JLON) = 0.0_JPRB
ENDDO   

DO JLEV = 0, KLEV
  DO JLON = KIDIA, KFDIA
    ZFCQNG(JLON,JLEV) = 0.0_JPRB
  ENDDO   
ENDDO   

DO JLEV = 1, KLEV
  DO JLON = KIDIA, KFDIA
    ZGSDPDT(JLON,JLEV) = RG * PRDELP(JLON,JLEV) * TSPHY
  ENDDO   
ENDDO   

DO JLEV=1,KLEV
  DO JLON=KIDIA,KFDIA
    ZQL(JLON,JLEV) = PQL(JLON,JLEV) + ZGSDPDT(JLON,JLEV) * (&
     & (PDIFTQL(JLON,JLEV-1) - PDIFTQL(JLON,JLEV)) +&
     & (PDIFCQL(JLON,JLEV-1) - PDIFCQL(JLON,JLEV)) +&
     & (PFACL  (JLON,JLEV-1) - PFACL  (JLON,JLEV)) +&
     & (PFASL  (JLON,JLEV-1) - PFASL  (JLON,JLEV)) -&
     & (PFCSQL (JLON,JLEV-1) - PFCSQL (JLON,JLEV)) -&
     & (PFCCQL (JLON,JLEV-1) - PFCCQL (JLON,JLEV)))
    ZQI(JLON,JLEV) = PQI(JLON,JLEV) + ZGSDPDT(JLON,JLEV) * (&
     & (PDIFTQI(JLON,JLEV-1) - PDIFTQI(JLON,JLEV)) +&
     & (PDIFCQI(JLON,JLEV-1) - PDIFCQI(JLON,JLEV)) +&
     & (PFACN  (JLON,JLEV-1) - PFACN  (JLON,JLEV)) +&
     & (PFASN  (JLON,JLEV-1) - PFASN  (JLON,JLEV)) -&
     & (PFCSQN (JLON,JLEV-1) - PFCSQN (JLON,JLEV)) -&
     & (PFCCQN (JLON,JLEV-1) - PFCCQN (JLON,JLEV)))
  ENDDO
ENDDO

DO JLEV=1,KTDIA-1
  DO JLON=KIDIA,KFDIA
    ZQLCORP(JLON) = ZQLCORP(JLON) - ZQL(JLON,JLEV)/ZGSDPDT(JLON,JLEV)
    ZQICORP(JLON) = ZQICORP(JLON) - ZQI(JLON,JLEV)/ZGSDPDT(JLON,JLEV)
    PFCSQL(JLON,JLEV) = PFCSQL(JLON,JLEV) + ZQLCORP(JLON)
    PFCSQN(JLON,JLEV) = PFCSQN(JLON,JLEV) + ZQICORP(JLON)
  ENDDO
ENDDO

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZQLCORP(JLON) = ZQLCORP(JLON)&
     & - MIN(0.0_JPRB,ZQL(JLON,JLEV))/ZGSDPDT(JLON,JLEV)
    ZQICORP(JLON) = ZQICORP(JLON)&
     & - MIN(0.0_JPRB,ZQI(JLON,JLEV))/ZGSDPDT(JLON,JLEV)
    PFCSQL(JLON,JLEV) = PFCSQL(JLON,JLEV) + ZQLCORP(JLON)
    PFCSQN(JLON,JLEV) = PFCSQN(JLON,JLEV) + ZQICORP(JLON)
  ENDDO
ENDDO

! -------------------
! CORRECT NEGATIVE QV
! -------------------

DO JLEV=1,KLEV
  DO JLON=KIDIA,KFDIA
    ZQ = PQ(JLON,JLEV) + ZGSDPDT(JLON,JLEV) * (&
     & (PDIFTQ (JLON,JLEV-1) - PDIFTQ (JLON,JLEV)) +&
     & (PDIFCQ (JLON,JLEV-1) - PDIFCQ (JLON,JLEV)) +&
     & (PFCCQL (JLON,JLEV-1) - PFCCQL (JLON,JLEV)) +&
     & (PFCCQN (JLON,JLEV-1) - PFCCQN (JLON,JLEV)) +&
     & (PFCSQL (JLON,JLEV-1) - PFCSQL (JLON,JLEV)) +&
     & (PFCSQN (JLON,JLEV-1) - PFCSQN (JLON,JLEV)) -&
     & (PFECL  (JLON,JLEV-1) - PFECL  (JLON,JLEV)) -&
     & (PFESL  (JLON,JLEV-1) - PFESL  (JLON,JLEV)) -&
     & (PFECN  (JLON,JLEV-1) - PFECN  (JLON,JLEV)) -&
     & (PFESN  (JLON,JLEV-1) - PFESN  (JLON,JLEV)))
    ZQCORP = ZQ + ZFCQNG(JLON,JLEV-1)*ZGSDPDT(JLON,JLEV)
    ZFCQNG(JLON,JLEV) = MIN(0.0_JPRB,ZQCORP)/ZGSDPDT(JLON,JLEV)
  ENDDO
ENDDO

DO JLEV=0,KLEV
  DO JLON=KIDIA,KFDIA
    PFCQNG(JLON,JLEV) = PFCQNG(JLON,JLEV) + ZFCQNG(JLON,JLEV)
  ENDDO   
ENDDO   

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('QNGCOR',1,ZHOOK_HANDLE)
END SUBROUTINE QNGCOR

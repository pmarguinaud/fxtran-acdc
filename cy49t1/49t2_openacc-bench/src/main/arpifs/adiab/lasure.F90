SUBROUTINE LASURE(&
 ! ----- INPUT ---------------------------------------------------------------
 & YDGEOMETRY,YDEPHY,YDDYN,YDEDYN,YDPHY,KST,KEND,PMAPPA,PGM,PBT,&
 ! ----- OUTPUT --------------------------------------------------------------
 & PBDT,PREDIV)  

!**** *LASURE*   Semi-Lagrangian scheme.
!                Set-up for other subroutines called by LACDYN.

!     Purpose.
!     --------
!       Computes some intermediate quantities necessary in the other
!       subroutines called by LACDYN.

!**   Interface.
!     ----------
!        *CALL* *LASURE(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KSTART  : first element of work.
!          KPROF   : depth of work.
!          PBETADT : BETADT or 0 according to configuration.
!          PDT     : time step for the first time-integration step of
!                    a leap-frog scheme or all time-integration steps of
!                    a two-time level scheme; 2*time step for the following
!                    time-integration steps of a leap-frog scheme.
!          KIBL    : index into YDGSGEOM in YDGEOMETRY

!        OUTPUT:
!          PDTS2   : 0.5*PDT.
!          PBT     : PDTS2*PBETADT.
!          LD2TLFF1: .T./.F.: Refined treatement of (2*Omega Vec r) at
!                    the origin point when there is t-dt (or t in SL2TL)
!                    physics / Other cases.
!          PBDT    : PBT or PBT*(c**2/GM**2) according to LSIDG.
!          PREDIV  : 1. or c**2/GM**2 according to LSIDG.
!          PESGP   : (1 + uncentering factor).
!          PESGM   : (1 - uncentering factor).

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation about semi-lagrangian scheme.

!     Externals.
!     ----------
!           none. 
!           Called by LACDYN, LACDYNTL and LACDYNAD.

!     Reference.
!     ----------
!             Arpege documentation about semi-lagrangian scheme.

!     Author.
!     -------
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after old part one of LACDYN. 
!      Loops are rewritten according to F90 norms.
!      Original : JULY 1995.

!     Modifications.
!     --------------
!      J.Vivoda 03-2002 PC schemes for NH dynamics (LPC_XXXX keys)
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!      K. Yessad (Dec 2011): use YDGSGEOM.
!      T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOEPHY       , ONLY : TEPHY
USE YOMPHY       , ONLY : TPHY
USE YEMDYN       , ONLY : TEDYN
USE YOMDYN       , ONLY : TDYN

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(TEPHY)       ,INTENT(IN)    :: YDEPHY
TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
TYPE(TEDYN)       ,INTENT(IN)    :: YDEDYN
TYPE(TPHY)        ,INTENT(IN)    :: YDPHY
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMAPPA(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGM(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBT 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBDT(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PREDIV(YDGEOMETRY%YRDIM%NPROMA) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) ::  JROF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------


IF (LHOOK) CALL DR_HOOK('LASURE',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDGEM=>YDGEOMETRY%YRGEM, YDMP=>YDGEOMETRY%YRMP)
ASSOCIATE(NPROMA=>YDDIM%NPROMA, L2TLFF=>YDDYN%L2TLFF, LSIDG=>YDDYN%LSIDG, VESL=>YDDYN%VESL, LESIDG=>YDEDYN%LESIDG, &
& LAGPHY=>YDEPHY%LAGPHY, LEPHYS=>YDEPHY%LEPHYS, RSTRET=>YDGEM%RSTRET, LMPHYS=>YDPHY%LMPHYS)
!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS:
!              ----------------------------

!     * Time step.

IF (LSIDG) THEN
  DO JROF=KST,KEND
    PBDT(JROF)  = PBT
    PREDIV(JROF)= 1.0_JPRB
  ENDDO
ELSEIF (LESIDG) THEN
  DO JROF=KST,KEND
    PBDT(JROF)  = PBT*PMAPPA(JROF)/(PGM(JROF)*PGM(JROF))
    PREDIV(JROF)= PMAPPA(JROF)/(PGM(JROF)*PGM(JROF))
  ENDDO
ELSE
  DO JROF=KST,KEND
    PBDT(JROF)  = PBT*RSTRET*RSTRET/(PGM(JROF)*PGM(JROF))
    PREDIV(JROF)= RSTRET*RSTRET/(PGM(JROF)*PGM(JROF))
  ENDDO
ENDIF

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('LASURE',1,ZHOOK_HANDLE)
END SUBROUTINE LASURE


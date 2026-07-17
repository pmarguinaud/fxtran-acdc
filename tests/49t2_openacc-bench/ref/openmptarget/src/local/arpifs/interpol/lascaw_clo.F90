SUBROUTINE LASCAW_CLO(YDDYNA,KFLEV,KPROMA,KST,KEND,LDT,PSLHDKMIN,PDLO,PDLOMAD,PKAPPA,&
 & PCLO,PCLOMAD,PCLOSLD)

!$ACDC singlecolumn  --process-pointers


!     ------------------------------------------------------------------

!**** *LASCAW_CLO  -  Weights for semi-LAgrangian interpolator:
!                     Computes PCLO, PCLOMAD and PCLOSLD for one layer
!                     (high-order meridian weights)

!      Can be also called for zonal high-order weights if plane geometry
!       (no need to code a specific ELASCAW_CLA).

!     Purpose.
!     --------

!**   Interface.
!     ----------
!        *CALL* *LASCAW_CLO( ... )

!        Explicit arguments :
!        --------------------

!        * INPUT:
!        KFLEV    - Vertical dimension
!        KPROMA   - horizontal dimension.
!        KST      - first element of arrays where computations are performed.
!        KEND    - depth of work.
!        LDT      - keys for SLHD and horizontal turbulence.
!        PSLHDKMIN - either HOISLH or SLHDKMIN
!        PDLO     - distances for horizontal linear interpolations in longitude.
!        PDLOMAD  -  PDLO for COMAD
!        PKAPPA   - kappa function ("coefficient of SLHD").

!        * OUTPUT:
!        PCLO     - weights for horizontal cubic interpolations in longitude.
!        PCLOMAD  - cf. PCLO, COMAD case.
!        PCLOSLD  - cf. PCLO, SLHD case.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation about semi-Lagrangian scheme.

!     Externals.
!     ----------

!        No external.
!        Called by some (E)LASCAW.. routines.

!     Reference.
!     ----------

!     Author.
!     -------
!        K. YESSAD, after former LASCAW code (JAN 2009).
!        METEO-FRANCE, CNRM/GMAP.

!     Modifications.
!     --------------
!     F. Vana 22-Feb-2011: Horiz. turbulence and diff on phys. tendencies
!     S. Malardel (Nov 2013): COMAD weights for SL interpolations
!     F. Vana 13-feb-2014 SLHD weights for heat variables
!     K. Yessad (March 2017): simplify level numbering.
!     F. Vana    21-Nov-2017: Option LHOISLT
!     H Petithomme (Dec 2020): optimization, option 3DTURB moved out
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK, JPHOOK 

! arp/ifs dependencies to be solved later.
USE YOMDYNA  , ONLY : TDYNA

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TDYNA)       , INTENT(IN)  :: YDDYNA
INTEGER(KIND=JPIM), INTENT(IN)  :: KFLEV
INTEGER(KIND=JPIM), INTENT(IN)  :: KPROMA
INTEGER(KIND=JPIM), INTENT(IN)  :: KST
INTEGER(KIND=JPIM), INTENT(IN)  :: KEND
LOGICAL           , INTENT(IN)  :: LDT(4)
REAL(KIND=JPRB)   , INTENT(IN)  :: PSLHDKMIN
REAL(KIND=JPRB)   , INTENT(IN)  :: PDLO(KPROMA,KFLEV)
REAL(KIND=JPRB)   , INTENT(IN)  :: PDLOMAD(KPROMA,KFLEV)
REAL(KIND=JPRB)   , INTENT(IN)  :: PKAPPA(KPROMA,KFLEV)
REAL(KIND=JPRB)   , INTENT(OUT) :: PCLO(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   , INTENT(OUT) :: PCLOMAD(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   , INTENT(OUT) :: PCLOSLD(KPROMA,KFLEV,3)

!     ------------------------------------------------------------------

#include "lascaw_clo.func.h"

INTEGER(KIND=JPIM) :: JROF,JLEV
REAL(KIND=JPRB) :: ZWH1,ZWH2,ZWH3,ZWD1,ZWD2,ZWD3,ZWDS1,ZWDS2,ZWDS3,ZWL1,ZWL2,ZWL3
REAL(KIND=JPRB) :: Z1M2EPSH,ZSIGN, ZSLHDKMIN, ZKAP
LOGICAL :: LLSLHD,LLSLHDQUAD,LLSLHD_OLD
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LASCAW_CLO',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

LLSLHD=LDT(1)
LLSLHDQUAD=LDT(2)
LLSLHD_OLD=LDT(3)

! optim: separate cases for avoiding constant trivial tests inside loops on levels
IF (LLSLHDQUAD.OR.LLSLHD) THEN
  ! * Auxiliary quantity for Laplacian smoother:
  Z1M2EPSH=1.0_JPRB-2.0_JPRB*YDDYNA%SLHDEPSH

  DO JLEV=1,KFLEV
    !CDIR NODEP
    DO JROF=KST,KEND
      PD12=F12(PDLO(JROF,JLEV))
      ZWH1=FLAG1(PDLO(JROF,JLEV),PD12)
      ZWH2=FLAG2(PDLO(JROF,JLEV),PD12)
      ZWH3=FLAG3(PDLO(JROF,JLEV))

      IF (LLSLHD) THEN
        IF (LLSLHDQUAD) THEN
          ZWL1=FQUAD1(PDLO(JROF,JLEV))-ZWH1
          ZWL2=FQUAD2(PDLO(JROF,JLEV))-ZWH2
          ZWL3=FQUAD3(PDLO(JROF,JLEV))-ZWH3
        ELSE
          ! case LLSLHD_OLD (= .NOT.LLSLHDQUAD)
          ZWL1=1.0_JPRB-PDLO(JROF,JLEV)-ZWH1
          ZWL2=PDLO(JROF,JLEV)-ZWH2
          ZWL3=-ZWH3
        ENDIF

        ZWD1=ZWH1+YDDYNA%SLHDKMAX*ZWL1
        ZWD2=ZWH2+YDDYNA%SLHDKMAX*ZWL2
        ZWD3=ZWH3+YDDYNA%SLHDKMAX*ZWL3
        ZWDS1=Z1M2EPSH*ZWD1+YDDYNA%SLHDEPSH*ZWD2
        ZWDS2=YDDYNA%SLHDEPSH*ZWD1+Z1M2EPSH*ZWD2
        ZWDS3=YDDYNA%SLHDEPSH*ZWD2+ZWD3

        IF (PKAPPA(JROF,JLEV) < 0._JPRB) THEN
          ZSLHDKMIN=YDDYNA%SLHDKREF
          ZKAP=-PKAPPA(JROF,JLEV)
        ELSE
          ZSLHDKMIN=PSLHDKMIN
          ZKAP=PKAPPA(JROF,JLEV)
        ENDIF

        ZWH1=ZWH1+ZSLHDKMIN*ZWL1
        ZWH2=ZWH2+ZSLHDKMIN*ZWL2
        ZWH3=ZWH3+ZSLHDKMIN*ZWL3

        PCLO(JROF,JLEV,1)=ZWH1
        PCLO(JROF,JLEV,2)=ZWH2
        PCLO(JROF,JLEV,3)=ZWH3
        PCLOSLD(JROF,JLEV,1)=ZWH1+ZKAP*(ZWDS1-ZWH1)
        PCLOSLD(JROF,JLEV,2)=ZWH2+ZKAP*(ZWDS2-ZWH2)
        PCLOSLD(JROF,JLEV,3)=ZWH3+ZKAP*(ZWDS3-ZWH3)
      ELSE
        ZWL1=FQUAD1(PDLO(JROF,JLEV))
        ZWL2=FQUAD2(PDLO(JROF,JLEV))
        ZWL3=FQUAD3(PDLO(JROF,JLEV))
        ZWH1=ZWH1+PSLHDKMIN*(ZWL1-ZWH1)
        ZWH2=ZWH2+PSLHDKMIN*(ZWL2-ZWH2)
        ZWH3=ZWH3+PSLHDKMIN*(ZWL3-ZWH3)
        PCLO(JROF,JLEV,1)=ZWH1
        PCLO(JROF,JLEV,2)=ZWH2
        PCLO(JROF,JLEV,3)=ZWH3
        PCLOSLD(JROF,JLEV,1)=ZWH1
        PCLOSLD(JROF,JLEV,2)=ZWH2
        PCLOSLD(JROF,JLEV,3)=ZWH3
      ENDIF
    ENDDO
  ENDDO
ELSE
  DO JLEV=1,KFLEV
    !CDIR NODEP
    DO JROF=KST,KEND
      PD12=F12(PDLO(JROF,JLEV))
      ZWH1=FLAG1(PDLO(JROF,JLEV),PD12)
      ZWH2=FLAG2(PDLO(JROF,JLEV),PD12)
      ZWH3=FLAG3(PDLO(JROF,JLEV))

      PCLO(JROF,JLEV,1)=ZWH1
      PCLO(JROF,JLEV,2)=ZWH2
      PCLO(JROF,JLEV,3)=ZWH3
      PCLOSLD(JROF,JLEV,1)=ZWH1
      PCLOSLD(JROF,JLEV,2)=ZWH2
      PCLOSLD(JROF,JLEV,3)=ZWH3
    ENDDO
  ENDDO
ENDIF

DO JLEV=1,KFLEV
  !CDIR NODEP
  DO JROF=KST,KEND
    PD12=F12(PDLOMAD(JROF,JLEV))
    PCLOMAD(JROF,JLEV,1)=FLAG1(PDLOMAD(JROF,JLEV),PD12)
    PCLOMAD(JROF,JLEV,2)=FLAG2(PDLOMAD(JROF,JLEV),PD12)
    PCLOMAD(JROF,JLEV,3)=FLAG3(PDLOMAD(JROF,JLEV))
  ENDDO
ENDDO

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LASCAW_CLO',1,ZHOOK_HANDLE)
END SUBROUTINE LASCAW_CLO

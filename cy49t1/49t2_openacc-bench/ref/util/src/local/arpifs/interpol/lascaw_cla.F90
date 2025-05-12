SUBROUTINE LASCAW_CLA(YDSL,YDDYNA,KFLEV,KPROMA,KST,KEND,LDT,PSLHDKMIN,KILA,PDLAT,PKAPPA,&
 & YDDHSLMER,PSLDW,PCLA,PCLASLD)

!$ACDC singlecolumn  --process-pointers


!     ------------------------------------------------------------------

!**** *LASCAW_CLA  -  Weights for semi-LAgrangian interpolator:
!                     Computes PCLA and PCLASLD for one layer
!                     (high-order zonal weights)
!      Spherical geometry only.

!     Purpose.
!     --------

!**   Interface.
!     ----------
!        *CALL* *LASCAW_CLA( ... )

!        Explicit arguments :
!        --------------------

!        INPUT:
!          YDSL     - SL_STRUCT definition
!          KFLEV    - Vertical dimension
!          KPROMA   - horizontal dimension.
!          KST      - first element of arrays where computations are performed.
!          KEND    - depth of work.
!          LDT      - key for SLHD and horizontal turbulence.
!          KILA     - cf. ILA in LASCAW.
!          PSLHDKMIN - either HOISLH or SLHDKMIN
!          PDLAT    - distance for horizontal linear interpolations in latitude
!          PKAPPA   - kappa function ("coefficient of SLHD").
!          PSLD     - auxiliary quantity for SLHD interpolation in latitude
!          PSLDW    - weights for SLHD Laplacian smoother in latitude
!        OUTPUT:
!          PCLA    - weights for horizontal cubic interpolations in latitude.
!          PCLASLD - cf. PCLA, SLHD case.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation about semi-Lagrangian scheme.

!     Externals.
!     ----------

!        No external.
!        Called by LASCAW.

!     Reference.
!     ----------

!     Author.
!     -------
!        K. YESSAD, after former LASCAW code (JAN 2009).
!        METEO-FRANCE, CNRM/GMAP.

!     Modifications.
!     --------------
!      F. Vana  22-Feb-2011: horizontal turbulence and phys tendencies diff
!      G. Mozdzynski (May 2012): further cleaning
!      S. Malardel (Nov 2013): COMAD weights for SL interpolations
!      F. Vana 13-feb-2014 SLHD weights for heat variables
!      K. Yessad (March 2017): simplify level numbering.
!      F. Vana    21-Nov-2017: Option LHOISLT
!      F. Vana  18-Jul-2019: SLVF + cleaning
!      F. Vana  30-Oct-2019: More precision to LSLHDQUAD
!      H Petithomme (Dec 2020): use PCLA already set, option 3DTURB moved out
!      R. El Khatib 01-Jun-2022 Remove JPDUP
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIB,JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK 

! arp/ifs dependencies to be solved later.
USE YOMDYNA  , ONLY : TDYNA
USE EINT_MOD , ONLY : SL_STRUCT
USE YOMHSLMER, ONLY : THSLMER

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(SL_STRUCT),    INTENT(IN)  :: YDSL
TYPE(TDYNA),        INTENT(IN)  :: YDDYNA
INTEGER(KIND=JPIM), INTENT(IN)  :: KFLEV
INTEGER(KIND=JPIM), INTENT(IN)  :: KPROMA
INTEGER(KIND=JPIM), INTENT(IN)  :: KST
INTEGER(KIND=JPIM), INTENT(IN)  :: KEND
LOGICAL           , INTENT(IN)  :: LDT(4)
INTEGER(KIND=JPIM), INTENT(IN)  :: KILA(KPROMA,KFLEV)
REAL(KIND=JPRB)   , INTENT(IN)  :: PSLHDKMIN
REAL(KIND=JPRB)   , INTENT(IN)  :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB)   , INTENT(IN)  :: PKAPPA(KPROMA,KFLEV)
TYPE(THSLMER)     , INTENT(IN)  :: YDDHSLMER
REAL(KIND=JPRB)   , INTENT(IN)  :: PSLDW(3,3,YDSL%NDGSAH:YDSL%NDGENH)
REAL(KIND=JPRB)   , INTENT(INOUT) :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   , INTENT(OUT) :: PCLASLD(KPROMA,KFLEV,3)

INTEGER(KIND=JPIB) :: ILA64
INTEGER(KIND=JPIM) :: ILA,JROF,JLEV
LOGICAL :: LLSLHD,LLSLHDQUAD,LLSLHD_OLD
REAL(KIND=JPRB) :: ZWD1,ZWD2,ZWD3,ZWDS1,ZWDS2,ZWDS3,ZWL1,ZWL2,ZWL3,ZSQR
REAL(KIND=JPRB) :: ZSLHDKMIN,ZKAP
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LASCAW_CLA',0,ZHOOK_HANDLE)

LLSLHD=LDT(1)
LLSLHDQUAD=LDT(2)
LLSLHD_OLD=LDT(3)

! * Calculation of PCLA and PCLASLD:
IF (LLSLHDQUAD.OR.LLSLHD) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ! warning: LLSLHD and LLSLHDQUAD can both be true, so no test simplification
      ! or reordering. On the contrary, LSLHDQUAD and LSLHD_OLD can not both be true.
      ! These simpler if/else tests help optimization (fewer predicates)
      IF (LLSLHDQUAD) THEN
        ILA=KILA(JROF,JLEV)+1
        ZSQR=PDLAT(JROF,JLEV)*(1._JPRB-PDLAT(JROF,JLEV))
        ZWL1=ZSQR*YDDHSLMER%RSLD(ILA,1) + (1._JPRB-PDLAT(JROF,JLEV))
        ZWL2=ZSQR*YDDHSLMER%RSLD(ILA,2) + PDLAT(JROF,JLEV)
        ZWL3=ZSQR*YDDHSLMER%RSLD(ILA,3)
      ENDIF

      IF (LLSLHD) THEN
        IF (LLSLHD_OLD) THEN
          ZWL2=PDLAT(JROF,JLEV)
          ZWL1=1.0_JPRB-ZWL2
          ZWL3=0.0_JPRB
        ENDIF

        ! optim: 64-bit indexing in PSLDW
        ILA64=KILA(JROF,JLEV)+1
#ifdef __INTEL_COMPILER
        CALL MM_PREFETCH(PSLDW(1,1,ILA64),2)
#endif
        ! note: mind the order of settings for ZWDi/PCLA
        ZWD1=PCLA(JROF,JLEV,1)+YDDYNA%SLHDKMAX*(ZWL1-PCLA(JROF,JLEV,1))
        ZWD2=PCLA(JROF,JLEV,2)+YDDYNA%SLHDKMAX*(ZWL2-PCLA(JROF,JLEV,2))
        ZWD3=PCLA(JROF,JLEV,3)+YDDYNA%SLHDKMAX*(ZWL3-PCLA(JROF,JLEV,3))

        ZWDS1=PSLDW(1,1,ILA64)*ZWD1+PSLDW(1,2,ILA64)*ZWD2+PSLDW(1,3,ILA64)*ZWD3
        ZWDS2=PSLDW(2,1,ILA64)*ZWD1+PSLDW(2,2,ILA64)*ZWD2+PSLDW(2,3,ILA64)*ZWD3
        ZWDS3=PSLDW(3,1,ILA64)*ZWD1+PSLDW(3,2,ILA64)*ZWD2+PSLDW(3,3,ILA64)*ZWD3

        IF (PKAPPA(JROF,JLEV) < 0._JPRB) THEN
          ZKAP=-PKAPPA(JROF,JLEV)
          ZSLHDKMIN=YDDYNA%SLHDKREF
        ELSE
          ZKAP=PKAPPA(JROF,JLEV)
          ZSLHDKMIN=PSLHDKMIN
        ENDIF

        PCLA(JROF,JLEV,1)=PCLA(JROF,JLEV,1)+ZSLHDKMIN*(ZWL1-PCLA(JROF,JLEV,1))
        PCLA(JROF,JLEV,2)=PCLA(JROF,JLEV,2)+ZSLHDKMIN*(ZWL2-PCLA(JROF,JLEV,2))
        PCLA(JROF,JLEV,3)=PCLA(JROF,JLEV,3)+ZSLHDKMIN*(ZWL3-PCLA(JROF,JLEV,3))

        PCLASLD(JROF,JLEV,1)=PCLA(JROF,JLEV,1)+ZKAP*(ZWDS1-PCLA(JROF,JLEV,1))
        PCLASLD(JROF,JLEV,2)=PCLA(JROF,JLEV,2)+ZKAP*(ZWDS2-PCLA(JROF,JLEV,2))
        PCLASLD(JROF,JLEV,3)=PCLA(JROF,JLEV,3)+ZKAP*(ZWDS3-PCLA(JROF,JLEV,3))
      ELSE
        ! note: mind the order of settings for PCLA/PCLASLD
        PCLA(JROF,JLEV,1)=PCLA(JROF,JLEV,1)+PSLHDKMIN*(ZWL1-PCLA(JROF,JLEV,1))
        PCLA(JROF,JLEV,2)=PCLA(JROF,JLEV,2)+PSLHDKMIN*(ZWL2-PCLA(JROF,JLEV,2))
        PCLA(JROF,JLEV,3)=PCLA(JROF,JLEV,3)+PSLHDKMIN*(ZWL3-PCLA(JROF,JLEV,3))
        PCLASLD(JROF,JLEV,1)=PCLA(JROF,JLEV,1)
        PCLASLD(JROF,JLEV,2)=PCLA(JROF,JLEV,2)
        PCLASLD(JROF,JLEV,3)=PCLA(JROF,JLEV,3)
      ENDIF
    ENDDO
  ENDDO
ELSE
  ! optim: separate case since no reference to KILA needed
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PCLASLD(JROF,JLEV,1)=PCLA(JROF,JLEV,1)
      PCLASLD(JROF,JLEV,2)=PCLA(JROF,JLEV,2)
      PCLASLD(JROF,JLEV,3)=PCLA(JROF,JLEV,3)
    ENDDO
  ENDDO
ENDIF

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LASCAW_CLA',1,ZHOOK_HANDLE)
END SUBROUTINE LASCAW_CLA


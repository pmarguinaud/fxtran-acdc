SUBROUTINE GPHPRE_EXPL_VERTFE1 (YDCVER, TOPPRES, YDCST, KPROMA, KFLEV, KST, KEND, YDVAB, PRESH, PRESF, LHSET, &
                              & LDELP, LALPHA, LRTGR, LRPP, PDELP, PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP)

!**** *GPHPRE_EXPL_VERTFE1* - Computes half and full level pressure
!                Modern version of former GPPRE.
!                Modern version of former GPPREH+GPXYB+GPPREF

!     Purpose.
!     --------
!           Computes pressures at half and full model levels.

!**   Interface.
!     ----------
!        *CALL* *GPHPRE_EXPL_VERTFE1(...)

!        Explicit arguments :
!        --------------------

!          KPROMA    : horizontal dimensioning                                (in)
!          KFLEV     : vertical dimensioning                                  (in)
!          KSTART    : start of work                                          (in)
!          KPROF     : depth of work                                          (in)
!          YDVAB     : contains information about hybrid vertical coordinate  (in)
!          PRESH     : half level pressure                                    (inout)
!          PRESF     : full level pressure                                    (opt out)
!          LDELP,LALPHA,... : activation keys for partial computations        (opt in)

!        Implicit arguments :  NONE.
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.  None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      K. YESSAD (Sep 2011) after GPPRE, GPPREH, GPXYB and GPPREF.

!     Modifications.
!     --------------
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (Mar 2017): Introduce NDLNPR=2 for NHQE model.
!   H Petithomme (Dec 2020): add options, use of pointers, group VFE tests
!     ------------------------------------------------------------------

USE PARKIND1,ONLY: JPIM, JPRB
USE YOMHOOK, ONLY: LHOOK, JPHOOK, DR_HOOK

USE YOMCST,    ONLY: TCST
USE YOMVERT,   ONLY: TVAB
USE YOMCVER,   ONLY: TCVER

IMPLICIT NONE

TYPE(TCVER)        ,INTENT(IN)                       :: YDCVER
REAL(KIND=JPRB)    ,INTENT(IN)                       :: TOPPRES
TYPE(TCST)         ,INTENT(IN)                       :: YDCST
INTEGER(KIND=JPIM) ,INTENT(IN)                       :: KPROMA,KFLEV,KST,KEND
TYPE(TVAB)         ,INTENT(IN)                       :: YDVAB
REAL(KIND=JPRB)    ,INTENT(INOUT)                    :: PRESH(KPROMA,0:KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)   ,OPTIONAL ,TARGET  :: PRESF(KPROMA,KFLEV)
LOGICAL            ,INTENT(IN)    ,OPTIONAL          :: LHSET,LDELP,LALPHA,LRTGR,LRPP
REAL(KIND=JPRB)    ,INTENT(OUT)   ,OPTIONAL ,TARGET  :: PDELP(KPROMA,KFLEV),PLNPR(KPROMA,KFLEV),PRDELP(KPROMA,KFLEV),PALPH(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)   ,OPTIONAL ,TARGET  :: PRTGR(KPROMA,KFLEV),PRPRE(KPROMA,KFLEV),PRPP(KPROMA,KFLEV)

INTEGER(KIND=JPIM) :: IFIRST,JL,JROF
LOGICAL :: LLDELP, LLALPHA, LLRTGR, LLRPP, LTEST, LLXYB
REAL(KIND=JPRB) :: ZPRE(KPROMA)

REAL(KIND=JPRB),TARGET :: ZZPRESF(KPROMA,KFLEV)
REAL(KIND=JPRB),TARGET :: ZZDELP(KPROMA,KFLEV), ZZLNPR(KPROMA,KFLEV), ZZRDELP(KPROMA,KFLEV), ZZALPH(KPROMA,KFLEV)
REAL(KIND=JPRB),TARGET :: ZZRTGR(KPROMA,KFLEV), ZZRPRE(KPROMA,KFLEV), ZZRPP(KPROMA,KFLEV)

REAL(KIND=JPRB),POINTER :: ZDELP (:,:), ZLNPR (:,:), ZRDELP (:,:), ZALPH (:,:), ZRTGR (:,:), ZRPRE (:,:), ZRPP (:,:), ZPRESF (:,:)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GPHPRE_EXPL_VERTFE1', 0, ZHOOK_HANDLE)

LLXYB  = &
& (PRESENT (PDELP) .AND.  PRESENT (PLNPR) .AND.  PRESENT (PRDELP) .AND.  PRESENT (PALPH) .AND. &
&  PRESENT (PRTGR) .AND.  PRESENT (PRPRE) .AND.  PRESENT (PRPP))

IF (LLXYB) THEN
  LLDELP = .TRUE.
  IF (PRESENT (LDELP)) LLDELP = LDELP
  LLALPHA = .TRUE.
  IF (PRESENT (LALPHA)) LLALPHA = LALPHA
  LLRTGR = .TRUE.
  IF (PRESENT (LRTGR)) LLRTGR = LRTGR

  ZDELP  => PDELP 
  ZLNPR  => PLNPR 
  ZRDELP => PRDELP
  ZALPH  => PALPH 
  ZRTGR  => PRTGR 
  ZRPRE  => PRPRE 
  ZRPP   => PRPP 

  IF (PRESENT(PRESF)) THEN
    ZPRESF => PRESF
  ELSE
    ZPRESF => ZZPRESF
  ENDIF
ELSE
  NULLIFY (ZPRESF)
ENDIF


IF (LLXYB) THEN
  DO JL=1,KFLEV
    !DIR$ IVDEP
    !CDIR NODEP
    DO JROF=KST,KEND
      ZDELP(JROF,JL) = YDVAB%VDELA(JL)+YDVAB%VDELB(JL)*PRESH(JROF,KFLEV)
      ZPRESF(JROF,JL) = YDVAB%VAF(JL)+YDVAB%VBF(JL)*PRESH(JROF,KFLEV)
      ZPRE(JROF) = 1._JPRB/ZPRESF(JROF,JL)
      ZLNPR(JROF,JL) = ZDELP(JROF,JL)*ZPRE(JROF)
    ENDDO

    IF (LLDELP) THEN
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        ZRDELP(JROF,JL) = 1._JPRB/ZDELP(JROF,JL)
      ENDDO
    ENDIF

    IF (LLALPHA) THEN
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        ZALPH(JROF,JL) = (PRESH(JROF,JL)-ZPRESF(JROF,JL))*ZPRE(JROF)
      ENDDO
    ENDIF

    IF (LLRTGR) THEN
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        ZRTGR(JROF,JL) = YDVAB%VBF(JL)*ZPRE(JROF)
      ENDDO
    ENDIF
  ENDDO
ELSE IF (PRESENT(PRESF)) THEN
  DO JL=1,KFLEV
    !DIR$ IVDEP
    !CDIR NODEP
    DO JROF=KST,KEND
      PRESF(JROF,JL) = YDVAB%VAF(JL)+YDVAB%VBF(JL)*PRESH(JROF,KFLEV)
    ENDDO
  ENDDO
ENDIF

IF (LHOOK) CALL DR_HOOK('GPHPRE_EXPL_VERTFE1', 1, ZHOOK_HANDLE)

END SUBROUTINE


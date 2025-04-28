SUBROUTINE GPHPRE_EXPL_VERTFE0 (YDCVER, TOPPRES, YDCST, KPROMA, KFLEV, KST, KEND, YDVAB, PRESH, PRESF, LHSET, &
                              & LDELP, LALPHA, LRTGR, LRPP, PDELP, PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP)

!**** *GPHPRE_EXPL_VERTFE0* - Computes half and full level pressure
!                Modern version of former GPPRE.
!                Modern version of former GPPREH+GPXYB+GPPREF

!     Purpose.
!     --------
!           Computes pressures at half and full model levels.

!**   Interface.
!     ----------
!        *CALL* *GPHPRE_EXPL_VERTFE0(...)

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
LOGICAL :: LLDELP,LLALPHA,LLRTGR,LLRPP,LTEST,LLXYB
REAL(KIND=JPRB) :: ZPRE(KPROMA)
REAL(KIND=JPRB) :: ZCOUNT(KPROMA)
REAL(KIND=JPRB) :: ZSUM

REAL(KIND=JPRB),TARGET :: ZZDELP(KPROMA,KFLEV), ZZLNPR(KPROMA,KFLEV), ZZRDELP(KPROMA,KFLEV), ZZALPH(KPROMA,KFLEV)
REAL(KIND=JPRB),TARGET :: ZZRTGR(KPROMA,KFLEV), ZZRPRE(KPROMA,KFLEV), ZZRPP(KPROMA,KFLEV)

REAL(KIND=JPRB),POINTER :: ZDELP (:,:), ZLNPR (:,:), ZRDELP (:,:), ZALPH (:,:), ZRTGR (:,:), ZRPRE (:,:), ZRPP (:,:)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GPHPRE_EXPL_VERTFE0', 0, ZHOOK_HANDLE)

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
  LLRPP = .TRUE.
  IF (PRESENT (LRPP)) LLRPP = LRPP

  ! broader condition for computing alpha: 
  IF (PRESENT(PRESF)) LLALPHA = LLALPHA.OR.YDCVER%NDLNPR == 1.OR.YDCVER%NDLNPR == 2.OR..NOT.YDCVER%LAPRXPK

  ZDELP  => PDELP  
  ZLNPR  => PLNPR  
  ZRDELP => PRDELP 
  ZALPH  => PALPH  
  ZRTGR  => PRTGR  
  ZRPRE  => PRPRE  
  ZRPP   => PRPP   

ELSE
  LLDELP = .FALSE.
  LLRTGR = .FALSE.
  LLRPP = .FALSE.

  ! reduced condition for computing alpha: 
  LLALPHA = PRESENT(PRESF).AND.(YDCVER%NDLNPR == 1.OR.YDCVER%NDLNPR == 2.OR..NOT.YDCVER%LAPRXPK)

  IF (LLALPHA) THEN
    ZDELP  => ZZDELP  
    ZLNPR  => ZZLNPR  
    ZRDELP => ZZRDELP 
    ZALPH  => ZZALPH  
    ZRTGR  => ZZRTGR  
    ZRPRE  => ZZRPRE  
    ZRPP   => ZZRPP   
  ENDIF
ENDIF

IF (LLXYB.OR.LLALPHA) THEN
  ! pressure at top
  ZPRE(KST:KEND) = YDVAB%VAH(0)+YDVAB%VBH(0)*PRESH(KST:KEND,KFLEV)

  ZCOUNT(KST:KEND) = 0._JPRB

  DO JROF=KST,KEND
    IF (ZPRE(JROF) <= TOPPRES) THEN
      ZCOUNT(JROF)=ZCOUNT(JROF)+1
    ENDIF
  END DO

  ZSUM = SUM (ZCOUNT(KST:KEND))

  IF (ZSUM > 0._JPRB) THEN
    IFIRST = 2
  ELSE
    IFIRST = 1
  ENDIF

  IF (YDCVER%NDLNPR == 0) THEN
    IF (IFIRST == 2) THEN
      ZLNPR(KST:KEND,1) = LOG(PRESH(KST:KEND,1)/TOPPRES)

      IF (LLDELP.OR.LLRTGR) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZDELP(JROF,1) = PRESH(JROF,1)-PRESH(JROF,0)
          ZRDELP(JROF,1) = 1._JPRB/ZDELP(JROF,1)
        ENDDO
      ENDIF

      IF (LLALPHA) ZALPH(KST:KEND,1) = YDCVER%RHYDR0

      IF (LLRTGR) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZRTGR(JROF,1) = ZRDELP(JROF,1)*YDVAB%VDELB(1)
        ENDDO
      ENDIF

      IF (LLRPP) THEN
        DO JROF=KST,KEND
          ZRPRE(JROF,1) = 1._JPRB/PRESH(JROF,1)
          ZRPP(JROF,1) = 1._JPRB/(PRESH(JROF,1)*TOPPRES)
        ENDDO
      ENDIF
    ENDIF

    ZPRE(KST:KEND) = 1._JPRB/PRESH(KST:KEND,IFIRST-1)

    DO JL=IFIRST,KFLEV
      ZLNPR(KST:KEND,JL) = LOG(PRESH(KST:KEND,JL)*ZPRE(KST:KEND))

      IF (LLDELP.OR.LLALPHA.OR.LLRTGR) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZDELP(JROF,JL) = PRESH(JROF,JL)-PRESH(JROF,JL-1)
          ZRDELP(JROF,JL) = 1._JPRB/ZDELP(JROF,JL)
        ENDDO
      ENDIF

      IF (LLALPHA) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZALPH(JROF,JL) = 1._JPRB-PRESH(JROF,JL-1)*ZRDELP(JROF,JL)*&
            ZLNPR(JROF,JL)
        ENDDO
      ENDIF

      IF (LLRTGR) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZRTGR(JROF,JL) = ZRDELP(JROF,JL)*(YDVAB%VDELB(JL)+&
            YDVAB%VC(JL)*ZLNPR(JROF,JL)*ZRDELP(JROF,JL))
        ENDDO
      ENDIF

      IF (LLRPP) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZRPRE(JROF,JL) = 1._JPRB/PRESH(JROF,JL)
          ZRPP(JROF,JL) = ZRPRE(JROF,JL)*ZPRE(JROF)
          ZPRE(JROF) = ZRPRE(JROF,JL)
        ENDDO
      ELSE
        ZPRE(KST:KEND) = 1._JPRB/PRESH(KST:KEND,JL)
      ENDIF
    ENDDO
  ELSE IF (YDCVER%NDLNPR == 1.OR.YDCVER%NDLNPR == 2) THEN
    LTEST = LLDELP.OR.LLALPHA.OR.LLRTGR

    DO JL=IFIRST,KFLEV
      ! optim: keep lnpr separate but consistent
      IF (LTEST) THEN
        DO JROF=KST,KEND
          ZDELP(JROF,JL) = PRESH(JROF,JL)-PRESH(JROF,JL-1)
          ZRDELP(JROF,JL) = 1._JPRB/ZDELP(JROF,JL)
          ZRPP(JROF,JL) = 1._JPRB/(PRESH(JROF,JL)*PRESH(JROF,JL-1))
          ZLNPR(JROF,JL) = ZDELP(JROF,JL)*SQRT(ZRPP(JROF,JL))
        ENDDO
      ELSE
        DO JROF=KST,KEND
          ZLNPR(JROF,JL) = (PRESH(JROF,JL)-PRESH(JROF,JL-1))/&
            SQRT(PRESH(JROF,JL)*PRESH(JROF,JL-1))
        ENDDO
      ENDIF

      IF (LLALPHA) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZALPH(JROF,JL) = 1._JPRB-PRESH(JROF,JL-1)*ZRDELP(JROF,JL)*&
            ZLNPR(JROF,JL)
        ENDDO
      ENDIF

      IF (LLRTGR) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZRTGR(JROF,JL) = ZRDELP(JROF,JL)*(YDVAB%VDELB(JL)+&
            YDVAB%VC(JL)*ZLNPR(JROF,JL)*ZRDELP(JROF,JL))
        ENDDO
      ENDIF

      IF (LLRPP) ZRPRE(KST:KEND,JL) = 1._JPRB/PRESH(KST:KEND,JL)
    ENDDO

    IF (IFIRST == 2) THEN
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        ZDELP(JROF,1) = PRESH(JROF,1)
        ZRDELP(JROF,1) = 1._JPRB/ZDELP(JROF,1)
      ENDDO

      IF (YDCVER%NDLNPR == 1) THEN
        ZLNPR(KST:KEND,1) = 2._JPRB+YDCST%RCVD/YDCST%RD
      ELSE
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZLNPR(JROF,1) = 1._JPRB+ZLNPR(JROF,2)*&
            (ZDELP(JROF,1)/ZDELP(JROF,2))*SQRT(PRESH(JROF,2)/PRESH(JROF,1))
        ENDDO
      ENDIF

      IF (LLALPHA) ZALPH(KST:KEND,1) = 1._JPRB

      IF (LLRTGR) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZRTGR(JROF,1) = ZRDELP(JROF,1)*YDVAB%VDELB(1)
        ENDDO
      ENDIF

      IF (LLRPP) THEN
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          ZRPRE(JROF,1) = 1._JPRB/PRESH(JROF,1)
          ZRPP(JROF,1) = (ZLNPR(JROF,1)*ZRDELP(JROF,1))**2
        ENDDO
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF (PRESENT(PRESF)) THEN
  IF (YDCVER%NDLNPR == 0) THEN
    IF (YDCVER%LAPRXPK) THEN
      DO JL=1,KFLEV
        PRESF(KST:KEND,JL) = (PRESH(KST:KEND,JL-1)+PRESH(KST:KEND,JL))*0.5_JPRB
      ENDDO
    ELSE
      DO JL=1,KFLEV
        !DIR$ IVDEP
        !CDIR NODEP
        DO JROF=KST,KEND
          PRESF(JROF,JL) = EXP(-ZALPH(JROF,JL))*PRESH(JROF,JL)
        ENDDO
      ENDDO
    ENDIF
  ELSE IF (YDCVER%NDLNPR == 1.OR.YDCVER%NDLNPR == 2) THEN
    DO JL=IFIRST,KFLEV
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        PRESF(JROF,JL) = (1._JPRB-ZALPH(JROF,JL))*PRESH(JROF,JL)
      ENDDO
    ENDDO

    IF (IFIRST == 2) THEN
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        PRESF(JROF,1) = PRESH(JROF,1)/ZLNPR(JROF,1)
      ENDDO
    ENDIF
  ENDIF
ENDIF

IF (LHOOK) CALL DR_HOOK('GPHPRE_EXPL_VERTFE0', 1, ZHOOK_HANDLE)

END SUBROUTINE


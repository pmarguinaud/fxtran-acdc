SUBROUTINE GPGRXYB_EXPL (YDCVER, KPROMA, KST, KEND, KFLEV, LDCOEF, YDVAB, PREL, PREM, PDELP, &
& PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP, PCOEFD_DER, PLNPRL_DER, PLNPRM_DER, PCOEFA_DER,  &
& PCOEFAPL_DER, PALPHPLL_DER, PALPHPLM_DER, PALPHL_DER, PALPHM_DER)

!**** *GPGRXYB_EXPL* - Complement to routine "GPXYB".
!                 Computation of the horizontal gradient of quantities
!                 "alpha" and "delta" at model levels.

!     Purpose.
!     --------

!     "alpha" and "delta" are computed at model levels in routine "GPXYB",
!     but not their horizontal gradient. So this routine provides the
!     horizontal gradients at full levels. Quantity
!     "(grad(alpha)) + (grad(prehyd)/prehyd)"
!     is also provided separately (for case LVERTFE=.F.
!     its "NDLNPR=0" expression is simpler than the expressions
!     of "grad(alpha)" and "grad(prehyd)/prehyd").
!     Discretisation depends on variables "NDLNPR" and "LVERTFE".

!     For LVERTFE=.F., NDLNPR=0, discretisations are:

!      (grad(delta))[l] =
!      - (A[lbar]*B[lbar-1]-A[lbar-1]*B[lbar])/(prehyd[lbar]*prehyd[lbar-1])
!      * (grad prehyds)

!      (grad(alpha))[l] + (grad(prehyd)/prehyd)[l] =
!      B[lbar]/prehyd[lbar] * (grad prehyds)

!      Quantity "(grad(alpha))[l]" is computed by substracting
!      "(grad(prehyd)/prehyd)[l]" from
!      "(grad(alpha))[l] + (grad(prehyd)/prehyd)[l]"

!     For LVERTFE=.F., NDLNPR=1 or 2, discretisations are:

!      (grad(delta))[l] =
!      - delta[l] * (A[lbar]*B[lbar-1]-A[lbar-1]*B[lbar])
!      * (1/sqrt(prehyd[lbar]*prehyd[lbar-1])) * (1/(delta prehyd[l]))
!      * (grad prehyds)

!      (grad(alpha))[l] =
!      - alpha[l] * (A[lbar]*B[lbar-1]-A[lbar-1]*B[lbar])
!      * (1/sqrt(prehyd[lbar]*prehyd[lbar-1])) * (1/(delta prehyd[l]))
!      * (grad prehyds)

!      (grad(prehyd)/prehyd)[l] = prtgr[l] * (grad prehyds)
!      where "prtgr[l]" is computed in routine "gpxyb" as:
!      prtgr[l] = { (delta B)[l]
!      + delta[l] * (A[lbar]*B[lbar-1]-A[lbar-1]*B[lbar])/(delta prehyd[l]) }
!      * { 1/(delta prehyd[l]) }

!      In this case "(grad(alpha))[l]" is computed prior to
!      "(grad(alpha))[l] + (grad(prehyd)/prehyd)[l]"

!     For LVERTFE=.T., NDLNPR=0, discretisations are:

!      (grad(delta))[l] =
!      delta[l] * ((Delta B)[l]/(Delta prehyd)[l] - B[l]/prehyd[l])
!      * (grad prehyds)

!      grad(alpha) is useless in this case.

!     Notations:
!      - "grad" is the horizontal gradient operator.
!        (grad X = vnabla X = M vnabla' X)
!      - "prehyd" is the hydrostatic pressure.
!      - "prehyds" is the surface hydrostatic pressure.

!**   Interface.
!     ----------
!        *CALL* *GPGRXYB(...)

!        Explicit arguments :
!        --------------------
!         * INPUT:
!           KPROMA       : horizontal dimension
!           KD           : start of work
!           KF           : working length
!           KFLEV        : number of levels
!           LDCOEF       : if T, stores ZCOEFD, ZCOEFA, ZCOEFAPL in PXYBDER.
!           YDVAB        : contains information about hybrid vertical coordinate
!           PREL         : zonal component of "grad prehyds"
!           PREM         : meridian component of "grad prehyds"
!           PXYB         : contains pressure depth, "delta", "alpha".

!         * OUTPUT:
!           PXYBDER      : contains grad(delta), grad(alpha), grad(alpha + log prehyd)

!        Implicit arguments :   None.
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.    None.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------
!        K. YESSAD
!        Original : 00-08-11

!     Modifications.
!     --------------
!        K. Yessad (Dec 2008): remove dummy CDLOCK
!        K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!        K. Yessad (Dec 2011): use YDVAB.
!        K. Yessad (June 2017): introduce NDLNPR=2 (for NHQE model).
!        H. Petithomme (Dec 2020): use of pointers for optimisation
!     ------------------------------------------------------------------

USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, JPHOOK, DR_HOOK
USE YOMCVER   , ONLY : TCVER
USE YOMVERT   , ONLY : TVAB


 

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCVER)        ,INTENT(IN)           :: YDCVER
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KPROMA 
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KST 
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KEND 
INTEGER(KIND=JPIM) ,INTENT(IN)           :: KFLEV 
LOGICAL            ,INTENT(IN)           :: LDCOEF
TYPE(TVAB)         ,INTENT(IN)           :: YDVAB
REAL(KIND=JPRB)    ,INTENT(IN)           :: PREL(KPROMA) 
REAL(KIND=JPRB)    ,INTENT(IN)           :: PREM(KPROMA) 
REAL(KIND=JPRB)    ,INTENT(IN)           :: PDELP(KPROMA,KFLEV),PLNPR(KPROMA,KFLEV),PRDELP(KPROMA,KFLEV),PALPH(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)           :: PRTGR(KPROMA,KFLEV),PRPRE(KPROMA,KFLEV),PRPP(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT) ,TARGET  :: PCOEFD_DER(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)          :: PLNPRL_DER(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)          :: PLNPRM_DER(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT) ,TARGET  :: PCOEFA_DER(KPROMA,KFLEV),PCOEFAPL_DER(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)          :: PALPHPLL_DER(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)          :: PALPHPLM_DER(KPROMA,KFLEV),PALPHL_DER(KPROMA,KFLEV),PALPHM_DER(KPROMA,KFLEV)

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF
REAL(KIND=JPRB) :: ZCOEFDT, ZCOEFAT, ZCOEFAPLT
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GPGRXYB_EXPL', 0, ZHOOK_HANDLE)

!     ------------------------------------------------------------------

!*    1/ Calculation of "grad delta" at full levels.


! optim: compilers may report dependence between pxyb and ydvab, ignored with ivdep/nodep

IF(YDCVER%LVERTFE) THEN
  DO JLEV=1,KFLEV
    !DIR$ IVDEP
    !CDIR NODEP
    DO JROF=KST,KEND
      ZCOEFDT=(YDVAB%VDELB(JLEV)*PRDELP(JROF,JLEV)&
       & -PRTGR(JROF,JLEV))*PLNPR(JROF,JLEV)  
      PLNPRL_DER(JROF,JLEV)=ZCOEFDT*PREL(JROF)
      PLNPRM_DER(JROF,JLEV)=ZCOEFDT*PREM(JROF)
      IF (LDCOEF) THEN
        PCOEFD_DER(JROF,JLEV)=ZCOEFDT
      ENDIF
    ENDDO
  ENDDO
ELSE
  DO JLEV=1,KFLEV
    !DIR$ IVDEP
    !CDIR NODEP
    DO JROF=KST,KEND
      ZCOEFDT=-YDVAB%VC(JLEV)*PRPP(JROF,JLEV)
      PLNPRL_DER(JROF,JLEV)=ZCOEFDT*PREL(JROF)
      PLNPRM_DER(JROF,JLEV)=ZCOEFDT*PREM(JROF)
      IF (LDCOEF) THEN
        PCOEFD_DER(JROF,JLEV)=ZCOEFDT
      ENDIF
    ENDDO
  ENDDO

!*    2/ Calculation of "grad (alpha + log prehyd)" at full levels
!        discretised as "grad alpha + (grad prehyd) / prehyd ",
!        and calculation of "grad alpha" at full levels.

  IF(YDCVER%NDLNPR == 0) THEN
    DO JLEV=1,KFLEV
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        ZCOEFAPLT=YDVAB%VBH(JLEV)*PRPRE(JROF,JLEV)
        PALPHPLL_DER(JROF,JLEV)=ZCOEFAPLT*PREL(JROF)
        PALPHPLM_DER(JROF,JLEV)=ZCOEFAPLT*PREM(JROF)

        ZCOEFAT=ZCOEFAPLT-PRTGR(JROF,JLEV)
        PALPHL_DER(JROF,JLEV)=ZCOEFAT*PREL(JROF)
        PALPHM_DER(JROF,JLEV)=ZCOEFAT*PREM(JROF)

        IF (LDCOEF) THEN
          PCOEFA_DER(JROF,JLEV) = ZCOEFAT
          PCOEFAPL_DER(JROF,JLEV) = ZCOEFAPLT
        END IF

      ENDDO
    ENDDO
  ELSEIF(YDCVER%NDLNPR == 1 .OR. YDCVER%NDLNPR == 2) THEN
    DO JLEV=1,KFLEV
      !DIR$ IVDEP
      !CDIR NODEP
      DO JROF=KST,KEND
        ZCOEFAT=-YDVAB%VC(JLEV)*PRPP(JROF,JLEV)&
          & *PALPH(JROF,JLEV)/PLNPR(JROF,JLEV)
        PALPHL_DER(JROF,JLEV)=ZCOEFAT*PREL(JROF)
        PALPHM_DER(JROF,JLEV)=ZCOEFAT*PREM(JROF)

        ZCOEFAPLT=ZCOEFAT+PRTGR(JROF,JLEV)
        PALPHPLL_DER(JROF,JLEV)=ZCOEFAPLT*PREL(JROF)
        PALPHPLM_DER(JROF,JLEV)=ZCOEFAPLT*PREM(JROF)

        IF (LDCOEF) THEN
          PCOEFA_DER(JROF,JLEV) = ZCOEFAT
          PCOEFAPL_DER(JROF,JLEV) = ZCOEFAPLT
        END IF

      ENDDO
    ENDDO
  ENDIF
ENDIF

IF (LHOOK) CALL DR_HOOK('GPGRXYB_EXPL', 1, ZHOOK_HANDLE)

END SUBROUTINE GPGRXYB_EXPL

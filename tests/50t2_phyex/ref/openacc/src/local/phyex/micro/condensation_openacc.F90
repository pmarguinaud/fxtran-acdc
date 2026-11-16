
SUBROUTINE CONDENSATION_OPENACC (D, CST, ICEP, NEBN, TURBN, HFRAC_ICE, HCONDENS, HLAMBDA3, PPABS&
&, PZZ, PRHODREF, PT, PRV_IN, PRV_OUT, PRC_IN, PRC_OUT, PRI_IN, PRI_OUT, PRR, PRS, PRG, PSIGS, LMFCONV&
&, PMFCONV, PCLDFR, PSIGRC, OUSERI, OSIGMAS, OCND2, PICLDFR, PWCLDFR, PSSIO, PSSIU, PIFR, PSIGQSAT&
&, PLV, PLS, PCPH, PHLC_HRC, PHLC_HCF, PHLI_HRI, PHLI_HCF, PICE_CLD_WGT, YDSTACK)
!$ACC ROUTINE (CONDENSATION_OPENACC) SEQ      


USE MODD_DIMPHYEX,ONLY:DIMPHYEX_T
USE MODD_CST,ONLY:CST_T
USE MODD_RAIN_ICE_PARAM_N,ONLY:RAIN_ICE_PARAM_T
USE MODD_NEB_N,ONLY:NEB_T
USE MODD_TURB_N,ONLY:TURB_T
USE MODE_TIWMX,ONLY:ESATW, ESATI
USE MODE_ICECLOUD,ONLY:ICECLOUD
#include "fxtran_acdc_stack.h"
USE FXTRAN_ACDC_STACK_MOD
USE FXTRAN_ACDC_ABORT_MOD

IMPLICIT NONE


TYPE (DIMPHYEX_T), INTENT (IN)::D
TYPE (CST_T), INTENT (IN)::CST
TYPE (RAIN_ICE_PARAM_T), INTENT (IN)::ICEP
TYPE (NEB_T), INTENT (IN)::NEBN
TYPE (TURB_T), INTENT (IN)::TURBN
CHARACTER (LEN=1), INTENT (IN)::HFRAC_ICE
CHARACTER (LEN=4), INTENT (IN)::HCONDENS
CHARACTER (LEN=4), INTENT (IN)::HLAMBDA3
REAL, INTENT (IN)::PPABS(D%NIJT, D%NKT)
REAL, INTENT (IN)::PZZ(D%NIJT, D%NKT)
REAL, INTENT (IN)::PRHODREF(D%NIJT, D%NKT)
REAL, INTENT (INOUT)::PT(D%NIJT, D%NKT)
REAL, INTENT (IN)::PRV_IN(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PRV_OUT(D%NIJT, D%NKT)
REAL, INTENT (IN)::PRC_IN(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PRC_OUT(D%NIJT, D%NKT)
REAL, INTENT (IN)::PRI_IN(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PRI_OUT(D%NIJT, D%NKT)
REAL, INTENT (IN)::PRR(D%NIJT, D%NKT)
REAL, INTENT (IN)::PRS(D%NIJT, D%NKT)
REAL, INTENT (IN)::PRG(D%NIJT, D%NKT)
REAL, INTENT (IN)::PSIGS(D%NIJT, D%NKT)
LOGICAL, INTENT (IN)::LMFCONV
REAL, INTENT (IN)::PMFCONV(MERGE (D%NIJT, 0, LMFCONV), MERGE (D%NKT, 0, LMFCONV))
REAL, INTENT (OUT)::PCLDFR(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PSIGRC(D%NIJT, D%NKT)
LOGICAL, INTENT (IN)::OUSERI
LOGICAL, INTENT (IN)::OSIGMAS
LOGICAL, INTENT (IN)::OCND2
REAL, INTENT (OUT)::PICLDFR(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PWCLDFR(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PSSIO(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PSSIU(D%NIJT, D%NKT)
REAL, INTENT (OUT)::PIFR(D%NIJT, D%NKT)
REAL, INTENT (IN)::PSIGQSAT(D%NIJT)
REAL, OPTIONAL, INTENT (IN)::PLV(D%NIJT, D%NKT)
REAL, OPTIONAL, INTENT (IN)::PLS(D%NIJT, D%NKT)
REAL, OPTIONAL, INTENT (IN)::PCPH(D%NIJT, D%NKT)
REAL, OPTIONAL, INTENT (OUT)::PHLC_HRC(D%NIJT, D%NKT)
REAL, OPTIONAL, INTENT (OUT)::PHLC_HCF(D%NIJT, D%NKT)
REAL, OPTIONAL, INTENT (OUT)::PHLI_HRI(D%NIJT, D%NKT)
REAL, OPTIONAL, INTENT (OUT)::PHLI_HCF(D%NIJT, D%NKT)
REAL, OPTIONAL, INTENT (IN)::PICE_CLD_WGT(D%NIJT)
TYPE(FXTRAN_ACDC_STACK), INTENT (IN) :: YDSTACK
TYPE(FXTRAN_ACDC_STACK) :: YLSTACK
INTEGER::JKP
INTEGER::JK
INTEGER::JI
INTEGER::IIJE
INTEGER::IIJB
INTEGER::IKL
INTEGER::IKE
INTEGER::IKB
INTEGER::IKTE
INTEGER::IKTB
fxtran_acdc_temp (REAL, ZRT, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZTLK, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZL, (D%NIJT, D%NKT))
INTEGER::ITPL
REAL::ZTMIN
fxtran_acdc_temp (REAL, ZCPD, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZLS, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZLV, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZCRIAUTI, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZGAUTI, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZGAUTC, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZGAUI, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZGAUC, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZGAUV, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZAUTI, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZAUTC, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZGCOND, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZLVS, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZQSI, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZQSL, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZPIV, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZPV, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZZZ, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, DZZ, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZLL, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZSIG_CONV, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDTL, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDRW, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZAH, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZQ1, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZSIGMA, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZSBAR, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZB, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZA, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZCOND, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZFRAC, (D%NIJT, D%NKT))
fxtran_acdc_temp (INTEGER, INQ1, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZINC, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZRCOLD, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZRSDIF, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZRFRAC, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZRSW, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZRSP, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ESATW_T, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZLWINC, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZPRIFACT, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDUM4, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDUM3, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDUM2, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDUM1, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZCLDINI, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZARDUM2, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZARDUM, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDZ, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZICE_CLD_WGT, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZSIGQSAT, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDZREF, (D%NIJT, D%NKT))
fxtran_acdc_temp (REAL, ZDZFACT, (D%NIJT, D%NKT))

INTEGER::JKMK(D%NKT)
INTEGER::JKPK(D%NKT)
REAL::ZBLH
REAL, PARAMETER::ZL0=600.
REAL, PARAMETER::ZCSIGMA=0.2
REAL, PARAMETER::ZCSIG_CONV=0.30E-2
REAL, PARAMETER::ZSRC_1D(-22:11)=(/0., 0., 2.0094444E-04, 0.316670E-03, 4.9965648E-04, 0.785956E&
&-03, 1.2341294E-03, 0.193327E-02, 3.0190963E-03, 0.470144E-02, 7.2950651E-03, 0.112759E-01, 1.7350994E&
&-02, 0.265640E-01, 4.0427860E-02, 0.610997E-01, 9.1578111E-02, 0.135888E+00, 0.1991484, 0.230756E&
&+00, 0.2850565, 0.375050E+00, 0.5000000, 0.691489E+00, 0.8413813, 0.933222E+00, 0.9772662, 0.993797E&
&+00, 0.9986521, 0.999768E+00, 0.9999684, 0.999997E+00, 1.0000000, 1.000000/)

YLSTACK = YDSTACK



IF (KIND (ZRT) == 8) THEN
    fxtran_acdc_alloc8 (ZRT)
ELSEIF (KIND (ZRT) == 4) THEN
    fxtran_acdc_alloc4 (ZRT)
ELSE
    STOP 1
ENDIF


IF (KIND (ZTLK) == 8) THEN
    fxtran_acdc_alloc8 (ZTLK)
ELSEIF (KIND (ZTLK) == 4) THEN
    fxtran_acdc_alloc4 (ZTLK)
ELSE
    STOP 1
ENDIF


IF (KIND (ZL) == 8) THEN
    fxtran_acdc_alloc8 (ZL)
ELSEIF (KIND (ZL) == 4) THEN
    fxtran_acdc_alloc4 (ZL)
ELSE
    STOP 1
ENDIF


IF (KIND (ZCPD) == 8) THEN
    fxtran_acdc_alloc8 (ZCPD)
ELSEIF (KIND (ZCPD) == 4) THEN
    fxtran_acdc_alloc4 (ZCPD)
ELSE
    STOP 1
ENDIF


IF (KIND (ZLS) == 8) THEN
    fxtran_acdc_alloc8 (ZLS)
ELSEIF (KIND (ZLS) == 4) THEN
    fxtran_acdc_alloc4 (ZLS)
ELSE
    STOP 1
ENDIF


IF (KIND (ZLV) == 8) THEN
    fxtran_acdc_alloc8 (ZLV)
ELSEIF (KIND (ZLV) == 4) THEN
    fxtran_acdc_alloc4 (ZLV)
ELSE
    STOP 1
ENDIF


IF (KIND (ZCRIAUTI) == 8) THEN
    fxtran_acdc_alloc8 (ZCRIAUTI)
ELSEIF (KIND (ZCRIAUTI) == 4) THEN
    fxtran_acdc_alloc4 (ZCRIAUTI)
ELSE
    STOP 1
ENDIF


IF (KIND (ZGAUTI) == 8) THEN
    fxtran_acdc_alloc8 (ZGAUTI)
ELSEIF (KIND (ZGAUTI) == 4) THEN
    fxtran_acdc_alloc4 (ZGAUTI)
ELSE
    STOP 1
ENDIF


IF (KIND (ZGAUTC) == 8) THEN
    fxtran_acdc_alloc8 (ZGAUTC)
ELSEIF (KIND (ZGAUTC) == 4) THEN
    fxtran_acdc_alloc4 (ZGAUTC)
ELSE
    STOP 1
ENDIF


IF (KIND (ZGAUI) == 8) THEN
    fxtran_acdc_alloc8 (ZGAUI)
ELSEIF (KIND (ZGAUI) == 4) THEN
    fxtran_acdc_alloc4 (ZGAUI)
ELSE
    STOP 1
ENDIF


IF (KIND (ZGAUC) == 8) THEN
    fxtran_acdc_alloc8 (ZGAUC)
ELSEIF (KIND (ZGAUC) == 4) THEN
    fxtran_acdc_alloc4 (ZGAUC)
ELSE
    STOP 1
ENDIF


IF (KIND (ZGAUV) == 8) THEN
    fxtran_acdc_alloc8 (ZGAUV)
ELSEIF (KIND (ZGAUV) == 4) THEN
    fxtran_acdc_alloc4 (ZGAUV)
ELSE
    STOP 1
ENDIF


IF (KIND (ZAUTI) == 8) THEN
    fxtran_acdc_alloc8 (ZAUTI)
ELSEIF (KIND (ZAUTI) == 4) THEN
    fxtran_acdc_alloc4 (ZAUTI)
ELSE
    STOP 1
ENDIF


IF (KIND (ZAUTC) == 8) THEN
    fxtran_acdc_alloc8 (ZAUTC)
ELSEIF (KIND (ZAUTC) == 4) THEN
    fxtran_acdc_alloc4 (ZAUTC)
ELSE
    STOP 1
ENDIF


IF (KIND (ZGCOND) == 8) THEN
    fxtran_acdc_alloc8 (ZGCOND)
ELSEIF (KIND (ZGCOND) == 4) THEN
    fxtran_acdc_alloc4 (ZGCOND)
ELSE
    STOP 1
ENDIF


IF (KIND (ZLVS) == 8) THEN
    fxtran_acdc_alloc8 (ZLVS)
ELSEIF (KIND (ZLVS) == 4) THEN
    fxtran_acdc_alloc4 (ZLVS)
ELSE
    STOP 1
ENDIF


IF (KIND (ZQSI) == 8) THEN
    fxtran_acdc_alloc8 (ZQSI)
ELSEIF (KIND (ZQSI) == 4) THEN
    fxtran_acdc_alloc4 (ZQSI)
ELSE
    STOP 1
ENDIF


IF (KIND (ZQSL) == 8) THEN
    fxtran_acdc_alloc8 (ZQSL)
ELSEIF (KIND (ZQSL) == 4) THEN
    fxtran_acdc_alloc4 (ZQSL)
ELSE
    STOP 1
ENDIF


IF (KIND (ZPIV) == 8) THEN
    fxtran_acdc_alloc8 (ZPIV)
ELSEIF (KIND (ZPIV) == 4) THEN
    fxtran_acdc_alloc4 (ZPIV)
ELSE
    STOP 1
ENDIF


IF (KIND (ZPV) == 8) THEN
    fxtran_acdc_alloc8 (ZPV)
ELSEIF (KIND (ZPV) == 4) THEN
    fxtran_acdc_alloc4 (ZPV)
ELSE
    STOP 1
ENDIF


IF (KIND (ZZZ) == 8) THEN
    fxtran_acdc_alloc8 (ZZZ)
ELSEIF (KIND (ZZZ) == 4) THEN
    fxtran_acdc_alloc4 (ZZZ)
ELSE
    STOP 1
ENDIF


IF (KIND (DZZ) == 8) THEN
    fxtran_acdc_alloc8 (DZZ)
ELSEIF (KIND (DZZ) == 4) THEN
    fxtran_acdc_alloc4 (DZZ)
ELSE
    STOP 1
ENDIF


IF (KIND (ZLL) == 8) THEN
    fxtran_acdc_alloc8 (ZLL)
ELSEIF (KIND (ZLL) == 4) THEN
    fxtran_acdc_alloc4 (ZLL)
ELSE
    STOP 1
ENDIF


IF (KIND (ZSIG_CONV) == 8) THEN
    fxtran_acdc_alloc8 (ZSIG_CONV)
ELSEIF (KIND (ZSIG_CONV) == 4) THEN
    fxtran_acdc_alloc4 (ZSIG_CONV)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDTL) == 8) THEN
    fxtran_acdc_alloc8 (ZDTL)
ELSEIF (KIND (ZDTL) == 4) THEN
    fxtran_acdc_alloc4 (ZDTL)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDRW) == 8) THEN
    fxtran_acdc_alloc8 (ZDRW)
ELSEIF (KIND (ZDRW) == 4) THEN
    fxtran_acdc_alloc4 (ZDRW)
ELSE
    STOP 1
ENDIF


IF (KIND (ZAH) == 8) THEN
    fxtran_acdc_alloc8 (ZAH)
ELSEIF (KIND (ZAH) == 4) THEN
    fxtran_acdc_alloc4 (ZAH)
ELSE
    STOP 1
ENDIF


IF (KIND (ZQ1) == 8) THEN
    fxtran_acdc_alloc8 (ZQ1)
ELSEIF (KIND (ZQ1) == 4) THEN
    fxtran_acdc_alloc4 (ZQ1)
ELSE
    STOP 1
ENDIF


IF (KIND (ZSIGMA) == 8) THEN
    fxtran_acdc_alloc8 (ZSIGMA)
ELSEIF (KIND (ZSIGMA) == 4) THEN
    fxtran_acdc_alloc4 (ZSIGMA)
ELSE
    STOP 1
ENDIF


IF (KIND (ZSBAR) == 8) THEN
    fxtran_acdc_alloc8 (ZSBAR)
ELSEIF (KIND (ZSBAR) == 4) THEN
    fxtran_acdc_alloc4 (ZSBAR)
ELSE
    STOP 1
ENDIF


IF (KIND (ZB) == 8) THEN
    fxtran_acdc_alloc8 (ZB)
ELSEIF (KIND (ZB) == 4) THEN
    fxtran_acdc_alloc4 (ZB)
ELSE
    STOP 1
ENDIF


IF (KIND (ZA) == 8) THEN
    fxtran_acdc_alloc8 (ZA)
ELSEIF (KIND (ZA) == 4) THEN
    fxtran_acdc_alloc4 (ZA)
ELSE
    STOP 1
ENDIF


IF (KIND (ZCOND) == 8) THEN
    fxtran_acdc_alloc8 (ZCOND)
ELSEIF (KIND (ZCOND) == 4) THEN
    fxtran_acdc_alloc4 (ZCOND)
ELSE
    STOP 1
ENDIF


IF (KIND (ZFRAC) == 8) THEN
    fxtran_acdc_alloc8 (ZFRAC)
ELSEIF (KIND (ZFRAC) == 4) THEN
    fxtran_acdc_alloc4 (ZFRAC)
ELSE
    STOP 1
ENDIF


IF (KIND (INQ1) == 8) THEN
    fxtran_acdc_alloc8 (INQ1)
ELSEIF (KIND (INQ1) == 4) THEN
    fxtran_acdc_alloc4 (INQ1)
ELSE
    STOP 1
ENDIF


IF (KIND (ZINC) == 8) THEN
    fxtran_acdc_alloc8 (ZINC)
ELSEIF (KIND (ZINC) == 4) THEN
    fxtran_acdc_alloc4 (ZINC)
ELSE
    STOP 1
ENDIF


IF (KIND (ZRCOLD) == 8) THEN
    fxtran_acdc_alloc8 (ZRCOLD)
ELSEIF (KIND (ZRCOLD) == 4) THEN
    fxtran_acdc_alloc4 (ZRCOLD)
ELSE
    STOP 1
ENDIF


IF (KIND (ZRSDIF) == 8) THEN
    fxtran_acdc_alloc8 (ZRSDIF)
ELSEIF (KIND (ZRSDIF) == 4) THEN
    fxtran_acdc_alloc4 (ZRSDIF)
ELSE
    STOP 1
ENDIF


IF (KIND (ZRFRAC) == 8) THEN
    fxtran_acdc_alloc8 (ZRFRAC)
ELSEIF (KIND (ZRFRAC) == 4) THEN
    fxtran_acdc_alloc4 (ZRFRAC)
ELSE
    STOP 1
ENDIF


IF (KIND (ZRSW) == 8) THEN
    fxtran_acdc_alloc8 (ZRSW)
ELSEIF (KIND (ZRSW) == 4) THEN
    fxtran_acdc_alloc4 (ZRSW)
ELSE
    STOP 1
ENDIF


IF (KIND (ZRSP) == 8) THEN
    fxtran_acdc_alloc8 (ZRSP)
ELSEIF (KIND (ZRSP) == 4) THEN
    fxtran_acdc_alloc4 (ZRSP)
ELSE
    STOP 1
ENDIF


IF (KIND (ESATW_T) == 8) THEN
    fxtran_acdc_alloc8 (ESATW_T)
ELSEIF (KIND (ESATW_T) == 4) THEN
    fxtran_acdc_alloc4 (ESATW_T)
ELSE
    STOP 1
ENDIF


IF (KIND (ZLWINC) == 8) THEN
    fxtran_acdc_alloc8 (ZLWINC)
ELSEIF (KIND (ZLWINC) == 4) THEN
    fxtran_acdc_alloc4 (ZLWINC)
ELSE
    STOP 1
ENDIF


IF (KIND (ZPRIFACT) == 8) THEN
    fxtran_acdc_alloc8 (ZPRIFACT)
ELSEIF (KIND (ZPRIFACT) == 4) THEN
    fxtran_acdc_alloc4 (ZPRIFACT)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDUM4) == 8) THEN
    fxtran_acdc_alloc8 (ZDUM4)
ELSEIF (KIND (ZDUM4) == 4) THEN
    fxtran_acdc_alloc4 (ZDUM4)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDUM3) == 8) THEN
    fxtran_acdc_alloc8 (ZDUM3)
ELSEIF (KIND (ZDUM3) == 4) THEN
    fxtran_acdc_alloc4 (ZDUM3)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDUM2) == 8) THEN
    fxtran_acdc_alloc8 (ZDUM2)
ELSEIF (KIND (ZDUM2) == 4) THEN
    fxtran_acdc_alloc4 (ZDUM2)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDUM1) == 8) THEN
    fxtran_acdc_alloc8 (ZDUM1)
ELSEIF (KIND (ZDUM1) == 4) THEN
    fxtran_acdc_alloc4 (ZDUM1)
ELSE
    STOP 1
ENDIF


IF (KIND (ZCLDINI) == 8) THEN
    fxtran_acdc_alloc8 (ZCLDINI)
ELSEIF (KIND (ZCLDINI) == 4) THEN
    fxtran_acdc_alloc4 (ZCLDINI)
ELSE
    STOP 1
ENDIF


IF (KIND (ZARDUM2) == 8) THEN
    fxtran_acdc_alloc8 (ZARDUM2)
ELSEIF (KIND (ZARDUM2) == 4) THEN
    fxtran_acdc_alloc4 (ZARDUM2)
ELSE
    STOP 1
ENDIF


IF (KIND (ZARDUM) == 8) THEN
    fxtran_acdc_alloc8 (ZARDUM)
ELSEIF (KIND (ZARDUM) == 4) THEN
    fxtran_acdc_alloc4 (ZARDUM)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDZ) == 8) THEN
    fxtran_acdc_alloc8 (ZDZ)
ELSEIF (KIND (ZDZ) == 4) THEN
    fxtran_acdc_alloc4 (ZDZ)
ELSE
    STOP 1
ENDIF


IF (KIND (ZICE_CLD_WGT) == 8) THEN
    fxtran_acdc_alloc8 (ZICE_CLD_WGT)
ELSEIF (KIND (ZICE_CLD_WGT) == 4) THEN
    fxtran_acdc_alloc4 (ZICE_CLD_WGT)
ELSE
    STOP 1
ENDIF


IF (KIND (ZSIGQSAT) == 8) THEN
    fxtran_acdc_alloc8 (ZSIGQSAT)
ELSEIF (KIND (ZSIGQSAT) == 4) THEN
    fxtran_acdc_alloc4 (ZSIGQSAT)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDZREF) == 8) THEN
    fxtran_acdc_alloc8 (ZDZREF)
ELSEIF (KIND (ZDZREF) == 4) THEN
    fxtran_acdc_alloc4 (ZDZREF)
ELSE
    STOP 1
ENDIF


IF (KIND (ZDZFACT) == 8) THEN
    fxtran_acdc_alloc8 (ZDZFACT)
ELSEIF (KIND (ZDZFACT) == 4) THEN
    fxtran_acdc_alloc4 (ZDZFACT)
ELSE
    STOP 1
ENDIF


JI = D%NIB


IKTB=D%NKTB
IKTE=D%NKTE
IKB=D%NKB
IKE=D%NKE
IKL=D%NKL
IIJB=D%NIB
IIJE=D%NIE

DO JK=IKTB, IKTE
  
  ZSIGQSAT (JI, JK)=PSIGQSAT (JI)
  
ENDDO


IF (PRESENT (PICE_CLD_WGT)) THEN

  DO JK=IKTB, IKTE
    
    ZICE_CLD_WGT (JI, JK)=PICE_CLD_WGT (JI)
  
  ENDDO

ENDIF

PCLDFR (JI,:)=0.
PSIGRC (JI,:)=0.
PRV_OUT (JI,:)=0.
PRC_OUT (JI,:)=0.
PRI_OUT (JI,:)=0.
ZPRIFACT (JI,:)=1.
ZARDUM2 (JI,:)=0.
ZARDUM (JI,:)=0.
ZCLDINI (JI,:)=-1.
PIFR (JI,:)=10.
ZDZREF (JI,:)=ICEP%XFRMIN (25)



DO JK=IKTB, IKTE
  
  ZRT (JI, JK)=PRV_IN (JI, JK)+PRC_IN (JI, JK)+PRI_IN (JI, JK)*ZPRIFACT (JI, JK)
  
ENDDO


IF (PRESENT (PLV).AND.PRESENT (PLS)) THEN
  ZLV (JI,:)=PLV (JI,:)
  ZLS (JI,:)=PLS (JI,:)
ELSE

  DO JK=IKTB, IKTE
    
    ZLV (JI, JK)=CST%XLVTT+(CST%XCPV-CST%XCL)*(PT (JI, JK)-CST%XTT)
    ZLS (JI, JK)=CST%XLSTT+(CST%XCPV-CST%XCI)*(PT (JI, JK)-CST%XTT)
  
  ENDDO

ENDIF


IF (PRESENT (PCPH)) THEN
  ZCPD (JI,:)=PCPH (JI,:)
ELSE

  DO JK=IKTB, IKTE
    
    ZCPD (JI, JK)=CST%XCPD+CST%XCPV*PRV_IN (JI, JK)+CST%XCL*PRC_IN (JI, JK)+CST%XCI&
    &*PRI_IN (JI, JK)+CST%XCL*PRR (JI, JK)+CST%XCI*(PRS (JI, JK)+PRG (JI, JK))
  
  ENDDO

ENDIF


IF (.NOT.OSIGMAS) THEN

  DO JK=IKTB, IKTE
    
    ZTLK (JI, JK)=PT (JI, JK)-ZLV (JI, JK)*PRC_IN (JI, JK)/ZCPD (JI, JK&
    &)-ZLS (JI, JK)*PRI_IN (JI, JK)/ZCPD (JI, JK)*ZPRIFACT (JI, JK)
  
  ENDDO

  ITPL =IKB+IKL
  ZTMIN =400.

  DO JK=IKTB+1, IKTE-1

    

    IF (PT (JI, JK)<ZTMIN ) THEN
      ZTMIN =PT (JI, JK)
      ITPL =JK
    ENDIF

  
  ENDDO

  ZL (JI, IKB)=20.

  DO JK=IKB+IKL, IKE, IKL
    
    ZL (JI, JK)=ZL0
    ZZZ (JI, JK)=PZZ (JI, JK)-PZZ (JI, IKB)
    JKP=ITPL 

    IF (ZL0>ZZZ (JI, JK))  THEN
      ZL (JI, JK)=ZZZ (JI, JK)
    ENDIF


    IF (ZZZ (JI, JK)>0.9*(PZZ (JI, JKP)-PZZ (JI, IKB)))  THEN
      ZL (JI, JK)=.6*ZL (JI, JK-IKL)
    ENDIF

  
  ENDDO

ENDIF


DO JK=IKTB, IKTE
  JKPK (JK)=MAX (MIN (JK+IKL, IKTE), IKTB)
  JKMK (JK)=MAX (MIN (JK-IKL, IKTE), IKTB)
ENDDO


#ifdef MNH_COMPILER_CCE
#endif

DO JK=IKTB, IKTE
  
  
  ZPV (JI, JK)=MIN (EXP (CST%XALPW-CST%XBETAW/PT (JI, JK&
  &)-CST%XGAMW*LOG (PT (JI, JK))),.99*PPABS (JI, JK))
  ZPIV (JI, JK)=MIN (EXP (CST%XALPI-CST%XBETAI/PT (JI, JK&
  &)-CST%XGAMI*LOG (PT (JI, JK))),.99*PPABS (JI, JK))
  
  ZFRAC (JI, JK)=0.

  IF (OUSERI) THEN

    IF (PRC_IN (JI, JK)+PRI_IN (JI, JK)>1.E-20) THEN
      ZFRAC (JI, JK)=PRI_IN (JI, JK)/(PRC_IN (JI, JK)+PRI_IN (JI, JK))
    ENDIF


    

    SELECTCASE (HFRAC_ICE)
    CASE ('T')
      ZFRAC (JI, JK)=MAX (0., MIN (1.,((NEBN%XTMAXMIX-PT (JI, JK))/(NEBN%XTMAXMIX-NEBN%XTMINMIX))))
    CASE ('O')
      ZFRAC (JI, JK)=MAX (0., MIN (1.,((CST%XTT-PT (JI, JK))/40.)))
    CASE ('N')
      ZFRAC (JI, JK)=0.
    CASE ('S')
      ZFRAC (JI, JK)=MAX (0., MIN (1., ZFRAC (JI, JK)))
    ENDSELECT

  
  
  ENDIF

  ZQSL (JI, JK)=CST%XRD/CST%XRV*ZPV (JI, JK)/(PPABS (JI, JK)-ZPV (JI, JK))
  ZQSI (JI, JK)=CST%XRD/CST%XRV*ZPIV (JI, JK)/(PPABS (JI, JK)-ZPIV (JI, JK))
  ZQSL (JI, JK)=(1.-ZFRAC (JI, JK))*ZQSL (JI, JK)+ZFRAC (JI, JK)*ZQSI (JI, JK)
  ZLVS (JI, JK)=(1.-ZFRAC (JI, JK))*ZLV (JI, JK)+ZFRAC (JI, JK)*ZLS (JI, JK)
  ZAH (JI, JK)=ZLVS (JI, JK)*ZQSL (JI, JK)/(CST%XRV*PT (JI, JK)**2)*(CST%XRV*ZQSL (JI, JK)/CST%XRD+1.)
  ZA (JI, JK)=1./(1.+ZLVS (JI, JK)/ZCPD (JI, JK)*ZAH (JI, JK))
  ZB (JI, JK)=ZAH (JI, JK)*ZA (JI, JK)
  ZSBAR (JI, JK)=ZA (JI, JK)*(ZRT (JI, JK)-ZQSL (JI, JK)+ZAH (JI, JK)*ZLVS (JI&
  &, JK)*(PRC_IN (JI, JK)+PRI_IN (JI, JK)*ZPRIFACT (JI, JK))/ZCPD (JI, JK))

  IF (OSIGMAS) THEN

    IF (ZSIGQSAT (JI, JK)/=0.) THEN
      ZDZFACT (JI, JK)=1.

      IF (NEBN%LHGT_QS.AND.JK+1<=IKTE) THEN
        ZDZFACT (JI, JK)=MAX (ICEP%XFRMIN (23), MIN (ICEP%XFRMIN&
        & (24),(PZZ (JI, JK)-PZZ (JI, JK+1))/ZDZREF (JI, JK)))
      ELSEIF (NEBN%LHGT_QS) THEN
        ZDZFACT (JI, JK)=MAX (ICEP%XFRMIN (23), MIN (ICEP%XFRMIN (24&
        &),((PZZ (JI, JK-1)-PZZ (JI, JK)))*0.8/ZDZREF (JI, JK)))
      ENDIF


      IF (NEBN%LSTATNW) THEN
        ZSIGMA (JI, JK)=SQRT ((PSIGS (JI, JK))**2+(ZSIGQSAT (JI,&
        & JK)*ZDZFACT (JI, JK)*ZQSL (JI, JK)*ZA (JI, JK))**2)
      ELSE
        ZSIGMA (JI, JK)=SQRT ((2*PSIGS (JI, JK))**2+(ZSIGQSAT (JI, JK)*ZQSL (JI, JK)*ZA (JI, JK))**2)
      ENDIF

    ELSE

      IF (NEBN%LSTATNW) THEN
        ZSIGMA (JI, JK)=PSIGS (JI, JK)
      ELSE
        ZSIGMA (JI, JK)=2*PSIGS (JI, JK)
      ENDIF

    ENDIF


    IF (NEBN%LCONDBORN) THEN
      ZSIGMA (JI, JK)=MIN (ZSIGMA (JI, JK), ZRT (JI, JK)/2.)
    ENDIF

  ELSE
    DZZ (JI, JK)=PZZ (JI, JKPK (JK))-PZZ (JI, JKMK (JK))
    ZDRW (JI, JK)=ZRT (JI, JKPK (JK))-ZRT (JI, JKMK (JK))
    ZDTL (JI, JK)=ZTLK (JI, JKPK (JK))-ZTLK (JI, JKMK (JK))+CST%XG/ZCPD (JI, JK)*DZZ (JI, JK)
    ZLL (JI, JK)=ZL (JI, JK)
    ZSIG_CONV (JI, JK)=0.

    IF (LMFCONV)  THEN
      ZSIG_CONV (JI, JK)=ZCSIG_CONV*PMFCONV (JI, JK)/ZA (JI, JK)
    ENDIF

    ZSIGMA (JI, JK)=SQRT (MAX (1.E-25, ZCSIGMA*ZCSIGMA*ZLL (JI, JK)*ZLL (JI, JK)/(DZZ&
    & (JI, JK)*DZZ (JI, JK))*(ZA (JI, JK)*ZA (JI, JK)*ZDRW (JI, JK)*ZDRW (JI, JK)-2&
    &.*ZA (JI, JK)*ZB (JI, JK)*ZDRW (JI, JK)*ZDTL (JI, JK)+ZB (JI, JK)*ZB (JI, JK)&
    &*ZDTL (JI, JK)*ZDTL (JI, JK))+ZSIG_CONV (JI, JK)*ZSIG_CONV (JI, JK)))
  ENDIF

  ZSIGMA (JI, JK)=MAX (1.E-10, ZSIGMA (JI, JK))
  ZQ1 (JI, JK)=ZSBAR (JI, JK)/ZSIGMA (JI, JK)

  IF (HCONDENS=='GAUS') THEN
    ZGCOND (JI, JK)=-ZQ1 (JI, JK)/SQRT (2.)
    ZGAUV (JI, JK)=1+ERF (-ZGCOND (JI, JK))
    PCLDFR (JI, JK)=MAX (0., MIN (1., 0.5*ZGAUV (JI, JK)))
    ZCOND (JI, JK)=(EXP (-ZGCOND (JI, JK)**2)-ZGCOND (JI, JK)*SQRT &
    &(CST%XPI)*ZGAUV (JI, JK))*ZSIGMA (JI, JK)/SQRT (2.*CST%XPI)
    ZCOND (JI, JK)=MAX (ZCOND (JI, JK), 0.)

    IF (NEBN%LCONDBORN) THEN
      ZCOND (JI, JK)=MIN (ZCOND (JI, JK), ZRT (JI, JK)-1.E-7)
    ENDIF


    IF (ZCOND (JI, JK)<1.E-12.OR.PCLDFR (JI, JK)==0.) THEN
      PCLDFR (JI, JK)=0.
      ZCOND (JI, JK)=0.
    ENDIF

    PSIGRC (JI, JK)=PCLDFR (JI, JK)

    IF (PRESENT (PHLC_HCF).AND.PRESENT (PHLC_HRC)) THEN

      IF (1-ZFRAC (JI, JK)>1.E-20) THEN
        ZAUTC (JI, JK)=(ZSBAR (JI, JK)-ICEP%XCRIAUTC/(PRHODREF (JI, JK)*(1-ZFRAC (JI, JK))))/ZSIGMA (JI, JK)
        ZGAUTC (JI, JK)=-ZAUTC (JI, JK)/SQRT (2.)
        ZGAUC (JI, JK)=1+ERF (-ZGAUTC (JI, JK))
        PHLC_HCF (JI, JK)=MAX (0., MIN (1., 0.5*ZGAUC (JI, JK)))
        PHLC_HRC (JI, JK)=(1-ZFRAC (JI, JK))*(EXP (-ZGAUTC (JI, JK)**2)-ZGAUTC (JI&
        &, JK)*SQRT (CST%XPI)*ZGAUC (JI, JK))*ZSIGMA (JI, JK)/SQRT (2.*CST%XPI)
        PHLC_HRC (JI, JK)=PHLC_HRC (JI, JK)+ICEP%XCRIAUTC/PRHODREF (JI, JK)*PHLC_HCF (JI, JK)
        PHLC_HRC (JI, JK)=MAX (PHLC_HRC (JI, JK), 0.)

        IF (PHLC_HRC (JI, JK)<1.E-12.OR.PHLC_HCF (JI, JK)<1.E-6) THEN
          PHLC_HRC (JI, JK)=0.
          PHLC_HCF (JI, JK)=0.
        ENDIF

      ELSE
        PHLC_HCF (JI, JK)=0.
        PHLC_HRC (JI, JK)=0.
      ENDIF

    ENDIF


    IF (PRESENT (PHLI_HCF).AND.PRESENT (PHLI_HRI)) THEN

      IF (ZFRAC (JI, JK)>1.E-20) THEN
        ZCRIAUTI (JI, JK)=MIN (ICEP%XCRIAUTI, 10**(ICEP%XACRIAUTI*(PT (JI, JK)-CST%XTT)+ICEP%XBCRIAUTI))
        ZAUTI (JI, JK)=(ZSBAR (JI, JK)-ZCRIAUTI (JI, JK)/ZFRAC (JI, JK))/ZSIGMA (JI, JK)
        ZGAUTI (JI, JK)=-ZAUTI (JI, JK)/SQRT (2.)
        ZGAUI (JI, JK)=1+ERF (-ZGAUTI (JI, JK))
        PHLI_HCF (JI, JK)=MAX (0., MIN (1., 0.5*ZGAUI (JI, JK)))
        PHLI_HRI (JI, JK)=ZFRAC (JI, JK)*(EXP (-ZGAUTI (JI, JK)**2)-ZGAUTI (JI, JK&
        &)*SQRT (CST%XPI)*ZGAUI (JI, JK))*ZSIGMA (JI, JK)/SQRT (2.*CST%XPI)
        PHLI_HRI (JI, JK)=PHLI_HRI (JI, JK)+ZCRIAUTI (JI, JK)*PHLI_HCF (JI, JK)
        PHLI_HRI (JI, JK)=MAX (PHLI_HRI (JI, JK), 0.)

        IF (PHLI_HRI (JI, JK)<1.E-12.OR.PHLI_HCF (JI, JK)<1.E-6) THEN
          PHLI_HRI (JI, JK)=0.
          PHLI_HCF (JI, JK)=0.
        ENDIF

      ELSE
        PHLI_HCF (JI, JK)=0.
        PHLI_HRI (JI, JK)=0.
      ENDIF

    ENDIF

  ELSEIF (HCONDENS=='CB02') THEN

    IF (ZQ1 (JI, JK)>0..AND.ZQ1 (JI, JK)<=2) THEN
      ZCOND (JI, JK)=MIN (EXP (-1.)+.66*ZQ1 (JI, JK)+.086*ZQ1 (JI, JK)**2, 2.)
    ELSEIF (ZQ1 (JI, JK)>2.) THEN
      ZCOND (JI, JK)=ZQ1 (JI, JK)
    ELSE
      ZCOND (JI, JK)=EXP (1.2*ZQ1 (JI, JK)-1.)
    ENDIF

    ZCOND (JI, JK)=ZCOND (JI, JK)*ZSIGMA (JI, JK)

    IF (NEBN%LCONDBORN) THEN
      ZCOND (JI, JK)=MIN (ZCOND (JI, JK), ZRT (JI, JK)-1.E-7)
    ENDIF


    IF (ZCOND (JI, JK)<1.E-12) THEN
      PCLDFR (JI, JK)=0.
    ELSE
      PCLDFR (JI, JK)=MAX (0., MIN (1., 0.5+0.36*ATAN (1.55*ZQ1 (JI, JK))))
    ENDIF


    IF (PCLDFR (JI, JK)==0.) THEN
      ZCOND (JI, JK)=0.
    ENDIF

    INQ1 (JI, JK)=MIN (MAX (-22, FLOOR (MIN (100., MAX (-100., 2*ZQ1 (JI, JK))))), 10)
    ZINC (JI, JK)=2.*ZQ1 (JI, JK)-INQ1 (JI, JK)
    PSIGRC (JI, JK)=MIN (1.,(1.-ZINC (JI, JK))*ZSRC_1D (INQ1&
    & (JI, JK))+ZINC (JI, JK)*ZSRC_1D (INQ1 (JI, JK)+1))

    IF (PRESENT (PHLC_HCF).AND.PRESENT (PHLC_HRC)) THEN
      PHLC_HCF (JI, JK)=0.
      PHLC_HRC (JI, JK)=0.
    ENDIF


    IF (PRESENT (PHLI_HCF).AND.PRESENT (PHLI_HRI)) THEN
      PHLI_HCF (JI, JK)=0.
      PHLI_HRI (JI, JK)=0.
    ENDIF

  ENDIF

  
  PRC_OUT (JI, JK)=(1.-ZFRAC (JI, JK))*ZCOND (JI, JK)
  PRI_OUT (JI, JK)=ZFRAC (JI, JK)*ZCOND (JI, JK)
  PT (JI, JK)=PT (JI, JK)+((PRC_OUT (JI, JK)-PRC_IN (JI, JK))*ZLV (JI, JK&
  &)+(PRI_OUT (JI, JK)-PRI_IN (JI, JK))*ZLS (JI, JK))/ZCPD (JI, JK)
  PRV_OUT (JI, JK)=ZRT (JI, JK)-PRC_OUT (JI, JK)-PRI_OUT (JI, JK)*ZPRIFACT (JI, JK)

  

  IF (HLAMBDA3=='CB') THEN
    PSIGRC (JI, JK)=PSIGRC (JI, JK)*MIN (3., MAX (1., 1.-ZQ1 (JI, JK)))
  ENDIF

  
ENDDO





END SUBROUTINE CONDENSATION_OPENACC

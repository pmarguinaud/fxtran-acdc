SUBROUTINE CPXFU(YDXFU, YDPRECIPS, YDCPG_OPTS, YDCPG_BNDS, YDMF_PHYS_OUT, YDCPG_MISC, PSTRCU, PSTRCV, &
& PSTRDU, PSTRDV, PSTRTU, PSTRTV, PXUCLS, PXVCLS, PXNUCLS, PXNVCLS, PXUGST, PXVGST, PDPRECIPS,        &
& PDPRECIPS2)

!$ACDC GENERATE, TARGET=Parallel
!$ACDC GENERATE, TARGET=SyncHost/SingleColumnFieldAPIHost/FieldAPIHost
!$ACDC GENERATE, TARGET=SyncDevice/SingleColumnFieldAPIDevice

!**** *CPXFU* - INTERFACE FOR INSTANTANEOUS FLUXES.

!     Purpose.
!     --------
!           DIAGNOSTICS OF PHYSICAL FLUXES.

!**   Interface.
!     ----------
!        *CALL* *CPXFU*

!        Explicit arguments :
!        --------------------

!       NPROMA                 - HORIZONTAL DIMENSION                 (INPUT)
!       KGL1,KGL2              - KGL1 TO KGL2 LATITUDES OF WORK.      (INPUT)
!       KSTABUF                - KSTABUF(JGL) FOR LATITUDE JGL        (INPUT)
!       FLUXES COMING FROM THE PHYSICAL PARAMETERIZATIONS             (INPUT)

!        --------------------

!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------
!      O. CLARY
!      Original : 06/92 D'APRES CUMCO

!     Modifications.
!     --------------
!      Modified : 01-08-07 by R. El Khatib - Pruning options
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      M.Hamrud      10-Jan-2004 CY28R1 Cleaning
!      Y.Bouteloup   26-Mars-2004 introduction of MTS radiance
!      F.Bouyssel    10-Nov-2005 introduction of PQLI,PQICE
!      Y.Seity       15-Jan-2007 introduction of Graupel and Hail
!      Y.Seity       30-Jan-2008 introduction of Theta'w' flux
!      S. Riette    : 05-may-2009 extreme gusts and mean cls wind added
!      S. Riette     24-Feb-2010 max gusts over NXGSTPERIOD seconds
!      Y. Seity     04-Jan-2011 Hail diagnostic for AROME
!      Y. Seity     04-Sept-2017 gust calculation over 2 different periods
!      R. El Khatib 05-Jun-2018 computation of periods moved from cnt4 (OOPS refactoring)
!      R. Brozkova  Sep-2018 added mean radiant temperature
!     ------------------------------------------------------------------

USE CPG_OPTS_TYPE_MOD, ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE CPG_TYPE_MOD     , ONLY : CPG_MISC_TYPE
USE PARKIND1         , ONLY : JPIM, JPRB
USE YOMHOOK          , ONLY : DR_HOOK, LHOOK
USE YOMXFU           , ONLY : TXFU
USE YOMDPRECIPS      , ONLY : TDPRECIPS
USE MF_PHYS_TYPE_MOD , ONLY : MF_PHYS_OUT_TYPE
USE TYPE_FLUXES      , ONLY : FLUXES_DESCRIPTOR

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TXFU)              ,INTENT(INOUT)            :: YDXFU
TYPE(TDPRECIPS)         ,INTENT(IN)               :: YDPRECIPS
TYPE(CPG_OPTS_TYPE)     ,INTENT(IN)               :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)     ,INTENT(IN)               :: YDCPG_BNDS
TYPE(MF_PHYS_OUT_TYPE)  ,INTENT(IN)               :: YDMF_PHYS_OUT
TYPE(CPG_MISC_TYPE)     ,INTENT(IN)               :: YDCPG_MISC
 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PSTRCU(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PSTRCV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PSTRDU(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PSTRDV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PSTRTU(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PSTRTV(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PXUCLS(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PXVCLS(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PXNUCLS(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PXNVCLS(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PXUGST(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PXVGST(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)         ,INTENT(IN)               :: PDPRECIPS(YDCPG_OPTS%KLON,YDPRECIPS%NDTPREC)
REAL(KIND=JPRB)         ,INTENT(IN)               :: PDPRECIPS2(YDCPG_OPTS%KLON,YDPRECIPS%NDTPREC2)


!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZGST
REAL(KIND=JPRB) :: ZGST2
LOGICAL         :: LLGST(YDCPG_OPTS%KLON)
LOGICAL         :: LLGST2(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZUMEAN(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZVMEAN(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZNUMEAN(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZNVMEAN(YDCPG_OPTS%KLON)

INTEGER(KIND=JPIM) :: JLON, JLEV

REAL(KIND=JPRB) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "dprecips_xfu.intfb.h"
#include "meanwind_xfu.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CPXFU', 0, ZHOOK_HANDLE)

ASSOCIATE(LRESET=>YDXFU%LRESET, LRESET_GST=>YDXFU%LRESET_GST, LRESET_GST2=>YDXFU%LRESET_GST2, LXMWINDCLS=>YDXFU%LXMWINDCLS,       &
& LXCLS=>YDXFU%LXCLS, LXNUVCLS=>YDXFU%LXNUVCLS, LXXGST=>YDXFU%LXXGST, LXXGST2=>YDXFU%LXXGST2, NMEANSTEPS=>YDXFU%NMEANSTEPS,       &
& LRESET_VISI=>YDXFU%LRESET_VISI,                                                                                                 &
& LRESET_VISI2=>YDXFU%LRESET_VISI2, LRESET_PRECIP=>YDXFU%LRESET_PRECIP)
!     ------------------------------------------------------------------

!$ACDC PARALLEL, TARGET=HOST {

!     ------------------------------------------------------------------

!*    2.    COPY FLUXES
!           -----------

!*    2.1   Extreme gusts
IF (LXXGST) THEN
  DO JLON=YDCPG_BNDS%KIDIA,YDCPG_BNDS%KFDIA
    IF (LRESET_GST) THEN
      ZGST=-1.0_JPRB
    ELSE
      ZGST=YDXFU%UGST(JLON)**2+YDXFU%VGST(JLON)**2
    ENDIF
    LLGST(JLON)=PXUGST(JLON)**2+PXVGST(JLON)**2>ZGST
  ENDDO
ENDIF

IF (LXXGST2) THEN
  DO JLON=YDCPG_BNDS%KIDIA,YDCPG_BNDS%KFDIA
    IF (LRESET_GST2) THEN
      ZGST2=-1.0_JPRB
    ELSE
      ZGST2=YDXFU%UGST2(JLON)**2+YDXFU%VGST2(JLON)**2
    ENDIF
    LLGST2(JLON)=PXUGST(JLON)**2+PXVGST(JLON)**2>ZGST2
  ENDDO
ENDIF


!*    2.2   Mean cls real/neutral wind
IF (LXMWINDCLS .AND. NMEANSTEPS/=0) THEN
  ! MEANWIND subroutine contained here (in CPXFU)
  IF (LXCLS) THEN
    CALL MEANWIND_XFU (YDCPG_OPTS, YDCPG_BNDS, NMEANSTEPS, YDXFU%UCLS, YDXFU%VCLS, PXUCLS, PXVCLS, YDXFU%RMWINDCALC, ZUMEAN, ZVMEAN)
  ENDIF

  IF (LXNUVCLS) THEN
    CALL MEANWIND_XFU (YDCPG_OPTS, YDCPG_BNDS, NMEANSTEPS, YDXFU%NUCLS, YDXFU%NVCLS, PXNUCLS, PXNVCLS, YDXFU%RMNWINDCALC, ZNUMEAN, ZNVMEAN)
  ENDIF
ELSEIF (LXMWINDCLS) THEN
  ZUMEAN  = 0._JPRB
  ZVMEAN  = 0._JPRB
  ZNUMEAN = 0._JPRB
  ZNVMEAN = 0._JPRB
ENDIF

!$ACDC }

!$ACDC PARALLEL, TARGET=HOST {

IF (YDXFU%YTRDU%LACTIVE) CALL COPY3_XFU (YDXFU%YTRDU, YDXFU%TRDU, PSTRDU)
IF (YDXFU%YTRDV%LACTIVE) CALL COPY3_XFU (YDXFU%YTRDV, YDXFU%TRDV, PSTRDV)
IF (YDXFU%YTRCU%LACTIVE) CALL COPY3_XFU (YDXFU%YTRCU, YDXFU%TRCU, PSTRCU)
IF (YDXFU%YTRCV%LACTIVE) CALL COPY3_XFU (YDXFU%YTRCV, YDXFU%TRCV, PSTRCV)
IF (YDXFU%YDICQ%LACTIVE) CALL COPY3_XFU (YDXFU%YDICQ, YDXFU%DICQ, YDMF_PHYS_OUT%DIFCQ)
IF (YDXFU%YDICS%LACTIVE) CALL COPY3_XFU (YDXFU%YDICS, YDXFU%DICS, YDMF_PHYS_OUT%DIFCS)
IF (YDXFU%YTRTU%LACTIVE) CALL COPY3_XFU (YDXFU%YTRTU, YDXFU%TRTU, PSTRTU)
IF (YDXFU%YTRTV%LACTIVE) CALL COPY3_XFU (YDXFU%YTRTV, YDXFU%TRTV, PSTRTV)
IF (YDXFU%YDITQ%LACTIVE) CALL COPY3_XFU (YDXFU%YDITQ, YDXFU%DITQ, YDMF_PHYS_OUT%DIFTQ)
IF (YDXFU%YDITS%LACTIVE) CALL COPY3_XFU (YDXFU%YDITS, YDXFU%DITS, YDMF_PHYS_OUT%DIFTS)
IF (YDXFU%YPLCL%LACTIVE) CALL COPY3_XFU (YDXFU%YPLCL, YDXFU%PLCL, YDMF_PHYS_OUT%FPLCL)
IF (YDXFU%YPLCN%LACTIVE) CALL COPY3_XFU (YDXFU%YPLCN, YDXFU%PLCN, YDMF_PHYS_OUT%FPLCN)
IF (YDXFU%YPLCG%LACTIVE) CALL COPY3_XFU (YDXFU%YPLCG, YDXFU%PLCG, YDMF_PHYS_OUT%FPLCG)
IF (YDXFU%YPLCH%LACTIVE) CALL COPY3_XFU (YDXFU%YPLCH, YDXFU%PLCH, YDMF_PHYS_OUT%FPLCH)
IF (YDXFU%YPLSL%LACTIVE) CALL COPY3_XFU (YDXFU%YPLSL, YDXFU%PLSL, YDMF_PHYS_OUT%FPLSL)
IF (YDXFU%YPLSN%LACTIVE) CALL COPY3_XFU (YDXFU%YPLSN, YDXFU%PLSN, YDMF_PHYS_OUT%FPLSN)
IF (YDXFU%YPLSG%LACTIVE) CALL COPY3_XFU (YDXFU%YPLSG, YDXFU%PLSG, YDMF_PHYS_OUT%FPLSG)
IF (YDXFU%YPLSH%LACTIVE) CALL COPY3_XFU (YDXFU%YPLSH, YDXFU%PLSH, YDMF_PHYS_OUT%FPLSH)
IF (YDXFU%YRSO %LACTIVE) CALL COPY3_XFU (YDXFU%YRSO , YDXFU%RSO , YDMF_PHYS_OUT%FRSO(:,:,1))
IF (YDXFU%YRTH %LACTIVE) CALL COPY3_XFU (YDXFU%YRTH , YDXFU%RTH , YDMF_PHYS_OUT%FRTH(:,:,1))
IF (YDXFU%YNEB %LACTIVE) CALL COPY3_XFU (YDXFU%YNEB , YDXFU%NEB , YDCPG_MISC%NEB)
IF (YDXFU%YQICE%LACTIVE) CALL COPY3_XFU (YDXFU%YQICE, YDXFU%QICE, YDCPG_MISC%QICE)
IF (YDXFU%YQLI %LACTIVE) CALL COPY3_XFU (YDXFU%YQLI , YDXFU%QLI , YDCPG_MISC%QLI)

IF (YDXFU%YNEBT %LACTIVE) CALL COPY1_XFU (YDXFU%NEBT , YDCPG_MISC%CLCT     )
IF (YDXFU%YFEVL %LACTIVE) CALL COPY1_XFU (YDXFU%FEVL , YDMF_PHYS_OUT%FEVL(:,1))
IF (YDXFU%YFONTE%LACTIVE) CALL COPY1_XFU (YDXFU%FONTE, YDMF_PHYS_OUT%FONTE )
IF (YDXFU%YFCHSP%LACTIVE) CALL COPY1_XFU (YDXFU%FCHSP, YDMF_PHYS_OUT%FCHSP(:,1))
IF (YDXFU%YFLWSP%LACTIVE) CALL COPY1_XFU (YDXFU%FLWSP, YDMF_PHYS_OUT%FLWSP )
IF (YDXFU%YRUISS%LACTIVE) CALL COPY1_XFU (YDXFU%RUISS, YDMF_PHYS_OUT%RUISS )
IF (YDXFU%YRUISP%LACTIVE) CALL COPY1_XFU (YDXFU%RUISP, YDMF_PHYS_OUT%RUISP )
IF (YDXFU%YFEVV %LACTIVE) CALL COPY1_XFU (YDXFU%FEVV , YDMF_PHYS_OUT%FEVV  )
IF (YDXFU%YFTR  %LACTIVE) CALL COPY1_XFU (YDXFU%FTR  , YDMF_PHYS_OUT%FTR   )
IF (YDXFU%YRUISL%LACTIVE) CALL COPY1_XFU (YDXFU%RUISL, YDMF_PHYS_OUT%RUISL )
IF (YDXFU%YNBCON%LACTIVE) CALL COPY1_XFU (YDXFU%NBCON, YDMF_PHYS_OUT%CLCC  )
IF (YDXFU%YNBHAU%LACTIVE) CALL COPY1_XFU (YDXFU%NBHAU, YDMF_PHYS_OUT%CLCH  )
IF (YDXFU%YNBMOY%LACTIVE) CALL COPY1_XFU (YDXFU%NBMOY, YDMF_PHYS_OUT%CLCM  )
IF (YDXFU%YNBBAS%LACTIVE) CALL COPY1_XFU (YDXFU%NBBAS, YDMF_PHYS_OUT%CLCL  )
IF (YDXFU%YCAPE %LACTIVE) CALL COPY1_XFU (YDXFU%CAPE , YDMF_PHYS_OUT%CAPE  )
IF (YDXFU%YCTOP %LACTIVE) CALL COPY1_XFU (YDXFU%CTOP , YDMF_PHYS_OUT%CTOP  )
IF (YDXFU%YMOCON%LACTIVE) CALL COPY1_XFU (YDXFU%MOCON, YDMF_PHYS_OUT%MOCON )
IF (YDXFU%YCLPH %LACTIVE) CALL COPY1_XFU (YDXFU%CLPH , YDMF_PHYS_OUT%CLPH  )
IF (YDXFU%YVEIN %LACTIVE) CALL COPY1_XFU (YDXFU%VEIN , YDMF_PHYS_OUT%VEIN  )
IF (YDXFU%YMRT  %LACTIVE) CALL COPY1_XFU (YDXFU%MRT  , YDMF_PHYS_OUT%MRT   )
IF (YDXFU%YTCLS %LACTIVE) CALL COPY1_XFU (YDXFU%TCLS , YDMF_PHYS_OUT%TCLS  )
IF (YDXFU%YSIC  %LACTIVE) CALL COPY1_XFU (YDXFU%SIC  , YDMF_PHYS_OUT%SIC   )
IF (YDXFU%YQCLS %LACTIVE) CALL COPY1_XFU (YDXFU%QCLS , YDMF_PHYS_OUT%QCLS  )
IF (YDXFU%YUGST %LACTIVE) CALL COPY1_XFU (YDXFU%UGST , PXUGST, LDMASK=LXXGST, LDVMASK=LLGST)
IF (YDXFU%YVGST %LACTIVE) CALL COPY1_XFU (YDXFU%VGST , PXVGST, LDMASK=LXXGST, LDVMASK=LLGST)
IF (YDXFU%YUGST2%LACTIVE .AND. LXXGST2) CALL COPY1_XFU (YDXFU%UGST2, PXUGST, LDMASK=.TRUE., LDVMASK=LLGST2)
IF (YDXFU%YVGST2%LACTIVE .AND. LXXGST2) CALL COPY1_XFU (YDXFU%VGST2, PXVGST, LDMASK=.TRUE., LDVMASK=LLGST2)
IF (YDXFU%YUCLS %LACTIVE) CALL COPY1_XFU (YDXFU%UCLS , ZUMEAN , PXUCLS , LDCOND=LXMWINDCLS)
IF (YDXFU%YVCLS %LACTIVE) CALL COPY1_XFU (YDXFU%VCLS , ZVMEAN , PXVCLS , LDCOND=LXMWINDCLS)
IF (YDXFU%YNUCLS%LACTIVE) CALL COPY1_XFU (YDXFU%NUCLS, ZNUMEAN, PXNUCLS, LDCOND=LXMWINDCLS)
IF (YDXFU%YNVCLS%LACTIVE) CALL COPY1_XFU (YDXFU%NVCLS, ZNVMEAN, PXNVCLS, LDCOND=LXMWINDCLS)

IF (YDXFU%YXDIAGH%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%XDIAGH, PVMAX=YDMF_PHYS_OUT%DIAGH, LDRESET=LRESET_GST, PRESET=0.0_JPRB)
ENDIF

IF (YDXFU%YVISICLD%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%VISICLD, PVMIN=YDMF_PHYS_OUT%VISICLD, PSMAX=0.0_JPRB, &
            & LDRESET=LRESET_VISI, PRESET=20000.0_JPRB)
ENDIF

IF (YDXFU%YVISIHYD%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%VISIHYD, PVMIN=YDMF_PHYS_OUT%VISIHYD, PSMAX=0.0_JPRB, &
            & LDRESET=LRESET_VISI, PRESET=20000.0_JPRB)
ENDIF

IF (YDXFU%YCLWC%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%CLWC, PVMAX=YDMF_PHYS_OUT%MXCLWC, LDRESET=LRESET_VISI, PRESET=0.0_JPRB)
  YDXFU%CLWC(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=MAX(YDXFU%CLWC(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA),0.0_JPRB)
ENDIF

IF (YDXFU%YVISICLD2%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%VISICLD2, PVMIN=YDMF_PHYS_OUT%VISICLD, PSMAX=0.0_JPRB, &
            & LDRESET=LRESET_VISI2, PRESET=20000.0_JPRB)
ENDIF

IF (YDXFU%YVISIHYD2%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%VISIHYD2, PVMIN=YDMF_PHYS_OUT%VISIHYD, PSMAX=0.0_JPRB, &
            & LDRESET=LRESET_VISI2, PRESET=20000.0_JPRB)
ENDIF

IF (YDXFU%YCLWC2%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%CLWC2, PVMAX=YDMF_PHYS_OUT%MXCLWC, LDRESET=LRESET_VISI2, PRESET=0.0_JPRB)
  YDXFU%CLWC2(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=MAX(YDXFU%CLWC2(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA),0.0_JPRB)
ENDIF

IF (YDXFU%YTPWCLS%LACTIVE) CALL COPY1_XFU (YDXFU%TPWCLS, YDMF_PHYS_OUT%TPWCLS)

IF (YDXFU%YRHCLS%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%RHCLS, YDMF_PHYS_OUT%RHCLS)
  YDXFU%RHCLS(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=MIN(1.0_JPRB,MAX(0.0_JPRB,YDXFU%RHCLS(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)))
ENDIF

IF (YDXFU%YTN%LACTIVE) CALL COPY1_XFU (YDXFU%TN, PVMIN=YDMF_PHYS_OUT%TCLS, LDRESET=LRESET, PRESET=100000._JPRB)
IF (YDXFU%YTX%LACTIVE) CALL COPY1_XFU (YDXFU%TX, PVMAX=YDMF_PHYS_OUT%TCLS, LDRESET=LRESET, PRESET=0.0_JPRB)

IF (YDXFU%YHUN%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%HUN, PVMIN=YDMF_PHYS_OUT%RHCLS, LDRESET=LRESET, PRESET=1.0_JPRB)
  YDXFU%HUN(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=MAX(0.0_JPRB,MIN(1.0_JPRB,YDXFU%HUN(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)))
ENDIF

IF (YDXFU%YHUX%LACTIVE) THEN
  CALL COPY1_XFU (YDXFU%HUX, PVMAX=YDMF_PHYS_OUT%RHCLS, LDRESET=LRESET, PRESET=0.0_JPRB)
  YDXFU%HUX(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)=MAX(0.0_JPRB,MIN(1.0_JPRB,YDXFU%HUX(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)))
ENDIF

IF (YDXFU%YTHW%LACTIVE) CALL COPY1_XFU (YDXFU%THW, YDMF_PHYS_OUT%FCS (:,1))

IF (YDXFU%YPTYPE%LACTIVE) THEN
  CALL DPRECIPS_XFU (YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA, YDCPG_OPTS%KLON, YDPRECIPS%NDTPREC, 0_JPIM, PDPRECIPS, YDXFU%PTYPE, LRESET_PRECIP)
ENDIF
 
IF (YDXFU%YPTYPESEV%LACTIVE) THEN
  CALL DPRECIPS_XFU (YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA, YDCPG_OPTS%KLON, YDPRECIPS%NDTPREC, 2_JPIM, PDPRECIPS, YDXFU%PTYPESEV, LRESET_PRECIP)
ENDIF
 
IF (YDXFU%YPTYPE2%LACTIVE) THEN
  CALL DPRECIPS_XFU (YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA, YDCPG_OPTS%KLON, YDPRECIPS%NDTPREC2, 0_JPIM, PDPRECIPS2, YDXFU%PTYPE2, LRESET_PRECIP)
ENDIF
 
IF (YDXFU%YPTYPESEV2%LACTIVE) THEN
  CALL DPRECIPS_XFU (YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA, YDCPG_OPTS%KLON, YDPRECIPS%NDTPREC2, 2_JPIM, PDPRECIPS2, YDXFU%PTYPESEV2, LRESET_PRECIP)
ENDIF
 
!$ACDC }

END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('CPXFU', 1, ZHOOK_HANDLE)

CONTAINS

SUBROUTINE COPY3_XFU (YDDESC, PXFU, PPHY)

TYPE(FLUXES_DESCRIPTOR) ,INTENT(IN)               :: YDDESC
REAL(KIND=JPRB)         ,INTENT(INOUT)            :: PXFU(:,:)
REAL(KIND=JPRB)         ,INTENT(IN)               :: PPHY(1:YDCPG_OPTS%KLON,YDDESC%ILEVBOT:YDCPG_OPTS%KFLEVG)

DO JLEV = 1, YDDESC%ILEV
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA,JLEV) = PPHY (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA,YDDESC%INUMLEV(JLEV))
ENDDO

END SUBROUTINE 

SUBROUTINE COPY1_XFU (PXFU, PPHY0, PPHY1, LDCOND, LDMASK, LDVMASK, &
                & PVMIN, PVMAX, PSMIN, PSMAX, LDRESET, PRESET)

REAL(KIND=JPRB)         ,INTENT(INOUT)            :: PXFU(1:YDCPG_OPTS%KLON)
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PPHY0(1:YDCPG_OPTS%KLON)
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PPHY1(1:YDCPG_OPTS%KLON)
LOGICAL                 ,INTENT(IN)    ,OPTIONAL  :: LDCOND
LOGICAL                 ,INTENT(IN)    ,OPTIONAL  :: LDMASK
LOGICAL                 ,INTENT(IN)    ,OPTIONAL  :: LDVMASK(1:YDCPG_OPTS%KLON)
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PVMIN(1:YDCPG_OPTS%KLON)
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PVMAX(1:YDCPG_OPTS%KLON)
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PSMIN
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PSMAX
REAL(KIND=JPRB)         ,INTENT(IN)    ,OPTIONAL  :: PRESET
LOGICAL                 ,INTENT(IN)    ,OPTIONAL  :: LDRESET

IF (PRESENT (LDRESET)) THEN
  IF (LDRESET) THEN
    PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = PRESET
  ELSEIF (YDXFU%LRESET) THEN
    PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = 0.0_JPRB
  ENDIF
ELSEIF (YDXFU%LRESET) THEN
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = 0.0_JPRB
ENDIF

IF (PRESENT (PSMIN) .AND. PRESENT (PSMAX)) THEN
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = MIN (PSMIN, MAX (PSMAX, PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)))
ELSEIF (PRESENT (PSMIN) .AND. PRESENT (PVMAX)) THEN
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = MIN (PSMIN, MAX (PVMAX, PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)))
ELSEIF (PRESENT (PVMIN) .AND. PRESENT (PSMAX)) THEN
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = MIN (PVMIN, MAX (PSMAX, PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)))
ELSEIF (PRESENT (PVMIN)) THEN
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = MIN (PVMIN, PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA))
ELSEIF (PRESENT (PVMAX)) THEN
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = MAX (PVMAX, PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA))
ELSEIF (PRESENT (LDCOND)) THEN
  IF (LDCOND) THEN
    PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = PPHY0 (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)
  ELSE
    PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = PPHY1 (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)
  ENDIF
ELSEIF (PRESENT (LDMASK)) THEN
  IF (LDMASK) THEN
    DO JLON = YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA
      IF (LDVMASK (JLON)) THEN
        PXFU (JLON) = PPHY0 (JLON)
      ENDIF
    ENDDO
  ELSE
    PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = PPHY0 (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)
  ENDIF
ELSE
  PXFU (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = PPHY0 (YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)
ENDIF

END SUBROUTINE 

END SUBROUTINE CPXFU

SUBROUTINE GNHGW2SVD(&
 ! ----- INPUT ---------------------------------------------------------------
 & YDGEOMETRY,YDCST,TOPPRES,KST,KEND,LDNHEE,KPDVAR,KVDVAR,PSP,PRT,PSPD,&
 ! ----- OUTPUT --------------------------------------------------------------
 & PSVD,&
 ! ----- OPTIONAL INPUT ------------------------------------------------------
 & PNHX,PLNPR,PDEP,PREF,PGWH,PGWF,PGWS,PGWRF,PGWRS,PGDWR, &
 & PGWDAMP,PTAUD_NL)

!$ACDC singlecolumn  --process-pointers


!* GNHGW2SVD - Conversion Gw ---> svd.

! Purpose
! -------
!   Convert "G times vertical velocity w" to vertical divergence variable svd.

!   For the NHQE (and the HYD too), pre is replaced by prehyd, some terms disappear.
!   For the NHQE model, T is a modified temperature

! Interface
! ---------
!  INPUT:
!   YDGEOMETRY   : structure containing all geometry.
!   KST          : start of work
!   KEND         : end of work
!   KVDVAR       : controls the prognostic variable for "svd" equation.
!   PSP          : ln(prehyds)
!   PRT          : (RT) at full levels, with the version of R used to define vertical divergence "dver".
!                  R may be Rdry or Rmoist according to definition of vertical divergence "dver".
!   PSPD         : NH pressure departure variable at full levels

!  OUTPUT:
!   PSVD         : vertical divergence variable "svd" (d3 or d4) at full levels

!  OPTIONAL INPUT:
!   PNHX         : X-term (used only for d4) at full levels
!   PLNPR        : delta = delta(log(prehyd)) at full levels
!   PDEP         : "pre - prehyd" at full levels
!   PREF         : "prehyd" at full levels
!   PGWH         : "Gw" at half levels 
!   PGWF         : "Gw" at full levels 
!   PGWS         : "Gw" at the surface 
!   PGWRF        : reference "Gw_ref" at full levels (to apply derivative to Gw-Gw_ref).
!   PGWRS        : reference "Gw_surf_ref".
!   PGDWR        : reference "G dw".
!   PGWDAMP      : rayleight upper boudary friction parameter on Gw

! Externals
! ---------

! Method
! ------

! Reference
! ---------

! Author
! ------
!   11-Oct-2002 J. Masek (SHMI)

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad (Dec 2011): Use GPHPRE.
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (June 2017): Introduce NHQE model.
!   J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   J. Vivoda and P. Smolikova (Sep 2020): VFE pruning.
!   P. Smolikova and J. Vivoda (Aug-2023): NHHY blended model introduction
!   F. Voitus (Sep 2023): VFE compatible introduction
!   F. Voitus (Oct 2023): Add an upper implicit friction for gw  
! End Modifications
!------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST       , ONLY : TCST

! -----------------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)     ,INTENT(IN)             :: YDGEOMETRY
TYPE(TCST)         ,INTENT(IN)             :: YDCST
REAL(KIND=JPRB)    ,INTENT(IN)             :: TOPPRES
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KST
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KEND
LOGICAL            ,INTENT(IN)             :: LDNHEE
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KPDVAR
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KVDVAR
REAL(KIND=JPRB)    ,INTENT(IN)             :: PSP(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PRT(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PSPD(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PSVD(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PNHX(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PLNPR(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PDEP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PREF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PGWH(YDGEOMETRY%YRDIM%NPROMA,0:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PGWF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PGWS(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PGWRF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PGWRS(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PGDWR(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PGWDAMP(0:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PTAUD_NL

! -----------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF
LOGICAL :: LLINC

REAL(KIND=JPRB) :: ZDELP (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB) :: ZLNPR (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB) :: ZRDELP(YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZALPH (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB) :: ZRTGR (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB) :: ZRPRE (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZRPP  (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG)

REAL(KIND=JPRB) :: ZPREF (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZPDEP (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZPREH (YDGEOMETRY%YRDIM%NPROMA, 0:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGWH  (YDGEOMETRY%YRDIM%NPROMA, 0:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGWF  (YDGEOMETRY%YRDIM%NPROMA, 0:YDGEOMETRY%YRDIMV%NFLEVG+1)
REAL(KIND=JPRB) :: ZGDW  (YDGEOMETRY%YRDIM%NPROMA,   YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZOUT  (YDGEOMETRY%YRDIM%NPROMA, 0:YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZTAUD, ZKAP_B

! -----------------------------------------------------------------------------

#include "abor1.intfb.h"
#include "gnhpre.intfb.h"
#include "gphpre_expl.intfb.h"
#include "verdisint.intfb.h"

! -----------------------------------------------------------------------------

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GNHGW2SVD',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV, &
 & YDVAB=>YDGEOMETRY%YRVERT_GEOM%YRVAB, YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA, &
 & YDVFE=>YDGEOMETRY%YRVERT_GEOM%YRVFE,YDCVER=>YDGEOMETRY%YRVERT_GEOM%YRCVER)
ASSOCIATE(NPROMA=>YDDIM%NPROMA,NFLEVG=>YDDIMV%NFLEVG)

! -----------------------------------------------------------------------------


IF (PRESENT(PTAUD_NL)) THEN
  ZTAUD = PTAUD_NL
ELSE
  ZTAUD = 1.0_JPRB
ENDIF
ZKAP_B=1.0_JPRB

! -----
! compute pressures
! -----

IF (PRESENT(PLNPR).AND.PRESENT(PDEP).AND.PRESENT(PREF)) THEN

  ! * Simply transfer dummy input arrays into local arrays:
  ZLNPR(KST:KEND,1:NFLEVG)=PLNPR(KST:KEND,1:NFLEVG)
  ZPREF(KST:KEND,1:NFLEVG)=PREF(KST:KEND,1:NFLEVG)
  IF (LDNHEE) THEN
    ZPDEP(KST:KEND,1:NFLEVG)=PDEP(KST:KEND,1:NFLEVG)
  ELSE
    ZPDEP(KST:KEND,1:NFLEVG)=0.0_JPRB
  ENDIF

ELSE

  ! * Do calculations for ZXYB(.,.,YYTXYB%M_LNPR), ZPDEP, ZPREF:

  ! convert ln(prehyds) to prehyds
  DO JROF=KST,KEND
    ZPREH(JROF,NFLEVG)=EXP(PSP(JROF))
  ENDDO
  ! compute half and full level pressures "pre"
  CALL GPHPRE_EXPL (YDCVER,TOPPRES,YDCST,NPROMA,NFLEVG,KST,KEND,YDVAB,ZPREH,ZPREF,&
                  & PDELP=ZDELP,PLNPR=ZLNPR,PRDELP=ZRDELP,PALPH=ZALPH,PRTGR=ZRTGR,PRPRE=ZRPRE,PRPP=ZRPP)

  ! transform NH pressure departure to (pre-prehyd)
  IF (LDNHEE) THEN
    CALL GNHPRE(YDGEOMETRY,KPDVAR,NPROMA,NFLEVG,KST,KEND,PSPD,ZPREF,PKAP=ZKAP_B,PDEP=ZPDEP)
  ELSE
    ZPDEP(KST:KEND,1:NFLEVG)=0.0_JPRB
  ENDIF

ENDIF

! -----
! transform Gw ---> svd
! -----

! transform Gw into -G.dw
IF( YDCVER%LVERTFE.AND.YDCVER%LVFE_GW )THEN
  IF (.NOT.PRESENT(PGWF)) CALL ABOR1('GNHGW2SVD: missing input PGWF')
  IF (PRESENT(PGWRF).AND.PRESENT(PGDWR)) THEN
    LLINC=.TRUE.
  ELSE
    ! in this case it is better to omit PGWRF, PGWRS and PGDWR.
    ! equivalent to have PGWRF=PGWRS=PGDWR=0.
    LLINC=.FALSE.
  ENDIF

  IF (LLINC) THEN
    ! * Compute derivative on increments -((Gw)-(Gw_ref)):

    IF (.NOT.PRESENT(PGWRS)) CALL ABOR1('GNHGW2SVD: missing input PGWRS')

    DO JLEV=1,NFLEVG
      DO JROF = KST, KEND
        ZGWH(JROF,JLEV)=-((PGWF(JROF,JLEV)-PGWRF(JROF,JLEV)) &
         & -(PGWS(JROF)-PGWRS(JROF)))
      ENDDO
    ENDDO
    ZGWH(KST:KEND,0) = ZGWH(KST:KEND,1)
    CALL VERDISINT(YDVFE,YDCVER,'DEGW','10',NPROMA,KST,KEND,NFLEVG,ZGWH,ZOUT)
    DO JLEV=1,NFLEVG
      ZGDW(KST:KEND,JLEV) = ZOUT(KST:KEND,JLEV)
    ENDDO
    
    ! compute -G(dw) from -G(dw/deta - dw_ref/deta) and -G(dw_ref).
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ZGDW(JROF,JLEV)=ZGDW(JROF,JLEV)/YDVETA%VFE_RDETAH(JLEV)-PGDWR(JROF,JLEV)
      ENDDO
    ENDDO
  ELSE
    ! * Compute derivative on -(Gw):
    IF (.NOT.PRESENT(PGWS)) CALL ABOR1('GNHGW2SVD: missing input PGWS')
    DO JLEV=1,NFLEVG
      DO JROF = KST, KEND
        ZGWF(JROF,JLEV)=-(PGWF(JROF,JLEV)-PGWS(JROF))
      ENDDO
    ENDDO  
    ZGWF(KST:KEND,0) = 0.0_JPRB
    ZGWF(KST:KEND,NFLEVG+1) = 0.0_JPRB
    CALL VERDISINT(YDVFE,YDCVER,'FDER','10',NPROMA,KST,KEND,NFLEVG,ZGWF,ZGDW)

    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ZGDW(JROF,JLEV)=ZGDW(JROF,JLEV)/YDVETA%VFE_RDETAH(JLEV)
      ENDDO
    ENDDO
  ENDIF
ELSE
  IF (.NOT.PRESENT(PGWH)) CALL ABOR1('GNHGW2SVD: missing input PGWH')
  ZGWH(KST:KEND,0:NFLEVG)=PGWH(KST:KEND,0:NFLEVG)
  IF (PRESENT(PGWDAMP)) THEN
    ! Gw -> "-G.d[nu*w]"
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ZGDW(JROF,JLEV)=PGWDAMP(JLEV-1)*ZGWH(JROF,JLEV-1) &
         & -PGWDAMP(JLEV)*ZGWH(JROF,JLEV)
      ENDDO
    ENDDO
  ELSE
    ! Gw -> "-G.dw"
    DO JLEV=1,NFLEVG
      DO JROF=KST,KEND
        ZGDW(JROF,JLEV)=ZGWH(JROF,JLEV-1)-ZGWH(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF
ENDIF


! transform "-G.dw" into "dver"
IF (YDCVER%LVFE_COMPATIBLE) THEN
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      PSVD(JROF,JLEV)=YDVETA%VDETA_RATIO(JLEV)*(ZGDW(JROF,JLEV)/&
       & (PRT(JROF,JLEV)*ZLNPR(JROF,JLEV)))*&
       & (ZTAUD*(ZPDEP(JROF,JLEV)/ZPREF(JROF,JLEV)) + 1.0_JPRB)
    ENDDO
  ENDDO
ELSE
  DO JLEV=1,NFLEVG
    DO JROF=KST,KEND
      PSVD(JROF,JLEV)= (ZGDW(JROF,JLEV)/&
       & (PRT(JROF,JLEV)*ZLNPR(JROF,JLEV)))*&
       & (ZTAUD*(ZPDEP(JROF,JLEV)/ZPREF(JROF,JLEV)) + 1.0_JPRB) 
    ENDDO
  ENDDO
ENDIF

! transform "dver" into "svd" (currently for "d4" or "d5" only)

IF ( KVDVAR == 4 .OR. KVDVAR == 5 ) THEN
  IF (.NOT.PRESENT(PNHX)) CALL ABOR1('GNHGW2SVD: missing input PNHX')
  DO JLEV = 1, NFLEVG
    DO JROF = KST, KEND
      PSVD(JROF,JLEV)=PSVD(JROF,JLEV)+PNHX(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF


! -----------------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('GNHGW2SVD',1,ZHOOK_HANDLE)

END SUBROUTINE GNHGW2SVD


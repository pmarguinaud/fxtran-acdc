SUBROUTINE ELARCHE(YDCST,YDML_DYN,KPROMA,KST,KEND,KFLEV,YDSL,YDEGSL,YDEGEO,PSCO,PCCO,&
                 &PGECLO, PGEMU, PGESLO, PGSQM2)

!$ACDC singlecolumn --dummy  --process-pointers


!**** *ELARCHE  -  semi-Lagrangian scheme:
!                  research of (lat,lon) coordinates of the origin point

!     Purpose.
!     --------
!        Computes the (lat,lon) geographical coordinates of the
!        origin point and then it computes the displacement
!        matrix type "Rochas" (elements p, q, see the general doc
!        on S-L scheme). Optionally it calculates the vectorial
!        product 2 omega x R (Coriolis contribution) at the
!        origin point exactly.

!**   Interface.
!     ----------

!     *CALL* *ELARCHE(...)

!     Explicit arguments:
!     -------------------

!     INPUT:
!       KPROMA      - horizontal dimension
!       KST      - start of work
!       KEND       - depth of work
!       KFLEV       - vertical dimension
!       YDSL        - SL_STRUCT definition.
!       PSCO        - information about position of interpol. point in the
!                     fractional system (not in metres!).

!     INPUT/OUTPUT:
!       PCCO        - "RLON" and "RLAT":
!                     * input : coordinates of the "true" origin point
!                     in the fractional system (even out of the C+I zone),
!                     used for the LADVF option
!                     * output: components of the 2 omega x R

!     OUTPUT:
!       PCCO        - "RQX" and "RQY": elements of the wind displacement matrix (p,q)

!             W A R N I N G:
!             ==============

!       The output wind rotation matrix PCCO(.,.,'RQX' and 'RQY')
!       contains in fact product
!       of two matrices: A * B, A being the "true" (p,q) rotation
!       matrix as described in the documentation, B being the
!       rotation matrix which provides (ug, vg) wind vector (g denotes
!       here for geographic components) starting from the "physical"
!       (that means non reduced!) map wind (u,v) at the origin point.
!       Then "true" (p,q) matrix takes care for displacement of the
!       GEOGRAPHIC wind components between the origin and the final
!       points of the trajectory. The rest of transformation at the
!       final point is done by the rotation matrix back to the "map"
!       north and east later in LAPINE... In total 3 rotation matrices
!       are involved, similarly to stretched and tilted ARPEGE.

!       ELARCHE treats ONLY PLANE(!) geometry, i.e. STEREO/LAMBERT
!       and MERCATOR projections. The LAT/LON spherical LAM
!       configuration is not yet ready.

!     Implicit arguments:
!     -------------------

!     Method.
!     -------
!       See documentation

!     Externals.
!     ----------

!     Author.
!     -------
!       Radmila Bubnova
!       Original : 31 JANUARY 1996.

!     Modifications.
!     --------------
!    J-D Gril : 2009-Mar-23 adding Mercator Rotated Tilted Case
!                          (From P. Benard formulae)
!    F. Vana   28-Apr-2009 small fix in Lambert/Stereo case
!    K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!    K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!    G.Mozdzynski (Feb 2011): OOPS cleaning, use of derived type TGSGEOM
!    B. Bochenek (Apr 2015): Phasing: move some variables.
!    K. Yessad (Feb 2018): remove deep-layer formulations.
!     -----------------------------------------------------------------

USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY : TCST
USE YEMGEO   , ONLY : TEGEO
USE YEMGSL   , ONLY : TEGSL
USE EINT_MOD , ONLY : SL_STRUCT

!     -----------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)                ,INTENT(IN)     :: YDCST
TYPE(MODEL_DYNAMICS_TYPE) ,INTENT(IN)     :: YDML_DYN
INTEGER(KIND=JPIM)        ,INTENT(IN)     :: KPROMA 
INTEGER(KIND=JPIM)        ,INTENT(IN)     :: KFLEV 
INTEGER(KIND=JPIM)        ,INTENT(IN)     :: KST 
INTEGER(KIND=JPIM)        ,INTENT(IN)     :: KEND 
TYPE(SL_STRUCT)           ,INTENT(IN)     :: YDSL
TYPE(TEGSL)               ,INTENT(IN)     :: YDEGSL
TYPE(TEGEO)               ,INTENT(IN)     :: YDEGEO
REAL(KIND=JPRB)           ,INTENT(IN)     :: PSCO(KPROMA,KFLEV,YDML_DYN%YYTSCO%NDIM)
REAL(KIND=JPRB)           ,INTENT(INOUT)  :: PCCO(KPROMA,KFLEV,YDML_DYN%YYTCCO%NDIM)
REAL(KIND=JPRB)           ,INTENT(IN)     :: PGECLO(KPROMA)
REAL(KIND=JPRB)           ,INTENT(IN)     :: PGEMU(KPROMA)
REAL(KIND=JPRB)           ,INTENT(IN)     :: PGESLO(KPROMA)
REAL(KIND=JPRB)           ,INTENT(IN)     :: PGSQM2(KPROMA)


! ----------------------------------------------------------------------

LOGICAL :: LLCOMPUTE_OMVAK_O

INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRB) :: ZXX_TMP(KPROMA), ZYY_TMP(KPROMA), ZRR_TMP(KPROMA)
REAL(KIND=JPRB) :: ZSINLAT_TMP(KPROMA), ZCOSLAT_TMP(KPROMA), ZLON_TMP(KPROMA)
REAL(KIND=JPRB) :: ZSINLON_TMP(KPROMA)

REAL(KIND=JPRB) :: ZAA, ZBIN, ZCC, ZCCC, &
 & ZCODLAM, ZCOPHI, ZCOSCO, &
 & ZCOSLAT, ZCOSLATC, ZCOSLATP, ZCOSLATS, &
 & ZCOSLON,ZCOSLONC,ZCOSLON0R,ZCOSLONP,ZCOSLONS,&
 & ZDGUN, ZDLUN, ZLON, ZLONS, ZNORDX, ZNORDY, ZPP, &
 & ZQQ, ZREGPK, ZRR2, ZSIDLAM, &
 & ZSINLAT, ZSINLATC, ZSINLATP, ZSINLATS, &
 & ZSINLON, ZSINLONC,ZSINLON0R, ZSINLONP,ZSINLONS, &
 & ZSINSI, ZSS, ZSSS, ZXP, ZXX, ZYP, ZYY

REAL(KIND=JPRB) :: ZEPS 
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! ----------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('ELARCHE',0,ZHOOK_HANDLE)

ASSOCIATE( &
 & LADVF=>YDML_DYN%YRDYN%LADVF, &
 & RIPORE=>YDEGSL%RIPORE, RJPORE=>YDEGSL%RJPORE, REGPK=>YDEGSL%REGPK, &
 & RNOBILE=>YDEGSL%RNOBILE, RLON0R=>YDEGSL%RLON0R, RHSUD=>YDEGSL%RHSUD, &
 & RMERCAT=>YDEGSL%RMERCAT, RELONC=>YDEGSL%RELONC, RELATC=>YDEGSL%RELATC, &
 & EDELX=>YDEGEO%EDELX, EDELY=>YDEGEO%EDELY, LMAP=>YDEGEO%LMAP, &
 & LMRT=>YDEGEO%LMRT)
! ----------------------------------------------------------------------

!        0. Initialization.
!        ------------------

ZEPS    = EPSILON(1.0_JPRB)*100.0_JPRB
ZDLUN   = REAL(YDSL%NDLUNG,JPRB)
ZDGUN   = REAL(YDSL%NDGUNG,JPRB)

LLCOMPUTE_OMVAK_O=LADVF

! ----------------------------------------------------------------------

!        1. Calculations.
!        ----------------

IF (LMAP) THEN

  IF(REGPK > 0) THEN

    !    1. Lambert/Stereo case.
    !    -----------------------

    ZREGPK  = 1.0_JPRB/REGPK
    DO JLEV = 1, KFLEV

      DO JROF = KST, KEND

        IF(LLCOMPUTE_OMVAK_O) THEN
          ZXX_TMP(JROF) = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)-ZDLUN-RIPORE)*EDELX
          ZYY_TMP(JROF) = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)-ZDGUN-RJPORE)*EDELY
        ELSE
          ZXX_TMP(JROF) = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)-ZDLUN-RIPORE)*EDELX
          ZYY_TMP(JROF) = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)-ZDGUN-RJPORE)*EDELY
        ENDIF

        ZRR2= ZXX_TMP(JROF)**2 + ZYY_TMP(JROF)**2
        ZRR_TMP(JROF) = MAX(SQRT(ZRR2),ZEPS)
        ZAA = (ZRR2*RNOBILE)**ZREGPK

        ! * calculation of geographical latitude: sinus and cosinus
        ZSINLAT_TMP(JROF) = (1.0_JPRB-ZAA)/(1.0_JPRB+ZAA)*RHSUD
        ZCOSLAT_TMP(JROF) = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLAT_TMP(JROF)*ZSINLAT_TMP(JROF)))

        ! * calculation of geographical longitude
        ZLON_TMP(JROF)    = RLON0R + ATAN2(ZXX_TMP(JROF),-ZYY_TMP(JROF)*RHSUD)*ZREGPK

        ZSINLON_TMP(JROF) = SIN(ZLON_TMP(JROF))
      ENDDO
! For an unknown reason (Intel compiler bug ?) this computation has to be split here
! in order to prevent non-reproducible results when the entire loop is vectorized
      DO JROF = KST, KEND
        ZCOSLON = COS(ZLON_TMP(JROF))

        ! * calculation of compas
        ZNORDX  = - RHSUD*ZXX_TMP(JROF)/ZRR_TMP(JROF)
        ZNORDY  = - RHSUD*ZYY_TMP(JROF)/ZRR_TMP(JROF)

        ! * calculation of the matrix (p,q) * the compas matrix
        ZCOSCO  = PGSQM2(JROF) * ZCOSLAT_TMP(JROF)
        ZSINSI  = PGEMU (JROF) * ZSINLAT_TMP(JROF)
        ZCODLAM = PGECLO(JROF) * ZCOSLON   +PGESLO(JROF) * ZSINLON_TMP(JROF)
        ZSIDLAM = PGESLO(JROF) * ZCOSLON   -PGECLO(JROF) * ZSINLON_TMP(JROF)
        ZCOPHI  = ZSINSI + ZCOSCO * ZCODLAM
        ZPP     = (ZCOSCO+(1.0_JPRB+ZSINSI)   *ZCODLAM)/(1.0_JPRB+ZCOPHI)
        ZQQ     = ((PGEMU(JROF)+ZSINLAT_TMP(JROF))*ZSIDLAM)/(1.0_JPRB+ZCOPHI)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQX) = ZPP * ZNORDY + ZQQ * ZNORDX
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQY) =-ZPP * ZNORDX + ZQQ * ZNORDY

        ! * calculation of the 2 omega x R vector
        IF (LLCOMPUTE_OMVAK_O) THEN
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)=  2.0_JPRB*YDCST%ROMEGA*YDCST%RA*ZCOSLAT_TMP(JROF)*ZNORDY
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)= -2.0_JPRB*YDCST%ROMEGA*YDCST%RA*ZCOSLAT_TMP(JROF)*ZNORDX
        ENDIF

      ENDDO

    ENDDO

  ELSEIF ((REGPK == 0).AND.LMRT) THEN

    !    2.1 Mercator Rotated Tilted case.
    !    ---------------------------------

    ZSINLONC = SIN(RELONC)
    ZCOSLONC = COS(RELONC)
    ZSINLATC = SIN(RELATC)
    ZCOSLATC = COS(RELATC)
    ZSINLON0R = SIN(RLON0R)
    ZCOSLON0R = COS(RLON0R)

    DO JLEV = 1, KFLEV
      DO JROF = KST, KEND

        IF(LLCOMPUTE_OMVAK_O) THEN
          ZXP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)-ZDLUN)*EDELX
          ZYP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)-ZDGUN)*EDELY
        ELSE
          ZXP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)-ZDLUN)*EDELX
          ZYP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)-ZDGUN)*EDELY
        ENDIF

        ZXX = ZXP - RIPORE*EDELX
        ZYY = ZYP - RJPORE*EDELY
        ZAA = EXP(-2.0_JPRB* RMERCAT*ZYY)

        ! * calculation of RT latitude theta" (sin and cos)
        ZSINLATS = (1.0_JPRB-ZAA)/(1.0_JPRB+ZAA)
        ZCOSLATS = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLATS*ZSINLATS))

        ! * calculation of RT longitude lambda" (and sin and cos)
        ZLONS   =  ZXX * RMERCAT  
        ZSINLONS = SIN(ZLONS)
        ZCOSLONS = COS(ZLONS)

        ! * calculation of R latitude theta' : sin and cos
        ZSINLATP = ZCOSLON0R*ZSINLATS-ZSINLON0R*ZCOSLATS*ZSINLONS
        ZCOSLATP = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLATP*ZSINLATP))

        ! * calculation of R longitude lambda' : sin and cos
        ZBIN=0.5_JPRB+SIGN(0.5_JPRB,ZCOSLATP-ZEPS)
        ZSS=(ZSINLON0R*ZSINLATS&
         & +ZCOSLON0R*ZCOSLATS*ZSINLONS)/MAX(ZCOSLATP,ZEPS)
        ZCC=ZCOSLATS*ZCOSLONS/MAX(ZCOSLATP,ZEPS)
        ZSINLONP = ZBIN*(ZSS)
        ZCOSLONP = ZBIN*(ZCC)+(1.0_JPRB - ZBIN)

        ! * calculation of Geogr latitude theta : sin and cos   
        ZSINLAT = ZCOSLATC*ZSINLATP + ZSINLATC*ZCOSLATP*ZCOSLONP
        ZCOSLAT = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLAT*ZSINLAT))

        ! * calculation of Geogr longitude lambda : sin and cos
        ZBIN=0.5_JPRB+SIGN(0.5_JPRB,ZCOSLAT-ZEPS)
        ZSS=ZCOSLATP*ZSINLONP/MAX(ZCOSLAT,ZEPS)
        ZCC=(-ZSINLATC*ZSINLATP+ZCOSLATC*ZCOSLATP*ZCOSLONP)/MAX(ZCOSLAT,ZEPS)
        ZSSS = ZBIN*(ZSS)
        ZCCC = ZBIN*(ZCC)+(1.0_JPRB - ZBIN)
        ZSINLON = ZSINLONC*ZCCC + ZCOSLONC*ZSSS
        ZCOSLON = ZCOSLONC*ZCCC - ZSINLONC*ZSSS

        ! * calculation of compass at origin point
        ZBIN=0.5_JPRB+SIGN(0.5_JPRB,ZCOSLATS-ZEPS)
        ZCC = (ZCOSLON0R*(ZCOSLATC*ZCOSLAT+ZSINLATC*ZSINLAT*ZCCC)&
         & - ZSINLON0R *ZSINLAT*ZSSS)/MAX(ZCOSLATS,ZEPS)
        ZSS = -(ZCOSLON0R*ZSINLATC*ZSSS+ZSINLON0R *ZCCC)/MAX(ZCOSLATS,ZEPS)
        ZNORDX = ZBIN*(ZSS)
        ZNORDY = ZBIN*(ZCC)+(1.0_JPRB - ZBIN)

        ! * calculation of the matrix (p,q) * the compas matrix
        ZCOSCO  = PGSQM2(JROF) * ZCOSLAT
        ZSINSI  = PGEMU (JROF) * ZSINLAT
        ZCODLAM = PGECLO(JROF) * ZCOSLON + PGESLO(JROF) * ZSINLON
        ZSIDLAM = PGESLO(JROF) * ZCOSLON - PGECLO(JROF) * ZSINLON
        ZCOPHI  = ZSINSI + ZCOSCO * ZCODLAM
        ZPP     = (ZCOSCO+(1.0_JPRB+ZSINSI) * ZCODLAM)/(1.0_JPRB+ZCOPHI)
        ZQQ     = ((PGEMU(JROF)+ZSINLAT)*ZSIDLAM)/(1.0_JPRB+ZCOPHI)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQX) = ZPP * ZNORDY + ZQQ * ZNORDX
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQY) =-ZPP * ZNORDX + ZQQ * ZNORDY

        ! * calculation of the 2 omega x R vector
        IF (LLCOMPUTE_OMVAK_O) THEN
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)=  2.0_JPRB*YDCST%ROMEGA*YDCST%RA*ZCOSLAT*ZNORDY
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)= -2.0_JPRB*YDCST%ROMEGA*YDCST%RA*ZCOSLAT*ZNORDX
        ENDIF

      ENDDO

    ENDDO

  ELSEIF ((REGPK == 0).AND..NOT.LMRT) THEN

    !    2.2 Mercator case.
    !    ------------------

    DO JLEV = 1, KFLEV
      DO JROF = KST, KEND

        IF(LLCOMPUTE_OMVAK_O) THEN
          ZXP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)-ZDLUN)*EDELX
          ZYP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)-ZDGUN)*EDELY
        ELSE
          ZXP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)-ZDLUN)*EDELX
          ZYP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)-ZDGUN)*EDELY
        ENDIF

        ZXX = ZXP - RIPORE*EDELX
        ZYY = ZYP - RJPORE*EDELY
        ZAA = EXP(-2.0_JPRB* RMERCAT*ZYY)

        ! * calculation of geographical latitude: sinus and cosinus
        ZSINLAT = (1.0_JPRB-ZAA)/(1.0_JPRB+ZAA)
        ZCOSLAT = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLAT*ZSINLAT))

        ! * calculation of geographical longitude
        ZLON    = RLON0R + ZXX * RMERCAT
        ZSINLON = SIN(ZLON)
        ZCOSLON = COS(ZLON)

        ! * calculation of the matrix (p,q) * the compas matrix
        ZCOSCO  = PGSQM2(JROF) * ZCOSLAT
        ZSINSI  = PGEMU (JROF) * ZSINLAT
        ZCODLAM = PGECLO(JROF) * ZCOSLON + PGESLO(JROF) * ZSINLON
        ZSIDLAM = PGESLO(JROF) * ZCOSLON - PGECLO(JROF) * ZSINLON
        ZCOPHI  = ZSINSI + ZCOSCO * ZCODLAM
        ZPP     = (ZCOSCO+(1.0_JPRB+ZSINSI) * ZCODLAM)/(1.0_JPRB+ZCOPHI)
        ZQQ     = ((PGEMU(JROF)+ZSINLAT)*ZSIDLAM)/(1.0_JPRB+ZCOPHI)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQX) = ZPP
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQY) = ZQQ

        ! * calculation of the 2 omega x R vector
        IF (LLCOMPUTE_OMVAK_O) THEN
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)=  2.0_JPRB*YDCST%ROMEGA*YDCST%RA*ZCOSLAT
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)=  0.0_JPRB
        ENDIF

      ENDDO

    ENDDO

  ENDIF

ELSE

  !      3. Academic case.
  !      -----------------

  PCCO(KST:KEND,1:KFLEV,YDML_DYN%YYTCCO%M_RQX) = 1.0_JPRB
  PCCO(KST:KEND,1:KFLEV,YDML_DYN%YYTCCO%M_RQY) = 0.0_JPRB

ENDIF

! ----------------------------------------------------------------------

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ELARCHE',1,ZHOOK_HANDLE)
END SUBROUTINE ELARCHE


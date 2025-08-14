SUBROUTINE SISEVE(YDCST, YDGEOMETRY, YDDYN, YDDYNA, KLON, KLEV, KIDIA, KFDIA, PV1, PV2, LD_LSTAR, LD_SKAPI)

!$ACDC singleblock 

!**** *SISEVE_GP_OPENMP* - SEcond VErtical derivative operator in Semi - Implicit

!     Purpose.
!     --------
!           Operator L_star or L_star_star to compute vertical Laplacian in non-hydrostatic
!           dynamics: formulation for semi - implicit

!           The discretisation of the vertical Laplacian must remain
!           consistent with the discretisation of the equivalent quantity
!           in the non linear part (pieces of codes spread in GNHPREH,
!           GNH_TNDLAGADIAB_GW and GNH_TNDLAGADIAB_SVD).

!           This vertical Laplacian is applied to a quantity "X" (in PV1)
!           which matches the following conditions:
!           X_top = 0
!           X_surf = X(l=nflevg)
!           (see for example the calculation of the top and bottom
!           pressure departure variable in GNHPREH).

!**   Interface.
!     ----------
!        *CALL* *SISEVE(...)

!        Explicit arguments :
!        --------------------
!        YDCST         : structure containing model constants
!        YDGEOMETRY    : structure containing geometry
!        YDDYN         : structure containing dynamics from YOMDYN
!        YDDYNA        : structure containing dynamics from YOMDYNA
!
!        KLON : TOTAL NUMBER OF COLUMNS IN THE TWO PARAMETER ARRAYS
!               (PV1,PV2)
!        KLEV : NUMBER OF VERTICAL LEVELS IN THE 3D PARAMETER ARRAYS,
!                AND PROCESSED
!        KIDIA : FIRST COLUMN PROCESSED
!        KFDIA : LAST COLUMN PROCESSED

!        TYPICAL VALUES ARE  (KLEV,KLON)=(NDLSUR,1)  FOR GRID POINT ARRAY
!                            (KLEV,KLON)=(1,NFLSUR)  FOR SPECTRAL ARRAY

!        PV1   : INPUT VARIABLE                                           (IN)
!        PV2   : DERIVED VARIABLE BY VERTICAL LAPLACIAN                   (OUT)
!        LD_LSTAR : T: operator L_star                                    (OPT IN)
!                   F: operator L_star_star

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        Arpege/Aladin documentation

!     Author.
!     -------
!      Radmila Bubnova GMAP/COMPAS, stage MRE
!      Original : 93-03-21

!     Modifications.
!     --------------
!      P. Smolikova and J. Vivoda (Oct 2013): new options for VFE-NH
!      K. Yessad (Dec 2016): Prune obsolete options.
!      K. Yessad (June 2017): Vertical-dependent SITRA.
!      J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!      F. Voitus (Feb 2020): new refinement option for VFD-NH
!      J. Vivoda and P. Smolikova (Sep 2020): VFE pruning.
!      F. Voitus (Aug 2022): introduce compatible VFE-NH
!      F. Voitus (Sep 2023): introduce new SIPRA and SISLP SI parameters
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST       , ONLY : TCST
USE YOMDYN       , ONLY : TDYN
USE YOMDYNA      , ONLY : TDYNA

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
TYPE(TDYNA)       ,INTENT(IN)    :: YDDYNA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV1(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PV2(KLON,KLEV) 
LOGICAL,OPTIONAL  ,INTENT(IN)    :: LD_LSTAR
LOGICAL,OPTIONAL  ,INTENT(IN)    :: LD_SKAPI

!     ------------------------------------------------------------------

LOGICAL :: LL_LSTAR, LL_SKAPI
 
INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZV2(KLON,KLEV)

REAL(KIND=JPRB) :: ZIN(KLON,KLEV)
REAL(KIND=JPRB) :: ZIN1(KLON,0:KLEV+1)

REAL(KIND=JPRB) :: ZVDERW_FE(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZVDERW_FD(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZVDERU(KLON,KLEV)
REAL(KIND=JPRB) :: ZVDERV(KLON,KLEV)
REAL(KIND=JPRB) :: ZDELPH(KLON,KLEV)
REAL(KIND=JPRB) :: ZRDELPF(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZINDETA(KLON,KLEV)
REAL(KIND=JPRB) :: ZINDETA2(KLON,KLEV)
REAL(KIND=JPRB) :: ZP2MDETA(KLON,KLEV)

REAL(KIND=JPRB) :: ZVLAPL(KLON,KLEV)
REAL(KIND=JPRB) :: ZKAP_B

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "verdisint.intfb.h"
#include "verderfe.intfb.h"
#include "siskapi.intfb.h"

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SISEVE',0,ZHOOK_HANDLE)

ASSOCIATE(YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA, &
& YDVFE=>YDGEOMETRY%YRVERT_GEOM%YRVFE, &
& YDCVER=>YDGEOMETRY%YRVERT_GEOM%YRCVER)
ASSOCIATE(NFLEVG=>YDGEOMETRY%YRDIMV%NFLEVG, &
& RD=>YDCST%RD, RG=>YDCST%RG, RKAPPA=>YDCST%RKAPPA,&
& SIPR=>YDDYN%SIPR, SIPRA=>YDDYN%SIPRA, &
& SIDELP_NH=>YDDYN%SIDELP_NH, SIRDEL_NH=>YDDYN%SIRDEL_NH, &
& SILNPR_NH=>YDDYN%SILNPR_NH, &
& SITLAF_NH=>YDDYN%SITLAF_NH, SITLAH_NH=>YDDYN%SITLAH_NH, &
& SIGFL=>YDDYN%SIGFL, SIGFM=>YDDYN%SIGFM, SIGHL=>YDDYN%SIGHL, &
& SIGHM=>YDDYN%SIGHM, SITFL=>YDDYN%SITFL, SITFM=>YDDYN%SITFM, &
& SIWF2H=>YDDYN%SIWF2H, SIWH2F=>YDDYN%SIWH2F, &
& SITR=>YDDYN%SITR, SITRM=>YDDYN%SITRM, SITRAM=>YDDYN%SITRAM, &
& SIZA=>YDDYN%SIZA, SIZC=>YDDYN%SIZC, &
& LGWFRIC=>YDDYN%LGWFRIC, RGWFRIC=>YDDYN%RGWFRIC)

!     ------------------------------------------------------------------
!* Key controlling  (Quasi to fully) Elastic blending 
ZKAP_B=1.0_JPRB
IF (YDDYNA%LNHQE.AND.YDDYN%LNHQE_SOLVER_SP) ZKAP_B=RKAPPA

!     ------------------------------------------------------------------

!*       1.    INNER LAYERS (AND BOUNDARY CONDITIONS)
!              ----------------------------------------------

IF(YDCVER%LVERTFE)THEN
  
  !* --- Finite elements ---

  IF (YDCVER%LCOMPATIBLE) THEN

  ! * transform PV1 into appropriate matrix form :
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        ZIN(JLON,JLEV)=PV1(JLON,JLEV)                  ! QCHA
        ZIN1(JLON,JLEV)=PV1(JLON,JLEV)*SITLAF_NH(JLEV) ! PDEP
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZIN1(JLON,0     )=0.0_JPRB
      ZIN1(JLON,KLEV+1)=ZIN1(JLON,KLEV)*(SITLAH_NH(KLEV)/SITLAF_NH(KLEV))
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV-1
      DO JLON=KIDIA,KFDIA
        ZRDELPF(JLON,JLEV)=1.0_JPRB/(SITLAF_NH(JLEV+1)-SITLAF_NH(JLEV)) 
        ZVDERW_FD(JLON,JLEV)=ZKAP_B*(ZIN1(JLON,JLEV+1)-ZIN1(JLON,JLEV))*ZRDELPF(JLON,JLEV)  &
        & +(1.0_JPRB-ZKAP_B)*SITLAH_NH(JLEV)*(ZIN(JLON,JLEV+1)-ZIN(JLON,JLEV))*ZRDELPF(JLON,JLEV)
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZRDELPF(JLON,0)=1.0_JPRB/(SITLAF_NH(1)-SITLAH_NH(0))
      ZVDERW_FD(JLON,0)=ZKAP_B*(ZIN1(JLON,1)-ZIN1(JLON,0))*ZRDELPF(JLON,0)
      ZRDELPF(JLON,KLEV)=1.0_JPRB/(SITLAH_NH(KLEV)-SITLAF_NH(KLEV))
      ZVDERW_FD(JLON,KLEV)=ZKAP_B*(ZIN1(JLON,KLEV+1)-ZIN1(JLON,KLEV))*ZRDELPF(JLON,KLEV)
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

    IF (YDCVER%LVFE_FD_MIX) THEN

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=0,KLEV
        DO JLON=KIDIA,KFDIA
          ZVDERW_FE(JLON,JLEV)=ZVDERW_FD(JLON,JLEV)
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

    ELSE ! NOT LVFE_FD_MIX 

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=1,KLEV
        DO JLON=KIDIA,KFDIA
          ZDELPH(JLON,JLEV)=SIDELP_NH(JLEV)
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

      ! d(pdep)/dpi is derived using linear/constant piecewise VFE 
      ! compatible operator on half levels --> tridiagonal inversion 
      CALL VERDERFE(KLON,KIDIA,KFDIA,KLEV,YDVETA%VDETA_RATIO,ZDELPH,ZRDELPF,ZVDERW_FD,ZVDERW_FE)

    ENDIF
  
    IF (YDDYN%NOPT_SISLP/=0) THEN

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=1,KLEV
        DO JLON=KIDIA,KFDIA
          ZVDERU(JLON,JLEV) = (SIGFL(JLEV)*(SIWH2F(JLEV,2)*ZVDERW_FD(JLON,JLEV)&
                   & + SIWH2F(JLEV,1)*ZVDERW_FD(JLON,JLEV-1))&
                   & - RD*SITRM(JLEV)*SITFL(JLEV)*ZIN(JLON,JLEV))/RG
          ZVDERV(JLON,JLEV) = (SIGFM(JLEV)*(SIWH2F(JLEV,2)*ZVDERW_FD(JLON,JLEV)&
                   & + SIWH2F(JLEV,1)*ZVDERW_FD(JLON,JLEV-1))&
                   & - RD*SITRM(JLEV)*SITFM(JLEV)*ZIN(JLON,JLEV))/RG
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=2,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZVLAPL(JLON,JLEV) = (SITRAM(JLEV)/SITR)*(RD/RG)*SITR*(&
                   & SITFL(JLEV)*ZVDERU(JLON,JLEV)+SITFM(JLEV)*ZVDERV(JLON,JLEV))&
                   & + ( RGWFRIC(JLEV)*ZVDERW_FE(JLON,JLEV)&
                   & - RGWFRIC(JLEV-1)*ZVDERW_FE(JLON,JLEV-1)&
                   & + ( (SIGFL(JLEV+1)*ZVDERU(JLON,JLEV+1)&
                   & + SIWF2H(JLEV)*(SIGFL(JLEV)*ZVDERU(JLON,JLEV)&
                   & - SIGFL(JLEV+1)*ZVDERU(JLON,JLEV+1)))&
                   & + (SIGFM(JLEV+1)*ZVDERV(JLON,JLEV+1)&
                   & + SIWF2H(JLEV)*(SIGFM(JLEV)*ZVDERV(JLON,JLEV)&
                   & - SIGFM(JLEV+1)*ZVDERV(JLON,JLEV+1)))&
                   & - (SIGFL(JLEV)*ZVDERU(JLON,JLEV)&
                   & + SIWF2H(JLEV-1)*(SIGFL(JLEV-1)*ZVDERU(JLON,JLEV-1)&
                   & - SIGFL(JLEV)*ZVDERU(JLON,JLEV)))&
                   & - (SIGFM(JLEV)*ZVDERV(JLON,JLEV)&
                   & + SIWF2H(JLEV-1)*(SIGFM(JLEV-1)*ZVDERV(JLON,JLEV-1)&
                   & - SIGFM(JLEV)*ZVDERV(JLON,JLEV))) )/RG )&
                   & /(SILNPR_NH(JLEV)/YDVETA%VDETA_RATIO(JLEV))
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,1) = (SITRAM(1)/SITR)*(RD/RG)*SITR*(&
                   & SITFL(1)*ZVDERU(JLON,1)+SITFM(1)*ZVDERV(JLON,1) )&
                   & + (RGWFRIC(1)*ZVDERW_FE(JLON,1)-RGWFRIC(0)*ZVDERW_FE(JLON,0)&
                   & + ( (SIGFL(2)*ZVDERU(JLON,2)&
                   & + SIWF2H(1)*(SIGFL(1)*ZVDERU(JLON,1)&
                   & - SIGFL(2)*ZVDERU(JLON,2)))&
                   & + (SIGFM(2)*ZVDERV(JLON,2)&
                   & + SIWF2H(1)*(SIGFM(1)*ZVDERV(JLON,1)&
                   & - SIGFM(2)*ZVDERV(JLON,2)))&
                   & - SIGFL(1)*ZVDERU(JLON,1)-SIGFM(1)*ZVDERV(JLON,1))/RG )&
                   & /(SILNPR_NH(1)/YDVETA%VDETA_RATIO(1))
        ZVLAPL(JLON,KLEV) = (SITRAM(KLEV)/SITR)*(RD/RG)*SITR*(&
                   &   SITFL(KLEV)*ZVDERU(JLON,KLEV)&
                   & + SITFM(KLEV)*ZVDERV(JLON,KLEV) )&
                   & + ( -RGWFRIC(KLEV-1)*ZVDERW_FE(JLON,KLEV-1)&
                   & + ( -(SIGFL(KLEV)*ZVDERU(JLON,KLEV) &
                   & + SIWF2H(KLEV-1)*(SIGFL(KLEV-1)*ZVDERU(JLON,KLEV-1)&
                   & - SIGFL(KLEV)*ZVDERU(JLON,KLEV)))&
                   & - (SIGFM(KLEV)*ZVDERV(JLON,KLEV) &
                   & + SIWF2H(KLEV-1)*(SIGFM(KLEV-1)*ZVDERV(JLON,KLEV-1)&
                   & - SIGFM(KLEV)*ZVDERV(JLON,KLEV))))/RG )&
                   & /(SILNPR_NH(KLEV)/YDVETA%VDETA_RATIO(KLEV))
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
    ELSE ! .not. lrefine_silapl

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=1,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZVLAPL(JLON,JLEV) = (RGWFRIC(JLEV)*ZVDERW_FE(JLON,JLEV)&
                   & - RGWFRIC(JLEV-1)*ZVDERW_FE(JLON,JLEV-1) )&
                   & /(SILNPR_NH(JLEV)/YDVETA%VDETA_RATIO(JLEV))
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,KLEV) = (-RGWFRIC(KLEV-1)*ZVDERW_FE(JLON,KLEV-1))&
                   & /(SILNPR_NH(KLEV)/YDVETA%VDETA_RATIO(KLEV))
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

    ENDIF ! end lrefine_silapl
    
  !*--------------------------------------------------------------------------------------
  ELSEIF (YDCVER%LVFE_LAPL_HALF) THEN

    ! --- VFE: case (LVFE_ECMWF=F, YDCVER%LVFE_GW_HALF=T) ---
    ! This piece of code uses RDERBH01.
    ! Two separate first order derivatives are done, with an intermediate half
    ! level quantity

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        ZIN1(JLON,JLEV)=SITLAF_NH(JLEV)*PV1(JLON,JLEV) ! pdep
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZIN1(JLON,0) = 0.0_JPRB
      ZIN1(JLON,KLEV+1)=0.0_JPRB
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

    ! d/deta ( pi*qhat )
    CALL VERDISINT(YDVFE,YDCVER,'HDER','01',KLON,KIDIA,KFDIA,KLEV,ZIN1,ZVDERW_FE)

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        ZIN1(JLON,JLEV)=PV1(JLON,JLEV) ! Qcha
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZIN1(JLON,0) = 0.0_JPRB
      ZIN1(JLON,KLEV+1)=0.0_JPRB
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

    ! d/deta ( qhat )
    CALL VERDISINT(YDVFE,YDCVER,'HDER','01',KLON,KIDIA,KFDIA,KLEV,ZIN1,ZVDERW_FD)

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV-1
      DO JLON=KIDIA,KFDIA
        ZVDERW_FE(JLON,JLEV)= (ZKAP_B*ZVDERW_FE(JLON,JLEV) &
          & +(1.0_JPRB-ZKAP_B)*SITLAH_NH(JLEV)*ZVDERW_FD(JLON,JLEV))*( &
          & (YDVETA%VFE_ETAF(JLEV+1)-YDVETA%VFE_ETAF(JLEV)) &
          & /(SITLAF_NH(JLEV+1)-SITLAF_NH(JLEV)) )
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZVDERW_FE(JLON,0   )= ZKAP_B*ZVDERW_FE(JLON,0)*( &
       & (YDVETA%VFE_ETAF(1)-YDVETA%VFE_ETAH(0))/(SITLAF_NH(1)-SITLAH_NH(0)))
      ZVDERW_FE(JLON,KLEV)=(ZKAP_B*ZVDERW_FE(JLON,KLEV) &
       & +(1.0_JPRB-ZKAP_B)*SITLAH_NH(KLEV)*ZVDERW_FD(JLON,KLEV))*( &
       & (YDVETA%VFE_ETAH(KLEV)-YDVETA%VFE_ETAF(KLEV))/(SITLAH_NH(KLEV)-SITLAF_NH(KLEV)))
    ENDDO
!$OMP END DO
!$OMP END PARALLEL
      ! * term pi/[d pi/d eta] * [dX/deta]    

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV-1
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,JLEV) = ( RGWFRIC(JLEV)*ZVDERW_FE(JLON,JLEV)&
         & - RGWFRIC(JLEV-1)*ZVDERW_FE(JLON,JLEV-1) )/SILNPR_NH(JLEV)
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZVLAPL(JLON,KLEV)=(-RGWFRIC(KLEV-1)*ZVDERW_FE(JLON,KLEV-1))/SILNPR_NH(KLEV)
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

  ELSE ! .not. (lvfe_compatible .or. lvfe_lapl_half) 
      
    ! * transform PV1 into form appropriate for VERDISINT:

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        ZIN(JLON,JLEV)=PV1(JLON,JLEV) 
        ZIN1(JLON,JLEV)=PV1(JLON,JLEV) ! Qcha
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZIN1(JLON,0     )=0.0_JPRB
      ZIN1(JLON,KLEV+1)=0.0_JPRB
    ENDDO
!$OMP END DO
!$OMP END PARALLEL
      
    ! * ( d Qcha/deta )
    CALL VERDISINT(YDVFE,YDCVER,'FDER','11',KLON,KIDIA,KFDIA,KLEV,ZIN1,ZINDETA)
    ! * (d^2 Qcha/deta^2)
    CALL VERDISINT(YDVFE,YDCVER,'DDER','01',KLON,KIDIA,KFDIA,KLEV,ZIN1,ZINDETA2)

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        ZIN1(JLON,JLEV)=(SITLAF_NH(JLEV)**2)*(&
         & SIRDEL_NH(JLEV)/YDVETA%VFE_RDETAH(JLEV))  ! *(p^2/[d pi/d eta])
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZIN1(JLON,0     )=0.0_JPRB
      ZIN1(JLON,KLEV+1)=2.0_JPRB*SIPRA
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

    ! * d/deta (p^2/[d pi/d eta])
    CALL VERDISINT(YDVFE,YDCVER,'FDER','01',KLON,KIDIA,KFDIA,KLEV,ZIN1,ZP2MDETA)     

    ! * compute Lapl = 2*(pi/[d pi/d eta]_l) * (dX/deta) 
    !                  + (pi/[d pi/d eta]_l)**2 * (d^2 X/deta^2)      
    IF(YDCVER%LVFE_LAPL_BC)THEN

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=1,KLEV
        DO JLON=KIDIA,KFDIA
          ZVLAPL(JLON,JLEV)= (SIRDEL_NH(JLEV)/YDVETA%VFE_RDETAH(JLEV))*(&
           & ZP2MDETA(JLON,JLEV)*ZINDETA(JLON,JLEV)+(SITLAF_NH(JLEV)**2)*(&
           & (SIRDEL_NH(JLEV)/YDVETA%VFE_RDETAH(JLEV))*ZINDETA2(JLON,JLEV)) &
           & -(1.0_JPRB-ZKAP_B)*SITLAF_NH(JLEV)*ZINDETA(JLON,JLEV))
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
    ELSE ! .not. lvfe_lapl_bc

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=2,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZVLAPL(JLON,JLEV) = (SIRDEL_NH(JLEV)/YDVETA%VFE_RDETAH(JLEV))*(&
           & ZP2MDETA(JLON,JLEV)*ZINDETA(JLON,JLEV)+(SITLAF_NH(JLEV)**2)*(&
           & (SIRDEL_NH(JLEV)/YDVETA%VFE_RDETAH(JLEV))*ZINDETA2(JLON,JLEV)) &
           & -(1.0_JPRB-ZKAP_B)*SITLAF_NH(JLEV)*ZINDETA(JLON,JLEV))
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,1)   =-SIZA(1)*ZIN(JLON,1) + SIZC(1)*(ZIN(JLON,2)-ZIN(JLON,1))
        ZVLAPL(JLON,KLEV)= SIZA(KLEV)*(ZIN(JLON,KLEV-1)-ZIN(JLON,KLEV)) - SIZC(KLEV)*ZIN(JLON,KLEV)
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
    ENDIF ! LVFE_LAPL_BC
  ENDIF ! LVFE_COMPATIBLE

ELSE !* not lvertfe

  ! --- Finite differences scheme ---

  ! * transform PV1 into appropriate matrix form :
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      ZIN(JLON,JLEV)=PV1(JLON,JLEV) ! array to be derivated
    ENDDO
  ENDDO
!$OMP END DO
!$OMP END PARALLEL

  IF (YDDYN%NOPT_SISLP/=0) THEN
  
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV-1
      DO JLON=KIDIA,KFDIA
        ZVDERW_FD(JLON,JLEV) = &
         & ZKAP_B*( (SITLAF_NH(JLEV+1)*ZIN(JLON,JLEV+1)-SITLAF_NH(JLEV)*ZIN(JLON,JLEV)) &
         & /(SITLAF_NH(JLEV+1)-SITLAF_NH(JLEV)) ) &
         & +(1.0_JPRB-ZKAP_B)*SITLAH_NH(JLEV)*( (ZIN(JLON,JLEV+1)-ZIN(JLON,JLEV)) &
         & /(SITLAF_NH(JLEV+1)-SITLAF_NH(JLEV)) )
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZVDERW_FD(JLON,0   ) = ZKAP_B*ZIN(JLON,1)
      ZVDERW_FD(JLON,KLEV) = ZKAP_B*ZIN(JLON,KLEV)
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

    IF (YDCVER%LCOMPATIBLE) THEN   

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        ZVDERU(JLON,JLEV) = ( SIGFL(JLEV)*(SIWH2F(JLEV,2)*ZVDERW_FD(JLON,JLEV)&
                     & + SIWH2F(JLEV,1)*ZVDERW_FD(JLON,JLEV-1))&
                     & - RD*SITRM(JLEV)*SITFL(JLEV)*ZIN(JLON,JLEV) )/RG
        ZVDERV(JLON,JLEV) = ( SIGFM(JLEV)*(SIWH2F(JLEV,2)*ZVDERW_FD(JLON,JLEV)&
                     & + SIWH2F(JLEV,1)*ZVDERW_FD(JLON,JLEV-1))&
                     & - RD*SITRM(JLEV)*SITFM(JLEV)*ZIN(JLON,JLEV) )/RG
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=2,KLEV-1
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,JLEV) = (SITRAM(JLEV)/SITR)*(RD/RG)*SITR*(&
                     & SITFL(JLEV)*ZVDERU(JLON,JLEV)+SITFM(JLEV)*ZVDERV(JLON,JLEV))&
                     & + (RGWFRIC(JLEV)*ZVDERW_FD(JLON,JLEV)&
                     & -RGWFRIC(JLEV-1)*ZVDERW_FD(JLON,JLEV-1)&
                     & + ( (SIGFL(JLEV+1)*ZVDERU(JLON,JLEV+1)&
                     & +SIWF2H(JLEV)*(SIGFL(JLEV)*ZVDERU(JLON,JLEV)&
                     & -SIGFL(JLEV+1)*ZVDERU(JLON,JLEV+1)))&
                     & + (SIGFM(JLEV+1)*ZVDERV(JLON,JLEV+1)&
                     & +SIWF2H(JLEV)*(SIGFM(JLEV)*ZVDERV(JLON,JLEV)&
                     & -SIGFM(JLEV+1)*ZVDERV(JLON,JLEV+1)))&
                     & - (SIGFL(JLEV)*ZVDERU(JLON,JLEV)&
                     & +SIWF2H(JLEV-1)*(SIGFL(JLEV-1)*ZVDERU(JLON,JLEV-1)&
                     & -SIGFL(JLEV)*ZVDERU(JLON,JLEV)))&
                     & - (SIGFM(JLEV)*ZVDERV(JLON,JLEV)&
                     & +SIWF2H(JLEV-1)*(SIGFM(JLEV-1)*ZVDERV(JLON,JLEV-1)&
                     & -SIGFM(JLEV)*ZVDERV(JLON,JLEV))))/RG)/SILNPR_NH(JLEV)
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLON=KIDIA,KFDIA
      ZVLAPL(JLON,1) = (SITRAM(1)/SITR)*(RD/RG)*SITR*(&
                     & SITFL(1)*ZVDERU(JLON,1)+SITFM(1)*ZVDERV(JLON,1) )&
                     & + (RGWFRIC(1)*ZVDERW_FD(JLON,1)-RGWFRIC(0)*ZVDERW_FD(JLON,0)&
                     & + (  (SIGFL(2)*ZVDERU(JLON,2)&
                     & +SIWF2H(1)*(SIGFL(2)*ZVDERU(JLON,1)-SIGFL(2)*ZVDERU(JLON,2)))&
                     & + (SIGFM(2)*ZVDERV(JLON,2)&
                     & +SIWF2H(1)*(SIGFM(1)*ZVDERV(JLON,1)-SIGFM(2)*ZVDERV(JLON,2)))&
                     & -SIGFL(1)*ZVDERU(JLON,1)-SIGFM(1)*ZVDERV(JLON,1))/RG)/SILNPR_NH(1)
      ZVLAPL(JLON,KLEV) = (SITRAM(KLEV)/SITR)*(RD/RG)*SITR*(&
                     &   SITFL(KLEV)*ZVDERU(JLON,KLEV)&
                     & + SITFM(KLEV)*ZVDERV(JLON,KLEV) )&
                     & + ( -RGWFRIC(KLEV-1)*ZVDERW_FD(JLON,KLEV-1)&
                     & + ( -(SIGFL(KLEV)*ZVDERU(JLON,KLEV)&
                     & +SIWF2H(KLEV-1)*(SIGFL(KLEV-1)*ZVDERU(JLON,KLEV-1)&
                     & - SIGFL(KLEV)*ZVDERU(JLON,KLEV)))&
                     & - (SIGFM(KLEV)*ZVDERV(JLON,KLEV)&
                     & +SIWF2H(KLEV-1)*(SIGFM(KLEV-1)*ZVDERV(JLON,KLEV-1)&
                     & -SIGFM(KLEV)*ZVDERV(JLON,KLEV))))/RG)/SILNPR_NH(KLEV)
    ENDDO
!$OMP END DO
!$OMP END PARALLEL
    ELSE ! .not. Lvfd_compatible but still refine_silapl
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=1,KLEV
        DO JLON=KIDIA,KFDIA
          ZVDERU(JLON,JLEV) = ((SIWH2F(JLEV,2)*SIGHL(JLEV)*ZVDERW_FD(JLON,JLEV)&
                     & + SIWH2F(JLEV,1)*SIGHL(JLEV-1)*ZVDERW_FD(JLON,JLEV-1))&
                     & - RD*SITRM(JLEV)*SITFL(JLEV)*ZIN(JLON,JLEV) )/RG
          ZVDERV(JLON,JLEV) = ((SIWH2F(JLEV,2)*SIGHM(JLEV)*ZVDERW_FD(JLON,JLEV)&
                     & + SIWH2F(JLEV,1)*SIGHM(JLEV-1)*ZVDERW_FD(JLON,JLEV-1))&
                     & - RD*SITRM(JLEV)*SITFM(JLEV)*ZIN(JLON,JLEV) )/RG
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=2,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZVLAPL(JLON,JLEV) = (SITRAM(JLEV)/SITR)*(RD/RG)*SITR*(&
                     & SITFL(JLEV)*ZVDERU(JLON,JLEV)+SITFM(JLEV)*ZVDERV(JLON,JLEV))&
                     & +(RGWFRIC(JLEV)*ZVDERW_FD(JLON,JLEV) &
                     & - RGWFRIC(JLEV-1)*ZVDERW_FD(JLON,JLEV-1)&
                     & +(SIGHL(JLEV)*(ZVDERU(JLON,JLEV+1)&
                     & + SIWF2H(JLEV)*(ZVDERU(JLON,JLEV)-ZVDERU(JLON,JLEV+1)))&
                     & + SIGHM(JLEV)*(ZVDERV(JLON,JLEV+1)&
                     & + SIWF2H(JLEV)*(ZVDERV(JLON,JLEV)-ZVDERV(JLON,JLEV+1)))&
                     & - SIGHL(JLEV-1)*(ZVDERU(JLON,JLEV)&
                     & + SIWF2H(JLEV-1)*(ZVDERU(JLON,JLEV-1)-ZVDERU(JLON,JLEV)))&
                     & - SIGHM(JLEV-1)*(ZVDERV(JLON,JLEV)&
                     & + SIWF2H(JLEV-1)*(ZVDERV(JLON,JLEV-1)-ZVDERV(JLON,JLEV))))/RG)&
                     & /SILNPR_NH(JLEV)
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,1) = (SITRAM(1)/SITR)*(RD/RG)*SITR*(&
                     &  SITFL(1)*ZVDERU(JLON,1)+SITFM(1)*ZVDERV(JLON,1) )&
                     & +(RGWFRIC(1)*ZVDERW_FD(JLON,1) &
                     & -RGWFRIC(0)*ZVDERW_FD(JLON,0)&
                     & +( SIGHL(1)*(ZVDERU(JLON,2)&
                     & +SIWF2H(1)*(ZVDERU(JLON,1)-ZVDERU(JLON,2)))&
                     & +SIGHM(1)*(ZVDERV(JLON,2)&
                     & +SIWF2H(1)*(ZVDERV(JLON,1)-ZVDERV(JLON,2)))&
                     & -SIGHL(0)*ZVDERU(JLON,1)-SIGHM(0)*ZVDERV(JLON,1))/RG)&
                     & /SILNPR_NH(1)
        ZVLAPL(JLON,KLEV) = (SITRAM(KLEV)/SITR)*(RD/RG)*SITR*(&
                     &  SITFL(KLEV)*ZVDERU(JLON,KLEV)&
                     & +SITFM(KLEV)*ZVDERV(JLON,KLEV) )&
                     & +(-RGWFRIC(KLEV-1)*ZVDERW_FD(JLON,KLEV-1)&
                     & +(-SIGHL(KLEV-1)*(ZVDERU(JLON,KLEV)&
                     & +SIWF2H(KLEV-1)*(ZVDERU(JLON,KLEV-1)-ZVDERU(JLON,KLEV)))&
                     & -SIGHM(KLEV-1)*(ZVDERV(JLON,KLEV)&
                     & +SIWF2H(KLEV-1)*(ZVDERV(JLON,KLEV-1)-ZVDERV(JLON,KLEV))))/RG)&
                     & /SILNPR_NH(KLEV)

      ENDDO
!$OMP END DO
!$OMP END PARALLEL
 
    ENDIF ! end lcompatible 

  ELSE !* .not. refine_silapl 

    IF (YDDYN%LGWFRIC) THEN

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=1,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZVDERW_FD(JLON,JLEV) = &
           & ZKAP_B*( (SITLAF_NH(JLEV+1)*ZIN(JLON,JLEV+1)-SITLAF_NH(JLEV)*ZIN(JLON,JLEV)) &
           & /(SITLAF_NH(JLEV+1)-SITLAF_NH(JLEV)) ) &
           & +(1.0_JPRB-ZKAP_B)*SITLAF_NH(JLEV)*( (ZIN(JLON,JLEV+1)-ZIN(JLON,JLEV)) &
           & /(SITLAF_NH(JLEV+1)-SITLAF_NH(JLEV)) )
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLON=KIDIA,KFDIA
        ZVDERW_FD(JLON,0   ) = ZKAP_B*ZIN(JLON,1)  
        ZVDERW_FD(JLON,KLEV) = ZKAP_B*ZIN(JLON,KLEV)
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=1,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZVLAPL(JLON,JLEV) = ( RGWFRIC(JLEV)*ZVDERW_FD(JLON,JLEV)&
           & -RGWFRIC(JLEV-1)*ZVDERW_FD(JLON,JLEV-1))/SILNPR_NH(JLEV)
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,KLEV) = (-RGWFRIC(KLEV-1)*ZVDERW_FD(JLON,KLEV-1))/SILNPR_NH(KLEV)
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
    ELSE  ! .not. lgwfric
!$OMP PARALLEL PRIVATE(JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLON=KIDIA,KFDIA
        ZVLAPL(JLON,1)    = -SIZA(1)*ZIN(JLON,1) &
         & + SIZC(1)*(ZIN(JLON,2)-ZIN(JLON,1))
        ZVLAPL(JLON,KLEV) = SIZA(KLEV)*(ZIN(JLON,KLEV-1)-ZIN(JLON,KLEV)) &
         & - SIZC(KLEV)*ZIN(JLON,KLEV) 
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
      DO JLEV=2,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZVLAPL(JLON,JLEV) = SIZC(JLEV)*(ZIN(JLON,JLEV+1)-ZIN(JLON,JLEV)) &
           & + SIZA(JLEV)*(ZIN(JLON,JLEV-1)-ZIN(JLON,JLEV))
        ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
    ENDIF ! end LGWFRIC
  ENDIF ! end (NOPT_SISLP/=0)   
ENDIF ! end LVERTFE


! memory transfert zvlapl ---> pv2
!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
DO JLEV=1,KLEV
  DO JLON=KIDIA,KFDIA
    PV2(JLON,JLEV)=ZVLAPL(JLON,JLEV)
  ENDDO
ENDDO
!$OMP END DO
!$OMP END PARALLEL

!     ------------------------------------------------------------------

!*       3.    MULTIPLY BY SITR/SITRAM
!              ----------------------


IF(PRESENT(LD_LSTAR)) THEN
  LL_LSTAR=LD_LSTAR
ELSE
  LL_LSTAR=.FALSE.
ENDIF

IF (.NOT.LL_LSTAR) THEN

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        PV2(JLON,JLEV)=(YDDYN%SITR/YDDYN%SITRAM(JLEV))*PV2(JLON,JLEV)
      ENDDO
    ENDDO
!$OMP END DO
!$OMP END PARALLEL

ENDIF

!* Apply Siskapi vertical operator 
IF(PRESENT(LD_SKAPI)) THEN
  LL_SKAPI=LD_SKAPI
ELSE
  LL_SKAPI=YDDYNA%LNHQE.AND.YDDYN%LNHQE_SOLVER_SP
ENDIF

IF (LL_SKAPI) THEN

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC)
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      ZV2(JLON,JLEV)=PV2(JLON,JLEV)
    ENDDO
  ENDDO
!$OMP END DO
!$OMP END PARALLEL
                                
  CALL SISKAPI(YDCST, YDGEOMETRY, YDDYN, YDDYNA, KLON, KLEV, KIDIA, KFDIA, ZV2, PV2)

ENDIF

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SISEVE',1,ZHOOK_HANDLE)
END SUBROUTINE SISEVE

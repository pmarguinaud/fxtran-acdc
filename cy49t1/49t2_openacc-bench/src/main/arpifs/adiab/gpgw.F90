SUBROUTINE GPGW(YDGEOMETRY,LDNHDYN,KFLEV,KPROMA,KST,KEND,LDGWF,LDGDWI,POROGL,POROGM,PLNPR,PALPH,&
 & PUS,PVS,PRT,PDVER,PGWH,PGWF,LDVFE,PRNHPPI,PTAUD_NL,PGDW)

! GPGW - Diagnoses "Gw" from the vertical divergence "dver" or from "-G dw".

! Purpose
! -------
!   Diagnoses "Gw" from the vertical divergence "dver" if LDGDWI=F, from "-G dw" if LDGDWI=T.
!   For the finite element vertical discretization (lvertfe=T + lvfe_gw=T), "Gw" is computed only at full levels.
!   For the finite difference vertical discretization, "Gw" is computed at both half and full levels.
!   Calculation is done by vertical integration of the formula:
!    dver = - G/(RT) (pre/prehyd) (d w / d log(prehyd))
!   with the following bottom condition:
!    w_surf = V_surf grad[Phi_s].

!   This routine can be used in a NHEE model: PRNHPPI should be present in this case.

!   This routine can be used in a NHQE or a hydrostatic model: in this case
!   the ratio (prehyd/pre) is equal to 1 and PRNHPPI should not be used.

! Interface
! ---------
!   * INPUT:
!   YDGEOMETRY   : structure containing all geometry.
!   KFLEV        : number of levels.
!   KPROMA       : horizontal dimension.
!   KST       : start of work.
!   KEND         : end of work.
!   LDGWF        : calculation of "Gw" at full layers asked for
!                  if finite difference vertical discretization.
!   LDGDWI       : T vs F: input content of PDVER is "-G dw" vs "dver".
!   POROGL       : zonal component of grad[Phi_s].
!   POROGM       : meridian component of grad[Phi_s].
!   PLNPR        : "delta" at full layers (computed in GPXYB).
!   PALPH        : "alpha" at full layers (computed in GPXYB).
!   PUS          : surface U wind.
!   PVS          : surface V wind.
!   PRT          : (RT) at full levels, with the version of R used to define vertical divergence "dver".
!                  R may be Rdry or Rmoist according to definition of vertical divergence "dver".
!   PDVER        : vertical divergence "dver" at full layers.

!   * OUTPUT:
!   PGWH         : G times vertical velocity w at half layers.
!                  (computed only if lvertfe=F)
!   PGWF         : G times vertical velocity w at full layers.

!   * OPTIONAL INPUT:
!   LDVFE        : T if VFE discretisation is used in this routine.
!   PRNHPPI      : (prehyd/pre) at full layers, required for NHEE.

!   * OPTIONAL OUTPUT:
!   PGDW         : contains 'G (dw)'.

! Externals
! ---------

! Method
! ------

! Reference
! ---------

! Author
! ------
!   K. Yessad, Dec 2004 (after GNHSVD2GW)

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (June 2017): Introduce NHQE model.
!   J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   J. Vivoda and P. Smolikova (Sep 2020): VFE pruning.
!   H. Petithomme (Dec 2020): optimisation and test re-organization
!------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK

! -----------------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)     ,INTENT(IN)                     :: YDGEOMETRY
LOGICAL            ,INTENT(IN)                     :: LDNHDYN
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KFLEV,KPROMA,KST,KEND
LOGICAL            ,INTENT(IN)                     :: LDGWF,LDGDWI
REAL(KIND=JPRB)    ,INTENT(IN)                     :: POROGL(KPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: POROGM(KPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PLNPR(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PALPH(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PUS(KPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PVS(KPROMA)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PRT(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PDVER(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)           ,TARGET  :: PGWH(KPROMA,0:KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT)                    :: PGWF(KPROMA,KFLEV)
LOGICAL            ,INTENT(IN)  ,OPTIONAL          :: LDVFE
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL          :: PRNHPPI(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL          :: PTAUD_NL
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL ,TARGET  :: PGDW(KPROMA,KFLEV)

INTEGER(KIND=JPIM) :: JLEV,JROF
REAL(KIND=JPRB),TARGET :: ZGDW0(KPROMA,KFLEV)
REAL(KIND=JPRB) :: ZIN(KPROMA,0:KFLEV+1)
REAL(KIND=JPRB),CONTIGUOUS,POINTER :: ZGDW(:,:)
REAL(KIND=JPRB) :: ZNHPPI, ZRNHPPI(KPROMA,KFLEV)
LOGICAL :: LLVFE
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "abor1.intfb.h"
#include "verdisint.intfb.h"

IF (LHOOK) CALL DR_HOOK('GPGW',0,ZHOOK_HANDLE)

IF (PRESENT(LDVFE)) THEN
  LLVFE = LDVFE
ELSE
  LLVFE = YDGEOMETRY%YRVERT_GEOM%YRCVER%LVERTFE.AND.(YDGEOMETRY%YRVERT_GEOM%YRCVER%LVFE_GW.OR..NOT.LDNHDYN) &
        & .AND. (.NOT.YDGEOMETRY%YRVERT_GEOM%YRCVER%LVFE_COMPATIBLE)
ENDIF

! optim: use of pointer avoids copying, but dependencies may arise (use ivdep/nodep)
IF (LLVFE.AND.PRESENT(PGDW)) THEN
  ZGDW => PGDW
ELSE
  ZGDW => ZGDW0
ENDIF

! * Compute "Gw" at the surface (free slip boundary condition)
! optim: use pointer on last level
PGWH(KST:KEND,KFLEV) = PUS(KST:KEND)*POROGL(KST:KEND)+&
 & PVS(KST:KEND)*POROGM(KST:KEND)

! * Transform "dver" into "-G.dw"
IF (LDGDWI) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ZGDW(JROF,JLEV) = PDVER(JROF,JLEV)
    ENDDO
  ENDDO
ELSE IF (PRESENT(PRNHPPI)) THEN
  IF (PRESENT(PTAUD_NL)) THEN
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZNHPPI = 1.0_JPRB + PTAUD_NL * (1.0_JPRB / PRNHPPI(JROF,JLEV) - 1.0_JPRB)
        ZRNHPPI(JROF,JLEV) = 1.0_JPRB / ZNHPPI
      ENDDO
    ENDDO
  ELSE
    ZRNHPPI(KST:KEND,1:KFLEV)=PRNHPPI(KST:KEND,1:KFLEV)
  ENDIF
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ZGDW(JROF,JLEV) = PDVER(JROF,JLEV)*PRT(JROF,JLEV)*PLNPR(JROF,JLEV)*PRNHPPI(JROF,JLEV)
    ENDDO
  ENDDO
ELSE
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ZGDW(JROF,JLEV) = PDVER(JROF,JLEV)*PRT(JROF,JLEV)*PLNPR(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

! * Compute "Gw" at full (llvfe=T) or half levels
IF (LLVFE) THEN
  ! * Store "G dw" at full levels.

  DO JLEV=1,KFLEV
    ZIN(KST:KEND,JLEV) = -ZGDW(KST:KEND,JLEV)*YDGEOMETRY%YRVERT_GEOM%YRVETA%VFE_RDETAH(JLEV)
  ENDDO

  IF (LDGDWI) THEN
    ZIN(KST:KEND,0)      = ZGDW(KST:KEND,1)
    ZIN(KST:KEND,KFLEV+1)= ZGDW(KST:KEND,KFLEV)
    ! Apply RINTBF00, constructed from bottom
    CALL VERDISINT(YDGEOMETRY%YRVERT_GEOM%YRVFE,YDGEOMETRY%YRVERT_GEOM%YRCVER,'ITOP','00',KPROMA,KST,KEND,&
     & KFLEV,ZIN,PGWF,PINS=PGWH(:,KFLEV),KLOUT=KFLEV)
  ELSEIF (LDNHDYN) THEN
    ZIN(KST:KEND,0)      = ZGDW(KST:KEND,1)
    ZIN(KST:KEND,KFLEV+1)= ZGDW(KST:KEND,KFLEV) ! not applied with INGW
    CALL VERDISINT(YDGEOMETRY%YRVERT_GEOM%YRVFE,YDGEOMETRY%YRVERT_GEOM%YRCVER,'INGW','00',KPROMA,KST,KEND,&
     & KFLEV,ZIN,PGWF,PINS=PGWH(:,KFLEV),KLOUT=KFLEV)
  ELSE
    ZIN(KST:KEND,0) = 0.0_JPRB
    ZIN(KST:KEND,KFLEV+1) = 0.0_JPRB
    CALL VERDISINT(YDGEOMETRY%YRVERT_GEOM%YRVFE,YDGEOMETRY%YRVERT_GEOM%YRCVER,'IBOT','11',KPROMA,KST,KEND,&
     & KFLEV,ZIN,PGWF,PINS=PGWH(:,KFLEV))
  ENDIF
ELSE

  IF (YDGEOMETRY%YRVERT_GEOM%YRCVER%LVFE_COMPATIBLE) THEN

    ! transform -G.dw into Gw
    DO JLEV=KFLEV,1,-1
      !CDIR NODEP
      !DIR$ IVDEP
      DO JROF=KST,KEND
        PGWH(JROF,JLEV-1) = PGWH(JROF,JLEV) + (ZGDW(JROF,JLEV)/YDGEOMETRY%YRVERT_GEOM%YRVETA%VDETA_RATIO(JLEV))
      ENDDO
    ENDDO

    IF (LDGWF) THEN
      IF (LDGDWI) call abor1(' GPGW: compute "Gw" at full levels: case not coded')

      ! compute "G(dw/deta)" at full levels
      DO JLEV=1,KFLEV
        DO JROF=KST,KEND
          ZIN(JROF,JLEV) = -ZGDW(JROF,JLEV)*( &
           &  YDGEOMETRY%YRVERT_GEOM%YRVETA%VDETA_RATIO(JLEV)/YDGEOMETRY%YRVERT_GEOM%YRVETA%VFE_RDETAH(JLEV) )
        ENDDO
      ENDDO
      ! Apply RINTBF11, constructed from bottom
      ZIN(KST:KEND,0) = 0.0_JPRB
      ZIN(KST:KEND,KFLEV+1) = 0.0_JPRB
      CALL VERDISINT(YDGEOMETRY%YRVERT_GEOM%YRVFE,YDGEOMETRY%YRVERT_GEOM%YRCVER,'IBOT','11',KPROMA,KST,KEND,&
        &KFLEV,ZIN,PGWF,PINS=PGWH(:,KFLEV))
    ENDIF

  ELSE

    ! transform -G.dw into Gw
    DO JLEV=KFLEV,1,-1
      !CDIR NODEP
      !DIR$ IVDEP
      DO JROF=KST,KEND
        PGWH(JROF,JLEV-1) = PGWH(JROF,JLEV)+ZGDW(JROF,JLEV)
      ENDDO
    ENDDO

    IF (LDGWF) THEN
      IF (LDGDWI) call abor1(' GPGW: compute "Gw" at full levels: case not coded')

      ! * Also compute "Gw" at full levels
      ! k.y.: formula pgwf(jlev)=pgwh(jlev)(1-palph(jlev)/plnpr(jlev))
      ! +pgwh(jlev-1)(palph(jlev)/plnpr(jlev)) must be equivalent.
      IF (PRESENT(PRNHPPI)) THEN
        DO JLEV=1,KFLEV
          DO JROF=KST,KEND
            PGWF(JROF,JLEV) = PGWH(JROF,JLEV)+&
              & PDVER(JROF,JLEV)*PRT(JROF,JLEV)*PALPH(JROF,JLEV)*ZRNHPPI(JROF,JLEV)
          ENDDO
        ENDDO
      ELSE
        DO JLEV=1,KFLEV
          DO JROF=KST,KEND
            PGWF(JROF,JLEV) = PGWH(JROF,JLEV)+PDVER(JROF,JLEV)*PRT(JROF,JLEV)*PALPH(JROF,JLEV)
          ENDDO
        ENDDO
      ENDIF
    ENDIF

  ENDIF

ENDIF

IF (LHOOK) CALL DR_HOOK('GPGW',1,ZHOOK_HANDLE)
END SUBROUTINE GPGW


SUBROUTINE SPNHSI(&
 ! --- INPUT -----------------------------------------------------------------
 & YDCST,YDGEOMETRY,YDLDDH,YDRIP,YDDYN,YDDYNA,KM,KMLOC,KSTA,KEND,KSPEC2V,LDONEM,LDOHD,&
 & LESIDG,&
 ! --- INOUT -----------------------------------------------------------------
 & PSPVORG,PSPDIVG,PSPTG,PSPSPG,PSPSPDG,PSPSVDG,&
 & PSPTNDSI_VORG,PSPTNDSI_DIVG,PSPTNDSI_TG,PSPTNDSI_SPDG,PSPTNDSI_SVDG,&
 & PSPAUXG)

!$ACDC semiimplicit 

!**** *SPNHSI* - SPECTRAL SPACE SEMI-IMPLICIT COMPUTATIONS FOR NH MODEL.

!     Purpose.
!     --------

!     Semi-implicit scheme in the NH model with (Qcha, dver or d4) as
!     NH prognostic variables.

!     When the constraint C1 is matched, I_NITERHELM=0; if not matched
!     I_NITERHELM>0.
!     The predictor has dver(t+dt) (or d4(t+dt)), D'(t+dt), zeta'(t+dt),
!     Qcha(t+dt), T(t+dt), log(prehyds)(t+dt) as unknown.
!     The corrector steps (JITER>0) work in an incremental manner,
!     the unknowns are (delta X) = X(jiter) - X(iter=0), where
!     X=dver(t+dt), D'(t+dt), etc...
!     The incremental method makes the calculation of the RHS simpler
!     in the corrector steps; this is equivalent to replace:
!      * zeta_star by 0.
!      * Dcha_star_star by 0.
!      * Dprim_star_star by
!        beta**2 (Delta t)**2 vnabla'**2 * C**2 * COR * Mbar**2 D'(t+dt,jiter-1)


!**   Interface.
!     ----------
!        *CALL* *SPNHSI(..)

!        Explicit arguments :
!        -------------------- 
!        * INPUT:
!          KM          - Zonal wavenumber 
!          KMLOC       - Zonal wavenumber (DM-local numbering)
!          KSTA        - First column processed
!          KEND        - Last column processed
!          LDONEM      - T if only one m if processed
!          LDOHD       - T if CDCONF eq A and NSPEC2 > 0 
!          LESIDG      - T if LELAM on stretched grid
!          PSPAUXG     - Auxilliary array for LIMPF case
!        * INPUT/OUTPUT:
!          PSPVORG     - Vorticity columns
!          PSPDIVG     - Divergence columns
!          PSPTG       - Temperature columns
!          PSPSPG      - Surface Pressure
!          PSPSPDG     - Pressure departure variable columns
!          PSPSVDG     - Vertical divergence variable columns
!          PSPTNDSI_VORG - [D vor/Dt]_SI SI tendencies for DDH
!          PSPTNDSI_DIVG - [D div/Dt]_SI SI tendencies for DDH
!          PSPTNDSI_TG   - [D T/Dt]_SI SI tendencies for DDH
!          PSPTNDSI_SPDG - Pressure departure variable (Qcha) SI tendencies for DDH
!          PSPTNDSI_SVDG - Vertical divergence variable (d4) SI tendencies for DDH
!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        K. Yessad (after SPCSI and ESPC).
!        Original : 09-Dec-2004

!     Modifications.
!     --------------
!   K. Yessad Aug-2007: iterative algorithm to relax the C1 constraint
!   K. Yessad Aug-2007: LIMPF in NH model.
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad (Aug 2009): remove LSITRIC option
!   K. Yessad (Feb 2012): tests on LL3D, LLDOSI in the caller, simplifications.
!   G. Mozdzynski (Oct 2012): improve NH performance at scale
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   K. Yessad (July 2014): Move some variables.
!   O. Marsden (May 2016): Remove redundant geometry argument
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (June 2017): Vertical-dependent SITRA.
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST       , ONLY : TCST
USE YOMMP0       , ONLY : MYSETV,MYSETN
USE YOMDYN       , ONLY : TDYN
USE YOMDYNA      , ONLY : TDYNA
USE YOMLDDH      , ONLY : TLDDH
USE YOMRIP       , ONLY : TRIP

!workaround for bit reproducibility
USE YOMCT0       , ONLY : LELAM

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)         ,INTENT(IN)      :: YDCST
TYPE(GEOMETRY)     ,INTENT(IN)      :: YDGEOMETRY
TYPE(TLDDH)        ,INTENT(IN)      :: YDLDDH
TYPE(TRIP)         ,INTENT(IN)      :: YDRIP
TYPE(TDYN)         ,INTENT(IN)      :: YDDYN
TYPE(TDYNA)        ,INTENT(IN)      :: YDDYNA
INTEGER(KIND=JPIM) ,INTENT(IN)      :: KM
INTEGER(KIND=JPIM) ,INTENT(IN)      :: KMLOC
INTEGER(KIND=JPIM) ,INTENT(IN)      :: KSTA 
INTEGER(KIND=JPIM) ,INTENT(IN)      :: KEND
INTEGER(KIND=JPIM) ,INTENT(IN)      :: KSPEC2V
LOGICAL            ,INTENT(IN)      :: LDONEM
LOGICAL            ,INTENT(IN)      :: LDOHD
LOGICAL            ,INTENT(IN)      :: LESIDG
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPVORG(KSPEC2V,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPDIVG(KSPEC2V,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPTG(KSPEC2V,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPSPG(KSPEC2V) 
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPSPDG(KSPEC2V,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPSVDG(KSPEC2V,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPTNDSI_VORG(YDGEOMETRY%YRMP%NSPEC2VDDH,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPTNDSI_DIVG(YDGEOMETRY%YRMP%NSPEC2VDDH,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPTNDSI_TG(YDGEOMETRY%YRMP%NSPEC2VDDH,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPTNDSI_SPDG(YDGEOMETRY%YRMP%NSPEC2VDDH,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(INOUT)   :: PSPTNDSI_SVDG(YDGEOMETRY%YRMP%NSPEC2VDDH,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)    ,INTENT(IN)      :: PSPAUXG(KSPEC2V,YDGEOMETRY%YRDIMV%NFLEVG)

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZZSPVORG(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZZSPDIVG(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZZSPTG(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZZSPSPG(KSTA:KEND)
REAL(KIND=JPRB) :: ZZSPSPDG(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZZSPSVDG(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)

!arrays only used if I_NITERHELM>0
REAL(KIND=JPRB) :: ZZSPGDIVG(KSTA:KEND,MERGE(YDGEOMETRY%YRDIMV%NFLEVG,0,&
    &(YDDYN%NITERHELM>0 .AND. .NOT. (YDGEOMETRY%YRVERT_GEOM%YRCVER%NDLNPR==1))))
REAL(KIND=JPRB) :: ZZSPDIV0G(KSTA:KEND,MERGE(YDGEOMETRY%YRDIMV%NFLEVG,0,&
    &(YDDYN%NITERHELM>0 .AND. .NOT. (YDGEOMETRY%YRVERT_GEOM%YRCVER%NDLNPR==1))))
REAL(KIND=JPRB) :: ZZSPGDIV0G(KSTA:KEND,MERGE(YDGEOMETRY%YRDIMV%NFLEVG,0,&
    &(YDDYN%NITERHELM>0 .AND. .NOT. (YDGEOMETRY%YRVERT_GEOM%YRCVER%NDLNPR==1))))
REAL(KIND=JPRB) :: ZZSPVOR0G(KSTA:KEND,MERGE(YDGEOMETRY%YRDIMV%NFLEVG,0,&
    &(YDDYN%NITERHELM>0 .AND. .NOT. (YDGEOMETRY%YRVERT_GEOM%YRCVER%NDLNPR==1))))
REAL(KIND=JPRB) :: ZZSPSVD0G(KSTA:KEND,MERGE(YDGEOMETRY%YRDIMV%NFLEVG,0,&
    &(YDDYN%NITERHELM>0 .AND. .NOT. (YDGEOMETRY%YRVERT_GEOM%YRCVER%NDLNPR==1))))

REAL(KIND=JPRB) :: ZSDIV(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZST(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSP(KSTA:KEND)
REAL(KIND=JPRB) :: ZSDIVP(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSPDIVP(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSVED(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZR1D(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZR1D2(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZR1DPRIM(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZR2D(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZR3D(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZR4D(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSRHS(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZWORK(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSRHS2(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSRHS2_INC(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSNHP(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)

INTEGER(KIND=JPIM) :: ISPCOL, JLEV, JSP, JN, IOFF, IMLOCSTA, IMLOCEND
INTEGER(KIND=JPIM) :: ILO, JL, IRSP, IMSP
INTEGER(KIND=JPIM) :: I_NITERHELM, JITER

REAL(KIND=JPRB) :: ZBDT, ZBDT2, ZRRT, ZCC, ZBTCM, ZBTCMH

LOGICAL :: LLNH_NOC1

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "mxmaoptr.h"
#include "siseve.intfb.h"
#include "si_cccor.intfb.h"
#include "sidd.intfb.h"
#include "sigam.intfb.h"
#include "spcimpfsolve.intfb.h"
#include "siptp.intfb.h"
#include "sitnu.intfb.h"
#include "spcsidg_part0nh.intfb.h"
#include "spcsidg_part1.intfb.h"
#include "spcsidg_part2.intfb.h"
#include "spcimpf_hsi1.intfb.h"
#include "spcimpf_hsi2.intfb.h"
#include "spcimpf_hsi3.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SPNHSI',0,ZHOOK_HANDLE)

ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM, &
 & YDDIMV=>YDGEOMETRY%YRDIMV, YDCVER=>YDGEOMETRY%YRVERT_GEOM%YRCVER, &
 & YDGEM=>YDGEOMETRY%YRGEM, YDMP=>YDGEOMETRY%YRMP,  &
 & YDLAP=>YDGEOMETRY%YRLAP, YDSPGEOM=>YDGEOMETRY%YSPGEOM)

ASSOCIATE(NSMAX=>YDDIM%NSMAX, &
 & NFLEVG=>YDDIMV%NFLEVG, &
 & LIMPF=>YDDYN%LIMPF, LSIDG=>YDDYN%LSIDG, NITERHELM=>YDDYN%NITERHELM, &
 & RBTS2=>YDDYN%RBTS2, SIFACI=>YDDYN%SIFACI, &
 & SIMI=>YDDYN%SIMI, SIMO=>YDDYN%SIMO, SITR=>YDDYN%SITR, &
 & SIVP=>YDDYN%SIVP, &
 & RSTRET=>YDGEM%RSTRET, &
 & LRSIDDH=>YDLDDH%LRSIDDH, &
 & NPTRSV=>YDMP%NPTRSV, NPTRSVF=>YDMP%NPTRSVF, &
 & TDT=>YDRIP%TDT, &
 & SCGMAP=>YDSPGEOM%SCGMAP, ESCGMAP=>YDSPGEOM%ESCGMAP,&
 & SIHEGTRA=>YDDYN%SIHEGTRA,SIHEG2TRA=>YDDYN%SIHEG2TRA,&
 & SIHEGBTRA=>YDDYN%SIHEGBTRA,SIHEGB2TRA=>YDDYN%SIHEGB2TRA,&
 & SIMITRA=>YDDYN%SIMITRA,SIMOTRA=>YDDYN%SIMOTRA,&
 & SIFACITRA=>YDDYN%SIFACITRA,NPTRMF=>YDMP%NPTRMF,&
 & SILAPIN=>YDDYN%SILAPIN,SILAPDI=>YDDYN%SILAPDI,SIVP2=>YDDYN%SIVP2,&
 & NSZNISNAX=>YDDYN%NSZNISNAX,SINISNAX=>YDDYN%SINISNAX,&
 & SIPD=>YDDYN%SIPD,SIPE=>YDDYN%SIPE,SIPF=>YDDYN%SIPF)

!     ------------------------------------------------------------------

!*       0.    TESTINGS.
!              ---------

!     ------------------------------------------------------------------



! * Case where the C1 constraint is not matched.
LLNH_NOC1=.NOT.(YDCVER%NDLNPR == 1)
IF (LLNH_NOC1) THEN
  I_NITERHELM=NITERHELM
ELSE
  I_NITERHELM=0
ENDIF

!     ------------------------------------------------------------------

!*       1.    MEMORY TRANSFER.
!              ----------------

IF (LIMPF) THEN

!$ACDC ABORT {

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      ZZSPVORG(JSP,JLEV)=PSPVORG(JSP,JLEV)  
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

!$ACDC }

ENDIF

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
DO JLEV=1,NFLEVG
  DO JSP=KSTA,KEND
    ZZSPTG  (JSP,JLEV)=PSPTG  (JSP,JLEV)
    ZZSPSPDG(JSP,JLEV)=PSPSPDG(JSP,JLEV)
  ENDDO
ENDDO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
DO JSP=KSTA,KEND
    ZZSPSPG (JSP)=PSPSPG (JSP)
ENDDO
!$OMP END PARALLEL DO

IF (LRSIDDH) THEN

!$ACDC ABORT {

  ! DDH memory transfer

  IF (LIMPF) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
    DO JLEV=1,NFLEVG
      DO JSP=KSTA,KEND
        PSPTNDSI_VORG(JSP,JLEV)=-PSPVORG(JSP,JLEV)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO

  ENDIF

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      PSPTNDSI_DIVG(JSP,JLEV)=-PSPDIVG(JSP,JLEV)
      PSPTNDSI_TG  (JSP,JLEV)=-PSPTG  (JSP,JLEV)
      PSPTNDSI_SPDG(JSP,JLEV)=-PSPSPDG(JSP,JLEV)
      PSPTNDSI_SVDG(JSP,JLEV)=-PSPSVDG(JSP,JLEV)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
  !the case of surface pressure has not been treated yet

!$ACDC }

ENDIF
!     ------------------------------------------------------------------

!*       2.    SEMI-IMPLICIT SPECTRAL COMPUTATIONS.
!              ------------------------------------

!*        2.1  Preliminary initialisations.

IF (LDONEM) THEN
  IOFF=NPTRSVF(MYSETV)-1
ELSE
  IOFF=NPTRSV(MYSETV)-1
ENDIF
ISPCOL=KEND-KSTA+1

IF ((LSIDG .OR. LESIDG) .OR. LIMPF) THEN
  !!SPCSIDG_PART1, SPCSIDG_PART2, SPCIMPF_HSI1, SPCIMPF_HSI2,
  !!SPCSIMPF_HSI3  and SPCIMPFSOLVE are
  !!called on KMLOC...KMLOC, except if KM=-999
  IF (KM==-999) THEN
    IMLOCSTA=NPTRMF(MYSETN)
    IMLOCEND=NPTRMF(MYSETN+1)-1
  ELSE
    IMLOCSTA=KMLOC
    IMLOCEND=KMLOC
  ENDIF
ENDIF

ZBDT=RBTS2*TDT
ZBDT2=(ZBDT*RSTRET)**2

ZRRT =  1.0_JPRB/(YDCST%RD*SITR)
ZCC  =  YDCST%RCPD/YDCST%RCVD
ZBTCM=  ZBDT*ZBDT*YDCST%RD*SITR*ZCC*RSTRET*RSTRET
ZBTCMH= ZBTCM*YDCST%RG*YDCST%RG*ZRRT*ZRRT

  ! ky: incorrect use of NSE0L; use the ALADIN counterpart NESE0L instead.
  !     (its set-up and allocation remain to be coded).
  !  IS0=YDLAP%NSE0L(KMLOC)
  !  IS02=0

! ky: if LESIDG, missing there the norther shift to have C+I and C+I+E
!     centered on the same reference latitude.

!*        2.2  Set up helper arrays for implicit Coriolis case.
!              (identical to the part 2.2 of SPCSI).

DO JITER=0,I_NITERHELM

  !*        2.3  Computes right-hand side of Helmholtz equation.

  IF (JITER == 0) THEN

    ! * Provides:
    !   a/ - [ gamma [SITR Qcha_rhs - T_rhs] - (Rd SITR)/(SIPR) logprehyd_rhs ]
    !      in array ZSDIV.
    !   b/ (g TRSI)/(H TARSI) LLstar Qcha_rhs in array ZSVED.


    CALL SIDD(YDCST,YDGEOMETRY,YDDYN,YDDYNA,ISPCOL,NFLEVG,1,ISPCOL,ZSDIV,ZSVED,ZZSPSPDG,ZZSPTG,ZZSPSPG)

    ! * Provides Dprim_star_star in ZSDIV.
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
        ZSDIV(JSP,JLEV)=PSPDIVG(JSP,JLEV)-ZBDT*SILAPDI(IOFF+JSP)*ZSDIV(JSP,JLEV)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO

  ELSE

    ! * Compute C**2 * COR * Mbar**2 D'(t+dt,jiter-1)
    CALL SI_CCCOR(YDCST,YDGEOMETRY,YDDYN,YDDYNA,ISPCOL,NFLEVG,1,ISPCOL,ZZSPGDIVG,ZSDIV)

    ! * Multiply by beta**2 (Delta t)**2 vnabla'**2
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
        ZSDIV(JSP,JLEV)=ZBDT*ZBDT*SILAPDI(IOFF+JSP)*ZSDIV(JSP,JLEV)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO

  ENDIF

  ! * Provides Dprim_star_star_star (in ZSDIV) from Dprim_star_star (in ZSDIV)
  !   (the quantity added is the same one as in the end of part 2.3 of SPCSI).
  IF (LIMPF) THEN

!$ACDC ABORT {

    IF (JITER == 0) THEN

     !!SPCIMPF_HSI1 called on KMLOC...KMLOC, except if KM=-999
       CALL SPCIMPF_HSI1(YDGEOMETRY,YDRIP,YDDYN,IMLOCSTA,IMLOCEND,KSTA,KEND,&
         &ZSDIV,ZZSPVORG)

    ELSE

      ! zeta_star is replaced by zero.
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
        DO JLEV=1,NFLEVG
          DO JSP=KSTA,KEND
            ZZSPVORG(JSP,JLEV)=0.0_JPRB
          ENDDO
        ENDDO
!$OMP END PARALLEL DO

    ENDIF

!$ACDC }

  ENDIF

  ! * Provides Dprim_star_star (resp. Dprim_star_star_star) in ZR1D and
  !   ZR1DPRIM if LIMPF=F (resp. LIMPF=T); and Dcha_star_star in ZR2D.
  IF (JITER == 0) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
    DO JLEV=1,NFLEVG
      DO JSP=KSTA,KEND
        ZR2D(JSP,JLEV)=PSPSVDG(JSP,JLEV)-ZBDT*ZSVED(JSP,JLEV)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO
  ELSE
    ! Dcha_star_star is replaced by zero.
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZR2D(JSP,JLEV)=0.0_JPRB
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
  ENDIF
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
    DO JLEV=1,NFLEVG
      DO JSP=KSTA,KEND
        ZR1D(JSP,JLEV)=ZSDIV(JSP,JLEV)
        ZR1DPRIM(JSP,JLEV)=ZSDIV(JSP,JLEV)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO

  IF (LSIDG .OR. LESIDG) THEN

!$ACDC HORIZONTAL {

    IF (LSIDG) THEN
   !!SPCSIDG_PART2 called on KMLOC...KMLOC, except if KM=-999   
      CALL SPCSIDG_PART2(YDGEOMETRY,KSTA,KEND,ZR1D,ZR1D2,&
        &IMLOCSTA,IMLOCEND,LSIDG,NSZNISNAX,SINISNAX,SCGMAP(:,1),SCGMAP(:,2),SCGMAP(:,3))
    ELSE
   !!SPCSIDG_PART2 called on KMLOC...KMLOC, except if KM=-999
      CALL SPCSIDG_PART2(YDGEOMETRY,KSTA,KEND,ZR1D,ZR1D2,&
        &IMLOCSTA,IMLOCEND,LSIDG,NSZNISNAX,SINISNAX,SIPD,SIPE,SIPF)
    ENDIF

!$ACDC }

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
    DO JLEV=1,NFLEVG
      DO JSP=KSTA,KEND
        ZR1D(JSP,JLEV)=ZR1D2(JSP,JLEV)/(RSTRET*RSTRET)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO

  ENDIF
  ! * Provides "LLstar Dprim_star_star" if LIMPF=F, or
  !   "LLstar Dprim_star_star_star" if LIMPF=T
  !   (mult by a constant coefficient) in ZSRHS.
  CALL SISEVE(YDCST,YDGEOMETRY,YDDYN,YDDYNA,ISPCOL,NFLEVG,1,ISPCOL,ZR1D,ZSRHS)
  ! * Provides "Tau Dprim_star_star" if LIMPF=F, or
  !   "Tau Dprim_star_star_star" if LIMPF=T 
  !   (mult by a constant coefficient) in ZST.
  CALL SITNU(YDCST,YDGEOMETRY,YDDYN,ISPCOL,NFLEVG,1,ISPCOL,ZR1D,ZST,ZSP)

  ! * Provides "LLstar Tau Dprim_star_star" if LIMPF=F, or
  !   "LLstar Tau Dprim_star_star_star" if LIMPF=T 
  !   (mult by a constant coefficient) in ZWORK.
  CALL SISEVE(YDCST,YDGEOMETRY,YDDYN,YDDYNA,ISPCOL,NFLEVG,1,ISPCOL,ZST,ZWORK)

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      ZWORK(JSP,JLEV)=ZWORK(JSP,JLEV)*YDCST%RCVD*ZRRT
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

  ! * Provides
  !   "[I - beta**2 (Delta t)**2 C**2 Mbar**2 vnabla'**2] Dcha_star_star"
  !   in ZSVED.
  IF (JITER == 0) THEN
    IF (LSIDG .OR. LESIDG) THEN
      ! First compute ZR3D=ZR2D*RLAPDI.
      ! Multiply ZR3D by M**2 (CALL MXPTMA using SCGMAP),
      !  put the result in a separate array ZR4D,
      !  use ZSDIVPL and ZSPDIVPL as intermediate work arrays.
      ! Compute ZSVED using ZR2D and ZR4D:
      !  ZSVED=ZR2D-(ZBTCM/(RSTRET*RSTRET))*ZR4D.
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZR3D(JSP,JLEV)=SILAPDI(IOFF+JSP)*ZR2D(JSP,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

!$ACDC HORIZONTAL {

    IF (LSIDG) THEN
      !!SPCSIDG_PART2 called on KMLOC...KMLOC, except if KM=-999
       CALL SPCSIDG_PART2(YDGEOMETRY,KSTA,KEND,ZR3D,ZR4D,&
       &IMLOCSTA,IMLOCEND,LSIDG,NSZNISNAX,&
       &SINISNAX,SCGMAP(:,1),SCGMAP(:,2),SCGMAP(:,3))
    ELSEIF (LESIDG) THEN
      !!SPCSIDG_PART2 called on KMLOC...KMLOC, except if KM=-999
       CALL SPCSIDG_PART2(YDGEOMETRY,KSTA,KEND,ZR3D,ZR4D,&
       &IMLOCSTA,IMLOCEND,LSIDG,NSZNISNAX,SINISNAX,SIPD,SIPE,SIPF)
    ENDIF

!$ACDC }

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZSVED(JSP,JLEV)=ZR2D(JSP,JLEV)-(ZBTCM/(RSTRET*RSTRET))*ZR4D(JSP,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
    ELSE

          if (LELAM) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
!essai avant22!          ZSVED(JSP,JLEV)=ZR2D(JSP,JLEV)-ZBTCM*ZR2D(JSP,JLEV)*SILAPDI(IOFF+JSP)
          ZSVED(JSP,JLEV)=ZR2D(JSP,JLEV)*(1.0_JPRB-ZBTCM*SILAPDI(IOFF+JSP)) !arome!
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

          else

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZSVED(JSP,JLEV)=ZR2D(JSP,JLEV)-ZBTCM*ZR2D(JSP,JLEV)*SILAPDI(IOFF+JSP)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

          endif
    ENDIF
  ELSE
    ! ZSVED is simply 0 in this case.
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZSVED(JSP,JLEV)=0.0_JPRB
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
  ENDIF

  ! * Provides "SIFAC * RHS of the Helmholtz eqn" in ZSRHS2.
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      ZSRHS2(JSP,JLEV)=ZBTCMH*&
       & (ZSRHS(JSP,JLEV)-ZWORK(JSP,JLEV))+&
       & ZSVED(JSP,JLEV)  
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

  ! * Case LIMPF: add terms containing Dcha_star_star(m,n),
  !   Dcha_star_star(m,n-2), Dcha_star_star(m,n+2) in the RHS.

  IF (LIMPF .AND. (JITER == 0)) THEN

!$ACDC ABORT {

     !!SPCIMPF_HSI2 called on KMLOC...KMLOC, except if KM=-999
       CALL SPCIMPF_HSI2(YDGEOMETRY,YDRIP,YDDYN,IMLOCSTA,IMLOCEND,KSTA,KEND,&
         &ZR2D,ZSRHS2_INC)

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZSRHS2(JSP,JLEV)=ZSRHS2(JSP,JLEV)&
           & +ZSRHS2_INC(JSP,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

!$ACDC }

  ENDIF

  ! * Multiply by array SIFACI to obtain the RHS of the Helmholtz eqn,
  !   stored in ZSRHS.

  CALL MXMAOPTR(SIFACI,1,NFLEVG,ZSRHS2,1,NFLEVG,ZSRHS,1,NFLEVG,&
   & NFLEVG,NFLEVG,ISPCOL)

  !*        2.4  Solve Helmholtz equation (compute dver(t+dt))

  ! Current space --> vertical eigenmodes space
  ! (multiply by matrix Q stored in SIMI).

  CALL MXMAOPTR(SIMI,1,NFLEVG,ZSRHS,1,NFLEVG,ZSDIVP,1,NFLEVG,&
   & NFLEVG,NFLEVG,ISPCOL)

  ! Inversion of Helmholtz equation itself.

  IF (LSIDG .OR. LESIDG) THEN

    ! Case designed for stretching :
    ! Use ZSDIVPL and ZSPDIVPL as intermediate work arrays.
  IF (KM==-999) THEN

!$ACDC HORIZONTAL {

    CALL SPCSIDG_PART1(YDGEOMETRY,YDDYN,KSTA,KEND,ZSDIVP,&
         &ZSPDIVP,NPTRMF(MYSETN),NPTRMF(MYSETN+1)-1,LSIDG,&
         &NSZNISNAX,SINISNAX,SIHEGTRA,SIHEG2TRA)
  
    CALL SPCSIDG_PART0NH(YDGEOMETRY,YDDYN,KSTA,KEND,ZSPDIVP,&
     & SILAPIN,KM,IOFF)

!$ACDC }

  ELSE

!$ACDC ABORT {

    CALL SPCSIDG_PART1(YDGEOMETRY,YDDYN,KSTA,KEND,ZSDIVP,&
         &ZSPDIVP,KMLOC,KMLOC,LSIDG,&
         &NSZNISNAX,SINISNAX,SIHEGTRA,SIHEG2TRA)
  
    CALL SPCSIDG_PART0NH(YDGEOMETRY,YDDYN,KSTA,KEND,ZSPDIVP,&
     & SILAPIN,KM,IOFF)

!$ACDC }

  ENDIF

  ELSE
    ! Case designed when no stretching :

    IF (LIMPF) THEN

!$ACDC ABORT {

      ! Solve complex pentadiagonal system
      !!SPCIMPFSOLVE called on KMLOC...KMLOC, except if KM=-999
      CALL SPCIMPFSOLVE(YDGEOMETRY,YDRIP,YDDYN,IMLOCSTA,IMLOCEND,KSTA,KEND,SIVP,&
        &ZSDIVP,ZSPDIVP)

!$ACDC }

    ELSE
      ! Inversion of a diagonal matrix, provides Q*dver(t+dt).
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZSPDIVP(JSP,JLEV)=ZSDIVP(JSP,JLEV)&
           &/(1.0_JPRB-ZBDT2*SIVP(JLEV)*SILAPDI(IOFF+JSP))
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
    ENDIF

  ENDIF

  ! Vertical eigenmodes space --> current space.
  ! (multiply by matrix Q**(-1) stored in SIMO).
  ! Provides dver(t+dt).

  CALL MXMAOPTR(SIMO,1,NFLEVG,ZSPDIVP,1,NFLEVG,ZZSPSVDG,1,&
   & NFLEVG,NFLEVG,NFLEVG,ISPCOL)

  !*        2.5  Recover the other prognostic variables
  !              (successively D, T, "spd" and log(prehyds)).
  !$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP)
  DO JSP=KSTA,KEND
    ZSP(JSP) = 0.0_JPRB
  ENDDO
  !$OMP END PARALLEL DO

  ! * Provides "Gamma d(t+dt)" in array ZWORK.
  CALL SIGAM(YDCST,YDGEOMETRY,YDDYN,ISPCOL,NFLEVG,1,ISPCOL,ZWORK,ZZSPSVDG,ZSP)

  ! * Provides
  !   " beta**2 (Delta t)**2 vnabla'**2 (- SITR Gamma + C**2) d(t+dt)
  !   + Dprim_star_star " if LIMPF=F, or
  !   " beta**2 (Delta t)**2 vnabla'**2 (- SITR Gamma + C**2) d(t+dt)
  !   + Dprim_star_star_star " if LIMPF=T
  !   in array ZSRHS.
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      ZSRHS(JSP,JLEV)=(ZZSPSVDG(JSP,JLEV)*YDCST%RD*ZCC&
       & -ZWORK(JSP,JLEV))*SITR*ZBDT*ZBDT*&
       & SILAPDI(IOFF+JSP)+ZR1DPRIM(JSP,JLEV)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
  ! * Provides D'(t+dt).
  IF (LSIDG .OR. LESIDG) THEN

    ! * Divide by the pentadiagonal operator
    !   [I - beta**2 (Delta t)**2 vnabla'**2 C**2 M**2]
    !   For KM>0 one works with the symmetric
    !   operator [vnabla'**(-2) - beta**2 (Delta t)**2 C**2 M**2]
    !   after having multiplied the RHS "ZSRHS" by "RLAPIN".
    !   Use ZSDIVPL and ZSPDIVPL as intermediate work arrays.

!$ACDC HORIZONTAL {

    CALL SPCSIDG_PART0NH(YDGEOMETRY,YDDYN,KSTA,KEND,ZSRHS,&
     & SILAPIN,KM,IOFF)

    !!SPCSIDG_PART1 called on KMLOC...KMLOC, except if KM=-999 
    CALL SPCSIDG_PART1(YDGEOMETRY,YDDYN,KSTA,KEND,ZSRHS,&
         &ZZSPDIVG,IMLOCSTA,IMLOCEND,LSIDG,NSZNISNAX,SINISNAX,&
         &SIHEGBTRA,SIHEGB2TRA)

!$ACDC }

  ELSE

    ! * Retrieve D'(t+dt).
    IF (LIMPF) THEN

!$ACDC ABORT {

      ! * Solve complex pentadiagonal system.
     !!SPCIMPFSOLVE called on KMLOC...KMLOC, except if KM=-999
     CALL SPCIMPFSOLVE(YDGEOMETRY,YDRIP,YDDYN,IMLOCSTA,IMLOCEND,KSTA,KEND,SIVP2,&
       &ZSRHS,ZZSPDIVG)

!$ACDC }

    ELSE
      ! * Division by the diagonal operator
      !   [I - beta**2 (Delta t)**2 vnabla'**2 C**2 RSTRET**2].
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZZSPDIVG(JSP,JLEV)=ZSRHS(JSP,JLEV)/(1.0_JPRB-ZBTCM*SILAPDI(IOFF+JSP))
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
    ENDIF

  ENDIF

  ! * Provides Mbar**2 D'(t+dt).
  IF (LSIDG) THEN
    ! Multiply ZZSPDIVG by M**2 (penta-diagonal operator).
    ! Use ZSDIVPL and ZSPDIVPL as intermediate work arrays.
    !!SPCSIDG_PART2 called on KMLOC...KMLOC, except if KM=-999

!$ACDC HORIZONTAL {

    CALL SPCSIDG_PART2(YDGEOMETRY,KSTA,KEND,ZZSPDIVG,ZWORK,&
      &IMLOCSTA,IMLOCEND,LSIDG,NSZNISNAX,&
      &SINISNAX,SCGMAP(:,1),SCGMAP(:,2),SCGMAP(:,3))

!$ACDC }

  ELSEIF (LESIDG) THEN

!$ACDC ABORT {

    !!SPCSIDG_PART2 called on KMLOC...KMLOC, except if KM=-999
    CALL SPCSIDG_PART2(YDGEOMETRY,KSTA,KEND,ZZSPDIVG,ZWORK,&
      &IMLOCSTA,IMLOCEND,LSIDG,NSZNISNAX,SINISNAX,SIPD,SIPE,SIPF)

!$ACDC }

  ELSE
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
    DO JLEV=1,NFLEVG
      DO JSP=KSTA,KEND
        ZWORK(JSP,JLEV)=ZZSPDIVG(JSP,JLEV)*RSTRET*RSTRET
      ENDDO
    ENDDO
!$OMP END PARALLEL DO
  ENDIF

  !*       2.6  Increment vorticity for case LIMPF=T
  !             (identical to part 2.6 of SPCSI).

    IF (LIMPF) THEN

!$ACDC HORIZONTAL {

     !!SPCIMPF_HSI3 called on KMLOC...KMLOC, except if KM=-999
     CALL SPCIMPF_HSI3(YDGEOMETRY,YDRIP,YDDYN,IMLOCSTA,IMLOCEND,KSTA,KEND,&
       &ZZSPDIVG,ZZSPVORG)

!$ACDC }

    ENDIF

  !*       2.7  Savings.

  IF (I_NITERHELM > 0) THEN
    IF (JITER == 0) THEN
      ! Save D'(t+dt,iter=0), Mbar**2 D'(t+dt,iter=0),
      !  zeta'(t+dt,iter=0), dver(t+dt,iter=0).
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JSP)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
        ZZSPDIV0G(JSP,JLEV)=ZZSPDIVG(JSP,JLEV)
        ZZSPGDIV0G(JSP,JLEV)=ZWORK(JSP,JLEV)
        ZZSPSVD0G(JSP,JLEV)=ZZSPSVDG(JSP,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
      IF (LIMPF) THEN

!$ACDC ABORT {

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JSP)
        DO JLEV=1,NFLEVG
          DO JSP=KSTA,KEND
            ZZSPVOR0G(JSP,JLEV)=ZZSPVORG(JSP,JLEV)
          ENDDO
        ENDDO
!$OMP END PARALLEL DO

!$ACDC }

      ENDIF
    ENDIF

    ! Save Mbar**2 D'(t+dt,jiter) in ZZSPGDIVG
    IF (JITER == 0) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JSP)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZZSPGDIVG(JSP,JLEV)=ZWORK(JSP,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
    ELSE
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JSP)
      DO JLEV=1,NFLEVG
        DO JSP=KSTA,KEND
          ZZSPGDIVG(JSP,JLEV)=ZWORK(JSP,JLEV)+ZZSPGDIV0G(JSP,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
    ENDIF
  ENDIF

ENDDO ! End loop on JITER

!*       2.8  Add the increment and recover X(t+dt)
!             for X=D',Mbar**2 D',zeta',dver, and deallocate.

IF (I_NITERHELM > 0) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JSP)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      ZZSPDIVG(JSP,JLEV)=ZZSPDIVG(JSP,JLEV)+ZZSPDIV0G(JSP,JLEV)
      ZWORK(JSP,JLEV)=ZWORK(JSP,JLEV)+ZZSPGDIV0G(JSP,JLEV)
      ZZSPSVDG(JSP,JLEV)=ZZSPSVDG(JSP,JLEV)+ZZSPSVD0G(JSP,JLEV)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
  IF (LIMPF) THEN

!$ACDC ABORT {

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JSP)
    DO JLEV=1,NFLEVG
      DO JSP=KSTA,KEND
        ZZSPVORG(JSP,JLEV)=ZZSPVORG(JSP,JLEV)+ZZSPVOR0G(JSP,JLEV)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO

!$ACDC }

  ENDIF

ENDIF
!*       2.9  Increment T, log(prehyds) and spd.

! * Provides some intermediate quantities allowing to compute
!   T(t+dt) (in ZST), log(prehyds)(t+dt) (in ZSP), spd(t+dt) (in ZSNHP).
CALL SIPTP(YDCST,YDGEOMETRY,YDDYN,YDDYNA,ISPCOL,NFLEVG,1,ISPCOL,ZWORK,ZZSPSVDG,ZSNHP,ZST,ZSP)
! * Provides T(t+dt) (in ZZSPTG) and spd(t+dt) (in ZZSPSPDG).
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
DO JLEV=1,NFLEVG
  DO JSP=KSTA,KEND
    PSPTG(JSP,JLEV)=ZZSPTG(JSP,JLEV)-ZBDT*ZST(JSP,JLEV)
    PSPSPDG(JSP,JLEV)=ZZSPSPDG(JSP,JLEV)-ZBDT*ZSNHP(JSP,JLEV)
  ENDDO
ENDDO
!$OMP END PARALLEL DO

! * Provides log(prehyds)(t+dt) (in ZZSPSPG).
!$OMP PARALLEL PRIVATE(JSP)
!$OMP DO SIMD SCHEDULE(STATIC) 
DO JSP=KSTA,KEND
  PSPSPG(JSP)=ZZSPSPG(JSP)-ZBDT*ZSP(JSP)
ENDDO
!$OMP END DO SIMD
!$OMP END PARALLEL 
! ky: if LESIDG, missing there the reverse shift.
!     ------------------------------------------------------------------

!*       3.    MEMORY TRANSFER AND DDH-SI UPDATE.
!              ----------------------------------
IF (LIMPF) THEN

!$ACDC ABORT {

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      PSPVORG(JSP,JLEV)=ZZSPVORG(JSP,JLEV)  
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

!$ACDC }

ENDIF

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
DO JLEV=1,NFLEVG
  DO JSP=KSTA,KEND
    PSPDIVG(JSP,JLEV)=ZZSPDIVG(JSP,JLEV)
    PSPSVDG(JSP,JLEV)=ZZSPSVDG(JSP,JLEV)
  ENDDO
ENDDO
!$OMP END PARALLEL DO

IF (LRSIDDH) THEN

!$ACDC ABORT {

  IF (LIMPF) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
    DO JLEV=1,NFLEVG
      DO JSP=KSTA,KEND
        PSPTNDSI_VORG(JSP,JLEV)=PSPTNDSI_VORG(JSP,JLEV) + PSPVORG(JSP,JLEV)
      ENDDO
    ENDDO
!$OMP END PARALLEL DO

  ENDIF
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      PSPTNDSI_DIVG(JSP,JLEV)=PSPTNDSI_DIVG(JSP,JLEV) + PSPDIVG(JSP,JLEV)
      PSPTNDSI_TG  (JSP,JLEV)=PSPTNDSI_TG  (JSP,JLEV) + PSPTG  (JSP,JLEV)
      PSPTNDSI_SPDG(JSP,JLEV)=PSPTNDSI_SPDG(JSP,JLEV) + PSPSPDG(JSP,JLEV)
      PSPTNDSI_SVDG(JSP,JLEV)=PSPTNDSI_SVDG(JSP,JLEV) + PSPSVDG(JSP,JLEV)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

!$ACDC }

ENDIF
!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('SPNHSI',1,ZHOOK_HANDLE)
END SUBROUTINE SPNHSI


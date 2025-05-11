!OCL  NOEVAL
SUBROUTINE GNHPRE(YDGEOMETRY, KPDVAR, KPROMA, KFLEV, KST, KEND, PSPD, PREF, PKAP, PNHPREF, &
& PNHPPI, PRNHPPI, PDEP, PEQCHAF)

!$ACDC singlecolumn  --process-pointers


! GNHPRE - Computation of the total pressure "pre" at full levels,
!          from the 'PD' prognostic variable.

! Purpose
! -------
!   Computes at model full levels.
!    * the total pressure "pre".
!    * the pressure departure "pre - prehyd".
!    * the ratios pre/prehyd and prehyd/pre.
!    * exp((R/cp) log(pre/prehyd)) and its inverse.

! Interface
! ---------
!   INPUT:
!    YDGEOMETRY   : structure containing all geometry.
!    KPROMA       : horizontal dimension.
!    KFLEV        : number of levels.
!    KST          : start of work.
!    KEND         : end of work.
!    PSPD         : NH pressure departure prognostic variable.
!    PREF         : hydrostatic pressure "prehyd" at full levels.

!   OPTIONAL INPUT:
!    PKAP         : kappa = R/cp (required for NHQE).

!   OPTIONAL OUTPUT:
!    PNHPREF      : total pressure "pre" at full levels.
!    PNHPPI       : ratio pre/prehyd at full levels (required for NHEE).
!    PRNHPPI      : ratio prehyd/pre at full levels (required for NHEE).
!    PDEP         : pressure departure "pre - prehyd" at full levels (required for NHEE).
!    PEQCHAF      : exp((R/cp) log(pre/prehyd)) at full levels (required for NHQE).

! Externals
! ---------

! Method
! ------

! Reference
! ---------

! Author
! ------
!   K. YESSAD (MF/CNRM/GMAP), Dec 2004.

! Modifications
! -------------
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   K. Yessad (Dec 2016): Prune obsolete options.
!   K. Yessad (June 2017): Introduce NHQE model.
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!   H Petithomme (Dec 2020): use of pointer
!------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK

! -----------------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)     ,INTENT(IN)                     :: YDGEOMETRY
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KPDVAR
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KPROMA
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KFLEV
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KST 
INTEGER(KIND=JPIM) ,INTENT(IN)                     :: KEND 
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PSPD(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)                     :: PREF(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL          :: PKAP 
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL          :: PNHPREF(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL ,TARGET  :: PNHPPI(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL          :: PRNHPPI(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL          :: PDEP(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL          :: PEQCHAF(KPROMA,KFLEV) 

! -----------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF
REAL(KIND=JPRB),TARGET :: ZNHPPI0(KPROMA,KFLEV) 
REAL(KIND=JPRB),CONTIGUOUS,POINTER :: ZNHPPI(:,:)
REAL(KIND=JPRB)  :: Z1SKAP

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -----------------------------------------------------------------------------

!IF ( KPDVAR /= 2 .AND. KPDVAR /= 3) RETURN

IF (LHOOK) CALL DR_HOOK('GNHPRE',0,ZHOOK_HANDLE)

! -----------------------------------------------------------------------------

IF ((KPDVAR == 3 .OR. PRESENT(PEQCHAF)) .AND. (.NOT.PRESENT(PKAP))) THEN
   CALL ABOR1(' GNHPRE: missing optional input arguments!')
END IF


IF(PRESENT(PNHPPI)) THEN
  ZNHPPI => PNHPPI
ELSE
  ZNHPPI => ZNHPPI0
ENDIF


IF (KPDVAR == 2) THEN

  ! valid for kpdvar=2 only
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ZNHPPI(JROF,JLEV)=EXP(PSPD(JROF,JLEV))
    ENDDO
  ENDDO

  ! valid for npdvar=2 only
  IF (PRESENT(PKAP).AND.PRESENT(PEQCHAF)) THEN
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        PEQCHAF(JROF,JLEV)=EXP( PKAP*PSPD(JROF,JLEV) )
      ENDDO
    ENDDO
  ENDIF

ELSEIF (KPDVAR == 3) THEN

  ! valid for npdvar=3 only
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      Z1SKAP = 1.0_JPRB/PKAP
      ZNHPPI(JROF,JLEV)=EXP(LOG(1.0_JPRB+PKAP*PSPD(JROF,JLEV))*Z1SKAP)
    ENDDO
  ENDDO

  ! valid for npdvar=3 only
  IF (PRESENT(PEQCHAF)) THEN
    ! * useful for NHQE.
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        PEQCHAF(JROF,JLEV)=1.0_JPRB+PKAP*PSPD(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

ENDIF

! valid for any npdvar 
! but only npdvar=2 or nvdvar=3 is treated here)

IF (PRESENT(PNHPREF)) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PNHPREF(JROF,JLEV)=ZNHPPI(JROF,JLEV)*PREF(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

IF (PRESENT(PRNHPPI)) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PRNHPPI(JROF,JLEV)=1.0_JPRB/ZNHPPI(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

IF (PRESENT(PDEP)) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PDEP(JROF,JLEV)=( ZNHPPI(JROF,JLEV) - 1.0_JPRB )*PREF(JROF,JLEV)  
    ENDDO
  ENDDO
ENDIF

! -----------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GNHPRE',1,ZHOOK_HANDLE)

END SUBROUTINE GNHPRE


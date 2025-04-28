!OCL  NOEVAL
SUBROUTINE GPGRGEO_EXPL(YDGEOMETRY, KPROMA, KST, KEND, KFLEV, PRT, PRTL, PRTM, PLNPR, PALPH, &
& POROGL, POROGM, PHIFL, PHIFM, PHIHL, PHIHM, LDNHEE, LDNHHY, PRNHPPI, PQCHAL, PQCHAM, PEPS_NL,   &
& PNH1L, PNH1M, PLNPRL_DER, PLNPRM_DER, PALPHL_DER, PALPHM_DER)  

!**** *GPGRGEO_EXPL* - Computes half and full level gradient of geopotential height "gz".

!     Purpose.
!     --------

!      Expression of this term is:

!       grad (gz) = grad Phi_s
!       + grad {int[prehyd'=prehyds to prehyd] (-RT/pre) d prehyd'}

!      where: 
!       - "Phi_s = g z[surf]" is the surface orography.
!       - "prehyd" is the hydrostatic pressure.
!       - "pre" is the total pressure including non-hydrostatic effects.
!       - "prehyds" is the surface hydrostatic pressure.
!       - "R" is the air constant (including water contribution).
!       - "T" is the temperature.

!      For the NHQE and the HYD model, pre is replaced by prehyd, some terms disappear.
!      For the NHQE model, T is a modified temperature

!      Discretisation of the gradient of geopotential height yields:

!      * "grad (gz)" at half level "lbar":

!        (grad (gz))[lbar] = (grad Phi_s)
!        + sum[k=L to l+1] (prehyd/pre)[k] R[k] T[k] (grad (delta))[k]
!        + sum[k=L to l+1] (prehyd/pre)[k] (grad (RT))[k] delta[k]
!        + sum[k=L to l+1] (prehyd/pre)[k] R[k] T[k] delta[k]
!          { (grad(prehyd)/prehyd)[k] - (grad(pre)/pre)[k] }

!      * "grad (gz)" at full level "l":

!        (grad (gz))[l] = (grad (gz))[lbar]
!        + (prehyd/pre)[l] R[l] T[l] (grad (alpha))[l]
!        + (prehyd/pre)[l] (grad (RT))[l] alpha[l]
!        + (prehyd/pre)[l] R[l] T[l] alpha[l]
!          { (grad(prehyd)/prehyd)[l] - (grad(pre)/pre)[l] }

!**   Interface.
!     ----------
!        *CALL* *GPGRGEO_EXPL(...)

!        Explicit arguments :
!        --------------------
!         * INPUT:
!           YDGEOMETRY   : structure containing all geometry
!           KPROMA       : horizontal dimension
!           KD           : start of work
!           KF           : working length
!           KFLEV        : number of levels
!           PRT          : (RT) at full levels
!           PRTL         : zonal component of "grad (RT)" at full levels
!           PRTM         : meridian component of "grad (RT)" at full levels
!           PLNPR        : "delta" at full levels
!           PALPH        : "alpha" at full levels
!           POROGL       : zonal component of "grad(surf orography)"
!           POROGM       : meridian component of "grad(surf orography)"

!         * OUTPUT:
!           PHIFL        : zonal component of "grad (gz)" at full levels
!           PHIFM        : merid component of "grad (gz)" at full levels
!           PHIHL        : zonal component of "grad (gz)" at half levels
!           PHIHM        : merid component of "grad (gz)" at half levels

!         * INPUT OPTIONAL:
!           LDNHEE       : .T.: fully elastic non hydrostatic (NHEE) model.
!                          .F.: hydrostatic or NHQE model.
!           PRNHPPI      : "prehyd/pre" at full levels.
!           PQCHAL,PQCHAM: zonal and meridian components at full levels of
!                          "grad(log(pre/prehyd))=(grad pre)/pre - (grad(prehyd))/prehyd"

!         * OUTPUT OPTIONAL:
!           PNH1L        : zonal comp of RT grad(log(prehyd/pre)) at full levels
!           PNH1M        : merid comp of RT grad(log(prehyd/pre)) at full levels

!        Implicit arguments :   None.
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.    None.
!     ----------

!     Reference.
!     ----------
!        See  documentation Arpege ALGORITHME CHAPITRE 6 paragraphe 6

!     Author.
!     -------
!      K. YESSAD
!      Original : 2000-08-11

!     Modifications.
!     --------------
!      K. Yessad (Dec 2008): remove dummy CDLOCK
!      K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!      K. Yessad (June 2017): Introduce NHQE model.
!      J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!      H. Petithomme (Nov 2020): use of pointers for avoiding array copies
!     ------------------------------------------------------------------

USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, JPHOOK, DR_HOOK

USE GEOMETRY_MOD , ONLY : GEOMETRY



!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)     ,INTENT(IN)             :: YDGEOMETRY
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KPROMA 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KST 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KEND 
INTEGER(KIND=JPIM) ,INTENT(IN)             :: KFLEV 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PRT(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PRTL(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PRTM(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PLNPR(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PALPH(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: POROGL(KPROMA) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: POROGM(KPROMA) 
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PHIFL(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PHIFM(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PHIHL(KPROMA,0:KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT)            :: PHIHM(KPROMA,0:KFLEV) 
LOGICAL            ,INTENT(IN)  ,OPTIONAL  :: LDNHEE
LOGICAL            ,INTENT(IN)  ,OPTIONAL  :: LDNHHY
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PRNHPPI(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PQCHAL(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PQCHAM(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)  ,OPTIONAL  :: PEPS_NL(KFLEV) 
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL  :: PNH1L(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(OUT) ,OPTIONAL  :: PNH1M(KPROMA,KFLEV)
REAL(KIND=JPRB)    ,INTENT(IN)             :: PLNPRL_DER(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PLNPRM_DER(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PALPHL_DER(KPROMA,KFLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)             :: PALPHM_DER(KPROMA,KFLEV)

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF
LOGICAL         :: LLNHEE, LLNHHY
CHARACTER(LEN=4) :: CLOPER
REAL(KIND=JPRB) :: ZNH1L(KPROMA,KFLEV), ZNH1M(KPROMA,KFLEV)
REAL(KIND=JPRB) :: ZINL(KPROMA,0:KFLEV+1),ZINM(KPROMA,0:KFLEV+1)
REAL(KIND=JPRB) :: ZRNHPPI(KPROMA,KFLEV)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "verdisint.intfb.h"

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GPGRGEO_EXPL', 0, ZHOOK_HANDLE)

!     ------------------------------------------------------------------

IF (PRESENT(LDNHEE)) THEN
  LLNHEE=LDNHEE
ELSE
  LLNHEE=.FALSE.
ENDIF

IF (PRESENT(LDNHHY)) THEN
  LLNHHY=LDNHHY
ELSE
  LLNHHY=.FALSE.
ENDIF

CLOPER='IBOT'
IF (YDGEOMETRY%YRVERT_GEOM%YRCVER%LVFE_COMPATIBLE) CLOPER='INTG'

!     ------------------------------------------------------------------

!*    1. Computation of term:
!        R[l] T[l] {(grad(prehyd)/prehyd)[l] - (grad(pre)/pre)[l]}
!        according to "llnhee".
!*    2.1 Calculation of "grad (gz)" at half levels.
!         ("delta" and "grad delta" terms contributions.

DO JROF=KST,KEND
  PHIHL(JROF,KFLEV)=POROGL(JROF)
  PHIHM(JROF,KFLEV)=POROGM(JROF)
ENDDO

IF (LLNHEE) THEN
  IF (.NOT.(PRESENT(PRNHPPI).AND.PRESENT(PQCHAL).AND.PRESENT(PQCHAM)))&
    CALL ABOR1(' GPGRGEO_EXPL: missing input PRNHPPI, PQCHAL, PQCHAM !!!')

  IF (PRESENT(PEPS_NL) .AND. LLNHHY) THEN
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZRNHPPI(JROF,JLEV) = 1.0_JPRB + PEPS_NL(JLEV) * (PRNHPPI(JROF,JLEV) - 1.0_JPRB)
        ZNH1L(JROF,JLEV)=-PEPS_NL(JLEV)*PRT(JROF,JLEV)*PQCHAL(JROF,JLEV)
        ZNH1M(JROF,JLEV)=-PEPS_NL(JLEV)*PRT(JROF,JLEV)*PQCHAM(JROF,JLEV)
      ENDDO
    ENDDO
  ELSE
    DO JLEV=1,KFLEV
      DO JROF = KST, KEND
        ZRNHPPI(JROF,JLEV) = PRNHPPI(JROF,JLEV)
        ZNH1L(JROF,JLEV)=-PRT(JROF,JLEV)*PQCHAL(JROF,JLEV)
        ZNH1M(JROF,JLEV)=-PRT(JROF,JLEV)*PQCHAM(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF

  DO JLEV=KFLEV,1,-1
    DO JROF=KST,KEND
      PHIHL(JROF,JLEV-1)= PHIHL(JROF,JLEV)&
       & +PLNPR(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*(PRTL(JROF,JLEV)+ZNH1L(JROF,JLEV))&
       & +PLNPRL_DER(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*PRT(JROF,JLEV)
      PHIHM(JROF,JLEV-1)= PHIHM(JROF,JLEV)&
       & +PLNPR(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*(PRTM(JROF,JLEV)+ZNH1M(JROF,JLEV))&
       & +PLNPRM_DER(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*PRT(JROF,JLEV)
    ENDDO
  ENDDO

  ! note: merge tests since both or none present (only present in cpg5_cp)
  IF (PRESENT(PNH1L).AND.PRESENT(PNH1M)) THEN
    PNH1L(KST:KEND,1:KFLEV)=ZNH1L(KST:KEND,1:KFLEV)
    PNH1M(KST:KEND,1:KFLEV)=ZNH1M(KST:KEND,1:KFLEV)
  END IF
ELSE
  ! if not present, these arrays are unused
  IF (PRESENT(PNH1L).AND.PRESENT(PNH1M)) THEN
    PNH1L(KST:KEND,1:KFLEV)=0.0_JPRB 
    PNH1M(KST:KEND,1:KFLEV)=0.0_JPRB
  ENDIF

  DO JLEV=KFLEV,1,-1
    DO JROF=KST,KEND
      PHIHL(JROF,JLEV-1)=PHIHL(JROF,JLEV)+PLNPR(JROF,JLEV)*PRTL(JROF,JLEV)&
       & +PLNPRL_DER(JROF,JLEV)*PRT(JROF,JLEV)
      PHIHM(JROF,JLEV-1)=PHIHM(JROF,JLEV)+PLNPR(JROF,JLEV)*PRTM(JROF,JLEV)&
       & +PLNPRM_DER(JROF,JLEV)*PRT(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

!*    2.2 Calculation of "grad (gz)" at half levels.
!         "alpha" and "grad alpha" terms contributions.

IF(YDGEOMETRY%YRVERT_GEOM%YRCVER%LVERTFE) THEN
  IF (LLNHEE) THEN
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZINL(JROF,JLEV)=(-PLNPR(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*&
         & (PRTL(JROF,JLEV)+ZNH1L(JROF,JLEV))&
         & -PLNPRL_DER(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*PRT(JROF,JLEV))&
         & *YDGEOMETRY%YRVERT_GEOM%YRVETA%VFE_RDETAH(JLEV)  
        ZINM(JROF,JLEV)=(-PLNPR(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*&
         & (PRTM(JROF,JLEV)+ZNH1M(JROF,JLEV))&
         & -PLNPRM_DER(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*PRT(JROF,JLEV))&
         & *YDGEOMETRY%YRVERT_GEOM%YRVETA%VFE_RDETAH(JLEV)  
      ENDDO
    ENDDO
  ELSE
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        ZINL(JROF,JLEV)=(-PLNPR(JROF,JLEV)*PRTL(JROF,JLEV)&
         & -PLNPRL_DER(JROF,JLEV)*PRT(JROF,JLEV))&
         & *YDGEOMETRY%YRVERT_GEOM%YRVETA%VFE_RDETAH(JLEV)
        ZINM(JROF,JLEV)=(-PLNPR(JROF,JLEV)*PRTM(JROF,JLEV)&
         & -PLNPRM_DER(JROF,JLEV)*PRT(JROF,JLEV))&
         & *YDGEOMETRY%YRVERT_GEOM%YRVETA%VFE_RDETAH(JLEV)
      ENDDO
    ENDDO
  ENDIF

  ZINL(KST:KEND,0)=0.0_JPRB
  ZINL(KST:KEND,KFLEV+1)=0.0_JPRB
  ZINM(KST:KEND,0)=0.0_JPRB
  ZINM(KST:KEND,KFLEV+1)=0.0_JPRB
  CALL VERDISINT(YDGEOMETRY%YRVERT_GEOM%YRVFE,YDGEOMETRY%YRVERT_GEOM%YRCVER,CLOPER,&
   & '11',KPROMA,KST,KEND,KFLEV,ZINL,PHIFL,PINS=POROGL)
  CALL VERDISINT(YDGEOMETRY%YRVERT_GEOM%YRVFE,YDGEOMETRY%YRVERT_GEOM%YRCVER,CLOPER,&
   & '11',KPROMA,KST,KEND,KFLEV,ZINM,PHIFM,PINS=POROGM)
ELSE
  IF (LLNHEE) THEN
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        PHIFL(JROF,JLEV)=PHIHL(JROF,JLEV)&
         & +PALPH(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*(PRTL(JROF,JLEV)+ZNH1L(JROF,JLEV))&
         & +PALPHL_DER(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*PRT(JROF,JLEV)
        PHIFM(JROF,JLEV)=PHIHM(JROF,JLEV)&
         & +PALPH(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*(PRTM(JROF,JLEV)+ZNH1M(JROF,JLEV))&
         & +PALPHM_DER(JROF,JLEV)*ZRNHPPI(JROF,JLEV)*PRT(JROF,JLEV)
      ENDDO
    ENDDO
  ELSE
    DO JLEV=1,KFLEV
      DO JROF=KST,KEND
        PHIFL(JROF,JLEV)=PHIHL(JROF,JLEV)+PALPH(JROF,JLEV)*PRTL(JROF,JLEV)&
         & +PALPHL_DER(JROF,JLEV)*PRT(JROF,JLEV)
        PHIFM(JROF,JLEV)=PHIHM(JROF,JLEV)+PALPH(JROF,JLEV)*PRTM(JROF,JLEV)&
         & +PALPHM_DER(JROF,JLEV)*PRT(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF
ENDIF

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GPGRGEO_EXPL', 1, ZHOOK_HANDLE)

END SUBROUTINE GPGRGEO_EXPL

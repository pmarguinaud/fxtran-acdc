SUBROUTINE GPGEO_EXPL(KPROMA, KST, KEND, KFLEV, PHI, PHIF, PT, PR, PLNPR, PALPH, &
& YDVGEOM)

!$ACDC singlecolumn  --process-pointers


!**** *GPGEO_EXPL* - Computes half and full level geopotential height "gz".

!     Purpose.
!     --------

!      Computes half and full level geopotential height "gz".

!      Laplace relation writes:

!       d (gz)/d prehyd = - RT/pre = - (RT/prehyd) * (prehyd/pre)

!      where:
!       - "gz" is the geopotential height.
!       - "prehyd" is the hydrostatic pressure.
!       - "pre" is the total pressure including non-hydrostatic effects.
!       - "R" is the air constant (including moisture effects).
!       - "T" is the temperature.

!      It is important to note that when relaxing the thin layer hypothesis
!      the geopotential height "gz" is different from the total geopotential
!      "Phi" (used in the RHS of the horizontal wind equation), except
!      at the surface where Phi_s can still be defined by Phi_s = g z[surf].

!      Integrating the Laplace equations yields the following discretisation
!      for "gz".

!      * "gz" at interlayer "lbar":

!        g z[lbar] = g z[surf]
!        + sum[k=L to l+1] (prehyd/pre)[k] R[k] T[k] delta[k]

!      * "gz" at layer "l":

!        g z[l] = g z[lbar] + (prehyd/pre)[l] R[l] T[l] alpha[l]

!**   Interface.
!     ----------
!        *CALL* *GPGEO_EXPL(...)

!        Explicit arguments :
!        --------------------
!          KPROMA : horizontal dimensioning                          (input)
!          KSTART : start of work                                    (input)
!          KPROF  : depth of work                                    (input)
!          KFLEV  : number of levels                                 (input)
!          PHI    : geopotential height "gz" at interlayers          (output)
!          PHIF   : geopotential height "gz" at layers               (output)
!          PT     : temperature at layers                            (input)
!          PR     : "R" at layers for hydrostatic model              (input)
!                    "(prehyd/pre) R" at layers for NH model
!          PLNPR  : term "delta" on layers                           (input)
!                   (= logarithm of ratio of pressure if "ndlnpr=0")
!          PALPH  : term "alpha" on layers                           (input)
!          YDVGEOM : vertical geometry from the model                (input)

!        Implicit arguments :    None.
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.   None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      Mats Hamrud and Philippe Courtier  *ECMWF*

!     Modifications.
!     --------------
!      J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!      H Petithomme (Dec 2020): merge VFD loops
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, JPHOOK, DR_HOOK

USE YOMVERT  , ONLY : TVERTICAL_GEOM

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM)   ,INTENT(IN)     :: KPROMA 
INTEGER(KIND=JPIM)   ,INTENT(IN)     :: KST 
INTEGER(KIND=JPIM)   ,INTENT(IN)     :: KEND 
INTEGER(KIND=JPIM)   ,INTENT(IN)     :: KFLEV 
REAL(KIND=JPRB)      ,INTENT(INOUT)  :: PHI(KPROMA,0:KFLEV) 
REAL(KIND=JPRB)      ,INTENT(OUT)    :: PHIF(KPROMA,KFLEV) 
REAL(KIND=JPRB)      ,INTENT(IN)     :: PT(KPROMA,KFLEV) 
REAL(KIND=JPRB)      ,INTENT(IN)     :: PR(KPROMA,KFLEV) 
REAL(KIND=JPRB)      ,INTENT(IN)     :: PLNPR(KPROMA,KFLEV) 
REAL(KIND=JPRB)      ,INTENT(IN)     :: PALPH(KPROMA,KFLEV) 
TYPE(TVERTICAL_GEOM) ,INTENT(IN)     :: YDVGEOM

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF
CHARACTER(LEN=4) :: CLOPER
REAL(KIND=JPRB) :: ZPHI(KPROMA,0:KFLEV+1)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "verdisint.intfb.h"

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GPGEO_EXPL', 0, ZHOOK_HANDLE)

!     ------------------------------------------------------------------

!*       1.    COMPUTES HALF AND FULL LEVEL GEOPOTENTIAL HEIGHT.
!              -------------------------------------------------

IF(YDVGEOM%YRCVER%LVERTFE) THEN

  CLOPER='IBOT'
  IF (YDVGEOM%YRCVER%LVFE_COMPATIBLE) CLOPER='INTG'

  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      ZPHI(JROF,JLEV)=-PR(JROF,JLEV)*PT(JROF,JLEV)&
       & *PLNPR(JROF,JLEV)*YDVGEOM%YRVETA%VFE_RDETAH(JLEV)  
    ENDDO
  ENDDO

  ZPHI(KST:KEND,0)=0.0_JPRB
  ZPHI(KST:KEND,KFLEV+1)=0.0_JPRB
  CALL VERDISINT(YDVGEOM%YRVFE,YDVGEOM%YRCVER,CLOPER,'11',KPROMA,KST,KEND,KFLEV,ZPHI,&
   & PHIF,PINS=PHI(:,KFLEV))

  DO JLEV=KFLEV,1,-1
    DO JROF=KST,KEND
      PHI(JROF,JLEV-1)=PHI(JROF,JLEV)+PR(JROF,JLEV)*PT(JROF,JLEV)*PLNPR(JROF,JLEV)
    ENDDO
  ENDDO
ELSE
  DO JLEV=KFLEV,1,-1
    DO JROF=KST,KEND
      PHI(JROF,JLEV-1) = PHI(JROF,JLEV)+PR(JROF,JLEV)*PT(JROF,JLEV)*PLNPR(JROF,JLEV)  
      PHIF(JROF,JLEV) = PHI(JROF,JLEV)+PALPH(JROF,JLEV)*PR(JROF,JLEV)*PT(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GPGEO_EXPL', 1, ZHOOK_HANDLE)
END SUBROUTINE GPGEO_EXPL


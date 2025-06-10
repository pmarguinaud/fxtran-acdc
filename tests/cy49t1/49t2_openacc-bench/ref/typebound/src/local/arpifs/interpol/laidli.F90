SUBROUTINE LAIDLI(KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,PDLAT,PDLO,KL0,PXSL,PXF)

!$ACDC singlecolumn  --process-pointers


!**** *LAIDLI  -  semi-LAgrangian scheme:
!                 Bilinear horizontal interpolations for one variable.

!     Purpose.
!     --------
!       Performs bilinear horizontal interpolations for one variable.

!**   Interface.
!     ----------
!        *CALL* *LAIDLI(...)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KSLB1  - horizontal dimension for grid-point quantities.
!          KPROMA  - horizontal dimension for interpolation point
!                    quantities.
!          KST  - first element of arrays where
!                    computations are performed.
!          KEND   - depth of work.
!          KFLEV   - vertical dimension.
!          KFLDN   - number of the first field.
!          KFLDX   - number of the last field.
!          PDLAT   - weight (distance) for horizontal linear interpolation
!                    on a same latitude.
!          PDLO    - weights (distances) for horizontal linear interpolation
!                    on a same longitude.
!          KL0     - indices of the four western points
!                    of the 16 points interpolation grid.
!          PXSL    - quantity to be interpolated.

!        OUTPUT:
!          PXF     - interpolated variable.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!        No external.

!     Reference.
!     ----------

!     Author.
!     -------
!        K. YESSAD, after the subroutines LAGINL3
!        written by Maurice IMBARD, Alain CRAPLET and Michel ROCHAS
!        METEO-FRANCE, CNRM/GMAP.

!     Modifications.
!     --------------
!        Original : FEBRUARY 1992.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        K. Yessad (Sep 2008): update comments + cleanings.
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KSLB1 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLAT(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMA,KFLEV,1:2) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMA,KFLEV,1:2) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KSLB1*(KFLDX-KFLDN+1)) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXF(KPROMA,KFLEV) 

INTEGER(KIND=JPIM) :: JLEV,JROF
REAL(KIND=JPRB) :: Z1,Z2
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "interp.func.h"

IF (LHOOK) CALL DR_HOOK('LAIDLI',0,ZHOOK_HANDLE)

!*       1.    INTERPOLATIONS.
!              ---------------

DO JLEV=1,KFLEV
  DO JROF=KST,KEND
    Z1 = PXSL(KL0(JROF,JLEV,1)+1)
    Z1 = FDWEIGHT(PDLO(JROF,JLEV,1),Z1,PXSL(KL0(JROF,JLEV,1)+2))

    Z2 = PXSL(KL0(JROF,JLEV,2)+1)
    Z2 = FDWEIGHT(PDLO(JROF,JLEV,2),Z2,PXSL(KL0(JROF,JLEV,2)+2))

    PXF(JROF,JLEV) = FDWEIGHT(PDLAT(JROF,JLEV),Z1,Z2)
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK('LAIDLI',1,ZHOOK_HANDLE)
END SUBROUTINE LAIDLI

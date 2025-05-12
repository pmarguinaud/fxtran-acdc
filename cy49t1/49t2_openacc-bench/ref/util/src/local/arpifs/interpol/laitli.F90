#ifdef NECSX
!option! -O extendreorder
#endif
SUBROUTINE LAITLI(KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,PDLAT,PDLO,KL0,PDVER,&
 & PXSL,PXF,LDADD)

!$ACDC singlecolumn  --process-pointers


!**** *LAITLI  -  semi-LAgrangian scheme:
!                 Trilinear interpolations for one variable.

!     Purpose.
!     --------
!       Performs trilinear interpolations for one variable.

!**   Interface.
!     ----------
!        *CALL* *LAITLI(KSLB1,KPROMA,KST,KEND,KFLEV
!                      ,KFLDN,KFLDX
!                      ,PDLAT,PDLO,KL0,PDVER
!                      ,PXSL,PXF)

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
!          PDVER   - weights (distances) for vertical linear interpolation
!                    on a same vertical.
!          PXSL    - semi-lagrangian variable.

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
!        Called by LARCIN.

!     Reference.
!     ----------

!     Author.
!     -------
!        K. YESSAD, after the subroutine LAGINL3
!        written by Maurice IMBARD, Alain CRAPLET and Michel ROCHAS
!        METEO-FRANCE, CNRM/GMAP.

!     Modifications.
!     --------------
!        Original : FEBRUARY 1992.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        F. Vana       26-Aug-2008 optimization for NEC
!        F. Courteille 16-Sep-2009 optimization for NEC SX-9
!        H Petithomme (Dec 2020): optimisation, no SIMD anymore
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN) :: KSLB1
INTEGER(KIND=JPIM),INTENT(IN) :: KPROMA
INTEGER(KIND=JPIM),INTENT(IN) :: KFLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KFLDN
INTEGER(KIND=JPIM),INTENT(IN) :: KFLDX
INTEGER(KIND=JPIM),INTENT(IN) :: KST
INTEGER(KIND=JPIM),INTENT(IN) :: KEND
INTEGER(KIND=JPIM),INTENT(IN) :: KL0(KPROMA,KFLEV,1:2)
LOGICAL,INTENT(IN),OPTIONAL :: LDADD
REAL(KIND=JPRB),INTENT(IN) :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB),INTENT(IN) :: PDLO(KPROMA,KFLEV,1:2)
REAL(KIND=JPRB),INTENT(IN) :: PDVER(KPROMA,KFLEV)
REAL(KIND=JPRB),INTENT(IN) :: PXSL(KSLB1*(KFLDX-KFLDN+1))
REAL(KIND=JPRB),INTENT(INOUT) :: PXF(KPROMA,KFLEV)
!     ------------------------------------------------------------------

! optim: prefetch next 6 levels (best)
#ifdef __INTEL_COMPILER
INTEGER(KIND=JPIM),PARAMETER :: JPREFETCH=6
#endif

INTEGER(KIND=JPIM) :: JLEV,JROF,ILEVP,IL1,IL2
LOGICAL :: LLADD
REAL(KIND=JPRB) :: Z11,Z12,Z21,Z22,Z1(KPROMA),Z2(KPROMA)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "interp.func.h"

IF (LHOOK) CALL DR_HOOK('LAITLI',0,ZHOOK_HANDLE)

LLADD = .FALSE.
IF (PRESENT(LDADD)) LLADD = LDADD

! offsets in PXSL for levels 1 and 2
IL1 = KSLB1
IL2 = IL1+KSLB1

! optim: prefetch one level first
#ifdef __INTEL_COMPILER
ILEVP = MIN(JPREFETCH,KFLEV)

DO JROF=KST,KEND,2
  CALL MM_PREFETCH(PXSL(KL0(JROF,ILEVP,1)+IL1+1),2)
  CALL MM_PREFETCH(PXSL(KL0(JROF,ILEVP,2)+IL1+1),2)
  CALL MM_PREFETCH(PXSL(KL0(JROF,ILEVP,1)+IL2+1),2)
  CALL MM_PREFETCH(PXSL(KL0(JROF,ILEVP,2)+IL2+1),2)
ENDDO
#endif

DO JLEV=1,KFLEV
  ! optim: prefetch one next level if needed
#ifdef __INTEL_COMPILER
  IF (JLEV+JPREFETCH <= KFLEV) THEN
    DO JROF=KST,KEND,2
      ! Prefetches only 1 level for the next iteration
      CALL MM_PREFETCH(PXSL(KL0(JROF,JLEV+JPREFETCH,1)+IL1+1),2)
      CALL MM_PREFETCH(PXSL(KL0(JROF,JLEV+JPREFETCH,2)+IL1+1),2)
      CALL MM_PREFETCH(PXSL(KL0(JROF,JLEV+JPREFETCH,1)+IL2+1),2)
      CALL MM_PREFETCH(PXSL(KL0(JROF,JLEV+JPREFETCH,2)+IL2+1),2)
    ENDDO
  ENDIF
#endif

  DO JROF=KST,KEND
    Z11 = PXSL(IL1+KL0(JROF,JLEV,1)+1)
    Z11 = FDWEIGHT(PDLO(JROF,JLEV,1),Z11,PXSL(IL1+KL0(JROF,JLEV,1)+2))

    Z12 = PXSL(IL1+KL0(JROF,JLEV,2)+1)
    Z12 = FDWEIGHT(PDLO(JROF,JLEV,2),Z12,PXSL(IL1+KL0(JROF,JLEV,2)+2))

    Z1(JROF) = FDWEIGHT(PDLAT(JROF,JLEV),Z11,Z12)

    Z21 = PXSL(IL2+KL0(JROF,JLEV,1)+1)
    Z21 = FDWEIGHT(PDLO(JROF,JLEV,1),Z21,PXSL(IL2+KL0(JROF,JLEV,1)+2))

    Z22 = PXSL(IL2+KL0(JROF,JLEV,2)+1)
    Z22 = FDWEIGHT(PDLO(JROF,JLEV,2),Z22,PXSL(IL2+KL0(JROF,JLEV,2)+2))

    Z2(JROF) = FDWEIGHT(PDLAT(JROF,JLEV),Z21,Z22)
  ENDDO

  IF (LLADD) THEN
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = PXF(JROF,JLEV)+FDWEIGHT(PDVER(JROF,JLEV),Z1(JROF),Z2(JROF))
    ENDDO
  ELSE
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = FDWEIGHT(PDVER(JROF,JLEV),Z1(JROF),Z2(JROF))
    ENDDO
  ENDIF
ENDDO

IF (LHOOK) CALL DR_HOOK('LAITLI',1,ZHOOK_HANDLE)
END SUBROUTINE LAITLI

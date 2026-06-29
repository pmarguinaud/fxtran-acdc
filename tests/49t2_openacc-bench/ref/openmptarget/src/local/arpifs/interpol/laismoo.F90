SUBROUTINE LAISMOO(KSLB1,KPROMA,KST,KEND,KFLEV,&
 & KFLDN,KFLDX,PDLAT,PDLO,KL0,PDVER,&
 & PXSL,PXF)  

!$ACDC singlecolumn  --process-pointers


!**** *LAISMOO  -  semi-LAgrangian scheme:
!                 Smoothing interpolation by linear least-square fit

!     Purpose.
!     --------
!       Performs smooting in the horizontal and linear in the vertical interpolation

!**   Interface.
!     ----------
!        *CALL* *LAISMOO(...)

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
!          PDLAT   - distance for horizontal interpolation
!          PDLO    - distances for horizontal interpolations
!          KL0     - index of the four western points
!                    of the 16 points interpolation grid.
!          PDVER   - weights for linear vertical interpolation
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
!       M.Hortal   ECMWF

!     Modifications.
!     --------------
!       Original : Dec 2003
!       K. Yessad (Sep 2008): update comments + cleanings.
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KSLB1 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLAT(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMA,KFLEV,0:3) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMA,KFLEV,0:3) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDVER(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KSLB1*(KFLDX-KFLDN+1)) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXF(KPROMA,KFLEV) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIB) :: IV1L0, IV1L1, IV1L2, IV1L3,&
 & IV2L0, IV2L1, IV2L2, IV2L3  
INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRB) :: ZPOI(0:3,0:3,2)

REAL(KIND=JPRB) :: ZDVER, ZIS, ZISLO, ZISLO1, ZISLO2, ZISLO3, &
 & ZSI, ZSILO, ZSILO1, ZSILO2, ZSILO3  

#include "laismoo.func.h"

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('LAISMOO',0,ZHOOK_HANDLE)

!     ------------------------------------------------------------------

!*       1.    INTERPOLATIONS.
!              ---------------

IV1L0=  KSLB1
IV1L1=1+KSLB1
IV1L2=2+KSLB1
IV1L3=3+KSLB1
IV2L0=IV1L0+KSLB1
IV2L1=IV1L1+KSLB1
IV2L2=IV1L2+KSLB1
IV2L3=IV1L3+KSLB1

DO JLEV=1,KFLEV
  DO JROF=KST,KEND

    ZPOI(1,1,1)=PXSL(KL0(JROF,JLEV,1)+IV1L1)
    ZPOI(2,1,1)=PXSL(KL0(JROF,JLEV,1)+IV1L2)
    ZPOI(1,2,1)=PXSL(KL0(JROF,JLEV,2)+IV1L1)
    ZPOI(2,2,1)=PXSL(KL0(JROF,JLEV,2)+IV1L2)
    ZPOI(1,1,2)=PXSL(KL0(JROF,JLEV,1)+IV2L1)
    ZPOI(2,1,2)=PXSL(KL0(JROF,JLEV,1)+IV2L2)
    ZPOI(1,2,2)=PXSL(KL0(JROF,JLEV,2)+IV2L1)
    ZPOI(2,2,2)=PXSL(KL0(JROF,JLEV,2)+IV2L2)

    ZDVER =PDVER(JROF,JLEV)

!     32 points smoothing interpolation.

    ZSILO =PXSL(KL0(JROF,JLEV,0)+IV1L0)*FSLO1(PDLO(JROF,JLEV,0)) + &
     & PXSL(KL0(JROF,JLEV,0)+IV1L1)*FSLO2(PDLO(JROF,JLEV,0)) + &
     & PXSL(KL0(JROF,JLEV,0)+IV1L2)*FSLO3(PDLO(JROF,JLEV,0)) + &
     & PXSL(KL0(JROF,JLEV,0)+IV1L3)*FSLO4(PDLO(JROF,JLEV,0))  
    ZSILO1=PXSL(KL0(JROF,JLEV,1)+IV1L0)*FSLO1(PDLO(JROF,JLEV,1)) + &
     & PXSL(KL0(JROF,JLEV,1)+IV1L1)*FSLO2(PDLO(JROF,JLEV,1)) + &
     & PXSL(KL0(JROF,JLEV,1)+IV1L2)*FSLO3(PDLO(JROF,JLEV,1)) + &
     & PXSL(KL0(JROF,JLEV,1)+IV1L3)*FSLO4(PDLO(JROF,JLEV,1))   
    ZSILO2=PXSL(KL0(JROF,JLEV,2)+IV1L0)*FSLO1(PDLO(JROF,JLEV,2)) + &
     & PXSL(KL0(JROF,JLEV,2)+IV1L1)*FSLO2(PDLO(JROF,JLEV,2)) + &
     & PXSL(KL0(JROF,JLEV,2)+IV1L2)*FSLO3(PDLO(JROF,JLEV,2)) + &
     & PXSL(KL0(JROF,JLEV,2)+IV1L3)*FSLO4(PDLO(JROF,JLEV,2))   
    ZSILO3=PXSL(KL0(JROF,JLEV,3)+IV1L0)*FSLO1(PDLO(JROF,JLEV,3)) + &
     & PXSL(KL0(JROF,JLEV,3)+IV1L1)*FSLO2(PDLO(JROF,JLEV,3)) + &
     & PXSL(KL0(JROF,JLEV,3)+IV1L2)*FSLO3(PDLO(JROF,JLEV,3)) + &
     & PXSL(KL0(JROF,JLEV,3)+IV1L3)*FSLO4(PDLO(JROF,JLEV,3))  

    ZISLO =PXSL(KL0(JROF,JLEV,0)+IV2L0)*FSLO1(PDLO(JROF,JLEV,0)) + &
     & PXSL(KL0(JROF,JLEV,0)+IV2L1)*FSLO2(PDLO(JROF,JLEV,0)) + &
     & PXSL(KL0(JROF,JLEV,0)+IV2L2)*FSLO3(PDLO(JROF,JLEV,0)) + &
     & PXSL(KL0(JROF,JLEV,0)+IV2L3)*FSLO4(PDLO(JROF,JLEV,0))  
    ZISLO1=PXSL(KL0(JROF,JLEV,1)+IV2L0)*FSLO1(PDLO(JROF,JLEV,1)) + &
     & PXSL(KL0(JROF,JLEV,1)+IV2L1)*FSLO2(PDLO(JROF,JLEV,1)) + &
     & PXSL(KL0(JROF,JLEV,1)+IV2L2)*FSLO3(PDLO(JROF,JLEV,1)) + &
     & PXSL(KL0(JROF,JLEV,1)+IV2L3)*FSLO4(PDLO(JROF,JLEV,1))  
    ZISLO2=PXSL(KL0(JROF,JLEV,2)+IV2L0)*FSLO1(PDLO(JROF,JLEV,2)) + &
     & PXSL(KL0(JROF,JLEV,2)+IV2L1)*FSLO2(PDLO(JROF,JLEV,2)) + &
     & PXSL(KL0(JROF,JLEV,2)+IV2L2)*FSLO3(PDLO(JROF,JLEV,2)) + &
     & PXSL(KL0(JROF,JLEV,2)+IV2L3)*FSLO4(PDLO(JROF,JLEV,2))  
    ZISLO3=PXSL(KL0(JROF,JLEV,3)+IV2L0)*FSLO1(PDLO(JROF,JLEV,3)) + &
     & PXSL(KL0(JROF,JLEV,3)+IV2L1)*FSLO2(PDLO(JROF,JLEV,3)) + &
     & PXSL(KL0(JROF,JLEV,3)+IV2L2)*FSLO3(PDLO(JROF,JLEV,3)) + &
     & PXSL(KL0(JROF,JLEV,3)+IV2L3)*FSLO4(PDLO(JROF,JLEV,3))  

    ZSI   = ZSILO *FSLO1(PDLAT(JROF,JLEV)) + &
     & ZSILO1*FSLO2(PDLAT(JROF,JLEV)) + &
     & ZSILO2*FSLO3(PDLAT(JROF,JLEV)) + &
     & ZSILO3*FSLO4(PDLAT(JROF,JLEV))  
    ZIS   = ZISLO *FSLO1(PDLAT(JROF,JLEV)) + &
     & ZISLO1*FSLO2(PDLAT(JROF,JLEV)) + &
     & ZISLO2*FSLO3(PDLAT(JROF,JLEV)) + &
     & ZISLO3*FSLO4(PDLAT(JROF,JLEV))  

    PXF(JROF,JLEV)=  ZSI + ZDVER*(ZIS - ZSI)

  ENDDO
ENDDO

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LAISMOO',1,ZHOOK_HANDLE)
END SUBROUTINE LAISMOO


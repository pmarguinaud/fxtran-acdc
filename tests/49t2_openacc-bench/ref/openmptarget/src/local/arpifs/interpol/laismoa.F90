SUBROUTINE LAISMOA(KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,PDLO,KL0,PDVER,PXSL,PXF)

!$ACDC singlecolumn  --process-pointers


!**** *LAISMOA  -  semi-LAgrangian scheme:
!                 Smoothing interpolation to the arrival point

!     Purpose.
!     --------
!       Performs a smoothing two-dimensional interpolation to the arrival point.

!**   Interface.
!     ----------
!        *CALL* *LAISMOA(KSLB1,KPROMA,KST,KEND,KFLEV
!                      ,KFLDN,KFLDX,PDLO,KL0,PDVER,PXSL,PXF)

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
!          PDLO    - weights (distances) for horizontal interpolations
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
!        Called by LARCINA and LARCINB.

!     Reference.
!     ----------

!     Author.
!     -------
!        M. Hortal (ECMWF)

!     Modifications.
!     --------------
!        Original : AUGUST 2003.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        K. Yessad (Sep 2008): update comments + cleanings.
!        H Petithomme (Jan 2023): optimization and cleaning
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMA,KFLEV,0:3) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMA,KFLEV,0:3) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDVER(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KSLB1*(KFLDX-KFLDN+1)) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXF(KPROMA,KFLEV) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF, IOFF, IL
REAL(KIND=JPRB) :: ZSILO, ZSILO1, ZSILO2, ZD
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LAISMOA',0,ZHOOK_HANDLE)

!     ------------------------------------------------------------------

!*       1.    INTERPOLATIONS.
!              ---------------

DO JLEV=1,KFLEV
  ! optim: now vectorizes
  DO JROF=KST,KEND
    IF(PDVER(JROF,JLEV) < 0.5_JPRB) THEN  
      IOFF = KPROMA
    ELSE
      IOFF = KPROMA+KPROMA
    ENDIF

    ! 11 points smoothing interpolation.
    IL = KL0(JROF,JLEV,0)+IOFF
    ZD = PDLO(JROF,JLEV,0)
    ZSILO = PXSL(IL)*(4-3*ZD)+PXSL(IL+1)*(3-ZD)+PXSL(IL+2)*(2+ZD)+PXSL(IL+3)*(1+3*ZD)
    IL = KL0(JROF,JLEV,2)+IOFF
    ZD = PDLO(JROF,JLEV,2)
    ZSILO2 = PXSL(IL)*(4-3*ZD)+PXSL(IL+1)*(3-ZD)+PXSL(IL+2)*(2+ZD)+PXSL(IL+3)*(1+3*ZD)
    IL = KL0(JROF,JLEV,1)+IOFF
    ZSILO1 = (PXSL(IL)+PXSL(IL+1)+PXSL(IL+2))/3

    PXF(JROF,JLEV) = (ZSILO + ZSILO1 + ZSILO2)/30._JPRB
  ENDDO
ENDDO

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LAISMOA',1,ZHOOK_HANDLE)
END SUBROUTINE LAISMOA


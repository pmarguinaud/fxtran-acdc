SUBROUTINE VERDERFE(KPROMA,KST,KEND,KFLEV,PDETA_RATIO,PDELPH,PRDELPF,PIN,POUT,PCORR,LD_CHECK_TRIDIAG)

!$ACDC singleblock

!**** *VERDERFE*   VERtical DIScretization -
!                 INTerface for finite element type vertical operations:
!                 derivative based on compatible approach  


!     Purpose.
!     --------
!        * compute vertical derivative from compatible finite-element 
!          method using linear/constant piecewise basis-functions

!**   Interface.
!     ----------
!        *CALL* *VERDERFE(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KPROMA        - horizontal dimension.
!          KST           - first element of work.
!          KEND          - depth of work.
!          KFLEV         - vertical dimension number of levels
!          PDETA_RATIO   - "delta_eta_d/delta_eta_i" at full levels
!          PDELPH        - "delta_prehyd" at full levels.
!          PRDELPF       - "1/delta_prehyd" at half levels.
      
!          PIN           - input field

!        OUTPUT:

!          POUT    - output: result 

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!           TRIDIA

!     Author.
!     -------
!        F. Voitus : 25-May-2022

!     Modifications.
!     --------------
!     
!     ------------------------------------------------------------------

USE YOMLUN   , ONLY : NULOUT

USE PARKIND1 , ONLY : JPIM, JPRB, JPRD
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA  
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDETA_RATIO(KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELPH(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELPF(KPROMA,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PIN(KPROMA,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: POUT(KPROMA,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)  ,OPTIONAL :: PCORR(KPROMA)
LOGICAL           ,INTENT(IN)  ,OPTIONAL :: LD_CHECK_TRIDIAG

!     ------------------------------------------------------------------

LOGICAL :: LL_CHECK_TRIDIAG

INTEGER(KIND=JPIM) :: JLEV, JROF, ILEV

REAL(KIND=JPRB) :: ZCOR(KPROMA)
REAL(KIND=JPRB) :: ZSOL(KPROMA,KFLEV+1)
REAL(KIND=JPRB) :: ZRHS(KPROMA,KFLEV+1)
REAL(KIND=JPRB) :: ZRES(KPROMA,KFLEV+1)
REAL(KIND=JPRB) :: ZM(KPROMA,KFLEV+1,-1:1)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "tridia2.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('VERDERFE',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

LL_CHECK_TRIDIAG=.FALSE.
IF (PRESENT(LD_CHECK_TRIDIAG)) LL_CHECK_TRIDIAG=LD_CHECK_TRIDIAG

ILEV=KFLEV+1

ZCOR(KST:KEND)=1.0_JPRB
IF (PRESENT(PCORR)) ZCOR(KST:KEND)=PCORR(KST:KEND)

!* memory transfert (can be avoided, but later!:) 

DO JLEV=1,KFLEV+1
  ZRHS(KST:KEND,JLEV)=PIN(KST:KEND,JLEV-1)
ENDDO

DO JLEV=2,KFLEV
  DO JROF=KST,KEND
    !* upper off-diagonal

    ZM(JROF,JLEV,1)=PRDELPF(JROF,JLEV-1)*(&
     & (PDELPH(JROF,JLEV)/PDETA_RATIO(JLEV))/6.0_JPRB)
    !* diagonal
    ZM(JROF,JLEV,0)=PRDELPF(JROF,JLEV-1)*(&
     & ((PDELPH(JROF,JLEV-1)/PDETA_RATIO(JLEV-1))&
     & +(PDELPH(JROF,JLEV)/PDETA_RATIO(JLEV)))/3.0_JPRB)
    !* lower off-diagonal
    ZM(JROF,JLEV,-1)=PRDELPF(JROF,JLEV-1)*(&
     & (PDELPH(JROF,JLEV-1)/PDETA_RATIO(JLEV-1))/6.0_JPRB)
  ENDDO
ENDDO  

!* special treatment at vertical boundaries
DO JROF=KST,KEND
  !* top condition
  ZM(JROF,1,1)=PRDELPF(JROF,0)*(&
   & (PDELPH(JROF,1)/PDETA_RATIO(1))/6.0_JPRB)        
  ZM(JROF,1,0)=PRDELPF(JROF,0)*(&
   & (PDELPH(JROF,1)/PDETA_RATIO(1))/3.0_JPRB)
  !* surface condition        
  ZM(JROF,KFLEV+1,0)=PRDELPF(JROF,KFLEV)*(& 
   & (PDELPH(JROF,KFLEV)/PDETA_RATIO(KFLEV))/3.0_JPRB) 
  ZM(JROF,KFLEV+1,-1)=PRDELPF(JROF,KFLEV)*(&
   & ZCOR(JROF)*(PDELPH(JROF,KFLEV)/PDETA_RATIO(KFLEV))/6.0_JPRB)                        
ENDDO

! Fv remark : Maybe not be the optimal algorithm to invert a tridiagonal matrix, 
! when single precision calculation is at stake. BLAS/Llapack tridiag inversion
! routines DGTSV (double prec) or SGTSV (simple prec.) should do a better the job. 
CALL TRIDIA2(ILEV,KPROMA,KST,KEND,ZM,ZRHS,ZSOL)

DO JLEV=0,KFLEV
  POUT(KST:KEND,JLEV)=ZSOL(KST:KEND,JLEV+1)
ENDDO

IF (LL_CHECK_TRIDIAG) THEN

  DO JLEV=2,KFLEV
    DO JROF=KST,KEND
      ZRES(JROF,JLEV)=ZRHS(JROF,JLEV)-( ZM(JROF,JLEV,0)*ZSOL(JROF,JLEV) &
      & + ZM(JROF,JLEV,-1)*ZSOL(JROF,JLEV-1) + ZM(JROF,JLEV,1)*ZSOL(JROF,JLEV+1))  
    ENDDO
  ENDDO

  DO JROF=KST,KEND
    ZRES(JROF,1)=ZRHS(JROF,1)-(ZM(JROF,1,0)*ZSOL(JROF,1) &
    & + ZM(JROF,1,1)*ZSOL(JROF,2))
    ZRES(JROF,KFLEV+1)=ZRHS(JROF,KFLEV+1)-(ZM(JROF,KFLEV+1,0)*ZSOL(JROF,KFLEV+1) &
    & + ZM(JROF,KFLEV+1,-1)*ZSOL(JROF,KFLEV))
  ENDDO

  WRITE(NULOUT,'(''* VERDERFE: Tridiag matrix inversion error = '',E14.8)') MAXVAL(ABS(ZRES))
ENDIF
!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('VERDERFE',1,ZHOOK_HANDLE)
END SUBROUTINE VERDERFE

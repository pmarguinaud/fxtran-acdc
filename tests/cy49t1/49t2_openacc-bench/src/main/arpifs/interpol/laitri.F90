!option! -O extendreorder
SUBROUTINE LAITRI(KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,CDTYPE,PDLAT,PCLA,&
 & PDLO,PCLO,KL0,PVINTW,PXSL,PXF,LDADD)

!$ACDC singlecolumn  --process-pointers


! Purpose :
! -------
!   LAITRI - semi-Lagrangian scheme: tri-dimensional 32-point
!   interpolations (with optional quasi-monotonic treatment). Type of high
!   order interpolator fully controlled by weights, low order
!   interpolator always linear.

! Interface :
! ---------
!   INPUT:
!     KSLB1  - horizontal dimension for grid-point quantities
!     KPROMA  - horizontal dimension for interpolation point quantities
!     KST     - first element of arrays where computations are performed
!     KEND   - depth of work
!     KFLEV   - vertical dimension
!     KFLDN   - number of the first field
!     KFLDX   - number of the last field
!     KQM     - index of monotonicity
!               0: not monotonous interpolation
!               1: horizontally quasi-monotonous interpolation
!               2: quasi-monotonous interpolation
!               3: vertically quasi-monotonous interpolation
!     PDLAT   - distance for horizontal linear interpolations in latitude
!     PCLA    - weights for horizontal cubic interpolations in latitude
!     PDLO    - distances for horizontal linear interpolations
!               in longitude (latitude rows 0, 1, 2, 3)
!     PCLO    - weights for horizontal cubic interpolations in longitude 
!               (latitude rows 1, 2)
!     KL0     - indices of the four western points of the 16 point
!               interpolation grid
!     PVINTW  - weights for cubic vertical interpolation
!     PXSL    - quantity to be interpolated
!     LDADD   - add result to input value
!   OUTPUT:
!     PXF     - interpolated variable

! Externals :
! ---------
!   None.

! Method :
! ------
!   See documentation.

! Reference :
! ---------

! Author :
! ------
!   ??-Feb-1992 K. Yessad, after the subroutine LAGINT3 written by
!   Maurice Imbard, Alain Craplet and Michel Rochas,
!   Meteo-France, CNRM/GMAP.

! Modifications :
! -------------
!   01-Oct-2003 M. Hamrud  CY28 Cleaning.
!   30-Jun-2008 J. Masek   High order interpolator fully driven by weights,
!     computation of all weights moved to (E)LASCAW.
!   03-Sep-2008 K. Yessad  Merge QM with not-QM version.
!   R. El Khatib 07-08-2009 Optimisation directive for NEC
!   09-Nov-2009 F. Vana Split to scalar and vector alternatives.
!   26-Jan-2010 R. El Khatib Fix optimisation directives for NEC
!   08-Sep-2014 R. El Khatib Cleaning + use vector code if not rs6k
!   K. Yessad (July 2014): Move some variables.
!   23-Jun-2014 J. Hague: Select "Vector" alternative  for Cray
!   F. Vana   21-Nov-2017: option KQM = 3 (useful for trajectory research in NL)
!   H Petithomme (Dec 2020): optimisation for scalar code
!   R. El Khatib 23-Feb-2023 Portability fix for NEC Sx-Aurora : remove an elemental attribute and
!                            re-instate compiler directives
! End Modifications
!------------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB  
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KSLB1 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KQM
CHARACTER(LEN=*),INTENT(IN)      :: CDTYPE
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLAT(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLA(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMA,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLO(KPROMA,KFLEV,3,2)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVINTW(KPROMA,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KSLB1*(KFLDX-KFLDN+1))
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXF(KPROMA,KFLEV)
LOGICAL,OPTIONAL,INTENT(IN)      :: LDADD

!------------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV,JROF
LOGICAL :: LLADD
REAL(KIND=JPRB) :: ZXF(KPROMA,KFLEV)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "laitri_scalar.intfb.h"
#include "laitri_vector.intfb.h"
#include "abor1.intfb.h"

!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LAITRI',0,ZHOOK_HANDLE)

LLADD = .FALSE.
IF (PRESENT(LDADD)) LLADD = LDADD

IF (CDTYPE=='SCALAR') THEN
  IF (LLADD) THEN
    CALL LAITRI_SCALAR (KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,&
     & PDLAT,PCLA,PDLO,PCLO,KL0,PVINTW,PXSL,ZXF)
  ELSE
    CALL LAITRI_SCALAR (KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,&
     & PDLAT,PCLA,PDLO,PCLO,KL0,PVINTW,PXSL,PXF)
  ENDIF
ELSEIF (CDTYPE == 'VECTOR') THEN
  IF (LLADD) THEN
    CALL LAITRI_VECTOR (KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,&
     & PDLAT,PCLA,PDLO,PCLO,KL0,PVINTW,PXSL,ZXF)
  ELSE
    CALL LAITRI_VECTOR (KSLB1,KPROMA,KST,KEND,KFLEV,KFLDN,KFLDX,KQM,&
     & PDLAT,PCLA,PDLO,PCLO,KL0,PVINTW,PXSL,PXF)
  ENDIF
ELSE
  CALL ABOR1 ('LAITRI: UNEXPECTED CDTYPE')
ENDIF

IF (LLADD) THEN
  DO JLEV=1,KFLEV
    DO JROF=KST,KEND
      PXF(JROF,JLEV) = PXF(JROF,JLEV)+ZXF(JROF,JLEV)
    ENDDO
  ENDDO
ENDIF

IF (LHOOK) CALL DR_HOOK('LAITRI',1,ZHOOK_HANDLE)
END SUBROUTINE LAITRI


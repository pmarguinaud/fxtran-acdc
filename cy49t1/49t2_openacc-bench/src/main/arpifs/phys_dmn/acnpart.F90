!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACNPART(YDCST, YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,&
 & PAPHI,PAPHIF,PAPRSF,PDECRD,PNEB,&
 & PCLCH,PCLCM,PCLCL,PCLCT,PCLCT_RAD,&
 ! optional arguments (convective cloud cover)
 & PCLCC,PNEBC,PTOPC)

! Purpose:
! --------
!   ACNPART - computes high/medium/low, convective and total cloud cover.
!   Several overlap options are implemented.

! Interface:
! ----------
! INPUT:
!   KIDIA     - initial index for horizontal loops
!   KFDIA     - final index for horizontal loops
!   KLON      - horizontal dimension of arrays
!   KTDIA     - initial index for vertical loops (usually 1)
!   KLEV      - vertical dimension of full level arrays
!   PAPHI     - half level geopotential
!   PAPHIF    - full level geopotential
!   PAPRSF    - full level pressure
!   PDECRD    - decorrelation depth for cloud overlaps [Pa]
!   PNEB      - total cloud cover on levels (protected from 0 and 1)

! OUTPUT:
!   PCLCH     - high cloud cover
!   PCLCM     - medium cloud cover
!   PCLCL     - low cloud cover
!   PCLCT     - total cloud cover
!   PCLCT_RAD - total cloud cover for radiation

! INPUT, OPTIONAL:
!   PNEBC     - convective cloud cover on levels (protected from 0 and 1,
!               missing in AROME)

! OUTPUT, OPTIONAL:
!   PCLCC     - convective cloud cover (missing in AROME)
!   PTOPC     - TOP of convective cloud  [Pa] (missing in AROME)


! Externals:
! ----------

! Method:
! -------

! Reference:
! ----------

! Author:
! -------
!   2007-02, R. Brozkova

! Modifications:
! --------------
!   2009-03, C. Wittmann
!   Introduction of LACPANMX and WMXOV.
!
!   2009-07, K. Yessad
!   Remove CDLOCK + some cleaning.
!
!   2009-10, L. Bengtsson
!   Introduction of LWMOCLOUD.
!
!   2016-04, J. Masek
!   Introduction of LRNUEXP (exponential-random overlap), fix for LWMOCLOUD,
!   modularization, reordering of arguments, PCLCC optional (missing in AROME).
!
!   2016-09, J. Masek
!   Introduction of radiative cloud cover PCLCT_RAD, needed for consistent
!   calculation of sunshine duration.
!
!   2018-09, J. Masek
!   Fix of convective cloud cover when WMXOV or RDECRDRED differ from 1.
!
!   2018-07, O. Jaron
!   Introduction of CONV_BASE_TOP to diagnose base and top of convective
!   clouds. (Base desactivated)
!
! End Modifications
!-------------------------------------------------------------------------------

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMCST   , ONLY : TCST

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(IN):: YDML_PHY_MF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDECRD(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEB(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLCH(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLCM(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLCL(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLCT(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLCT_RAD(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT),OPTIONAL :: PCLCC(KLON)
REAL(KIND=JPRB)   ,INTENT(IN), OPTIONAL :: PNEBC(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT),OPTIONAL :: PTOPC(KLON)

#include "acnpart_cloud_cover_wmo.intfb.h"
#include "acnpart_cloud_cover.intfb.h"
#include "acnpart_conv_base_top.intfb.h"

!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZDECRDRED,ZWMXOV
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ACNPART',0,ZHOOK_HANDLE)
ASSOCIATE(LACPANMX =>YDML_PHY_MF%YRPHY%LACPANMX,   &
 &        LRNUMX   =>YDML_PHY_MF%YRPHY%LRNUMX,     &
 &        LRNUEXP  =>YDML_PHY_MF%YRPHY%LRNUEXP,    &
 &        RDECRDRED=>YDML_PHY_MF%YRPHY0%RDECRDRED, &
 &        WMXOV    =>YDML_PHY_MF%YRPHY0%WMXOV,     &
 &        LWMOCLOUD=>YDML_PHY_MF%YRPHY2%LWMOCLOUD, &
 &        NTSML    =>YDML_PHY_MF%YRPHY2%NTSML,     &
 &        NTSHM    =>YDML_PHY_MF%YRPHY2%NTSHM,     &
 &        LPTOPC   =>YDML_PHY_MF%YRPHY%LPTOPC)
! settings for diagnostic cloud cover
ZDECRDRED=RDECRDRED
ZWMXOV   =WMXOV

IF ( LWMOCLOUD ) THEN

  ! high/medium/low cloud cover according to WMO heights
  CALL ACNPART_CLOUD_COVER_WMO(YDCST,YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZDECRDRED,ZWMXOV,PDECRD,PNEB,PAPHI,PAPRSF,PCLCH,PCLCM,PCLCL)

ELSE

  ! high/medium/low cloud cover according to fixed model levels
  CALL ACNPART_CLOUD_COVER(YDML_PHY_MF,KIDIA,KFDIA,KLON,KLEV,ZDECRDRED,ZWMXOV,KTDIA,NTSHM,PDECRD,PNEB,PAPRSF,PCLCH)  ! high
  CALL ACNPART_CLOUD_COVER(YDML_PHY_MF,KIDIA,KFDIA,KLON,KLEV,ZDECRDRED,ZWMXOV,NTSHM+1,NTSML,PDECRD,PNEB,PAPRSF,PCLCM)  ! medium
  CALL ACNPART_CLOUD_COVER(YDML_PHY_MF,KIDIA,KFDIA,KLON,KLEV,ZDECRDRED,ZWMXOV,NTSML+1,KLEV,PDECRD,PNEB,PAPRSF,PCLCL)  ! low

ENDIF

! total cloud cover
CALL ACNPART_CLOUD_COVER(YDML_PHY_MF,KIDIA,KFDIA,KLON,KLEV,ZDECRDRED,ZWMXOV,KTDIA,KLEV,PDECRD,PNEB,PAPRSF,PCLCT)

! convective cloud cover
IF ( PRESENT(PCLCC) ) THEN
  CALL ACNPART_CLOUD_COVER(YDML_PHY_MF,KIDIA,KFDIA,KLON,KLEV,ZDECRDRED,ZWMXOV,KTDIA,KLEV,PDECRD,PNEBC,PAPRSF,PCLCC)
ENDIF

! total cloud cover for radiation
IF ( LRNUMX.AND.(LACPANMX.OR.LRNUEXP) ) THEN
  ZDECRDRED=1._JPRB  ! do not reduce decorrelation depth
  ZWMXOV   =1._JPRB  ! ignore LACPANMX
  CALL ACNPART_CLOUD_COVER(YDML_PHY_MF,KIDIA,KFDIA,KLON,KLEV,ZDECRDRED,ZWMXOV,KTDIA,KLEV,PDECRD,PNEB ,PAPRSF,PCLCT_RAD)
ELSE
  PCLCT_RAD(:)=PCLCT(:)
ENDIF

! convective top
IF (PRESENT(PTOPC).AND. LPTOPC) THEN
  CALL ACNPART_CONV_BASE_TOP(YDCST,YDML_PHY_MF,KIDIA,KFDIA,KLON,KLEV,PNEBC,PAPRSF,PAPHIF,PTOPC)
ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACNPART',1,ZHOOK_HANDLE)

END SUBROUTINE ACNPART

SUBROUTINE GPMPFC_EXPL (YDVARS, YDML_GCONF, YDDYN, YDDYNA, YDCPG_OPTS, YDCPG_BNDS, KFLAG, PGM)

!$ACDC outline1    


!**** *GPMPFC_EXPL* - Apply map factor to convert 
!                reduced variables -> geographical variables if kflag=0
!                or
!                geographical variables -> reduced variables if kflag=1

!     Purpose.
!     --------
!           Multiply or divide by map factor.

!**   Interface.
!     ----------
!        *CALL* *GPMPFC_EXPL(...)

!        Explicit arguments :
!        --------------------

!      INPUT:
!      ------
!       KPROMA    - horizontal dimensioning
!       KFLEV     - number of layers
!       KST       - start of work
!       KEN       - depth of work
!       KFLAG     - 0 -> multiply, 1-> divide
!       PGM       - map factor

!      INPUT/OUTPUT:
!      -------------
!       PGMV      - GMV variables
!       PGMVS     - GMVS variables
!       PGFL      - GFL variables

!        Implicit arguments :  None
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.  None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      Mats Hamrud  *ECMWF*
!      Original : 1994-01-18

! Modifications
! -------------
!   Modified 2002-07-02 by C. Fischer  : rename NHS variables T0/T9
!   Modified 2002-11-13 by K. YESSAD   : some cleanings + improve vectorization
!   Modified 2003-08    by M. HAMRUD   : GFL
!   01-Oct-2003 M. Hamrud  CY28 Cleaning
!   08-Jun-2004 J. Masek   NH cleaning (LVSLWBC)
!   01-Jul-2004 K. Yessad  Make clearer the tests for PC scheme.
!   09-Feb-2006 M. Deque : Dasux compilance
!   K. Yessad (Dec 2008): remove dummy CDLOCK
!   K. Yessad (Jun 2011): new dataflow, GMVS too.
!   K. Yessad (Nov 2012): simplify testings.
!   K. Yessad (July 2014): Move some variables.
!   H. Petithomme (Dec 2020): optimisation
! End Modifications
!     ------------------------------------------------------------------
USE MODEL_GENERAL_CONF_MOD , ONLY : MODEL_GENERAL_CONF_TYPE
USE PARKIND1               , ONLY : JPIM, JPRB
USE YOMHOOK                , ONLY : LHOOK, JPHOOK, DR_HOOK
USE YOMDYN                 , ONLY : TDYN
USE YOMDYNA                , ONLY : TDYNA
USE FIELD_VARIABLES_MOD    , ONLY : FIELD_VARIABLES
USE CPG_OPTS_TYPE_MOD      , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(FIELD_VARIABLES)         ,INTENT(INOUT)          :: YDVARS
TYPE(MODEL_GENERAL_CONF_TYPE) ,INTENT(IN)             :: YDML_GCONF
TYPE(TDYN)                    ,INTENT(IN)             :: YDDYN
TYPE(TDYNA)                   ,INTENT(IN)             :: YDDYNA
TYPE(CPG_OPTS_TYPE)           ,INTENT(IN)             :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)           ,INTENT(IN)             :: YDCPG_BNDS
INTEGER(KIND=JPIM)            ,INTENT(IN)             :: KFLAG 
REAL(KIND=JPRB)               ,INTENT(IN)    ,TARGET  :: PGM(YDCPG_OPTS%KLON)


#include "abor1.intfb.h"
#include "gpmpfc_expl_part1.intfb.h"
#include "gpmpfc_expl_part2.intfb.h"

REAL(KIND=JPRB),CONTIGUOUS,POINTER :: ZGM(:)
REAL(KIND=JPRB),TARGET :: ZGM0(YDCPG_OPTS%KLON)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GPMPFC_EXPL', 0, ZHOOK_HANDLE)

!*       1. APPLY MAP FACTOR.
!           -----------------

IF (KFLAG == 0) THEN
  ZGM => PGM
ELSEIF (KFLAG == 1) THEN
  ZGM => ZGM0
ELSE
  CALL ABOR1('GPMPFC_EXPL: ILLEGAL KFLAG')
ENDIF

IF (KFLAG == 1) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

  ZGM(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA) = 1.0_JPRB/PGM(YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA)

!$ACDC }

ENDIF

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

CALL GPMPFC_EXPL_PART1 (YDML_GCONF, YDDYN, YDDYNA, YDCPG_OPTS%KLON, YDCPG_OPTS%KFLEVG, YDCPG_BNDS%KIDIA,           &
& YDCPG_BNDS%KFDIA, ZGM, P0U=YDVARS%U%T0, P0V=YDVARS%V%T0, P0DIV=YDVARS%DIV%T0, P0TL=YDVARS%T%DL, P0TM=YDVARS%T%DM,&
& P9U=YDVARS%U%T9, P9V=YDVARS%V%T9, P0UL=YDVARS%U%DL, P0VL=YDVARS%V%DL, P0VOR=YDVARS%VOR%T0, P0SPDL=YDVARS%SPD%DL, &
& P0SPDM=YDVARS%SPD%DM, P0SVDL=YDVARS%SVD%DL, P0SVDM=YDVARS%SVD%DM, P0NHXL=YDVARS%NHX%DL, P0NHXM=YDVARS%NHX%DM,    &
& P9DIV=YDVARS%DIV%T9, P9TL=YDVARS%T%DL9, P9TM=YDVARS%T%DM9, P9SPDL=YDVARS%SPD%DL9, P9SPDM=YDVARS%SPD%DM9,         &
& P9SVDL=YDVARS%SVD%DL9, P9SVDM=YDVARS%SVD%DM9, P0SPL=YDVARS%SP%DL, P0SPM=YDVARS%SP%DM, P9SPL=YDVARS%SP%DL9,       &
& P9SPM=YDVARS%SP%DM9)

!$ACDC }

CALL GPMPFC_EXPL_PART2 (YDVARS, YDCPG_OPTS, YDCPG_BNDS, ZGM)

IF (LHOOK) CALL DR_HOOK('GPMPFC_EXPL', 1, ZHOOK_HANDLE)

END SUBROUTINE GPMPFC_EXPL


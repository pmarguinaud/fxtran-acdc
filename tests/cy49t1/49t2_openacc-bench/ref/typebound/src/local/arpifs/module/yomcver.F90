MODULE YOMCVER

!$ACDC methods 


USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK,  JPHOOK

USE YOMLUN   , ONLY : NULOUT   ,NULNAM
USE YOMCT0   , ONLY : LR2D     ,LECMWF   , LARPEGEF

IMPLICIT NONE

SAVE

! =============================================================================

TYPE TCVER
! ------ Vertical discretisation --------------------------------------------

! NDLNPR  : NDLNPR=0: conventional formulation of delta, i.e. ln(P(l)/P(l-1)).
!           NDLNPR=1: formulation of delta used in non hydrostatic model,
!                     i.e. (P(l)-P(l-1))/SQRT(P(l)*P(l-1)).
! RHYDR0  : value given to "alpha(1) = depth of log(Pi) between top and full level nr 1"
!           in case where general formula to compute "alpha" gives an infinite value
!           (used only if LVERTFE=F, NDLNPR=0).
!           This quantity is never used in the following cases:
!            LVERTFE=T.
!            LVERTFE=F with NDLNPR=1.
! LAPRXPK : way of computing full-levels pressures in primitive equation
!           hydrostatic model.
!           .T.: full levels are computed by PK=(PK+1/2 + PK-1/2)*0.5
!           .F.: full levels are computed by a more complicated formula
!                consistent with "alpha" in geopotential formula.

LOGICAL :: LAPRXPK
INTEGER(KIND=JPIM) :: NDLNPR
REAL(KIND=JPRB) :: RHYDR0

! ----- vertical discretisation, vertical boundaries:
! LREGETA   : .T.: for the interlayer L, ETA(L)=L/NFLEVG
!             .F.: for the interlayer L, ETA(L)=A(L)/P0+B(L)
! LVFE_REGETA: cf. LREGETA for "eta" used in VFE operators.
LOGICAL :: LREGETA
LOGICAL :: LVFE_REGETA


! * Variables related to vertical discretisation in finite elements:

! NVFE_TYPE     : Type of spline basis used for finite element vertical discretisation.
!               (1 = linear, 3 = cubic)
! NVFE_ORDER    : Order of spline used in VFE; NVFE_ORDER=NVFE_TYPE+1
! NVFE_INTERNALS: number of internals knots

! LVERTFE       : .T./.F. Finite element/conventional vertical discretisation.
! LVFE_LAPL_BC  : VFE for boundary cond. in vert. Laplacian term (NH model)

! VFE for vertical velocity (NH model):
! LVFE_GW       : T - invertible RINTGW/RDERGW used under key LGWADV, full levels gw.
! LVFE_GW_HALF  : T - invertible RINTGW/RDERGW used under key LGWADV, half levels gw.
! LVFE_GWMPA    : T - VFE for AROME physics vertical velocity

! LVFE_CHEB     : chebyshev nodes (dense distribution of levels near BCs in eta space)
! LVFE_ECMWF    : T if original ECMWF way to compute vertical integral and derivative
! LVFE_NOBC     : T no boundary conditions applied for vert.derivative (RDERI)
!                 F boundary conditions applied for vert.derivative (RDERB)
! LPERCENTILS   : used in SUVFE_KNOT to determine method for computing knots for basis functions
! LVFE_VERBOSE  : print several diagnostics or not
! CVFE_ETAH     : half levels eta definition
!           REGETA - regular distribution
!           MCONST - general definition
!           MCNTRL - explicitly prescribed m = dpi/eta at boundaries; density control
!           CHORDAL - using A and Bs
! LDYN_ANALYSIS_STABILITY : this key is more general than VFE itself
!                 It turn on analysis of stability of linear operator under SUSI.
!                 We compute eigenvalues of mastrix "M = (I - tau L)^-1 (I + tau L)"
!                 M has dimension (2*NFLEVG + 1)*(2*NFLEVG + 1) and 
!                 correctly design L operator must have all eigenvalues Abs(eval) <= 1.0.
!                 (please move this key into NAMDYN and yomdyn.  
!                 I did not done it to save compilation time .. lazyness:-))
! RVFE_ALPHA    : Exponent that constrols definition of eta. 
!                 RVFE_ALPHA =  0   -  gives regular (the same like LVFE_REGETA)
!                 RVFE_ALPHA =  1   -  gives classic sigma (eta = sigma for pressure VP00)
! RVFE_BETA     : Exponent that constrols density of levels close to boundaries.
!                 RVFE_BETA  =  0   -  there is density transformation
!                 RVFE_BETA  =  1   -  chanyshev definition (when combined with RVFE_ALPHA=0.0)
! RVFE_ALPHA/RVFE_BETA : control definition of eta close to boundaries
!           RVFE_ALPHA/BETA =  0   -  gives regular (the same like LVFE_REGETA)
!           RVFE_ALPHA/BETA >  0   -  denser close to boundaries
!           RVFE_ALPHA/BETA <  0   -  denser close to inner domain (MCNTRL only)
! RVFE_KNOT_STRETCH : stretching of knots
! LREFINE_NHPRE_BBC : .T. refine treatment of surface nh pressure relying on 
!                     a consistent treatment of free-slip and rigid-bottom
!                     boundary condition. 
! LVFE_COMPATIBLE : .T. 
! LVFD_COMPATIBLE : .T.
! LVFE_SILAPL     : .T. 
! LVFE_FD_MIX     : .T.
! ----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NVFE_TYPE
INTEGER(KIND=JPIM) :: NVFE_ORDER
INTEGER(KIND=JPIM) :: NVFE_INTERNALS
LOGICAL :: LVERTFE
LOGICAL :: LVFE_LAPL_BC
LOGICAL :: LVFE_GW
LOGICAL :: LVFE_GW_HALF
LOGICAL :: LVFE_GWMPA
LOGICAL :: LVFE_CHEB
LOGICAL :: LVFE_ECMWF
LOGICAL :: LVFE_NOBC
LOGICAL :: LVFE_VERBOSE
LOGICAL :: LVFE_NORMALIZE
LOGICAL :: LDYN_ANALYSIS_STABILITY
LOGICAL :: LPERCENTILS
LOGICAL :: LVFE_COMPATIBLE
LOGICAL :: LVFE_FD_MIX
LOGICAL :: LVFE_SILAPL
LOGICAL :: LVFD_COMPATIBLE
LOGICAL :: LREFINE_NHPRE_BBC
REAL(KIND=JPRB) :: RVFE_ALPHA
REAL(KIND=JPRB) :: RVFE_BETA
REAL(KIND=JPRB) :: RVFE_KNOT_STRETCH
REAL(KIND=JPRB) :: RFAC1, RFAC2
CHARACTER(LEN=8) :: CVFE_ETAH
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TCVER
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TCVER
PROCEDURE :: ACDC_HOST => ACDC_HOST_TCVER
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TCVER
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TCVER
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TCVER
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TCVER
END TYPE TCVER
! =============================================================================

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TCVER (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TCVER), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TCVER (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TCVER), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TCVER (SELF)

IMPLICIT NONE
CLASS (TCVER), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TCVER (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TCVER), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TCVER (SELF, KLUN)

IMPLICIT NONE
CLASS (TCVER), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TCVER (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TCVER),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TCVER (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TCVER), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

CONTAINS

! =============================================================================
SUBROUTINE SUCVER_GEOM(YDCVER,LDNHDYN_GEOM)

!**** *SUCVER*   - Set-up for some keys
!                  used in the vertical finite elements discretisation

!     Purpose.
!     --------
!      sets-up YOMCVER

!**   Interface.
!     ----------
!        *CALL* *SUCVER(...)

!        Explicit arguments :
!        --------------------

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      K. Yessad (from some SUCT0 and SUDYN code).
!      Original : May 2012

! Modifications
! -------------
!      F. Vana  26-Sep-2019   Defaults changed to the new VFE scheme (LVFE_ECMWF=F)
!      P. Smolikova (Sep 2020): VFE pruning.

! End Modifications
!      ----------------------------------------------------------------

IMPLICIT NONE

TYPE(TCVER),TARGET,INTENT(INOUT) :: YDCVER
LOGICAL,INTENT(IN)        :: LDNHDYN_GEOM

LOGICAL           ,POINTER :: LAPRXPK
REAL(KIND=JPRB)   ,POINTER :: RHYDR0
INTEGER(KIND=JPIM),POINTER :: NDLNPR
LOGICAL           ,POINTER :: LREGETA
LOGICAL           ,POINTER :: LVFE_REGETA
INTEGER(KIND=JPIM),POINTER :: NVFE_TYPE
INTEGER(KIND=JPIM),POINTER :: NVFE_ORDER
INTEGER(KIND=JPIM),POINTER :: NVFE_INTERNALS
LOGICAL,POINTER :: LVERTFE
LOGICAL,POINTER :: LVFE_LAPL_BC
LOGICAL,POINTER :: LVFE_GW
LOGICAL,POINTER :: LVFE_GW_HALF
LOGICAL,POINTER :: LVFE_GWMPA
LOGICAL,POINTER :: LVFE_CHEB
LOGICAL,POINTER :: LPERCENTILS
REAL(KIND=JPRB),POINTER :: RVFE_ALPHA
REAL(KIND=JPRB),POINTER :: RVFE_BETA
REAL(KIND=JPRB),POINTER :: RVFE_KNOT_STRETCH
LOGICAL,POINTER :: LVFE_ECMWF
LOGICAL,POINTER :: LVFE_NOBC
LOGICAL,POINTER :: LVFE_VERBOSE
LOGICAL,POINTER :: LVFE_NORMALIZE
LOGICAL,POINTER :: LDYN_ANALYSIS_STABILITY
LOGICAL,POINTER :: LVFE_COMPATIBLE
LOGICAL,POINTER :: LVFD_COMPATIBLE
LOGICAL,POINTER :: LVFE_FD_MIX
LOGICAL,POINTER :: LVFE_SILAPL
LOGICAL,POINTER :: LREFINE_NHPRE_BBC
REAL(KIND=JPRB),POINTER :: RFAC1, RFAC2
CHARACTER(LEN=8),POINTER :: CVFE_ETAH

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "namcver.nam.h"

! =============================================================================

#include "abor1.intfb.h"
#include "posnam.intfb.h"

!      ----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('YOMCVER:SUCVER',0,ZHOOK_HANDLE)
!      ----------------------------------------------------------------

!*       1.    SET DEFAULT VALUES.
!
!              -------------------
LAPRXPK=>YDCVER%LAPRXPK
RHYDR0=>YDCVER%RHYDR0
NDLNPR=>YDCVER%NDLNPR
LREGETA=>YDCVER%LREGETA
LVFE_REGETA=>YDCVER%LVFE_REGETA
NVFE_TYPE=>YDCVER%NVFE_TYPE
NVFE_ORDER=>YDCVER%NVFE_ORDER
NVFE_INTERNALS=>YDCVER%NVFE_INTERNALS
LVERTFE=>YDCVER%LVERTFE
LVFE_LAPL_BC=>YDCVER%LVFE_LAPL_BC
LVFE_GW=>YDCVER%LVFE_GW
LVFE_GW_HALF=>YDCVER%LVFE_GW_HALF
LVFE_GWMPA=>YDCVER%LVFE_GWMPA
LVFE_CHEB=>YDCVER%LVFE_CHEB
LPERCENTILS=>YDCVER%LPERCENTILS
RVFE_ALPHA=>YDCVER%RVFE_ALPHA
RVFE_BETA=>YDCVER%RVFE_BETA
RVFE_KNOT_STRETCH=>YDCVER%RVFE_KNOT_STRETCH
LVFE_ECMWF=>YDCVER%LVFE_ECMWF
LVFE_NOBC=>YDCVER%LVFE_NOBC
LVFE_VERBOSE=>YDCVER%LVFE_VERBOSE
LVFE_NORMALIZE=>YDCVER%LVFE_NORMALIZE
LDYN_ANALYSIS_STABILITY=>YDCVER%LDYN_ANALYSIS_STABILITY
LVFE_COMPATIBLE=>YDCVER%LVFE_COMPATIBLE
LVFD_COMPATIBLE=>YDCVER%LVFD_COMPATIBLE
LVFE_FD_MIX=>YDCVER%LVFE_FD_MIX
LVFE_SILAPL=>YDCVER%LVFE_SILAPL
LREFINE_NHPRE_BBC=>YDCVER%LREFINE_NHPRE_BBC
RFAC1=>YDCVER%RFAC1
RFAC2=>YDCVER%RFAC2
CVFE_ETAH=>YDCVER%CVFE_ETAH

NDLNPR=0
LREGETA=.FALSE.
LVFE_REGETA=.FALSE.
IF (LECMWF .AND. .NOT. LARPEGEF) THEN
  LAPRXPK=.TRUE.
  RHYDR0=LOG(2._JPRB)
  IF(.NOT.LDNHDYN_GEOM .AND. .NOT.LR2D) THEN
    ! Assume semi-lagrangien to avoid dependency on model/MH
      LVERTFE=.TRUE.
      NVFE_TYPE=3
  ELSE
    LVERTFE=.FALSE.
    NVFE_TYPE=0
  ENDIF
ELSE
  LAPRXPK=.FALSE.
  RHYDR0=1._JPRB
  LVERTFE=.FALSE.
  NVFE_TYPE=0
ENDIF
NVFE_ORDER=NVFE_TYPE+1

NVFE_INTERNALS=0
LDYN_ANALYSIS_STABILITY = .FALSE.
LVFE_NORMALIZE = .FALSE.
LVFE_LAPL_BC=.FALSE.
LVFE_GW=.FALSE.

LVFE_CHEB=.FALSE.
LPERCENTILS=.FALSE.
RVFE_ALPHA=0.0_JPRB
RVFE_BETA =0.0_JPRB
RVFE_KNOT_STRETCH=1.0_JPRB
LVFE_ECMWF=.NOT.(LECMWF.OR.LDNHDYN_GEOM)
LVFE_GW_HALF=.FALSE.
LVFE_GWMPA=.FALSE.
LVFE_NOBC=.FALSE.
LVFE_VERBOSE=.FALSE.
LVFE_COMPATIBLE=.FALSE.
LVFE_FD_MIX=.FALSE.
LVFE_SILAPL=.FALSE.
LVFD_COMPATIBLE=.FALSE.
LREFINE_NHPRE_BBC=.FALSE.
RFAC1=0.0_JPRB
RFAC2=0.0_JPRB
CVFE_ETAH='CHORDAL'

!      ----------------------------------------------------------------
!*       2.    Modifies default values.
!              -----------------------

CALL POSNAM(NULNAM,'NAMCVER')
READ(NULNAM,NAMCVER)

!     ------------------------------------------------------------------

!*       3.    Reset variables and test.
!              -------------------------

! * (LAPRXPK,NDLNPR) reset to (T,0) if LVERTFE=T
IF (LVERTFE) THEN
  LAPRXPK=.TRUE.
  WRITE(UNIT=NULOUT,FMT='('' SUCVER_GEOM: VFE => LAPRXPK reset to TRUE '')')
  NDLNPR=0
  WRITE(UNIT=NULOUT,FMT='('' SUCVER_GEOM: VFE => NDLNPR reset to 0 '')')
ENDIF

IF(.NOT.LVERTFE) THEN
  NVFE_TYPE=0
ENDIF
NVFE_ORDER=NVFE_TYPE+1

! * Reset the LVFE_... keys to F if LVERTFE=F.
LVFE_LAPL_BC=(LVERTFE.AND.LDNHDYN_GEOM).AND.LVFE_LAPL_BC
LVFE_GW=LVERTFE.AND.LVFE_GW
LVFE_CHEB=LVERTFE.AND.LVFE_CHEB
LVFE_GW_HALF=LVERTFE.AND.LVFE_GW_HALF
LVFE_GWMPA=LVERTFE.AND.LVFE_GWMPA
LVFE_NOBC=LVERTFE.AND.LVFE_NOBC
LVFE_VERBOSE=LVERTFE.AND.LVFE_VERBOSE
LVFE_COMPATIBLE=LVERTFE.AND.LVFE_COMPATIBLE
LVFE_FD_MIX=LVERTFE.AND.LVFE_FD_MIX
LVFD_COMPATIBLE=(.NOT.LVERTFE).AND.LVFD_COMPATIBLE
LVFE_SILAPL=LVERTFE.AND.LVFE_SILAPL
LREFINE_NHPRE_BBC=LREFINE_NHPRE_BBC.AND.(LVFE_COMPATIBLE.OR.LVFD_COMPATIBLE)


! * Reset LVFE_ECMWF to .F. if LNHDYN
IF (LVERTFE .AND. (.NOT.LVFE_COMPATIBLE) .AND. LDNHDYN_GEOM) THEN
  IF (NVFE_TYPE==1) THEN
    CALL ABOR1(' SUCVER_GEOM: LVERTFE.AND.NVFE_TYPE=1 in NH model not possible.')
  ENDIF
  IF (LVFE_ECMWF) THEN
    WRITE(NULOUT,*) ' SUCVER_GEOM: LVERTFE in NH model => LACE operators; LVFE_ECMWF set to FALSE.'
    LVFE_ECMWF=.FALSE.
  ELSE
    WRITE(NULOUT,*) ' SUCVER_GEOM: LVERTFE in NH model => LACE operators.'
  ENDIF
ENDIF

IF (LVFE_COMPATIBLE .AND. LDNHDYN_GEOM) THEN
  IF(LVFE_ECMWF)THEN
    WRITE(NULOUT,*) ' SUCVER_GEOM: LVERTFE compatible in NH model => ECMWF operators'
  ELSE
    WRITE(NULOUT,*) ' SUCVER_GEOM: LVERTFE compatible in NH model => LACE operators'
  ENDIF
ENDIF

! * Reset LVFE_ECMWF to .T. if .NOT.LDNHDYN_GEOM
IF (LVERTFE.AND..NOT.LDNHDYN_GEOM) THEN
  IF(LVFE_ECMWF)THEN
    WRITE(NULOUT,*) ' SUCVER_GEOM: LVERTFE in hydrostatic model => ECMWF operators'
  ELSE
    WRITE(NULOUT,*) ' SUCVER_GEOM: LVERTFE in hydrostatic model => LACE operators'
  ENDIF
ENDIF

! * Reset LVFE_GW to .F. and LVFE_GW_HALF to .T. if LVFE_COMPATIBLE and LNHDYN 
IF (LVFE_COMPATIBLE.AND.LDNHDYN_GEOM) THEN
  LVFE_GW=.FALSE.
  LVFE_GW_HALF=.TRUE.
  LVFE_GWMPA=.FALSE.
  WRITE(NULOUT,*) ' SUCVER: LVFE_COMPATIBLE in NH model => LVFE_GW set to FALSE.'
ENDIF

! * Only one from LVFE_COMPATIBLE and LVFE_SILAPL may be true.
IF( LVERTFE .AND. LVFE_COMPATIBLE .AND. LVFE_SILAPL ) THEN
  CALL ABOR1(' SUCVER_GEOM: LVFE_COMPATIBLE and LVFE_SILAPL can not be both set to TRUE')
ENDIF


! Only ECMWF operators available without boundary conditions.
IF (LVFE_NOBC.AND..NOT.LVFE_ECMWF) THEN
  LVFE_NOBC=.FALSE.
  WRITE(NULOUT,*) ' SUCVER_GEOM: LACE operators in VFE => LVFE_NOBC set to FALSE.'
ENDIF

! * Only one from LVFE_GW and LVFE_GW_HALF may be true.
IF( LVERTFE .AND. LVFE_GW .AND. LVFE_GW_HALF ) THEN
  CALL ABOR1(' SUCVER_GEOM: LVFE_GW and LVFE_GW_HALF can not be both set to TRUE')
ENDIF

! * Bound RVFE_ALPHA between <0, 1>
IF (CVFE_ETAH=='MCONST') THEN
  RVFE_ALPHA=MAX(0._JPRB,MIN(1._JPRB,RVFE_ALPHA))
  RVFE_BETA =MAX(0._JPRB,MIN(1._JPRB,RVFE_BETA))
  WRITE(NULOUT,*) ' SUCVER_GEOM: CVFE_ETAH=MCONST, 0< RVFE_ALPHA/BETA <1.'
ELSEIF (.NOT.(CVFE_ETAH == "CHORDAL".OR.CVFE_ETAH == "REGETA".OR.CVFE_ETAH == "MCNTRL")) THEN
  WRITE(NULOUT,*) " SUCVER_GEOM: CVFE_ETAH '",CVFE_ETAH,"'"
  CALL ABOR1(" SUCVER: unknown option for CVFE_ETAH")
ENDIF

RVFE_KNOT_STRETCH=MAX(1._JPRB,RVFE_KNOT_STRETCH)

! * NVFE_TYPE must be >=1.
IF( LVERTFE .AND. NVFE_TYPE < 1 ) THEN
  CALL ABOR1(' SUCVER_GEOM: for VFE NVFE_TYPE must be >= 1')
ENDIF


!* Use of LREFINE_NHPRE_BBC options with improper options for vertical scheme? 
IF (LREFINE_NHPRE_BBC .AND. LVERTFE .AND. .NOT.LVFE_COMPATIBLE) THEN
  WRITE(NULOUT,'(1X,A,I3,A)') ' ! SUCVER_GEOM: WARNING ',' !!!'
  CALL ABOR1( ' LREFINE_NHPRE_SURF=.T. and LVERTFE=.T. requires LVFE_COMPATIBLE=.T.')
ENDIF
IF (LREFINE_NHPRE_BBC .AND. .NOT.(LVERTFE .OR. LVFD_COMPATIBLE)) THEN
  WRITE(NULOUT,'(1X,A,I3,A)') ' ! SUCVER_GEOM: WARNING ',' !!!'
  CALL ABOR1('LREFINE_NHPRE_SURF=.T. and LVERTFE=.F. requires LVFD_COMPATIBLE=.T.')
ENDIF


!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('YOMCVER:SUCVER',1,ZHOOK_HANDLE)
END SUBROUTINE SUCVER_GEOM

SUBROUTINE PRT_CVER_GEOM(YDCVER)

!**** *PRT_CVER*   - Prints YOMCVER/NAMCVER keys 
!      ----------------------------------------------------------------
IMPLICIT NONE
TYPE(TCVER),INTENT(INOUT) :: YDCVER
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!      ----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('YOMCVER:PRT_CVER_GEOM',0,ZHOOK_HANDLE)
!      ----------------------------------------------------------------

WRITE(UNIT=NULOUT,FMT='('' '')')
WRITE(UNIT=NULOUT,FMT='('' Printings of YOMCVER/NAMCVER variables '')')
WRITE(UNIT=NULOUT,FMT='('' LAPRXPK = '',L2,'' NDLNPR = '',I2,'' RHYDR0 = '',E10.4)') &
 & YDCVER%LAPRXPK,YDCVER%NDLNPR,YDCVER%RHYDR0
WRITE(UNIT=NULOUT,FMT='('' LREGETA = '',L2)') YDCVER%LREGETA
WRITE(UNIT=NULOUT,FMT='('' LVFE_REGETA = '',L2)') YDCVER%LVFE_REGETA
WRITE(UNIT=NULOUT,FMT='('' LVERTFE= '',L2,'' NVFE_TYPE= '',I2)') YDCVER%LVERTFE,YDCVER%NVFE_TYPE
WRITE(UNIT=NULOUT,FMT='('' CVFE_ETAH= '',A)') YDCVER%CVFE_ETAH
WRITE(UNIT=NULOUT,FMT='('' RVFE_ALPHA= '',F20.14,'' RVFE_BETA= '',F20.14)') YDCVER%RVFE_ALPHA, YDCVER%RVFE_BETA
WRITE(UNIT=NULOUT,FMT='('' NVFE_ORDER= '',I2)') YDCVER%NVFE_ORDER
WRITE(UNIT=NULOUT,FMT='('' LVFE_LAPL_BC = '',L2)') YDCVER%LVFE_LAPL_BC
WRITE(UNIT=NULOUT,FMT='('' LVFE_GW = '',L2)') YDCVER%LVFE_GW
WRITE(UNIT=NULOUT,FMT='('' LVFE_GW_HALF = '',L2)') YDCVER%LVFE_GW_HALF
WRITE(UNIT=NULOUT,FMT='('' LVFE_GWMPA = '',L2)') YDCVER%LVFE_GWMPA
WRITE(UNIT=NULOUT,FMT='('' LVFE_ECMWF = '',L2)') YDCVER%LVFE_ECMWF
WRITE(UNIT=NULOUT,FMT='('' LVFE_NOBC= '',L2)') YDCVER%LVFE_NOBC
WRITE(UNIT=NULOUT,FMT='('' LVFE_CHEB = '',L2)') YDCVER%LVFE_CHEB
WRITE(UNIT=NULOUT,FMT='('' LVFE_VERBOSE = '',L2)') YDCVER%LVFE_VERBOSE
WRITE(UNIT=NULOUT,FMT='('' RVFE_KNOT_STRETCH = '',F20.14)') YDCVER%RVFE_KNOT_STRETCH
WRITE(UNIT=NULOUT,FMT='('' LVFE_NORMALIZE = '',L2)') YDCVER%LVFE_NORMALIZE
WRITE(UNIT=NULOUT,FMT='('' LDYN_ANALYSIS_STABILITY = '',L2)') YDCVER%LDYN_ANALYSIS_STABILITY
WRITE(UNIT=NULOUT,FMT='('' LVFD_COMPATIBLE = '',L2)') YDCVER%LVFD_COMPATIBLE
WRITE(UNIT=NULOUT,FMT='('' LVFE_COMPATIBLE = '',L2)') YDCVER%LVFE_COMPATIBLE
WRITE(UNIT=NULOUT,FMT='('' LVFE_FD_MIX = '',L2)') YDCVER%LVFE_FD_MIX
WRITE(UNIT=NULOUT,FMT='('' LVFE_SILAPL = '',L2)') YDCVER%LVFE_SILAPL
WRITE(UNIT=NULOUT,FMT='('' LREFINE_NHPRE_BBC = '',L2)') YDCVER%LREFINE_NHPRE_BBC
WRITE(UNIT=NULOUT,FMT='('' RFAC1 = '',F20.14)') YDCVER%RFAC1
WRITE(UNIT=NULOUT,FMT='('' RFAC2 = '',F20.14)') YDCVER%RFAC2

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('YOMCVER:PRT_CVER_GEOM',1,ZHOOK_HANDLE)
END SUBROUTINE PRT_CVER_GEOM

! =============================================================================
END MODULE YOMCVER

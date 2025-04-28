SUBROUTINE VERDISINT(YDVFE,YDCVER,CDOPER,CDBC,KPROMA,KST,KEND,KFLEV,PIN,POUT,POUTS,PINS,&
 & KCHUNK,KLOUT)

!**** *VERDISINT*   VERtical DIScretization -
!                INTerface for finite element type vertical operations:
!                derivative or integral

!     Purpose.
!     --------
!          This subroutine prepares an interface to VERINT
!          computing vertical integral with respect to eta
!          and VERDER computing vertical derivative with 
!          respect to eta of a function given at full or 
!          half model levels using a general scheme.

!**   Interface.
!     ----------
!        *CALL* *VERDISINT(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          CDOPER    - type of integral or derivative applied:
!                      'ITOP' - integral from top
!                      'IBOT' - integral from bottom
!                      'INGW' - invertible integral operator
!                      'HDER' - first derivative at half levels
!                      'FDER' - first derivative on full levels
!                      'DDER' - second derivative on full levels
!                      'DEGW' - invertible derivative operator
!          CDBC      - boundary conditions used ('00','01','10','11', 
!                      first digit for top, second digit for bottom,
!                      0 for value prescribed, 1 for derivative prescribed)
!          KPROMA    - horizontal dimension.
!          KST       - first element of work.
!          KEND      - depth of work.
!          KFLEV     - vertical dimension for array PIN.
!          PIN       - 3D input field
!        OPTIONAL INPUT:
!          PINS      - "surface component" (phis, gws,...) to be added internally to POUT
!          KCHUNK    - Use NPROMA as blocking factor when called outside 
!                      OpenMP threaded region
!          KLOUT     - number of levels to work on (integration only)
!                      if present, should be ILEVOUT-1 (ie NFLEVG), avoiding full integral
!                      to be computed (ie useless supplementary level of POUT)

!        OPTIONAL OUTPUT:
!          POUT      - 3D integral or derivative of PIN according to CDOPER
!          POUTS     - the vertical integral (replace the old supplementary level of POUT)

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        Matrix multiply computing 3D integrals/derivatives + full integral:
!        - 3D int/der: POUT = ZOPER*t(ZIN) [+PINS]
!        - 2D full int: POUTS = ZOPER(N,:)*ZIN(:,N)
!        Both operations are optional
!        Adding the "surface component" (PINS) is also optional

!     Externals.
!     ----------
!        none

!     Reference.
!     ----------

!     Author.
!     -------
!        P. Smolikova (CHMI/LACE/ALADIN)

!     Modifications.
!     --------------
!        Original : Sep 2017
!        P.Smolikova (Sep 2020): VFE pruning.
!        H. Petithomme (July 2023): new options POUTS/PINS (ie integral and surface)
!     ------------------------------------------------------------------

USE PARKIND1,ONLY : JPIM, JPRB, JPRD
USE YOMCVER ,ONLY : TCVER
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMLUN  ,ONLY : NULERR
USE YOMVERT ,ONLY : TVFE
USE OML_MOD ,ONLY : OML_MAX_THREADS

!     ---------------------------------------------------------------------------

IMPLICIT NONE
TYPE(TVFE),TARGET ,INTENT(IN) :: YDVFE
TYPE(TCVER)       ,INTENT(IN) :: YDCVER
CHARACTER(LEN=*)  ,INTENT(IN) :: CDOPER
CHARACTER(LEN=2)  ,INTENT(IN) :: CDBC
INTEGER(KIND=JPIM),INTENT(IN) :: KPROMA, KST, KEND, KFLEV
INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN) :: KCHUNK,KLOUT
REAL(KIND=JPRB)   ,INTENT(IN) :: PIN(KPROMA,0:KFLEV+1)
REAL(KIND=JPRB),OPTIONAL,INTENT(OUT):: POUT(KPROMA,*),POUTS(KPROMA)
REAL(KIND=JPRB),OPTIONAL,INTENT(IN) :: PINS(KPROMA)

!     ------------------------------------------------------------------

LOGICAL :: LLVERINT_ON_CPU

CHARACTER(LEN=2)   :: CLBC
INTEGER(KIND=JPIM) :: ILEVIN, ILEVOUT, IND, IEND, ITYPE, JCHUNK
REAL(KIND=JPRD),CONTIGUOUS,POINTER :: ZOPER(:,:)
REAL(KIND=JPRD) :: ZOUTS(KPROMA)
INTEGER(KIND=JPIM) :: IMAXTHREADS
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "verder.intfb.h"
#include "verint.intfb.h"
#include "verints.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('VERDISINT',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

IF (LLVERINT_ON_CPU) THEN
  IMAXTHREADS = OML_MAX_THREADS ()
ELSE
  IMAXTHREADS = 1
ENDIF

!*   1. Initialization and control
!    -----------------------------

IF (YDCVER%LVFE_ECMWF) THEN
  IF (CDOPER(1:4) /= 'FDER'.OR.YDCVER%LVFE_NOBC) THEN
    CLBC='XX' ! no BC applied for derivatives
  ELSE
    CLBC='BC' ! BC according to Hortal applied on derivatives
  ENDIF
ELSE
  CLBC=CDBC
ENDIF


!*   2. Set operator size according to boundary conditions applied
!    --------------------------------------------------------------

IF (CDOPER(1:4)=='FDER'.OR.CDOPER(1:4)=='DDER') THEN
  ILEVOUT = KFLEV
ELSE
  ILEVOUT = KFLEV+1
ENDIF

IF (CDOPER(1:4)=='IBOT') THEN
  ITYPE=1
ELSE
  ITYPE=0
ENDIF

IF (CDOPER(1:4)=='INGW'.OR.CDOPER(1:4)=='DEGW') THEN
  IND=0
  ILEVIN = KFLEV+1
ELSEIF (CLBC=='XX'.OR.CDOPER=='INTG') THEN
  IND=1
  ILEVIN = KFLEV
ELSE
  IND=0
  ILEVIN = KFLEV+2
ENDIF
IEND=ILEVIN-1+IND

!*   3. Set operator according to boundary conditions applied
!    --------------------------------------------------------

!*   3.1 Vertical integral
!    ---------------------
IF (CDOPER(1:4)=='INGW') THEN
  write(0,*) "oper INWG",present(KLOUT),present(PINS)
  ZOPER => YDVFE%RINTGW(:,:)
ELSEIF (ANY(CDOPER(1:4) == (/'ITOP','IBOT'/))) THEN
  IF (CLBC=='XX') THEN
    ZOPER => YDVFE%RINTE(:,:)
  ELSEIF (CLBC=='00') THEN
    ZOPER => YDVFE%RINTBF00(:,:)
  ELSEIF (CLBC=='11') THEN
    ZOPER => YDVFE%RINTBF11(:,:)
  ELSE
    WRITE(NULERR,*) 'VERDISINT: ITOP/IBOT NOT IMPLEMENTED FOR CDBC=',CLBC
    CALL ABOR1(' VERDISINT: ABOR1 CALLED')
  ENDIF
ELSEIF (CDOPER=='INTG') THEN
  ZOPER => YDVFE%RINTG(:,:)
ENDIF

!*   3.2 Vertical derivative
!    -----------------------

IF (CDOPER(1:4)=='DEGW') THEN
  ZOPER => YDVFE%RDERGW(:,:)
ELSEIF (CDOPER(1:4)=='HDER') THEN
  IF (CLBC=='00') THEN
    ZOPER => YDVFE%RDERBH00(:,:)
  ELSEIF (CLBC=='01') THEN
    ZOPER => YDVFE%RDERBH01(:,:)
  ELSE
    WRITE(NULERR,*) 'VERDISINT: HDER NOT IMPLEMENTED FOR CDBC=',CLBC
    CALL ABOR1(' VERDISINT: ABOR1 CALLED')
  ENDIF
ELSEIF (CDOPER(1:4)=='FDER') THEN
  IF (CLBC=='XX') THEN
    ZOPER => YDVFE%RDERI(:,:)
  ELSEIF (CLBC=='BC') THEN
    ZOPER => YDVFE%RDERB(:,:)
  ELSEIF (CLBC=='00') THEN
    ZOPER => YDVFE%RDERBF00(:,:)
  ELSEIF (CLBC=='01') THEN
    ZOPER => YDVFE%RDERBF01(:,:)
  ELSEIF (CLBC=='10') THEN
    ZOPER => YDVFE%RDERBF10(:,:)
  ELSEIF (CLBC=='11') THEN
    ZOPER => YDVFE%RDERBF11(:,:)
  ELSE
    WRITE(NULERR,*) 'VERDISINT: FDER NOT IMPLEMENTED FOR CDBC=',CLBC
    CALL ABOR1(' VERDISINT: ABOR1 CALLED')
  ENDIF
ELSEIF (CDOPER(1:4)=='DDER') THEN
  IF (CLBC=='XX') THEN
    ZOPER => YDVFE%RDDERI(:,:)
  ELSEIF (CLBC=='01') THEN
    ZOPER => YDVFE%RDDERBF01(:,:)
  ELSE
    WRITE(NULERR,*) 'VERDISINT: DDER NOT IMPLEMENTED FOR CDBC=',CLBC
    CALL ABOR1(' VERDISINT: ABOR1 CALLED')
  ENDIF
ENDIF

!*   4. Apply the required operation
!    --------------------------------

IF (ANY(CDOPER(1:4) == (/'INGW','ITOP','IBOT'/))) THEN
  IF(PRESENT(KCHUNK)) THEN
    JCHUNK=KCHUNK
  ELSE
    JCHUNK=1+(KEND-KST)/IMAXTHREADS
  ENDIF

  ! note: mind all the various cases
  ! - no POUT: POUTS is present => only integral
  ! - POUT and POUTS present: 3D (POUT) + integral (POUTS)
  ! - type 1 (and no POUTS): use ZOUTS, no integral (meaningless)
  ! - no KLOUT (and no POUTS, type 0): "old" style => 3D integral + full integral in POUT
  ! - KLOUT present or ILEVOUT=KFLEV: "new" style => 3D integral in POUT, no full integral

  IF (.NOT.PRESENT(POUT)) THEN
    ! integral only: from top only, kflev+1 levels required in zoper
    IF (.NOT.PRESENT(POUTS)) CALL ABOR1("ERROR: NEEDS AT LEAST 1 OF POUT/POUTS")
    IF (ITYPE == 1) CALL ABOR1("ERROR POUTS: ITYPE=1 NOT POSSIBLE")
    IF (ILEVOUT == KFLEV) CALL ABOR1("ERROR POUTS: DERIVATIVE NOT POSSIBLE")

    CALL VERINTS(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),ZOUTS)
    POUTS(:) = ZOUTS(:)
  ELSE IF (PRESENT(POUTS)) THEN
    IF (ITYPE == 1) CALL ABOR1("ERROR KFLEV+1: ITYPE=1 NOT POSSIBLE")
    IF (PRESENT(PINS)) CALL ABOR1("ERROR: PINS MEANINGLESS WITH POUTS")

    CALL VERINTS(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),ZOUTS)
    POUTS(:) = ZOUTS(:)

    CALL VERINT(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),POUT,ITYPE,&
      &JCHUNK,ZOUTS)
  ELSE IF (ITYPE == 1) THEN
    ! no copy needed
    CALL VERINTS(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),ZOUTS)

    CALL VERINT(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),POUT,ITYPE,&
      &JCHUNK,ZOUTS,PINS)
  ELSE IF (ILEVOUT == KFLEV+1.AND..NOT.PRESENT(KLOUT)) THEN
    IF (PRESENT(PINS)) CALL ABOR1("ERROR: PINS MEANINGLESS WITH POUTS")

    CALL VERINTS(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),ZOUTS)
    POUT(:,ILEVOUT) = ZOUTS(:)

    CALL VERINT(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),POUT,ITYPE,&
      &JCHUNK,ZOUTS,PINS)
  ELSE
    CALL VERINT(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),POUT,ITYPE,&
      &JCHUNK,PINS=PINS)
  ENDIF
ELSEIF (ANY(CDOPER(1:4) == (/'FDER','HDER','DEGW','DDER'/))) THEN
  CALL VERDER(KPROMA,KST,KEND,ILEVIN,ILEVOUT,ZOPER,PIN(:,IND:IEND),POUT)
ELSE
  WRITE(NULERR,*) 'VERDISINT: UNKNOWN CDOPER=', CDOPER
  CALL ABOR1(' VERDISINT: ABOR1 CALLED')
ENDIF

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('VERDISINT',1,ZHOOK_HANDLE)
END SUBROUTINE VERDISINT

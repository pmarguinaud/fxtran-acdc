MODULE YEMLAP

!$ACDC methods 


USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    Constants related to the Laplace space

!  NCPL2M :  number of complex Laplace coefficient for m given (local)
!  NCPL4M :  number of real    Laplace coefficient for m given (local)
!  NCPLM  :  number of complex real/imag. Laplace coefficient for m given
!            (half of NCPL2M)
!  NCPL2N :  number of complex Laplace coefficient for n given
!  NCPL4N :  number of real    Laplace coefficient for n given
!  NCPLN  :  number of complex real/imag. coefficient for n given
!            (half of NCPL2N)

!  RLEPDIN:  eigen-values of the Laplace operator
!  RLEPINN:  eigen-values of its inverse
!  RLEPDIM:  eigen-values of the Laplace operator
!  RLEPINM:  eigen-values of its inverse

!  NESM0 : address (local) in a spectral array of (n, m=0)
!  NESM0G: address (global) in a spectral array of (n, m=0)
!  NESPZERO: global adresses in the (global-dimensioned) spectral arrays
!            of those coefficients which are always zero, id est:
!            m = 0 - coefficients 3 and 4 for every quadruplet (1,2,3,4)
!            n = 0 - coefficients 2 and 4 for every quadruplet (1,2,3,4)

!  NPME  : address for the Laplace operator and its inverse
!  NPNE  : address for the Laplace operator and its inverse
!  MVALUE: wavenumber M for the given spectral coefficient

TYPE :: TLEP
  INTEGER(KIND=JPIM),ALLOCATABLE:: NCPL2M(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NCPL4M(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NCPLM(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NCPL2N(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NCPL4N(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NCPLN(:)

  REAL(KIND=JPRB),ALLOCATABLE:: RLEPDIN(:)
  REAL(KIND=JPRB),ALLOCATABLE:: RLEPINN(:)
  REAL(KIND=JPRB),ALLOCATABLE:: RLEPDIM(:)
  REAL(KIND=JPRB),ALLOCATABLE:: RLEPINM(:)

  INTEGER(KIND=JPIM),ALLOCATABLE:: NESM0(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NESPZERO(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NESM0G(:)

  INTEGER(KIND=JPIM),ALLOCATABLE:: NPME(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: NPNE(:)
  INTEGER(KIND=JPIM),ALLOCATABLE:: MVALUE(:)
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TLEP
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TLEP
PROCEDURE :: ACDC_HOST => ACDC_HOST_TLEP
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TLEP
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TLEP
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TLEP
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TLEP
END TYPE TLEP

!     ------------------------------------------------------------------
INTERFACE

MODULE SUBROUTINE ACDC_COPY_TLEP (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TLEP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TLEP (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TLEP), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TLEP (SELF)

IMPLICIT NONE
CLASS (TLEP), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TLEP (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TLEP), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TLEP (SELF, KLUN)

IMPLICIT NONE
CLASS (TLEP), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TLEP (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TLEP),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TLEP (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TLEP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE YEMLAP

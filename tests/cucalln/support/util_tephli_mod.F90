MODULE UTIL_TEPHLI_MOD

USE YOEPHLI, ONLY : TEPHLI

INTERFACE SAVE
MODULE PROCEDURE SAVE_TEPHLI
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TEPHLI
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TEPHLI
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TEPHLI
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TEPHLI (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TEPHLI), INTENT (IN), TARGET :: YD

WRITE (KLUN) YD%LTLEVOL
WRITE (KLUN) YD%LPHYLIN
WRITE (KLUN) YD%LENOPERT
WRITE (KLUN) YD%LEPPCFLS
WRITE (KLUN) YD%LRAISANEN
WRITE (KLUN) YD%LOPPTWINS
WRITE (KLUN) YD%RLPTRC
WRITE (KLUN) YD%RLPAL1
WRITE (KLUN) YD%RLPAL2
WRITE (KLUN) YD%RLPBB
WRITE (KLUN) YD%RLPCC
WRITE (KLUN) YD%RLPDD
WRITE (KLUN) YD%RLPMIXL
WRITE (KLUN) YD%RLPBETA
WRITE (KLUN) YD%RLPDRAG
WRITE (KLUN) YD%RLPEVAP
WRITE (KLUN) YD%RLPP00
END SUBROUTINE

SUBROUTINE LOAD_TEPHLI (KLUN, YD)
USE PARKIND1, ONLY : JPRD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TEPHLI), INTENT (OUT), TARGET :: YD
REAL(KIND=JPRD) :: ZTMP0
READ (KLUN) YD%LTLEVOL
READ (KLUN) YD%LPHYLIN
READ (KLUN) YD%LENOPERT
READ (KLUN) YD%LEPPCFLS
READ (KLUN) YD%LRAISANEN
READ (KLUN) YD%LOPPTWINS
READ (KLUN) ZTMP0
YD%RLPTRC = ZTMP0
READ (KLUN) ZTMP0
YD%RLPAL1 = ZTMP0
READ (KLUN) ZTMP0
YD%RLPAL2 = ZTMP0
READ (KLUN) ZTMP0
YD%RLPBB = ZTMP0
READ (KLUN) ZTMP0
YD%RLPCC = ZTMP0
READ (KLUN) ZTMP0
YD%RLPDD = ZTMP0
READ (KLUN) ZTMP0
YD%RLPMIXL = ZTMP0
READ (KLUN) ZTMP0
YD%RLPBETA = ZTMP0
READ (KLUN) ZTMP0
YD%RLPDRAG = ZTMP0
READ (KLUN) ZTMP0
YD%RLPEVAP = ZTMP0
READ (KLUN) ZTMP0
YD%RLPP00 = ZTMP0
END SUBROUTINE


SUBROUTINE COPY_TEPHLI (YD, LDCREATED)

IMPLICIT NONE
TYPE (TEPHLI), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF

















END SUBROUTINE

SUBROUTINE WIPE_TEPHLI (YD, LDDELETED)

IMPLICIT NONE
TYPE (TEPHLI), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED


















LLDELETED = .FALSE.
IF (PRESENT (LDDELETED)) THEN
  LLDELETED = LDDELETED
ENDIF
IF (.NOT. LLDELETED) THEN
  !$acc exit data delete (YD)
ENDIF
END SUBROUTINE



END MODULE

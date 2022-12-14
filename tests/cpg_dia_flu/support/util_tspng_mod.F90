MODULE UTIL_TSPNG_MOD

USE SPNG_MOD, ONLY : TSPNG

INTERFACE SAVE
MODULE PROCEDURE SAVE_TSPNG
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TSPNG
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TSPNG
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TSPNG
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TSPNG (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TSPNG), INTENT (IN), TARGET :: YD
LOGICAL :: LRSPONGF
WRITE (KLUN) YD%LNSPONGE
LRSPONGF = ALLOCATED (YD%RSPONGF)
WRITE (KLUN) LRSPONGF
IF (LRSPONGF) THEN
  WRITE (KLUN) LBOUND (YD%RSPONGF)
  WRITE (KLUN) UBOUND (YD%RSPONGF)
  WRITE (KLUN) YD%RSPONGF
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_TSPNG (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TSPNG), INTENT (OUT), TARGET :: YD
INTEGER :: IL2(2), IU2(2)
LOGICAL :: LRSPONGF
READ (KLUN) YD%LNSPONGE
READ (KLUN) LRSPONGF
IF (LRSPONGF) THEN
  READ (KLUN) IL2
  READ (KLUN) IU2
  ALLOCATE (YD%RSPONGF (IL2(1):IU2(1), IL2(2):IU2(2)))
  READ (KLUN) YD%RSPONGF
ENDIF
END SUBROUTINE


SUBROUTINE COPY_TSPNG (YD, LDCREATED)

IMPLICIT NONE
TYPE (TSPNG), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LRSPONGF

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF

LRSPONGF = ALLOCATED (YD%RSPONGF)
IF (LRSPONGF) THEN
  !$acc enter data create (YD%RSPONGF)
  !$acc update device (YD%RSPONGF)
  !$acc enter data attach (YD%RSPONGF)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_TSPNG (YD, LDDELETED)

IMPLICIT NONE
TYPE (TSPNG), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LRSPONGF


LRSPONGF = ALLOCATED (YD%RSPONGF)
IF (LRSPONGF) THEN
  !$acc exit data detach (YD%RSPONGF)
  !$acc exit data delete (YD%RSPONGF)
ENDIF

LLDELETED = .FALSE.
IF (PRESENT (LDDELETED)) THEN
  LLDELETED = LDDELETED
ENDIF
IF (.NOT. LLDELETED) THEN
  !$acc exit data delete (YD)
ENDIF
END SUBROUTINE



END MODULE

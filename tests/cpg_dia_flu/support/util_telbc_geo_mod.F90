MODULE UTIL_TELBC_GEO_MOD

USE YEMLBC_GEO, ONLY : TELBC_GEO

INTERFACE SAVE
MODULE PROCEDURE SAVE_TELBC_GEO
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TELBC_GEO
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TELBC_GEO
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TELBC_GEO
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TELBC_GEO (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TELBC_GEO), INTENT (IN), TARGET :: YD
LOGICAL :: LMPTRCPLBLK, LNIND_LEN, LNIND_LIST
LNIND_LIST = ALLOCATED (YD%NIND_LIST)
WRITE (KLUN) LNIND_LIST
IF (LNIND_LIST) THEN
  WRITE (KLUN) LBOUND (YD%NIND_LIST)
  WRITE (KLUN) UBOUND (YD%NIND_LIST)
  WRITE (KLUN) YD%NIND_LIST
ENDIF
LNIND_LEN = ALLOCATED (YD%NIND_LEN)
WRITE (KLUN) LNIND_LEN
IF (LNIND_LEN) THEN
  WRITE (KLUN) LBOUND (YD%NIND_LEN)
  WRITE (KLUN) UBOUND (YD%NIND_LEN)
  WRITE (KLUN) YD%NIND_LEN
ENDIF
WRITE (KLUN) YD%NCPLBLKS
LMPTRCPLBLK = ALLOCATED (YD%MPTRCPLBLK)
WRITE (KLUN) LMPTRCPLBLK
IF (LMPTRCPLBLK) THEN
  WRITE (KLUN) LBOUND (YD%MPTRCPLBLK)
  WRITE (KLUN) UBOUND (YD%MPTRCPLBLK)
  WRITE (KLUN) YD%MPTRCPLBLK
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_TELBC_GEO (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TELBC_GEO), INTENT (OUT), TARGET :: YD
INTEGER :: IL1(1), IU1(1), IL2(2), IU2(2)
LOGICAL :: LMPTRCPLBLK, LNIND_LEN, LNIND_LIST
READ (KLUN) LNIND_LIST
IF (LNIND_LIST) THEN
  READ (KLUN) IL2
  READ (KLUN) IU2
  ALLOCATE (YD%NIND_LIST (IL2(1):IU2(1), IL2(2):IU2(2)))
  READ (KLUN) YD%NIND_LIST
ENDIF
READ (KLUN) LNIND_LEN
IF (LNIND_LEN) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%NIND_LEN (IL1(1):IU1(1)))
  READ (KLUN) YD%NIND_LEN
ENDIF
READ (KLUN) YD%NCPLBLKS
READ (KLUN) LMPTRCPLBLK
IF (LMPTRCPLBLK) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%MPTRCPLBLK (IL1(1):IU1(1)))
  READ (KLUN) YD%MPTRCPLBLK
ENDIF
END SUBROUTINE


SUBROUTINE COPY_TELBC_GEO (YD, LDCREATED)

IMPLICIT NONE
TYPE (TELBC_GEO), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LMPTRCPLBLK, LNIND_LEN, LNIND_LIST

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LNIND_LIST = ALLOCATED (YD%NIND_LIST)
IF (LNIND_LIST) THEN
  !$acc enter data create (YD%NIND_LIST)
  !$acc update device (YD%NIND_LIST)
  !$acc enter data attach (YD%NIND_LIST)
ENDIF

LNIND_LEN = ALLOCATED (YD%NIND_LEN)
IF (LNIND_LEN) THEN
  !$acc enter data create (YD%NIND_LEN)
  !$acc update device (YD%NIND_LEN)
  !$acc enter data attach (YD%NIND_LEN)
ENDIF


LMPTRCPLBLK = ALLOCATED (YD%MPTRCPLBLK)
IF (LMPTRCPLBLK) THEN
  !$acc enter data create (YD%MPTRCPLBLK)
  !$acc update device (YD%MPTRCPLBLK)
  !$acc enter data attach (YD%MPTRCPLBLK)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_TELBC_GEO (YD, LDDELETED)

IMPLICIT NONE
TYPE (TELBC_GEO), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LMPTRCPLBLK, LNIND_LEN, LNIND_LIST

LNIND_LIST = ALLOCATED (YD%NIND_LIST)
IF (LNIND_LIST) THEN
  !$acc exit data detach (YD%NIND_LIST)
  !$acc exit data delete (YD%NIND_LIST)
ENDIF

LNIND_LEN = ALLOCATED (YD%NIND_LEN)
IF (LNIND_LEN) THEN
  !$acc exit data detach (YD%NIND_LEN)
  !$acc exit data delete (YD%NIND_LEN)
ENDIF


LMPTRCPLBLK = ALLOCATED (YD%MPTRCPLBLK)
IF (LMPTRCPLBLK) THEN
  !$acc exit data detach (YD%MPTRCPLBLK)
  !$acc exit data delete (YD%MPTRCPLBLK)
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

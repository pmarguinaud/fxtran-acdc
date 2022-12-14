MODULE UTIL_VARIABLE_4D_MOD

USE VARIABLE_MODULE, ONLY : VARIABLE_4D

INTERFACE SAVE
MODULE PROCEDURE SAVE_VARIABLE_4D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_VARIABLE_4D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_VARIABLE_4D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_VARIABLE_4D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_VARIABLE_4D (KLUN, YD)
USE UTIL_FIELD_MOD
USE UTIL_VARIABLE_BASE_MOD, ONLY : VARIABLE_BASE, SAVE_VARIABLE_BASE
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (VARIABLE_4D), INTENT (IN), TARGET :: YD
CLASS (VARIABLE_BASE), POINTER :: YLSUPER
LOGICAL :: LFDL, LFDL9, LFDM, LFDM9, LFPH9, LFT0, LFT1, LFT9
YLSUPER => YD
CALL SAVE_VARIABLE_BASE (KLUN, YLSUPER)
LFT0 = ASSOCIATED (YD%FT0)
WRITE (KLUN) LFT0
IF (LFT0) THEN
  CALL SAVE (KLUN, YD%FT0)
ENDIF
LFT1 = ASSOCIATED (YD%FT1)
WRITE (KLUN) LFT1
IF (LFT1) THEN
  CALL SAVE (KLUN, YD%FT1)
ENDIF
LFT9 = ASSOCIATED (YD%FT9)
WRITE (KLUN) LFT9
IF (LFT9) THEN
  CALL SAVE (KLUN, YD%FT9)
ENDIF
LFPH9 = ASSOCIATED (YD%FPH9)
WRITE (KLUN) LFPH9
IF (LFPH9) THEN
  CALL SAVE (KLUN, YD%FPH9)
ENDIF
LFDL = ASSOCIATED (YD%FDL)
WRITE (KLUN) LFDL
IF (LFDL) THEN
  CALL SAVE (KLUN, YD%FDL)
ENDIF
LFDM = ASSOCIATED (YD%FDM)
WRITE (KLUN) LFDM
IF (LFDM) THEN
  CALL SAVE (KLUN, YD%FDM)
ENDIF
LFDL9 = ASSOCIATED (YD%FDL9)
WRITE (KLUN) LFDL9
IF (LFDL9) THEN
  CALL SAVE (KLUN, YD%FDL9)
ENDIF
LFDM9 = ASSOCIATED (YD%FDM9)
WRITE (KLUN) LFDM9
IF (LFDM9) THEN
  CALL SAVE (KLUN, YD%FDM9)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_VARIABLE_4D (KLUN, YD)
USE UTIL_FIELD_MOD
USE UTIL_VARIABLE_BASE_MOD, ONLY : VARIABLE_BASE, LOAD_VARIABLE_BASE

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (VARIABLE_4D), INTENT (OUT), TARGET :: YD
CLASS (VARIABLE_BASE), POINTER :: YLSUPER
LOGICAL :: LFDL, LFDL9, LFDM, LFDM9, LFPH9, LFT0, LFT1, LFT9
YLSUPER => YD
CALL LOAD_VARIABLE_BASE (KLUN, YLSUPER)
NULLIFY (YD%P)
NULLIFY (YD%T0)
NULLIFY (YD%T1)
NULLIFY (YD%T9)
NULLIFY (YD%PH9)
NULLIFY (YD%DL)
NULLIFY (YD%DM)
NULLIFY (YD%DL9)
NULLIFY (YD%DM9)
READ (KLUN) LFT0
IF (LFT0) THEN
  CALL LOAD (KLUN, YD%FT0)
ELSE
  NULLIFY (YD%FT0)
ENDIF
READ (KLUN) LFT1
IF (LFT1) THEN
  CALL LOAD (KLUN, YD%FT1)
ELSE
  NULLIFY (YD%FT1)
ENDIF
READ (KLUN) LFT9
IF (LFT9) THEN
  CALL LOAD (KLUN, YD%FT9)
ELSE
  NULLIFY (YD%FT9)
ENDIF
READ (KLUN) LFPH9
IF (LFPH9) THEN
  CALL LOAD (KLUN, YD%FPH9)
ELSE
  NULLIFY (YD%FPH9)
ENDIF
READ (KLUN) LFDL
IF (LFDL) THEN
  CALL LOAD (KLUN, YD%FDL)
ELSE
  NULLIFY (YD%FDL)
ENDIF
READ (KLUN) LFDM
IF (LFDM) THEN
  CALL LOAD (KLUN, YD%FDM)
ELSE
  NULLIFY (YD%FDM)
ENDIF
READ (KLUN) LFDL9
IF (LFDL9) THEN
  CALL LOAD (KLUN, YD%FDL9)
ELSE
  NULLIFY (YD%FDL9)
ENDIF
READ (KLUN) LFDM9
IF (LFDM9) THEN
  CALL LOAD (KLUN, YD%FDM9)
ELSE
  NULLIFY (YD%FDM9)
ENDIF
END SUBROUTINE


SUBROUTINE COPY_VARIABLE_4D (YD, LDCREATED)
USE UTIL_FIELD_MOD
USE UTIL_VARIABLE_BASE_MOD, ONLY : VARIABLE_BASE, COPY_VARIABLE_BASE
IMPLICIT NONE
TYPE (VARIABLE_4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
CLASS (VARIABLE_BASE), POINTER :: YLSUPER
LOGICAL :: LFDL, LFDL9, LFDM, LFDM9, LFPH9, LFT0, LFT1, LFT9

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
YLSUPER => YD
CALL COPY_VARIABLE_BASE (YLSUPER, LDCREATED=.TRUE.)
LFT0 = ASSOCIATED (YD%FT0)
IF (LFT0) THEN
  !$acc enter data create (YD%FT0)
  !$acc update device (YD%FT0)
  CALL COPY (YD%FT0, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FT0)
ENDIF

LFT1 = ASSOCIATED (YD%FT1)
IF (LFT1) THEN
  !$acc enter data create (YD%FT1)
  !$acc update device (YD%FT1)
  CALL COPY (YD%FT1, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FT1)
ENDIF

LFT9 = ASSOCIATED (YD%FT9)
IF (LFT9) THEN
  !$acc enter data create (YD%FT9)
  !$acc update device (YD%FT9)
  CALL COPY (YD%FT9, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FT9)
ENDIF

LFPH9 = ASSOCIATED (YD%FPH9)
IF (LFPH9) THEN
  !$acc enter data create (YD%FPH9)
  !$acc update device (YD%FPH9)
  CALL COPY (YD%FPH9, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FPH9)
ENDIF

LFDL = ASSOCIATED (YD%FDL)
IF (LFDL) THEN
  !$acc enter data create (YD%FDL)
  !$acc update device (YD%FDL)
  CALL COPY (YD%FDL, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FDL)
ENDIF

LFDM = ASSOCIATED (YD%FDM)
IF (LFDM) THEN
  !$acc enter data create (YD%FDM)
  !$acc update device (YD%FDM)
  CALL COPY (YD%FDM, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FDM)
ENDIF

LFDL9 = ASSOCIATED (YD%FDL9)
IF (LFDL9) THEN
  !$acc enter data create (YD%FDL9)
  !$acc update device (YD%FDL9)
  CALL COPY (YD%FDL9, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FDL9)
ENDIF

LFDM9 = ASSOCIATED (YD%FDM9)
IF (LFDM9) THEN
  !$acc enter data create (YD%FDM9)
  !$acc update device (YD%FDM9)
  CALL COPY (YD%FDM9, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%FDM9)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_VARIABLE_4D (YD, LDDELETED)
USE UTIL_FIELD_MOD
USE UTIL_VARIABLE_BASE_MOD, ONLY : VARIABLE_BASE, WIPE_VARIABLE_BASE
IMPLICIT NONE
TYPE (VARIABLE_4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
CLASS (VARIABLE_BASE), POINTER :: YLSUPER
LOGICAL :: LFDL, LFDL9, LFDM, LFDM9, LFPH9, LFT0, LFT1, LFT9

YLSUPER => YD
CALL WIPE_VARIABLE_BASE (YLSUPER, LDDELETED=.TRUE.)
LFT0 = ASSOCIATED (YD%FT0)
IF (LFT0) THEN
  !$acc exit data detach (YD%FT0)
  CALL WIPE (YD%FT0, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FT0)
ENDIF

LFT1 = ASSOCIATED (YD%FT1)
IF (LFT1) THEN
  !$acc exit data detach (YD%FT1)
  CALL WIPE (YD%FT1, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FT1)
ENDIF

LFT9 = ASSOCIATED (YD%FT9)
IF (LFT9) THEN
  !$acc exit data detach (YD%FT9)
  CALL WIPE (YD%FT9, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FT9)
ENDIF

LFPH9 = ASSOCIATED (YD%FPH9)
IF (LFPH9) THEN
  !$acc exit data detach (YD%FPH9)
  CALL WIPE (YD%FPH9, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FPH9)
ENDIF

LFDL = ASSOCIATED (YD%FDL)
IF (LFDL) THEN
  !$acc exit data detach (YD%FDL)
  CALL WIPE (YD%FDL, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FDL)
ENDIF

LFDM = ASSOCIATED (YD%FDM)
IF (LFDM) THEN
  !$acc exit data detach (YD%FDM)
  CALL WIPE (YD%FDM, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FDM)
ENDIF

LFDL9 = ASSOCIATED (YD%FDL9)
IF (LFDL9) THEN
  !$acc exit data detach (YD%FDL9)
  CALL WIPE (YD%FDL9, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FDL9)
ENDIF

LFDM9 = ASSOCIATED (YD%FDM9)
IF (LFDM9) THEN
  !$acc exit data detach (YD%FDM9)
  CALL WIPE (YD%FDM9, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%FDM9)
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

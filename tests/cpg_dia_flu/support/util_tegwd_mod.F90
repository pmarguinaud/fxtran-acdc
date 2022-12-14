MODULE UTIL_TEGWD_MOD

USE YOEGWD, ONLY : TEGWD

INTERFACE SAVE
MODULE PROCEDURE SAVE_TEGWD
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TEGWD
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TEGWD
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TEGWD
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TEGWD (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TEGWD), INTENT (IN), TARGET :: YD

WRITE (KLUN) YD%NKTOPG
WRITE (KLUN) YD%NGWDLIM
WRITE (KLUN) YD%NGWDTOP
WRITE (KLUN) YD%NDIFF_STRATO
WRITE (KLUN) YD%GFRCRIT
WRITE (KLUN) YD%GRCRIT
WRITE (KLUN) YD%GKDRAG
WRITE (KLUN) YD%GKWAKE
WRITE (KLUN) YD%GSSEC
WRITE (KLUN) YD%GTSEC
WRITE (KLUN) YD%GVSEC
WRITE (KLUN) YD%GTENLIM
WRITE (KLUN) YD%GRFPLM
WRITE (KLUN) YD%LRDIFF_STRATO
WRITE (KLUN) YD%LDIAG_STRATO
END SUBROUTINE

SUBROUTINE LOAD_TEGWD (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TEGWD), INTENT (OUT), TARGET :: YD

READ (KLUN) YD%NKTOPG
READ (KLUN) YD%NGWDLIM
READ (KLUN) YD%NGWDTOP
READ (KLUN) YD%NDIFF_STRATO
READ (KLUN) YD%GFRCRIT
READ (KLUN) YD%GRCRIT
READ (KLUN) YD%GKDRAG
READ (KLUN) YD%GKWAKE
READ (KLUN) YD%GSSEC
READ (KLUN) YD%GTSEC
READ (KLUN) YD%GVSEC
READ (KLUN) YD%GTENLIM
READ (KLUN) YD%GRFPLM
READ (KLUN) YD%LRDIFF_STRATO
READ (KLUN) YD%LDIAG_STRATO
END SUBROUTINE


SUBROUTINE COPY_TEGWD (YD, LDCREATED)

IMPLICIT NONE
TYPE (TEGWD), INTENT (IN), TARGET :: YD
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

SUBROUTINE WIPE_TEGWD (YD, LDDELETED)

IMPLICIT NONE
TYPE (TEGWD), INTENT (IN), TARGET :: YD
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

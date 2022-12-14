MODULE UTIL_TCLI_MOD

USE YOMCLI, ONLY : TCLI

INTERFACE SAVE
MODULE PROCEDURE SAVE_TCLI
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TCLI
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TCLI
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TCLI
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TCLI (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TCLI), INTENT (IN), TARGET :: YD

WRITE (KLUN) YD%LIEEE
WRITE (KLUN) YD%LGLOBE
WRITE (KLUN) YD%LZ0THER
WRITE (KLUN) YD%NPINT
WRITE (KLUN) YD%NDATX
WRITE (KLUN) YD%NDATY
WRITE (KLUN) YD%NAEROF
WRITE (KLUN) YD%NGLOBX
WRITE (KLUN) YD%NGLOBY
WRITE (KLUN) YD%NTPMER
WRITE (KLUN) YD%NTPGLA
WRITE (KLUN) YD%NTPDES
WRITE (KLUN) YD%NTPLAC
WRITE (KLUN) YD%NSLICE
WRITE (KLUN) YD%ELONSW
WRITE (KLUN) YD%ELATSW
WRITE (KLUN) YD%ELONNE
WRITE (KLUN) YD%ELATNE
WRITE (KLUN) YD%EDLON
WRITE (KLUN) YD%EDLAT
WRITE (KLUN) YD%SMASK
WRITE (KLUN) YD%SMANQ
WRITE (KLUN) YD%SVEG
WRITE (KLUN) YD%SFCZ0
WRITE (KLUN) YD%RSTR
WRITE (KLUN) YD%RSWR
WRITE (KLUN) YD%STHER
WRITE (KLUN) YD%SALBN
WRITE (KLUN) YD%SALBX
WRITE (KLUN) YD%SALBM
WRITE (KLUN) YD%SALBG
WRITE (KLUN) YD%SALBB
WRITE (KLUN) YD%SALBD
WRITE (KLUN) YD%SEMIN
WRITE (KLUN) YD%SEMIX
WRITE (KLUN) YD%SEMIM
WRITE (KLUN) YD%SEMIG
WRITE (KLUN) YD%SEMIB
WRITE (KLUN) YD%SEMID
WRITE (KLUN) YD%SDEPN
WRITE (KLUN) YD%SDEPX
WRITE (KLUN) YD%SDEPD
WRITE (KLUN) YD%SARGN
WRITE (KLUN) YD%SARGX
WRITE (KLUN) YD%SARGD
WRITE (KLUN) YD%SSABN
WRITE (KLUN) YD%SSABX
WRITE (KLUN) YD%SSABD
WRITE (KLUN) YD%SRSMN
WRITE (KLUN) YD%SRSMX
WRITE (KLUN) YD%SRSMD
WRITE (KLUN) YD%SZZ0N
WRITE (KLUN) YD%SZZ0M
WRITE (KLUN) YD%SZZ0B
WRITE (KLUN) YD%SZZ0U
WRITE (KLUN) YD%SZZ0D
END SUBROUTINE

SUBROUTINE LOAD_TCLI (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TCLI), INTENT (OUT), TARGET :: YD

READ (KLUN) YD%LIEEE
READ (KLUN) YD%LGLOBE
READ (KLUN) YD%LZ0THER
READ (KLUN) YD%NPINT
READ (KLUN) YD%NDATX
READ (KLUN) YD%NDATY
READ (KLUN) YD%NAEROF
READ (KLUN) YD%NGLOBX
READ (KLUN) YD%NGLOBY
READ (KLUN) YD%NTPMER
READ (KLUN) YD%NTPGLA
READ (KLUN) YD%NTPDES
READ (KLUN) YD%NTPLAC
READ (KLUN) YD%NSLICE
READ (KLUN) YD%ELONSW
READ (KLUN) YD%ELATSW
READ (KLUN) YD%ELONNE
READ (KLUN) YD%ELATNE
READ (KLUN) YD%EDLON
READ (KLUN) YD%EDLAT
READ (KLUN) YD%SMASK
READ (KLUN) YD%SMANQ
READ (KLUN) YD%SVEG
READ (KLUN) YD%SFCZ0
READ (KLUN) YD%RSTR
READ (KLUN) YD%RSWR
READ (KLUN) YD%STHER
READ (KLUN) YD%SALBN
READ (KLUN) YD%SALBX
READ (KLUN) YD%SALBM
READ (KLUN) YD%SALBG
READ (KLUN) YD%SALBB
READ (KLUN) YD%SALBD
READ (KLUN) YD%SEMIN
READ (KLUN) YD%SEMIX
READ (KLUN) YD%SEMIM
READ (KLUN) YD%SEMIG
READ (KLUN) YD%SEMIB
READ (KLUN) YD%SEMID
READ (KLUN) YD%SDEPN
READ (KLUN) YD%SDEPX
READ (KLUN) YD%SDEPD
READ (KLUN) YD%SARGN
READ (KLUN) YD%SARGX
READ (KLUN) YD%SARGD
READ (KLUN) YD%SSABN
READ (KLUN) YD%SSABX
READ (KLUN) YD%SSABD
READ (KLUN) YD%SRSMN
READ (KLUN) YD%SRSMX
READ (KLUN) YD%SRSMD
READ (KLUN) YD%SZZ0N
READ (KLUN) YD%SZZ0M
READ (KLUN) YD%SZZ0B
READ (KLUN) YD%SZZ0U
READ (KLUN) YD%SZZ0D
END SUBROUTINE


SUBROUTINE COPY_TCLI (YD, LDCREATED)

IMPLICIT NONE
TYPE (TCLI), INTENT (IN), TARGET :: YD
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

SUBROUTINE WIPE_TCLI (YD, LDDELETED)

IMPLICIT NONE
TYPE (TCLI), INTENT (IN), TARGET :: YD
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

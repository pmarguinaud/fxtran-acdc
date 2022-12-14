MODULE LOAD_TNORGWD_MOD

USE YOMNORGWD, ONLY : TNORGWD
USE PARKIND1, ONLY : JPRB, JPRD

INTERFACE LOAD
MODULE PROCEDURE LOAD_TNORGWD
END INTERFACE

CONTAINS

SUBROUTINE LOAD_TNORGWD (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TNORGWD), INTENT (OUT) :: YD
REAL(KIND=JPRD) :: ZTEMP0
READ (KLUN) YD%NORGWD_SCHEME
READ (KLUN) ZTEMP0
YD%NORGWD_PRMAX = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_DZ = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_PTROPO = ZTEMP0
READ (KLUN) YD%NORGWD_NTROPO
READ (KLUN) ZTEMP0
YD%NORGWD_RUWMAX = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_SAT = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_RDISS = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_DELTAT = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_KMIN = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_KMAX = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_CMIN = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_CMAX = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_PLAUNCH = ZTEMP0
READ (KLUN) YD%NORGWD_NLAUNCH
READ (KLUN) ZTEMP0
YD%NORGWD_PNOVERDIF = ZTEMP0
READ (KLUN) YD%NORGWD_NNOVERDIF
READ (KLUN) ZTEMP0
YD%NORGWD_DZFRON = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_GFRON = ZTEMP0
READ (KLUN) ZTEMP0
YD%NORGWD_GB = ZTEMP0
END SUBROUTINE


END MODULE

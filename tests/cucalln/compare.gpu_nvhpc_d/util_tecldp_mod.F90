MODULE UTIL_TECLDP_MOD

USE YOECLDP, ONLY : TECLDP

INTERFACE SAVE
MODULE PROCEDURE SAVE_TECLDP
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TECLDP
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TECLDP
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TECLDP
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TECLDP (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TECLDP), INTENT (IN), TARGET :: YD

WRITE (KLUN) YD%RAMID
WRITE (KLUN) YD%RCLDIFF
WRITE (KLUN) YD%RCLDIFF_CONVI
WRITE (KLUN) YD%RCLCRIT
WRITE (KLUN) YD%RCLCRIT_SEA
WRITE (KLUN) YD%RCLCRIT_LAND
WRITE (KLUN) YD%RKCONV
WRITE (KLUN) YD%RPRC1
WRITE (KLUN) YD%RPRC2
WRITE (KLUN) YD%RCLDMAX
WRITE (KLUN) YD%RPECONS
WRITE (KLUN) YD%RVRFACTOR
WRITE (KLUN) YD%RPRECRHMAX
WRITE (KLUN) YD%RTAUMEL
WRITE (KLUN) YD%RAMIN
WRITE (KLUN) YD%RLMIN
WRITE (KLUN) YD%RKOOPTAU
WRITE (KLUN) YD%RCLDTOPP
WRITE (KLUN) YD%RLCRITSNOW
WRITE (KLUN) YD%RSNOWLIN1
WRITE (KLUN) YD%RSNOWLIN2
WRITE (KLUN) YD%RICEHI1
WRITE (KLUN) YD%RICEHI2
WRITE (KLUN) YD%RICEINIT
WRITE (KLUN) YD%RVICE
WRITE (KLUN) YD%RVRAIN
WRITE (KLUN) YD%RVSNOW
WRITE (KLUN) YD%RTHOMO
WRITE (KLUN) YD%RCOVPMIN
WRITE (KLUN) YD%RCCN
WRITE (KLUN) YD%RNICE
WRITE (KLUN) YD%RCCNOM
WRITE (KLUN) YD%RCCNSS
WRITE (KLUN) YD%RCCNSU
WRITE (KLUN) YD%RCLDTOPCF
WRITE (KLUN) YD%RDEPLIQREFRATE
WRITE (KLUN) YD%RDEPLIQREFDEPTH
WRITE (KLUN) YD%RCL_OVERLAPLIQICE
WRITE (KLUN) YD%RCL_EFFRIME
WRITE (KLUN) YD%RCL_KKAAC
WRITE (KLUN) YD%RCL_KKBAC
WRITE (KLUN) YD%RCL_KKAAU
WRITE (KLUN) YD%RCL_KKBAUQ
WRITE (KLUN) YD%RCL_KKBAUN
WRITE (KLUN) YD%RCL_KK_CLOUD_NUM_SEA
WRITE (KLUN) YD%RCL_KK_CLOUD_NUM_LAND
WRITE (KLUN) YD%RCL_INHOMOGAUT
WRITE (KLUN) YD%RCL_INHOMOGACC
WRITE (KLUN) YD%LCLOUD_INHOMOG
WRITE (KLUN) YD%RCL_AI
WRITE (KLUN) YD%RCL_BI
WRITE (KLUN) YD%RCL_CI
WRITE (KLUN) YD%RCL_DI
WRITE (KLUN) YD%RCL_X1I
WRITE (KLUN) YD%RCL_X2I
WRITE (KLUN) YD%RCL_X3I
WRITE (KLUN) YD%RCL_X4I
WRITE (KLUN) YD%RCL_CONST1I
WRITE (KLUN) YD%RCL_CONST2I
WRITE (KLUN) YD%RCL_CONST3I
WRITE (KLUN) YD%RCL_CONST4I
WRITE (KLUN) YD%RCL_CONST5I
WRITE (KLUN) YD%RCL_CONST6I
WRITE (KLUN) YD%RCL_APB1
WRITE (KLUN) YD%RCL_APB2
WRITE (KLUN) YD%RCL_APB3
WRITE (KLUN) YD%RCL_AS
WRITE (KLUN) YD%RCL_BS
WRITE (KLUN) YD%RCL_CS
WRITE (KLUN) YD%RCL_DS
WRITE (KLUN) YD%RCL_X1S
WRITE (KLUN) YD%RCL_X2S
WRITE (KLUN) YD%RCL_X3S
WRITE (KLUN) YD%RCL_X4S
WRITE (KLUN) YD%RCL_CONST1S
WRITE (KLUN) YD%RCL_CONST2S
WRITE (KLUN) YD%RCL_CONST3S
WRITE (KLUN) YD%RCL_CONST4S
WRITE (KLUN) YD%RCL_CONST5S
WRITE (KLUN) YD%RCL_CONST6S
WRITE (KLUN) YD%RCL_CONST7S
WRITE (KLUN) YD%RCL_CONST8S
WRITE (KLUN) YD%RDENSWAT
WRITE (KLUN) YD%RDENSREF
WRITE (KLUN) YD%RCL_AR
WRITE (KLUN) YD%RCL_BR
WRITE (KLUN) YD%RCL_CR
WRITE (KLUN) YD%RCL_DR
WRITE (KLUN) YD%RCL_X1R
WRITE (KLUN) YD%RCL_X2R
WRITE (KLUN) YD%RCL_X4R
WRITE (KLUN) YD%RCL_X1R_MP
WRITE (KLUN) YD%RCL_X2R_MP
WRITE (KLUN) YD%RCL_X4R_MP
WRITE (KLUN) YD%RCL_KA273
WRITE (KLUN) YD%RCL_CDENOM1
WRITE (KLUN) YD%RCL_CDENOM2
WRITE (KLUN) YD%RCL_CDENOM3
WRITE (KLUN) YD%RCL_SCHMIDT
WRITE (KLUN) YD%RCL_DYNVISC
WRITE (KLUN) YD%RCL_CONST1R
WRITE (KLUN) YD%RCL_CONST2R
WRITE (KLUN) YD%RCL_CONST3R
WRITE (KLUN) YD%RCL_CONST4R
WRITE (KLUN) YD%RCL_CONST7R
WRITE (KLUN) YD%RCL_CONST8R
WRITE (KLUN) YD%RCL_FAC1
WRITE (KLUN) YD%RCL_FAC2
WRITE (KLUN) YD%RCL_FAC1_MP
WRITE (KLUN) YD%RCL_FAC2_MP
WRITE (KLUN) YD%RCL_CONST5R
WRITE (KLUN) YD%RCL_CONST6R
WRITE (KLUN) YD%RCL_FZRAB
WRITE (KLUN) YD%RCL_FZRBB
WRITE (KLUN) YD%RCL_CONST9R
WRITE (KLUN) YD%RCL_CONST10R
WRITE (KLUN) YD%RCL_EFF_RACW
WRITE (KLUN) YD%LCLDEXTRA
WRITE (KLUN) YD%LCLDBUDGET
WRITE (KLUN) YD%LCLDBUDC
WRITE (KLUN) YD%LCLDBUDL
WRITE (KLUN) YD%LCLDBUDI
WRITE (KLUN) YD%LCLDBUDT
WRITE (KLUN) YD%LCLDBUD_TIMEINT
WRITE (KLUN) YD%LCLDBUD_VERTINT
WRITE (KLUN) YD%NSSOPT
WRITE (KLUN) YD%NCLDTOP
WRITE (KLUN) YD%NAECLBC
WRITE (KLUN) YD%NAECLDU
WRITE (KLUN) YD%NAECLOM
WRITE (KLUN) YD%NAECLSS
WRITE (KLUN) YD%NAECLSU
WRITE (KLUN) YD%NCLDDIAG
WRITE (KLUN) YD%NAERCLD
WRITE (KLUN) YD%LAERLIQAUTOLSP
WRITE (KLUN) YD%LAERLIQAUTOCP
WRITE (KLUN) YD%LAERLIQAUTOCPB
WRITE (KLUN) YD%LAERLIQCOLL
WRITE (KLUN) YD%LAERICESED
WRITE (KLUN) YD%LAERICEAUTO
WRITE (KLUN) YD%NSHAPEP
WRITE (KLUN) YD%NSHAPEQ
WRITE (KLUN) YD%NBETA
WRITE (KLUN) YD%RBETA
WRITE (KLUN) YD%RBETAP1
END SUBROUTINE

SUBROUTINE LOAD_TECLDP (KLUN, YD)
USE PARKIND1, ONLY : JPRD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TECLDP), INTENT (OUT), TARGET :: YD
REAL(KIND=JPRD) :: ZTMP0
REAL(KIND=JPRD), ALLOCATABLE :: ZTMP1 (:)
READ (KLUN) ZTMP0
YD%RAMID = ZTMP0
READ (KLUN) ZTMP0
YD%RCLDIFF = ZTMP0
READ (KLUN) ZTMP0
YD%RCLDIFF_CONVI = ZTMP0
READ (KLUN) ZTMP0
YD%RCLCRIT = ZTMP0
READ (KLUN) ZTMP0
YD%RCLCRIT_SEA = ZTMP0
READ (KLUN) ZTMP0
YD%RCLCRIT_LAND = ZTMP0
READ (KLUN) ZTMP0
YD%RKCONV = ZTMP0
READ (KLUN) ZTMP0
YD%RPRC1 = ZTMP0
READ (KLUN) ZTMP0
YD%RPRC2 = ZTMP0
READ (KLUN) ZTMP0
YD%RCLDMAX = ZTMP0
READ (KLUN) ZTMP0
YD%RPECONS = ZTMP0
READ (KLUN) ZTMP0
YD%RVRFACTOR = ZTMP0
READ (KLUN) ZTMP0
YD%RPRECRHMAX = ZTMP0
READ (KLUN) ZTMP0
YD%RTAUMEL = ZTMP0
READ (KLUN) ZTMP0
YD%RAMIN = ZTMP0
READ (KLUN) ZTMP0
YD%RLMIN = ZTMP0
READ (KLUN) ZTMP0
YD%RKOOPTAU = ZTMP0
READ (KLUN) ZTMP0
YD%RCLDTOPP = ZTMP0
READ (KLUN) ZTMP0
YD%RLCRITSNOW = ZTMP0
READ (KLUN) ZTMP0
YD%RSNOWLIN1 = ZTMP0
READ (KLUN) ZTMP0
YD%RSNOWLIN2 = ZTMP0
READ (KLUN) ZTMP0
YD%RICEHI1 = ZTMP0
READ (KLUN) ZTMP0
YD%RICEHI2 = ZTMP0
READ (KLUN) ZTMP0
YD%RICEINIT = ZTMP0
READ (KLUN) ZTMP0
YD%RVICE = ZTMP0
READ (KLUN) ZTMP0
YD%RVRAIN = ZTMP0
READ (KLUN) ZTMP0
YD%RVSNOW = ZTMP0
READ (KLUN) ZTMP0
YD%RTHOMO = ZTMP0
READ (KLUN) ZTMP0
YD%RCOVPMIN = ZTMP0
READ (KLUN) ZTMP0
YD%RCCN = ZTMP0
READ (KLUN) ZTMP0
YD%RNICE = ZTMP0
READ (KLUN) ZTMP0
YD%RCCNOM = ZTMP0
READ (KLUN) ZTMP0
YD%RCCNSS = ZTMP0
READ (KLUN) ZTMP0
YD%RCCNSU = ZTMP0
READ (KLUN) ZTMP0
YD%RCLDTOPCF = ZTMP0
READ (KLUN) ZTMP0
YD%RDEPLIQREFRATE = ZTMP0
READ (KLUN) ZTMP0
YD%RDEPLIQREFDEPTH = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_OVERLAPLIQICE = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_EFFRIME = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KKAAC = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KKBAC = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KKAAU = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KKBAUQ = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KKBAUN = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KK_CLOUD_NUM_SEA = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KK_CLOUD_NUM_LAND = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_INHOMOGAUT = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_INHOMOGACC = ZTMP0
READ (KLUN) YD%LCLOUD_INHOMOG
READ (KLUN) ZTMP0
YD%RCL_AI = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_BI = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CI = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_DI = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X1I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X2I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X3I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X4I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST1I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST2I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST3I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST4I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST5I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST6I = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_APB1 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_APB2 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_APB3 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_AS = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_BS = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CS = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_DS = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X1S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X2S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X3S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X4S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST1S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST2S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST3S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST4S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST5S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST6S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST7S = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST8S = ZTMP0
READ (KLUN) ZTMP0
YD%RDENSWAT = ZTMP0
READ (KLUN) ZTMP0
YD%RDENSREF = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_AR = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_BR = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CR = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_DR = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X1R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X2R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X4R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X1R_MP = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X2R_MP = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_X4R_MP = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_KA273 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CDENOM1 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CDENOM2 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CDENOM3 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_SCHMIDT = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_DYNVISC = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST1R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST2R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST3R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST4R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST7R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST8R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_FAC1 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_FAC2 = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_FAC1_MP = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_FAC2_MP = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST5R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST6R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_FZRAB = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_FZRBB = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST9R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_CONST10R = ZTMP0
READ (KLUN) ZTMP0
YD%RCL_EFF_RACW = ZTMP0
READ (KLUN) YD%LCLDEXTRA
READ (KLUN) YD%LCLDBUDGET
READ (KLUN) YD%LCLDBUDC
READ (KLUN) YD%LCLDBUDL
READ (KLUN) YD%LCLDBUDI
READ (KLUN) YD%LCLDBUDT
READ (KLUN) YD%LCLDBUD_TIMEINT
READ (KLUN) YD%LCLDBUD_VERTINT
READ (KLUN) YD%NSSOPT
READ (KLUN) YD%NCLDTOP
READ (KLUN) YD%NAECLBC
READ (KLUN) YD%NAECLDU
READ (KLUN) YD%NAECLOM
READ (KLUN) YD%NAECLSS
READ (KLUN) YD%NAECLSU
READ (KLUN) YD%NCLDDIAG
READ (KLUN) YD%NAERCLD
READ (KLUN) YD%LAERLIQAUTOLSP
READ (KLUN) YD%LAERLIQAUTOCP
READ (KLUN) YD%LAERLIQAUTOCPB
READ (KLUN) YD%LAERLIQCOLL
READ (KLUN) YD%LAERICESED
READ (KLUN) YD%LAERICEAUTO
READ (KLUN) ZTMP0
YD%NSHAPEP = ZTMP0
READ (KLUN) ZTMP0
YD%NSHAPEQ = ZTMP0
READ (KLUN) YD%NBETA
ALLOCATE (ZTMP1(LBOUND(YD%RBETA,1):UBOUND(YD%RBETA,1)))
READ (KLUN) ZTMP1
YD%RBETA = ZTMP1
DEALLOCATE (ZTMP1)
ALLOCATE (ZTMP1(LBOUND(YD%RBETAP1,1):UBOUND(YD%RBETAP1,1)))
READ (KLUN) ZTMP1
YD%RBETAP1 = ZTMP1
DEALLOCATE (ZTMP1)
END SUBROUTINE


SUBROUTINE COPY_TECLDP (YD, LDCREATED)

IMPLICIT NONE
TYPE (TECLDP), INTENT (IN), TARGET :: YD
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

SUBROUTINE WIPE_TECLDP (YD, LDDELETED)

IMPLICIT NONE
TYPE (TECLDP), INTENT (IN), TARGET :: YD
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

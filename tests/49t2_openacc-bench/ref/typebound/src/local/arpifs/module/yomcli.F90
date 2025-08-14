MODULE YOMCLI

!$ACDC methods 


USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!  *YOMCLI* : constants for new configuration 923

! Dataset description

!  - namelist -
!  NPINT  : size of the interpolation box 
!  LIEEE  : if ieee format is used
!  LGLOBE : if global dataset
!  NDATX  : x-size of the dataset (longitude) 
!  NDATY  : y-size of the dataset (longitude) 
!  NAEROF : index of aerosol file 0=tegen,1=camsaod,2=camsmmr 
!  ELONSW , ELATSW , ELONNE , ELATNE : for local datasets (parts 5, 7 or LAM)
!           latitudes and longitudes of the SW and NE corners (in degrees) 
!  - local, set directly by INCLI0 from namelist -
!  EDLON , EDLAT  : resolution of the dataset (in degrees)
!  NGLOBX, NGLOBY : corresponding numbers of points on the globe
!  - local, set by VAL923 according to LSOLV -
!  SMASK  : thresholds defining the masked areas
!  SMANQ  : value for missing data
!  NTPMER : land-use type for sea (or lake)
!  NTPGLA : land-use type for ice-cap
!  NTPDES : land-use type for low vegetation, including deserts (land default)
!  NTPLAC : land-use type for lakes if a distinction is required
!  NSLICE : number of packet of latitudes which slice the domain

! Fields characteristics
!  - namelist -
!  SVEG   : threshold for significant vegetation cover 
!           (below, vegetation characteristics are not considered)
!  LZ0THER: .FALSE. if no orographic part in the thermic roughness length
!  SFCZ0  : scaling factor for the secondary part of z0 (urban., veget.)
!  RSTR   : thresholds on T-RTT used to control snow cover in Part 6
!           SST-RTT, constant, in Part 10 when no input dataset is available
!  RSWR   : thresholds on T-RTT and Wr used to control snow cover in Part 6
!  - local, set by VAL923 according to LSOLV -
!  STHER  : ration of thermal to kinetic roughness length (over land)
!  SALBN,SALBX,SALBM,SALBG,SALBB,SALBD : albedo
!  SEMIN,SEMIX,SEMIM,SEMIG,SEMIB,SEMID : emissivity
!  (minimum,maximum,sea,ice,sea ice,desert)
!  SDEPN,SDEPX,SDEPD : soil depth
!  SARGN,SARGX,SARGD : clay percentage
!  SSABN,SSABX,SSABD : sand percentage
!  SRSMN,SRSMX,SRSMD : minimum surface resistance
!  (minimum,maximum,desert)
!  SZZ0N,SZZ0M,SZZ0B,SZZ0U,SZZ0D : roughness length
!  (minimum,sea,sea-ice,urban areas,desert)

TYPE TCLI
LOGICAL :: LIEEE
LOGICAL :: LGLOBE
LOGICAL :: LZ0THER
INTEGER(KIND=JPIM) :: NPINT
INTEGER(KIND=JPIM) :: NDATX
INTEGER(KIND=JPIM) :: NDATY
INTEGER(KIND=JPIM) :: NAEROF
INTEGER(KIND=JPIM) :: NGLOBX
INTEGER(KIND=JPIM) :: NGLOBY
INTEGER(KIND=JPIM) :: NTPMER
INTEGER(KIND=JPIM) :: NTPGLA
INTEGER(KIND=JPIM) :: NTPDES
INTEGER(KIND=JPIM) :: NTPLAC
INTEGER(KIND=JPIM) :: NSLICE
REAL(KIND=JPRB) :: ELONSW
REAL(KIND=JPRB) :: ELATSW
REAL(KIND=JPRB) :: ELONNE
REAL(KIND=JPRB) :: ELATNE
REAL(KIND=JPRB) :: EDLON
REAL(KIND=JPRB) :: EDLAT
REAL(KIND=JPRB) :: SMASK
REAL(KIND=JPRB) :: SMANQ
REAL(KIND=JPRB) :: SVEG
REAL(KIND=JPRB) :: SFCZ0
REAL(KIND=JPRB) :: RSTR
REAL(KIND=JPRB) :: RSWR
REAL(KIND=JPRB) :: STHER
REAL(KIND=JPRB) :: SALBN
REAL(KIND=JPRB) :: SALBX
REAL(KIND=JPRB) :: SALBM
REAL(KIND=JPRB) :: SALBG
REAL(KIND=JPRB) :: SALBB
REAL(KIND=JPRB) :: SALBD
REAL(KIND=JPRB) :: SEMIN
REAL(KIND=JPRB) :: SEMIX
REAL(KIND=JPRB) :: SEMIM
REAL(KIND=JPRB) :: SEMIG
REAL(KIND=JPRB) :: SEMIB
REAL(KIND=JPRB) :: SEMID
REAL(KIND=JPRB) :: SDEPN
REAL(KIND=JPRB) :: SDEPX
REAL(KIND=JPRB) :: SDEPD
REAL(KIND=JPRB) :: SARGN
REAL(KIND=JPRB) :: SARGX
REAL(KIND=JPRB) :: SARGD
REAL(KIND=JPRB) :: SSABN
REAL(KIND=JPRB) :: SSABX
REAL(KIND=JPRB) :: SSABD
REAL(KIND=JPRB) :: SRSMN
REAL(KIND=JPRB) :: SRSMX
REAL(KIND=JPRB) :: SRSMD
REAL(KIND=JPRB) :: SZZ0N
REAL(KIND=JPRB) :: SZZ0M
REAL(KIND=JPRB) :: SZZ0B
REAL(KIND=JPRB) :: SZZ0U
REAL(KIND=JPRB) :: SZZ0D
CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TCLI
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TCLI
PROCEDURE :: ACDC_HOST => ACDC_HOST_TCLI
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TCLI
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TCLI
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TCLI
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TCLI
END TYPE TCLI

TYPE (TCLI), TARGET :: YRCLI 

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TCLI (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TCLI), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TCLI (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TCLI), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TCLI (SELF)

IMPLICIT NONE
CLASS (TCLI), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TCLI (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TCLI), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TCLI (SELF, KLUN)

IMPLICIT NONE
CLASS (TCLI), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TCLI (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TCLI),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TCLI (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TCLI), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE YOMCLI

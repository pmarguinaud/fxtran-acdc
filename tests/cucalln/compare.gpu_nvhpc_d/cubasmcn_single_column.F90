SUBROUTINE CUBASMCN_SINGLE_COLUMN (YDCST, YDECUMF, KIDIA, KFDIA, KLON, KLEV,&
& KK, PTEN, PQEN, PQSEN, PVERVEL, PGEO, PGEOH, LDCUM, KTYPE, KLAB, KCBOT, PMFU&
&, PMFUB, PLRAIN, PTU, PQU, PLU, PMFUS, PMFUQ, PMFUL, PDMFUP, YDSTACK)
!$acc routine (CUBASMCN_SINGLE_COLUMN) seq
USE PARKIND1,ONLY:JPIM, JPRB

USE YOMCST,ONLY:TCST
USE YOECUMF,ONLY:TECUMF
USE STACK_MOD
#include "stack.h"

IMPLICIT NONE

TYPE (TCST), INTENT (IN)::YDCST
TYPE (TECUMF), INTENT (IN)::YDECUMF
INTEGER (KIND=JPIM), INTENT (IN)::KLON
INTEGER (KIND=JPIM), INTENT (IN)::KLEV
INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
INTEGER (KIND=JPIM), INTENT (IN)::KK
REAL (KIND=JPRB), INTENT (IN)::PTEN (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PQEN (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PQSEN (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PVERVEL (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PGEO (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PGEOH (KLON, KLEV+1)
LOGICAL, INTENT (IN)::LDCUM (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KTYPE (KLON)
INTEGER (KIND=JPIM), INTENT (INOUT)::KLAB (KLON, KLEV)
INTEGER (KIND=JPIM), INTENT (OUT)::KCBOT (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PMFU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PMFUB (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PLRAIN (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PTU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PQU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PLU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PMFUS (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PMFUQ (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PMFUL (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PDMFUP (KLON, KLEV)
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK
INTEGER (KIND=JPIM)::JLON

REAL (KIND=JPRB)::ZORCPD
REAL (KIND=JPRB)::ZRG
REAL (KIND=JPRB)::ZZZMB


YLSTACK = YDSTACK




JLON = KIDIA


ZRG=1.0_JPRB/YDCST%RG
ZORCPD=1.0_JPRB/YDCST%RCPD


IF (.NOT.LDCUM (JLON).AND.KLAB (JLON, KK+1)==0) THEN

  IF (YDECUMF%LMFMID.AND.KK>YDECUMF%NJKT7.AND.PQEN (JLON, KK)>0.80_JPRB*PQSEN (JLON, KK)) THEN
    PTU (JLON, KK+1)=(YDCST%RCPD*PTEN (JLON, KK)+PGEO (JLON, KK)-PGEOH (JLON, KK+1))*ZORCPD
    PQU (JLON, KK+1)=PQEN (JLON, KK)
    PLU (JLON, KK+1)=0.0_JPRB
    ZZZMB=MAX (1.E-6_JPRB,-PVERVEL (JLON, KK)*ZRG)
    ZZZMB=MIN (ZZZMB, YDECUMF%RMFLIA)
    PMFUB (JLON)=ZZZMB
    PMFU (JLON, KK+1)=PMFUB (JLON)
    PMFUS (JLON, KK+1)=PMFUB (JLON)*(YDCST%RCPD*PTU (JLON, KK+1)+PGEOH (JLON, KK+1))
    PMFUQ (JLON, KK+1)=PMFUB (JLON)*PQU (JLON, KK+1)
    PMFUL (JLON, KK+1)=0.0_JPRB
    PDMFUP (JLON, KK+1)=0.0_JPRB
    PLRAIN (JLON, KK+1)=0.0_JPRB
    KCBOT (JLON)=KK
    KLAB (JLON, KK+1)=1
    KTYPE (JLON)=3
  ENDIF

ENDIF




ENDSUBROUTINE CUBASMCN_SINGLE_COLUMN


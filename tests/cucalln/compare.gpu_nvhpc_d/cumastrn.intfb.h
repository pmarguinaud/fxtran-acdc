INTERFACE
SUBROUTINE CUMASTRN &
& (PPLDARE, PPLRG,    YDTHF,   YDCST, YDML_PHY_SLIN,     YDML_PHY_EC, YGFL,&
& KIDIA,    KFDIA,    KLON,    KLEV,   PDX, LDTDKMF,   KSPPN2D,&
& LDLAND,   PTSPHY,&
& PTEN,     PQEN,     PUEN,     PVEN,     PLITOT,&
& PVERVEL,  PQSEN,    PQHFL,    PAHFS,&
& PAP,      PAPH,     PGEO,     PGEOH,    PGAW,&
& PCUCONVCA,PGP2DSPP, &
& PTENT,    PTENQ,    PTENU,    PTENV,&
& LDCUM,    KTYPE,    KCBOT,    KCTOP,&
& KBOTSC,   LDSC,&
& LDSHCV,&
& PLCRIT_AER,&
& PTU,      PQU,      PLU,      PLUDE,    PLUDELI,   PSNDE,&
& PENTH,    PMFLXR,   PMFLXS,   PRAIN,    PLRAIN,    PRSUD,&
& PMFU,     PMFD,&
& PMFUDE_RATE,        PMFDDE_RATE,    PCAPE,   PWMEAN, PDISS,&
& KTRAC,    PCEN,     PTENC,    PSCAV )
USE MODEL_PHYSICS_ECMWF_MOD , ONLY : MODEL_PHYSICS_ECMWF_TYPE
USE MODEL_PHYSICS_SIMPLINEAR_MOD , ONLY : MODEL_PHYSICS_SIMPLINEAR_TYPE
USE YOM_YGFL  , ONLY : TYPE_GFLD
USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK
USE YOMCST    , ONLY : TCST
USE YOETHF    , ONLY : TTHF
USE SPP_MOD  , ONLY : YSPP_CONFIG, YSPP
USE YOMCAPE  , ONLY : LMCAPEA
REAL(KIND=JPRB)   ,INTENT(IN)    :: PPLDARE
REAL(KIND=JPRB)   ,INTENT(IN)    :: PPLRG
TYPE(TTHF)                         ,INTENT(IN):: YDTHF
TYPE(TCST)                         ,INTENT(IN):: YDCST
TYPE(MODEL_PHYSICS_ECMWF_TYPE)     ,INTENT(IN):: YDML_PHY_EC
TYPE(MODEL_PHYSICS_SIMPLINEAR_TYPE),INTENT(IN):: YDML_PHY_SLIN
TYPE(TYPE_GFLD)   ,INTENT(IN)    :: YGFL
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KSPPN2D
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KTRAC
LOGICAL           ,INTENT(IN)    :: LDLAND(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSPHY
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLCRIT_AER(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTEN(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQEN(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUEN(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVEN(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLITOT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVERVEL(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQSEN(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQHFL(KLON,KLEV+1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAHFS(KLON,KLEV+1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPH(KLON,KLEV+1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEO(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOH(KLON,KLEV+1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGAW(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCUCONVCA(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGP2DSPP(KLON,KSPPN2D)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCEN(KLON,KLEV,KTRAC)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSCAV(KTRAC)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDX(KLON)
LOGICAL           ,INTENT(IN)    :: LDTDKMF
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTENT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTENQ(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTENU(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTENV(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTENC(KLON,KLEV,KTRAC)
LOGICAL           ,INTENT(OUT)   :: LDCUM(KLON)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KTYPE(KLON)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KCBOT(KLON)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KCTOP(KLON)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KBOTSC(KLON)
LOGICAL           ,INTENT(OUT)   :: LDSC(KLON)
LOGICAL           ,INTENT(IN)    :: LDSHCV(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTU(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQU(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLU(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLUDE(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLUDELI(KLON,KLEV,4)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSNDE(KLON,KLEV,2)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PENTH(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFLXR(KLON,KLEV+1)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFLXS(KLON,KLEV+1)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRAIN(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLRAIN(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRSUD(KLON,KLEV,2)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFU(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFD(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFUDE_RATE(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFDDE_RATE(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAPE(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWMEAN(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDISS(KLON,KLEV)
END SUBROUTINE CUMASTRN

END INTERFACE

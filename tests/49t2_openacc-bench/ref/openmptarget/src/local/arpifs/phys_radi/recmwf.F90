!OPTIONS XOPT(NOEVAL)
SUBROUTINE RECMWF (YDDIMV, YDMODEL, YDCPG_OPTS, YDCPG_BNDS,     &
 & KOZOCL  ,KMACCAERO ,  KAERO     ,                            &
 & PALBD   , PALBP    ,  PAPRS     , PAPRSF     ,               &
 & PNEB    , PQO3     ,  PQCO2     , PQCH4      , PQN2O   ,     &
 & PQNO2   , PQC11    ,  PQC12     , PQC22      , PQCL4   ,     &
 & PAER    , PAERO    ,  PAEROCPL  , PDELP      , PEMIS   ,     &
 & PMU0M   , PQ       ,  PQSAT     , PQICE      , PQLIQ   ,     &
 & PS      , PRR      ,  PLSM      , PT         , PTS     ,     &
 & PEMTD   , PEMTU    ,  PTRSO     , PLWFC      , PLWFT   ,     &
 & PSWFC   , PSWFT    ,  PSFSWDIR  , PSFSWDIF   , PFSDNN  ,     &
 & PFSDNV  , PCTRSO   ,  PCEMTR    , PTRSOD     , PTRSODIR,     &
 & PTRSODIF, PPIZA_DST,  PCGA_DST  , PTAUREL_DST,               &
 & PAERINDS, PGELAM   ,  PGEMU     , PSWDIR     , PSWDIF  ,     &
 & PMU0LU  , PALB     ,  PVRMOON   , PCLDROP    , YSPP_RSWINHF, &
 & YSPP_RLWINHF)

!$ACDC pointerparallel    


!**** *RECMWF* - METEO-FRANCE RADIATION INTERFACE TO ECMWF RADIATION SCHEME

!**** PURPOSE :
!     ---------
!           SIMPLE INTERFACE TO RADLSW - RADLSWR - RADIATION_SCHEME

!**   INTERFACE.
!     ----------

!     EXPLICIT ARGUMENTS :
!        --------------------
! KIDIA    : START INDEX OF DATA IN KLON-LONG VECTOR
! KFDIA    : END   INDEX OF DATA IN KLON-LONG VECTOR
! KLON     : VECTOR LENGTH
! KLEV     : NUMBER OF LEVELS
! KOZOCL   : NOZOCL (Ozone clim choice)
! KMACCAERO: MACC AEROSOLS (clims or prognostics)
! KAERO    : NUMBER OF MACC AEROSOLS 
! PAER     : (KLON,KLEV ,6)     ; OPTICAL THICKNESS OF THE AEROSOLS
! PAERO    : (KLON,KLEV ,KAERO)    ; AEROSOLS MACC MMR 
!                                     (from netcdf clim file or GFL)
! PAEROCPL : (KLON,KLEV ,16,6)  ; OPTICAL AEROSOL PROPERTIES FOR RADIATION 
! PAERINDS : (KLON,KLEV)        ; OPTICAL THICKNESS OF THE SULFATE AEROSOL (indirect effect only)
! PALBD    : (KLON,NSW)         ; DIFFUSE ALBEDO IN THE 2 SW INTERVALS
! PALBP    : (KLON,NSW)         ; PARALLEL ALBEDO IN THE 2 SW INTERVALS
! PAPRS    : (KLON,KLEV+1)      ; HALF LEVEL PRESSURE
! PAPRSF   : (KLON,KLEV )       ; FULL LEVEL PRESSURE
! PNEB     : (KLON,KLEV )       ; CLOUD FRACTIONAL COVER
! PQO3     : (KLON,0:KLEV )     ; OZONE MIXING RATIO (MASS)
! PQCO2    : (KLON,KLEV)        ; CO2 from MACC clims 
! PQCH4    : (KLON,KLEV)        ; from MACC clims 
! PQN2O    : (KLON,KLEV)        ; from MACC clims   
! PQNO2    : (KLON,KLEV)        ; from MACC clims
! PQC11    : (KLON,KLEV)        ; from MACC clims   
! PQC12    : (KLON,KLEV)        ; from MACC clims    
! PQC22    : (KLON,KLEV)        ; from MACC clims    
! PQCL4    : (KLON,KLEV)        ; from MACC clims
! PDELP    : (KLON,KLEV)        ; LAYER PRESSURE THICKNESS
! PEMIS    : (KLON)             ; SURFACE EMISSIVITY
! PMU0M    : (KLON)             ; MEAN SOLAR ANGLE
! PQ       : (KLON,KLEV )       ; SPECIFIC HUMIDITY PA/PA
! PQSAT    : (KLON,KLEV )       ; SATURATION SPECIFIC HUMIDITY PA/PA
! PQICE    : (KLON,KLEV )       ; ICE    WATER KG/KG
! PQLIQ    : (KLON,KLEV )       ; LIQUID WATER KG/KG
! PLSM     : (KLON)             ; LAND-SEA MASK
! PT       : (KLON,KLEV)        ; FULL LEVEL TEMPERATURE
! PTS      : (KLON)             ; SURFACE TEMPERATURE
! PPIZA_DST: (KLON,KLEV,NSW); Single scattering albedo of dust 
! PCGA_DST : (KLON,KLEV,NSW); Assymetry factor for dust 
! PTAUREL_DST: (KLON,KLEV,NSW); Optical depth of dust relative to at 550nm

!     ==== OUTPUTS ===
! PEMTD (KLON,KLEV+1)         ; TOTAL DOWNWARD LONGWAVE EMISSIVITY
! PEMTU (KLON,KLEV+1)         ; TOTAL UPWARD   LONGWAVE EMISSIVITY
! PTRSO (KLON,KLEV+1)         ; TOTAL SHORTWAVE TRANSMISSIVITY
! PCTRSO(KLON,2)              ; CLEAR-SKY SHORTWAVE TRANSMISSIVITY
! PCEMTR(KLON,2)              ; CLEAR-SKY NET LONGWAVE EMISSIVITY
! PTRSOD(KLON)                ; TOTAL-SKY SURFACE SW TRANSMISSITY
! PLWFC (KLON,2)              ; CLEAR-SKY LONGWAVE FLUXES
! PLWFT (KLON,KLEV+1)         ; TOTAL-SKY LONGWAVE FLUXES
! PSWFC (KLON,2)              ; CLEAR-SKY SHORTWAVE FLUXES
! PSWFT (KLON,KLEV+1)         ; TOTAL-SKY SHORTWAVE FLUXES
! PTRSODIR(KLON,NSW)          ; DIRECT SURFACE SW TRANSMISSITY
! PTRSODIF(KLON,NSW)          : DIFFUSE SURFACE SW TRANSMISSITY

!   === OPTIONAL ===
! PCLDROP(KLON,KLEV)          ; Cloud Droplet number conc. from n.r.t. aerosols

!        IMPLICIT ARGUMENTS :   NONE
!        --------------------

!     METHOD.
!     -------
!     SEE DOCUMENTATION

!     EXTERNALS.
!     ----------

!     REFERENCE.
!     ----------
!     ECMWF RESEARCH DEPARTMENT DOCUMENTATION OF THE IFS

!     AUTHORS.
!     --------
!     ORIGINAL BY  B. RITTER   *ECMWF*        83-10-13
!     REWRITING FOR IFS BY J.-J. MORCRETTE    94-11-15

!     MODIFICATIONS.
!     --------------
!     2003-09: Y.   Bouteloup              : Duplication of RFMR to use present (cy25) ECMWF radiation scheme
!     2004-09: F.   Bouyssel               : Use of 6 aerosols & introduce NSW
!     2018-04: Y.   Seity                  : 4 New arguments for AROME 
!     2005-10: Y.   Seity                  : 3 optional arguments for dust optical properties 
!     2006-07: J.J. Morcrette              : PP of clear-sky PAR and TOA incident solar radiation (ECMWF)
!     2011-02: A.   Voldoire               : add PAERINDS for sulfate indirect effect computation
!     2011-06: M.   Jerczynski             : some cleaning to meet norms
!     2014-05: J.M. Piriou                 : force qv used by radiation to be larger to some factor of qsat.
!     2014-11: M.Benamara  + Y. Bouteloup  : Call to RADLSWR if LSRTM
!     2017-09: M.Bouzghaia + Y.  Bouteloup : Call to ECRAD
!     2018-09: M.Bouzghaia + Y.  Bouteloup : ECRAD adaptation to MF + Cleanings
!     2019-10: Y. Bouteloup : Phasing to cy47t0
!     2020-12: U. Andrae : Introduce SPP for HARMONIE-AROME
!     2021-01: D. Martin Add cloud droplet number conc. from nrt aerosols
!     2022-01: Y. Bouteloup and Yann Seity : Fixes to run Arome with EcRad and cleaning of commented lines
!     21-Jun-2022 R. El Khatib portability fix
!     2023-01, M. Michou : modifications so that chemistry interacts with ecRad
!     2023-09, P.Nabat : add PAEROCPL for TACTIC aerosol-radiation coupling
!-----------------------------------------------------------------------

USE TYPE_MODEL       , ONLY : MODEL
USE YOMDIMV          , ONLY : TDIMV
USE PARKIND1         , ONLY : JPIM, JPRB
USE YOMHOOK          , ONLY : DR_HOOK, JPHOOK, LHOOK
USE SPP_MOD_TYPE     , ONLY : APPLY_SPP, TSPP_CONFIG_TYPE 
USE CPG_OPTS_TYPE_MOD, ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE

!-----------------------------------------------------------------------

!*       0.1   ARGUMENTS.
!              ----------

IMPLICIT NONE

TYPE(TDIMV)            ,INTENT(IN)               :: YDDIMV
TYPE(MODEL)            ,INTENT(IN)               :: YDMODEL
TYPE(CPG_OPTS_TYPE)    ,INTENT(IN)               :: YDCPG_OPTS
TYPE(CPG_BNDS_TYPE)    ,INTENT(IN)               :: YDCPG_BNDS
INTEGER(KIND=JPIM)     ,INTENT(IN)               :: KOZOCL
INTEGER(KIND=JPIM)     ,INTENT(IN)               :: KMACCAERO
INTEGER(KIND=JPIM)     ,INTENT(IN)               :: KAERO
REAL(KIND=JPRB)        ,INTENT(IN)               :: PGELAM(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PGEMU(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PALBD(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PALBP(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PAPRS(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PAPRSF(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PNEB(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQO3(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQCO2(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQCH4(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQN2O(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQNO2(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQC11(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQC12(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQC22(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQCL4(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PAER(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,6) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PAERO(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,KAERO)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PAEROCPL(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_PHY_RAD%YRERAD%NTLW,MERGE (6,0,YDMODEL%YRML_PHY_RAD%YREAERATM%LAERRADCPL))
REAL(KIND=JPRB)        ,INTENT(IN)               :: PAERINDS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PDELP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PEMIS(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PMU0M(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQ(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQSAT(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQICE(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PQLIQ(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PLSM(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PT(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(IN)               :: PTS(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PPIZA_DST(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PCGA_DST(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PTAUREL_DST(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PEMTD(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PEMTU(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PTRSO(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PCTRSO(YDCPG_OPTS%KLON,0:1) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PCEMTR(YDCPG_OPTS%KLON,0:1) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PTRSOD(YDCPG_OPTS%KLON) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PLWFC(YDCPG_OPTS%KLON,0:1) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PLWFT(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG,1:YDCPG_OPTS%KTSSG+1) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PSWFC(YDCPG_OPTS%KLON,0:1) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PSWFT(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG,1:YDCPG_OPTS%KTSSG+1) 
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PSFSWDIR(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PSFSWDIF(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PFSDNN(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PFSDNV(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PTRSODIR(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB)        ,INTENT(OUT)              :: PTRSODIF(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PS(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PRR(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB)        ,INTENT(INOUT)            :: PSWDIR(YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB)        ,INTENT(INOUT)            :: PSWDIF(YDCPG_OPTS%KLON,YDCPG_OPTS%KSW)
REAL(KIND=JPRB)        ,INTENT(IN)               :: PMU0LU(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(INOUT)            :: PALB(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(INOUT)            :: PVRMOON(YDCPG_OPTS%KLON)
REAL(KIND=JPRB)        ,INTENT(IN)    ,OPTIONAL  :: PCLDROP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
TYPE(TSPP_CONFIG_TYPE) ,INTENT(INOUT) ,OPTIONAL  :: YSPP_RSWINHF,YSPP_RLWINHF 

!*       0.2   LOCAL ARRAYS.
!              -------------
REAL(KIND=JPRB) :: ZTH(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZRAER  (YDCPG_OPTS%KLON,6,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZRCLC  (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZRMU0  (YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZQLWP  (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG ) , ZQIWP (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG )
REAL(KIND=JPRB) :: ZPQO3  (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZQS    (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZQ     (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZCLDROP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZNBAS  (YDCPG_OPTS%KLON) , ZNTOP  (YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZCCNL  (YDCPG_OPTS%KLON) , ZCCNO  (YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZPFRSODC(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZPFRTEDC(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZRCH4(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG), ZRCO2(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG), ZRN2O(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG), ZRNO2(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZRC11(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG), ZRC12(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG), ZRC22(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG), ZRCL4(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) ::  ZPAERO  (YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG,KAERO)
REAL(KIND=JPRB) ::  ZPFDIR  (YDCPG_OPTS%KLON) , ZPFDIF   (YDCPG_OPTS%KLON), ZPCDIR         (YDCPG_OPTS%KLON)
REAL(KIND=JPRB) ::  ZFLUX_UV(YDCPG_OPTS%KLON) , ZFLUX_PAR(YDCPG_OPTS%KLON), ZFLUX_PAR_CLEAR(YDCPG_OPTS%KLON) 
!  output of radlswr

REAL(KIND=JPRB) :: ZFRTED(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZLWDERIVATIVE(YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZSWDIFFUSEBAND(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)
REAL(KIND=JPRB) :: ZSWDIRECTBAND(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NSW)

!  output of radlsw

REAL(KIND=JPRB) :: ZEMIT  (YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZFCT   (YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZFCS   (YDCPG_OPTS%KLON,0:YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB) :: ZFRSOD (YDCPG_OPTS%KLON),ZSUDU(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZPARF  (YDCPG_OPTS%KLON),ZUVDF(YDCPG_OPTS%KLON),ZPARCF(YDCPG_OPTS%KLON),ZTINCF(YDCPG_OPTS%KLON)
REAL(KIND=JPRB) :: ZRSWINHF(YDCPG_OPTS%KLON),ZRLWINHF(YDCPG_OPTS%KLON)

INTEGER(KIND=JPIM) :: JLEV, JLON, JSW, JEMIS
REAL(KIND=JPRB)    :: ZCRAE, ZEMIW(YDCPG_OPTS%KLON),ZFRAC, ZSW
REAL(KIND=JPRB)    :: ZSPECTRALEMISS(YDCPG_OPTS%KLON,YDMODEL%YRML_PHY_RAD%YRERAD%NLWEMISS)
REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE

#include "radlsw.intfb.h"
#include "radlswr.intfb.h"
#include "radiation_scheme_cpu.intfb.h"
#include "radiation_scheme_gpu.intfb.h"
#include "acralu.intfb.h"
! ================================================================================

IF (LHOOK) CALL DR_HOOK('RECMWF',0,ZHOOK_HANDLE)
ASSOCIATE(&
 & YDARPHY => YDMODEL%YRML_PHY_MF%YRARPHY  , YDERAD  => YDMODEL%YRML_PHY_RAD%YRERAD  , &
 & YDPHY3  => YDMODEL%YRML_PHY_MF%YRPHY3   , YDERDI  => YDMODEL%YRML_PHY_RAD%YRERDI  , &
 & YDPHY   => YDMODEL%YRML_PHY_MF%YRPHY    , YDRIP   => YDMODEL%YRML_GCONF%YRRIP     , & 
 & YDTOPH  => YDMODEL%YRML_PHY_MF%YRTOPH   , YDEAERD => YDMODEL%YRML_PHY_RAD%YREAERD , &
 & YDPARAR => YDMODEL%YRML_PHY_MF%YRPARAR  , YDCST   => YDMODEL%YRCST)
 
ASSOCIATE( LUSEPRE2017RAD=>YDERAD%LUSEPRE2017RAD      , &
 & LSRTM   =>YDERAD%LSRTM   , LRRTM  =>YDERAD%LRRTM   , &
 & NAER    =>YDERAD%NAER    , NSW    =>YDERAD%NSW     , &
 & REPH2O  =>YDERDI%REPH2O  , RRAE   =>YDERDI%RRAE    , &
 & RSWINHF =>YDERAD%RSWINHF,  RLWINHF=>YDERAD%RLWINHF,  &
 & LRDUST  =>YDARPHY%LRDUST , GQVTOP =>YDPARAR%GQVTOP , &
 & XCQVR   =>YDPARAR%XCQVR  , RII0   =>YDPHY3%RII0    , &
 & RMCH4   =>YDCST%RMCH4 )

ASSOCIATE (KLON => YDCPG_OPTS%KLON, KLEV => YDCPG_OPTS%KFLEVG, KIDIA => YDCPG_BNDS%KIDIA, KFDIA => YDCPG_BNDS%KFDIA)

!*       1.    PREPARATORY WORK
!              ----------------

!*       1.1    LOCAL CONSTANTS
!                ---------------

ZCRAE = RRAE*(RRAE+2.0_JPRB)

IF (PRESENT(PCLDROP)) THEN

!$ACDC ABORT {

  ZCLDROP(:,:) = PCLDROP(:,:)

!$ACDC }

ENDIF

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=PREPARE {

IF (KOZOCL==4.OR.KOZOCL==5.OR.KOZOCL==6) THEN ! note that H2O or PQ is not in the list below
! coupling of radiative and chemical water vapor is under CHEM_LCHQ key  
  ZRCH4(:,:)   = PQCH4(:,:)
  ZRN2O(:,:)   = PQN2O(:,:)
  ZRNO2(:,:)   = PQNO2(:,:)
  ZRC11(:,:)   = PQC11(:,:)
  ZRC12(:,:)   = PQC12(:,:)
  ZRC22(:,:)   = PQC22(:,:)
  ZRCL4(:,:)   = PQCL4(:,:)
  ZRCO2(:,:)   = PQCO2(:,:)
ELSE
  ZRCH4(:,:)   = YDERDI%RCH4
  ZRN2O(:,:)   = YDERDI%RN2O
  ZRNO2(:,:)   = YDERDI%RNO2
  ZRC11(:,:)   = YDERDI%RCFC11
  ZRC12(:,:)   = YDERDI%RCFC12
  ZRC22(:,:)   = YDERDI%RCFC22
  ZRCL4(:,:)   = YDERDI%RCCL4
  ZRCO2(:,:)   = YDERDI%RCARDI
ENDIF

IF (KAERO>0) THEN
  ZPAERO(:,:,:)= PAERO(:,:,:)
ELSE
  ZPAERO(:,:,:)= 0.0_JPRB
ENDIF

ZRAER(:,:,:) = 0.0_JPRB
ZNBAS(:)     = 1.0_JPRB
ZNTOP(:)     = 1.0_JPRB
ZCCNL(:)     = YDERAD%RCCNLND
ZCCNO(:)     = YDERAD%RCCNSEA

  !*       2.1    FULL-LEVEL QUANTITIES
DO JLEV=1,KLEV
  DO JLON=KIDIA,KFDIA
    ZPQO3(JLON,JLEV)=PQO3(JLON,JLEV)*PDELP(JLON,JLEV)
    ZRCLC(JLON,JLEV)=MAX( 0.0_JPRB ,MIN( 1.0_JPRB ,PNEB(JLON,JLEV)))
    IF(YDPARAR%LICERAD)THEN ! Use fraction of ice/snow/graupel near one instead of
!      fraction as cloud cover
       ZFRAC=PQICE(JLON,JLEV)/(PQLIQ(JLON,JLEV) + PQICE(JLON,JLEV) + 1.0E-15_JPRB)
       ZRCLC(JLON,JLEV) = ZFRAC + (1.0_JPRB-ZFRAC)*ZRCLC(JLON,JLEV) ! Higher cloud condensate
!      fraction when more cloudice/snow
    ENDIF
    IF (ZRCLC(JLON,JLEV) > YDERDI%REPCLC) THEN
      ZQLWP(JLON,JLEV)=PQLIQ(JLON,JLEV)
      ZQIWP(JLON,JLEV)=PQICE(JLON,JLEV)
    ELSE
      ZQLWP(JLON,JLEV)=REPH2O*ZRCLC(JLON,JLEV)
      ZQIWP(JLON,JLEV)=REPH2O*ZRCLC(JLON,JLEV)
    ENDIF
    ZQS (JLON,JLEV)=MAX(2.0_JPRB*REPH2O,PQSAT(JLON,JLEV))
    ZQ  (JLON,JLEV)=MAX(REPH2O,MIN(PQ(JLON,JLEV),ZQS(JLON,JLEV)*(1.0_JPRB-REPH2O)))
    IF(YDPARAR%LQVTOP) THEN
      ! qv in input to radiation is set at least to XCQVR*qsat, in order to have
      ! greater qv between 100 and 200 hPa, and thus larger radiative heating at
      ! those levels. At the top of atmosphere, the value GQVTOP is used.
      ! The transition between GQVTOP and  XCQVR*qsat is done by ZFRAC,
      ! depending on pressure.
      ZFRAC=MIN(1._JPRB,PAPRSF(JLON,JLEV)/YDPARAR%GQVPLIM)
      ZQ(JLON,JLEV)=ZFRAC*MAX(XCQVR*PQSAT(JLON,JLEV),ZQ(JLON,JLEV))+(1._JPRB-ZFRAC)*GQVTOP
    ENDIF
  ENDDO
ENDDO

DO JEMIS = 1, YDMODEL%YRML_PHY_RAD%YRERAD%NLWEMISS
  ZSPECTRALEMISS(:,JEMIS)=PEMIS(:)
ENDDO

DO JLON=KIDIA,KFDIA
  ZEMIW(JLON)=PEMIS(JLON)
ENDDO

IF (NAER == 0) THEN
  ZRAER=YDEAERD%RCAEROS
ELSE
  DO JLEV=1,KLEV
  DO JLON=KIDIA,KFDIA
    IF (YDPHY%LAEROLAN) ZRAER(JLON,1,JLEV)=PAER(JLON,JLEV,1)
    IF (YDPHY%LAEROSEA) ZRAER(JLON,2,JLEV)=PAER(JLON,JLEV,2)
    IF (YDPHY%LAERODES) ZRAER(JLON,3,JLEV)=PAER(JLON,JLEV,3)
    IF (YDPHY%LAEROSOO) ZRAER(JLON,4,JLEV)=PAER(JLON,JLEV,4)
    IF (YDPHY%LAEROVOL) ZRAER(JLON,5,JLEV)=YDEAERD%RCAEROS
    IF (YDPHY%LAEROSUL) ZRAER(JLON,6,JLEV)=PAER(JLON,JLEV,6)
  ENDDO
  ENDDO
ENDIF

!*       2.2    HALF-LEVEL TEMPERATURE

DO JLEV=2,KLEV
  DO JLON=KIDIA,KFDIA
    ZTH(JLON,JLEV-1)=&
     & (PT(JLON,JLEV-1)*PAPRSF(JLON,JLEV-1)*(PAPRSF(JLON,JLEV)-PAPRS(JLON,JLEV-1))&
     & +PT(JLON,JLEV)*PAPRSF(JLON,JLEV)*(PAPRS(JLON,JLEV-1)-PAPRSF(JLON,JLEV-1)))&
     & *(1.0_JPRB/(PAPRS(JLON,JLEV-1)*(PAPRSF(JLON,JLEV)-PAPRSF(JLON,JLEV-1))))  
  ENDDO
ENDDO  
DO JLON=KIDIA,KFDIA
  ZTH(JLON,KLEV)=PTS(JLON)
  ZTH(JLON,0)=PT(JLON,1)-PAPRSF(JLON,1)*(PT(JLON,1)-ZTH(JLON,1))&
  & /(PAPRSF(JLON,1)-PAPRS(JLON,1))  
ENDDO

!*       3.1     SOLAR ZENITH ANGLE IS EARTH'S CURVATURE CORRECTED

DO JLON=KIDIA,KFDIA
  IF (PMU0M(JLON) > 1.E-10_JPRB) THEN
    ZRMU0(JLON)=RRAE/(SQRT(PMU0M(JLON)**2+ZCRAE)-PMU0M(JLON))
  ELSE
    ZRMU0(JLON)= RRAE/SQRT(ZCRAE)
  ENDIF
ENDDO

!$ACDC }

! Setup SPP patterns
IF (PRESENT(YSPP_RSWINHF)) THEN

!$ACDC ABORT {

  IF (YSPP_RSWINHF%LPERT) THEN
    CALL APPLY_SPP(YSPP_RSWINHF,KLON,KIDIA,KFDIA,RSWINHF,ZRSWINHF)
  ELSE
    ZRSWINHF(KIDIA:KFDIA) = RSWINHF
  ENDIF

!$ACDC }

ELSE

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

  ZRSWINHF(KIDIA:KFDIA) = RSWINHF

!$ACDC }

ENDIF

IF (PRESENT(YSPP_RLWINHF)) THEN

!$ACDC ABORT {

  IF (YSPP_RLWINHF%LPERT) THEN
    CALL APPLY_SPP(YSPP_RLWINHF,KLON,KIDIA,KFDIA,RLWINHF,ZRLWINHF)
  ELSE
    ZRLWINHF(KIDIA:KFDIA) = RLWINHF
  ENDIF

!$ACDC }

ELSE

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

  ZRLWINHF(KIDIA:KFDIA) = RLWINHF

!$ACDC }

ENDIF

!*       2.    CALL TO THE RADIATION SCHEME
!              ----------------------------

IF (LSRTM .and. LUSEPRE2017RAD) THEN

!$ACDC PARALLEL, TARGET=OpenMP, NAME=SRTM {

!**** 2.a/ CALL TO RADIATION SCHEME WITH RRTM LW+SW  (SRTM)

  CALL RADLSWR(                                                              &
! ---- INPUTS ----------------------------------------------------------------
   & YDDIMV  , YDMODEL , KIDIA    , KFDIA   , KLON , KLEV   , KAERO  , RII0  ,        &
   & ZRAER    , ZPAERO  , PALBD   , PALBP  , PAPRS  , PAPRSF ,                &
   & ZCCNL   , ZCCNO   , PGELAM  , PGEMU  ,                                  &
   & ZRCO2   , ZRCH4   , ZRN2O   , ZRNO2  , ZRC11  , ZRC12  , ZRC22 , ZRCL4 ,&
   & PNEB   , PDELP     , PEMIS   , ZEMIW  , PLSM   , ZRMU0  , ZPQO3 ,        &
   & ZQ      , ZQIWP   , PS   , ZQLWP  , PRR  ,                         &
   & ZTH     , PT     , PTS     ,                                           &
! ---- OUTPUTS ---------------------------------------------------------------
   &  ZEMIT, ZFCT  , PLWFT (:,:,1) , ZFCS , PSWFT (:,:,1)  , ZFRSOD, &
   &  ZFRTED, ZPFRSODC, ZPFRTEDC ,&
   &  ZSUDU, ZUVDF , ZPARF, ZPARCF, ZTINCF, &
   &  ZPFDIR, ZPFDIF, ZPCDIR, &
   &  ZLWDERIVATIVE, ZSWDIFFUSEBAND, ZSWDIRECTBAND )

!$ACDC }

ELSEIF (LRRTM .and. .not. LSRTM .and. LUSEPRE2017RAD) THEN

!$ACDC PARALLEL, TARGET=OpenMP, NAME=RRTM {

!**** 2.b/ CALL TO RADIATION SCHEME WITH RRTM LW ONLY

  CALL RADLSW(                                                                 &
  ! ---- INPUTS ------------------------------------------------------------------
   & YDDIMV,YDMODEL%YRML_PHY_RAD,YDMODEL%YRML_PHY_SLIN%YREPHLI,YDMODEL%YRML_PHY_SLIN%YRPHNC,YDMODEL%YRML_PHY_EC%YRECLD, &
   & YDMODEL%YRML_PHY_MF%YRPARAR, &
   & KIDIA    , KFDIA     , KLON  , KLEV    , YDERAD%NMODE  , NAER ,                  &
   & RII0    ,                                                                 &
   & ZRAER    , PALBD    , PALBP   , PAPRS   , PAPRSF ,                         &
   & ZCCNL   , ZCCNO    ,                                                      &
   & YDERDI%RCARDI , ZRCLC    , PDELP     , PEMIS   , ZEMIW  , PLSM  , ZRMU0 , ZPQO3,  &
   & ZQ      , ZQIWP    , ZQLWP   , ZQS     ,                                  &
   & ZTH     , PT      , PTS     , ZNBAS   , ZNTOP  ,                         &
  ! ---- OUTPUTS -----------------------------------------------------------------
   & ZEMIT , ZFCT   , PLWFT (:,:,1)    , ZFCS    , PSWFT (:,:,1),&
   & ZFRSOD, ZSUDU  , ZPARF   , ZUVDF   , ZPARCF, ZTINCF, PSFSWDIR,&
   & PSFSWDIF,PFSDNN,PFSDNV   ,&
   & LRDUST,PPIZA_DST,PCGA_DST,PTAUREL_DST,PAERINDS, &
   & ZRSWINHF,ZRLWINHF,ZCLDROP)

!$ACDC }

ELSEIF ( .not. LUSEPRE2017RAD ) THEN

!**** 2.c/ CALL TO MODULAR RADIATION SCHEME ECRAD

  IF (YDERAD%CECRADARCH == 'CPU') THEN

!$ACDC PARALLEL, TARGET=OpenMP, NAME=ECRAD_CPU {

    CALL RADIATION_SCHEME_CPU (YDMODEL                            , &
    ! ---- INPUTS -------------------------------------------------------
     & KIDIA     , KFDIA     , KLON     , KLEV       , KAERO      , &
     & RII0     , ZRMU0    , PTS        , PALBD      , PALBP      , &
     & ZSPECTRALEMISS      ,                                        &
     & ZCCNL    , ZCCNO    ,                                        &
     & PGELAM   , PGEMU    , PLSM       ,                           &
     & PAPRSF   , PT       ,                                        &
     & PAPRS    , ZTH      ,                                        &
     & ZQ       , ZRCO2    , ZRCH4      , ZRN2O      , ZRNO2      , &
     & ZRC11    , ZRC12    , ZRC22      , ZRCL4      , ZPQO3      , &
     & ZRCLC    , ZQLWP    , ZQIWP      , PRR        , PS         , &
     & ZRAER    , ZPAERO   , PAEROCPL   ,                           &
    ! ---- OUTPUTS ------------------------------------------------------
     & PSWFT (:,:,1)    , PLWFT (:,:,1)    , ZFCS       , ZFCT    , &
     & ZFRSOD   , ZFRTED   , ZPFRSODC   , ZPFRTEDC   ,              &
     & ZPFDIR   , ZPCDIR   , ZSUDU      ,                           &
     & ZFLUX_UV , ZFLUX_PAR, ZFLUX_PAR_CLEAR         ,              &
     & ZTINCF   , ZEMIT    ,                                        &
     & ZLWDERIVATIVE       ,                                        &
     & ZSWDIFFUSEBAND      ,  ZSWDIRECTBAND )

!$ACDC }

  ENDIF

  IF (YDERAD%CECRADARCH == 'GPU') THEN

!$ACDC PARALLEL, TARGET=OpenMP, NAME=ECRAD_GPU {

    CALL RADIATION_SCHEME_GPU (YDMODEL                            , &
    ! ---- INPUTS -------------------------------------------------------
     & KIDIA     , KFDIA     , KLON     , KLEV       , KAERO      , &
     & RII0     , ZRMU0    , PTS        , PALBD      , PALBP      , &
     & ZSPECTRALEMISS      ,                                        &
     & ZCCNL    , ZCCNO    ,                                        &
     & PGELAM   , PGEMU    , PLSM       ,                           &
     & PAPRSF   , PT       ,                                        &
     & PAPRS    , ZTH      ,                                        &
     & ZQ       , ZRCO2    , ZRCH4      , ZRN2O      , ZRNO2      , &
     & ZRC11    , ZRC12    , ZRC22      , ZRCL4      , ZPQO3      , &
     & ZRCLC    , ZQLWP    , ZQIWP      , PRR        , PS         , &
     & ZRAER    , ZPAERO   , PAEROCPL   ,                           &
    ! ---- OUTPUTS ------------------------------------------------------
     & PSWFT (:,:,1)    , PLWFT (:,:,1)    , ZFCS       , ZFCT    , &
     & ZFRSOD   , ZFRTED   , ZPFRSODC   , ZPFRTEDC   ,              &
     & ZPFDIR   , ZPCDIR   , ZSUDU      ,                           &
     & ZFLUX_UV , ZFLUX_PAR, ZFLUX_PAR_CLEAR         ,              &
     & ZTINCF   , ZEMIT    ,                                        &
     & ZLWDERIVATIVE       ,                                        &
     & ZSWDIFFUSEBAND      ,  ZSWDIRECTBAND , LACC=YDERAD%LECRADACC )

!$ACDC }

  ENDIF


!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=ECRAD_POST {

  ZPFDIF(KIDIA:KFDIA) = ZFRSOD(KIDIA:KFDIA) - ZPFDIR(KIDIA:KFDIA)

!$ACDC }

ENDIF

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn, NAME=FINISH {

DO JLON=KIDIA,KFDIA
  PSWFC (JLON, 0)=ZFCS(JLON,     0)
  PSWFC (JLON, 1)=ZFCS(JLON,KLEV+0)
  PLWFC (JLON, 0)=ZFCT(JLON,     0)
  PLWFC (JLON, 1)=ZFCT(JLON,KLEV+0)
ENDDO

PEMTD=PLWFT (:,:,1)
PEMTU=PLWFT (:,:,1)
PCEMTR=PLWFC


IF (LSRTM .AND. .NOT. LUSEPRE2017RAD) THEN   ! EcRad case
  DO JSW=1,NSW
    DO JLON=KIDIA,KFDIA
      PTRSODIR(JLON,JSW)=ZSWDIRECTBAND(JLON,JSW)*(1.-PALBP(JLON,JSW))/(RII0*ZRMU0(JLON))
      PTRSODIF(JLON,JSW)=ZSWDIFFUSEBAND(JLON,JSW)*(1.-PALBD(JLON,JSW))/(RII0*ZRMU0(JLON))
    ENDDO
  ENDDO      
ELSEIF (LSRTM .AND. LUSEPRE2017RAD) THEN     ! LSRTM / RRTM case
  ZSW = REAL(NSW,JPRB)
  DO JSW=1,NSW
     DO JLON=KIDIA,KFDIA
       PTRSODIR(JLON,JSW)=ZPFDIR(JLON)*(1.-PALBP(JLON,JSW))/ZSW/(RII0*ZRMU0(JLON))
       PTRSODIF(JLON,JSW)=ZPFDIF(JLON)*(1.-PALBD(JLON,JSW))/ZSW/(RII0*ZRMU0(JLON))
    ENDDO
  ENDDO      
ELSE                                         ! Other cases (SW and RRTM)
  DO JSW=1,NSW
     DO JLON=KIDIA,KFDIA
       PTRSODIR(JLON,JSW)=PSFSWDIR(JLON,JSW)*(1.-PALBP(JLON,JSW))/(RII0*ZRMU0(JLON))
       PTRSODIF(JLON,JSW)=PSFSWDIF(JLON,JSW)*(1.-PALBD(JLON,JSW))/(RII0*ZRMU0(JLON))
     ENDDO
  ENDDO
ENDIF

!*         4.2     TRANSFORM FLUXES TO MODEL HISTORICAL VARIABLES

DO JLEV=0,KLEV
  DO JLON=KIDIA,KFDIA
    PTRSO(JLON,JLEV)=PSWFT(JLON,JLEV,1)/(RII0*ZRMU0(JLON))
  ENDDO
ENDDO

DO JLEV=0,1
  DO JLON=KIDIA,KFDIA
    PCTRSO(JLON,JLEV)=PSWFC(JLON,JLEV)/(RII0*ZRMU0(JLON))
  ENDDO
ENDDO

DO JLON=KIDIA,KFDIA
  PTRSOD(JLON)=MAX(0.0_JPRB,MIN(1.0_JPRB,ZFRSOD(JLON)/(RII0*ZRMU0(JLON))))
ENDDO

!*         7.3   RECONSTRUCT FLUXES FOR DIAGNOSTICS

DO JLON=KIDIA,KFDIA
  IF (PMU0M(JLON) < 1.E-10_JPRB) THEN
    ZRMU0(JLON)=0.0_JPRB
    PSFSWDIR(JLON,:)=0.0_JPRB
    PSFSWDIF(JLON,:)=0.0_JPRB
  ENDIF
ENDDO

IF (YDARPHY%LMSE) THEN
  IF (NSW==6) THEN
    DO JSW=1,NSW
      PSWDIR(KIDIA:KFDIA,JSW)= PTRSODIR(KIDIA:KFDIA,JSW)
      PSWDIF(KIDIA:KFDIA,JSW)= PTRSODIF(KIDIA:KFDIA,JSW)
    ENDDO
  ELSEIF (NSW==1) THEN
    PSWDIR(KIDIA:KFDIA,1)=PFSDNN(KIDIA:KFDIA)+PFSDNV(KIDIA:KFDIA)
    PSWDIF(KIDIA:KFDIA,1)=0.0_JPRB
  ENDIF
ENDIF


IF (YDPHY%LRAYLU) THEN
   CALL ACRALU (YDRIP,YDPHY3,KIDIA,KFDIA,KLON,YDTOPH%NTRADI,KLEV,&
       & PAPRS, PAPRSF, PDELP, PNEB, PQ, ZRCO2, PQICE, PQLIQ, PQO3, PT,&
       & PALB,PMU0LU, PVRMOON, PAER)
ENDIF

!$ACDC }

END ASSOCIATE
END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('RECMWF',1,ZHOOK_HANDLE)
END SUBROUTINE RECMWF


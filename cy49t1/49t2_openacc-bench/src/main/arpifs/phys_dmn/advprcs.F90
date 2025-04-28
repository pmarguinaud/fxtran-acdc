SUBROUTINE ADVPRCS (YDCST, YDML_PHY_MF,KIDIA, KFDIA, KLON, KTDIA, KFLEV,&
 & PT, PQ, PQL, PQI, PAUTOL, PAUTOI, &
 & PQR, PQS, PNEB, &
 & PCP, PR, PAPHI, PAPRSF, PDELP,&
 & PFPLSL, PFPLSN, PFPEVPL, PFPEVPN, PFPFPL, PFPFPN, PSEDIQL, PSEDIQN )  
!DEC$ OPTIMIZE:3

! ========================================================

!   THIS ROUTINE PERFORMS THE VERTICAL ADVECTION OF
!   PRECIPITATION PARTICLES.
!   IT ALSO COMPUTES COLLECTION AND EVAPORATION PROCESSES.

! ========================================================

!   Auteur: Yves Bouteloup, CNRM/GMAP FROM ADVPRC

!   Date: 2006-06   

!     Modifications.
!     --------------
!     2006-10-30, F. Bouyssel : Introduction of RHEVAP and ZALPHA
!     2007-04-06, F. Bouyssel : Change in precipitation evaporation (LLEVAPX)
!     2008-01-24, Y. Bouteloup : Change in taking account of the melting (like evaporation for snow !)
! This is very important. Each disparition process for a species must be treated as this
!     2010-04-06, F. Bouyssel : Sedimentation speed for clouds
!     2010-04-30, Y. Bouteloup : Freezing of rain + some cleaning and simplifications
!     2010-12-03, F. Bouyssel : Removal of ZQFRZ=0=ZQFRZX
!     2011-06-08, O. Riviere: Introduction of LSMOOTHMELT to smooth melting
!     around 0°C
!      R. El Khatib 22-Jul-2014 Vectorizations
!      R. El Khatib 12-Aug-2016 optimization by directive
!      Y. Bouteloup 15-mar-2017 Bug correction (Back to old formulation at the
!                               begining of statistical advection)
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
! ========================================================

! ---------------
! INPUT VARIABLES
! ---------------

! KIDIA, : DEBUT/FIN DES BOUCLES HORIZONTALES (IST,IEND DANS CPG).
! KFDIA  : START/END OF HORIZONTAL LOOP       (IST,IEND IN   CPG).
! KLON   : DIMENSION HORIZONTALE              (NPROMA   DANS CPG).
!        : HORIZONTAL DIMENSION               (NPROMA   IN   CPG).
! KTDIA  : INDICE DE DEPART DES BOUCLES VERTICALES.
!        : START OF THE VERTICAL LOOP IN THE PHYSICS.
! KLEV   : FIN BOUCLE VERTICALES, DIMENSION VERTICALE (NFLEVG DANS CPG).
!        : END OF VERTICAL LOOP, VERTICAL DIMENSION   (NFLEVG IN   CPG).

! PT     : TEMPERATURE.
!        : TEMPERATURE.
! PQ     : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
!        : SPECIFIC HUMIDITY OF WATER VAPOUR.
! PQL    : QUANTITE SPECIFIQUE D'EAU CONDENSEE LIQUIDE
!        : LIQUID CONDENSED WATER SPECIFIC HUMIDITY
! PQI    : QUANTITE SPECIFIQUE D'EAU CONDENSEE SOLIDE
!        : SOLID CONDENSED WATER SPECIFIC HUMIDITY
! PAUTOL : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE LIQ.
!        : GENERATION OF PRECIPITATION FROM LIQUID CLOUD WATER (ACMICRO). 
! PAUTOI : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE SOLIDE.
!        : GENERATION OF PRECIPITATION FROM SOLID CLOUD WATER (ACMICRO). 
! PQR    : QUANTITE SPECIFIQUE D'EAU PRECIPITANTE LIQUIDE.
!        : LIQUID PRECIPITATING WATER SPECIFIC HUMIDITY.
! PQS    : QUANTITE SPECIFIQUE D'EAU PRECIPITANTE SOLIDE.
!        : SOLID PRECIPITATING WATER SPECIFIC HUMIDITY.
! PNEB   : NEBULOSITE TOTALE
!        : TOTAL CLOUDINESS  
! PCP    : CHALEUR MASSIQUE A PRESSION CONSTANTE DE L'AIR.
!        : SPECIFIC HEAT AT CONSTANT PRESSURE FOR AIR.
! PR     : CONSTANTE DES GAZ POUR L'AIR.
!        : GAS CONSTANT FOR AIR.
! PAPHI  : GEOPOTENTIEL SUR DEMI-NIVEAUX.
!        : GEOPOTENTIAL ON HALF-LEVELS.
! PAPRSF : PRESSION SUR LES NIVEAUX PLEINS.
!        : PRESSURE ON FULL LEVELS.
! PDELP  : EPAISSEUR EN PRESSION DE LA COUCHE.
!        : LAYER THICKNESS IN PRESSURE UNITS.

! ---------------
! OUTPUT VARIABLES
! ---------------

! PFPLSL  : FLUX DE PRECIPITATION LIQUIDE (PLUIE).
!         : RAIN FLUX.
! PFPLSN  : FLUX DE PRECIPITATION SOLIDE  (NEIGE).
!         : ICE PRECIPITATION FLUX.
! PFPEVPL : FLUX ASSOCIE A L'EVAPORATION DES PRECIP.
!         : FLUX ASSOCIATED TO EVAPORATION OF PRECIPITATIONS.
! PFPEVPN : FLUX ASSOCIE A LA SUBLIMATION DES PRECIP.
!         : FLUX ASSOCIATED TO SUBLIMATION OF PRECIPITATIONS.
! PFPFPL  : FLUX DE GENERATION DE PRECIPITATIONS LIQUIDES.
!         : FLUX OF LIQUID PRECIPITATION GENERATION.
! PFPFPN  : FLUX DE GENERATION DE PRECIPITATIONS SOLIDES.
!         : FLUX OF SOLID PRECIPITATION GENERATION.
! PSEDIQL : FLUX SEDIMENTATION D'EAU LIQUIDE NUAGEUSE.
!         : FLUX SEDIMENTATION OF CLOUD LIQUID WATER.
! PSEDIQN : FLUX SEDIMENTATION D'EAU SOLIDE NUAGEUSE.
!         : FLUX SEDIMENTATION OF CLOUD SOLID WATER.

! ========================================================

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1             , ONLY : JPIM     ,JPRB
USE YOMHOOK              , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST               , ONLY : TCST


IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(IN):: YDML_PHY_MF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT     (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ     (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL    (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI    (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAUTOL (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAUTOI (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQR    (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS    (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEB   (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCP    (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR     (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI  (KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP  (KLON,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSL (KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSN (KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPL(KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPN(KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPL (KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPN (KLON,0:KFLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQL(KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQN(KLON,0:KFLEV)

!REAL(KIND=JPRB), EXTERNAL :: FCGENERALIZED_GAMMA
REAL(KIND=JPRB) :: ZEPS,ZFVELR,ZFVELS,ZTMELT,ZRHOW,ZNRHOW      &
 & , ZDVISC,ZSQTVIS,ZCDARV,ZRHOREF,ZEXP1,ZEXP4,ZEXP6           &
 & , ZPREF,ZCLEAR,ZKDIFF,ZFACT3,ZFACT4                    &
 & , ZSSATW,ZCONDT,ZDIFFV,ZCEV,ZCSU                   &
 & , ZSSATI,ZQR,ZQS                                &
 & , ZACCR,ZAGGR,ZRIMI                             &
 & , ZCOEFF1,ZCOEFF2,ZCOEFF2B,ZCOEFF3,ZCOEFF4,ZCOEFF5,ZCOEFF6  &
 & , ZNU1,ZNU2,ZTAU1,ZTAU2,ZSIGMA1,ZSIGMA2                     &
 & , ZFVENTR1,ZFVENTR2,ZFVENTS1,ZFVENTS2                       &
 & , ZLHFUS,ZSUBSA,ZEVAPPL,ZEVAPPN,ZINT1,ZQMLTX,ZQFRZX,ZQFRZ   &
 & , ZTQEVAPPL,ZTQEVAPPN,ZTCOLLL,ZTCOLLN,ZQFPFPL,ZQFPFPN,ZQMLT &
 & , ZQPRTOT1,ZQPSTOT1,ZQPSTOT2,ZQPRTOT2       &
 & , ZALPHA,ZDZS, ZP1, ZP2, ZP3, ZDZL, ZDZL0, ZDZL1, ZDZI, ZP1L, ZP2L, ZP1I, ZP2I
REAL(KIND=JPRB) :: ZWORK1(KLON), ZWORK2(KLON), ZWORK3(KLON), ZPOW1(KLON), ZPOW2(KLON)
REAL(KIND=JPRB) :: ZQPSTOT(KLON), ZDZ(KLON)

REAL(KIND=JPRB) :: ZRHO(KLON,KFLEV)                 &
 & , ZALTIH   (KLON,0:KFLEV)                        &
 & , ZDPSG    (KLON,KFLEV)  , ZDPSGDT  (KLON,KFLEV) &
 & , ZDELT    (KLON,KFLEV)  , ZEFFA    (KLON,KFLEV) &
 & , ZNS      (KLON,KFLEV)                          &
 & , ZCEV1    (KLON,KFLEV)  , ZCEV2    (KLON,KFLEV) &
 & , ZCSU1    (KLON,KFLEV)  , ZCSU2    (KLON,KFLEV) &
 & , ZCAGG    (KLON,KFLEV)  , ZCACC    (KLON,KFLEV) &
 & , ZCRIM    (KLON,KFLEV)  , ZFVEL    (KLON,KFLEV) &
 & , ZQL      (KLON,KFLEV)  , ZQI      (KLON,KFLEV) &
 & , ZQPR     (KLON,KFLEV)  , ZQPS     (KLON,KFLEV) &
 & , ZQSATW   (KLON,KFLEV)  , ZQSATI   (KLON,KFLEV) &
 & , ZAUTOL   (KLON,KFLEV)  , ZAUTOI   (KLON,KFLEV)
 

LOGICAL :: LLMELTS,LLFREEZ
LOGICAL :: LLEVAPX
INTEGER(KIND=JPIM) :: JLEV, JLON
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "fcttrm.func.h"

! --------------------------------------------------------

!     CHECK RELIABILITY OF INPUT ARGUMENTS.

IF (LHOOK) CALL DR_HOOK('ADVPRCS',0,ZHOOK_HANDLE)
ASSOCIATE(RACCEF=>YDML_PHY_MF%YRPHY0%RACCEF, RSMOOTHMELT=>YDML_PHY_MF%YRPHY0%RSMOOTHMELT, REVASX=>YDML_PHY_MF%YRPHY0%REVASX, &
 & TFVL=>YDML_PHY_MF%YRPHY0%TFVL, TFVLD=>YDML_PHY_MF%YRPHY0%TFVLD, TFVI=>YDML_PHY_MF%YRPHY0%TFVI, &
 & RRIMEF=>YDML_PHY_MF%YRPHY0%RRIMEF, RNINTS=>YDML_PHY_MF%YRPHY0%RNINTS, RNINTR=>YDML_PHY_MF%YRPHY0%RNINTR, &
 & RAGGEF=>YDML_PHY_MF%YRPHY0%RAGGEF, TFVR=>YDML_PHY_MF%YRPHY0%TFVR, TFVS=>YDML_PHY_MF%YRPHY0%TFVS, &
 & TSPHY=>YDML_PHY_MF%YRPHY2%TSPHY, &
 & RG=>YDCST%RG, RPI=>YDCST%RPI, RTT=>YDCST%RTT, RV=>YDCST%RV, &
 & LSMOOTHMELT=>YDML_PHY_MF%YRPHY%LSMOOTHMELT, LCOLLEC=>YDML_PHY_MF%YRPHY%LCOLLEC, &
 & LEVAPP=>YDML_PHY_MF%YRPHY%LEVAPP)
ZDZL0  = TFVL*TSPHY
ZDZL1  = (TFVL+TFVLD)*TSPHY
ZDZI   = TFVI*TSPHY

!- - - - - - - - - - - - - - -
IF (TSPHY > 0.0_JPRB) THEN
!- - - - - - - - - - - - - - -

   LLMELTS  = YDML_PHY_MF%YRPHY0%YRADVPRCS%LLMELTS  
   LLFREEZ  = YDML_PHY_MF%YRPHY0%YRADVPRCS%LLFREEZ  
   LLEVAPX  = YDML_PHY_MF%YRPHY0%YRADVPRCS%LLEVAPX  
   ZEPS     = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZEPS     
   ZNU1     = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZNU1     
   ZNU2     = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZNU2     
   ZTAU1    = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZTAU1    
   ZTAU2    = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZTAU2    
   ZSIGMA1  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZSIGMA1  
   ZSIGMA2  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZSIGMA2  
   ZFVENTR1 = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZFVENTR1 
   ZFVENTR2 = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZFVENTR2 
   ZFVENTS1 = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZFVENTS1 
   ZFVENTS2 = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZFVENTS2 
   ZFVELR   = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZFVELR   
   ZFVELS   = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZFVELS   
   ZTMELT   = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZTMELT   
   ZRHOW    = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZRHOW    
   ZNRHOW   = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZNRHOW   
   ZDVISC   = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZDVISC   
   ZSQTVIS  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZSQTVIS  
   ZCDARV   = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCDARV   
   ZRHOREF  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZRHOREF  
   ZEXP1    = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZEXP1    
   ZEXP4    = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZEXP4    
   ZEXP6    = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZEXP6    
   ZPREF    = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZPREF    
   ZCOEFF1  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCOEFF1  
   ZCOEFF2  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCOEFF2  
   ZCOEFF2B = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCOEFF2B 
   ZCOEFF3  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCOEFF3  
   ZCOEFF4  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCOEFF4  
   ZCOEFF5  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCOEFF5  
   ZCOEFF6  = YDML_PHY_MF%YRPHY0%YRADVPRCS%ZCOEFF6  

      ! ---------------
      ! Initializations
      ! ---------------

  DO JLEV = 0, KFLEV
    DO JLON = KIDIA, KFDIA
      ZALTIH(JLON,JLEV) = PAPHI(JLON,JLEV) / RG 
    ENDDO
  ENDDO

    ! ==========================
    ! COMPUTE DENSITY, THICKNESS
    ! ==========================

  DO JLEV = KTDIA, KFLEV
    DO JLON = KIDIA, KFDIA
      ZDPSG(JLON,JLEV) = PDELP(JLON,JLEV) / RG
      ZDPSGDT(JLON,JLEV) = ZDPSG(JLON,JLEV) * TSPHY
      ZDELT(JLON,JLEV) = PT(JLON,JLEV) - RTT
      ZRHO(JLON,JLEV) = PAPRSF(JLON,JLEV) / PR(JLON,JLEV)&
       & / PT(JLON,JLEV)  
      ZQPR(JLON,JLEV) = PQR(JLON,JLEV)
      ZQPS(JLON,JLEV) = PQS(JLON,JLEV)
      ZAUTOL(JLON,JLEV) = PAUTOL(JLON,JLEV) * ZDPSGDT(JLON,JLEV)
      ZAUTOI(JLON,JLEV) = PAUTOI(JLON,JLEV) * ZDPSGDT(JLON,JLEV)
    ENDDO        
  ENDDO  

     ! ======================================
     ! OTHER INITIALIZATIONS FOR MICROPHYSICS
     ! ======================================

  DO JLEV=KTDIA,KFLEV
!   Isolate in a loop what may not vectorize:
    DO JLON=KIDIA,KFDIA
      ZWORK1(JLON)=FOEW(PT(JLON,JLEV),0.0_JPRB)
      ZWORK2(JLON)=FOEW(PT(JLON,JLEV),1.0_JPRB)
      ZWORK3(JLON)= ( ZRHOREF / ZRHO(JLON,JLEV) )**0.4_JPRB
    ENDDO
!   This loop should vectorize:
    DO JLON=KIDIA,KFDIA

      ZALPHA=MAX(ZEPS,PQS(JLON,JLEV))/MAX(ZEPS,PQR(JLON,JLEV)+PQS(JLON,JLEV))
      ZFVEL(JLON,JLEV) = ZALPHA*ZFVELS + (1.0_JPRB - ZALPHA)*ZFVELR 
       ! -----------------------------------------------------------
       ! Efficiency for ice aggregation as a function of temperature.
       ! -----------------------------------------------------------
      ZEFFA(JLON,JLEV) = EXP(0.025_JPRB * ZDELT(JLON,JLEV))

       ! ---------------------------------------------------------
       ! Intercept parameter for ice as a function of temperature.
       ! ---------------------------------------------------------
      ZNS(JLON,JLEV) = RNINTS * EXP(-0.1222_JPRB * ZDELT(JLON,JLEV))

      ZQL(JLON,JLEV) = MAX(0.0_JPRB,PQL(JLON,JLEV)&
       & -PAUTOL(JLON,JLEV)*TSPHY)
      ZQI(JLON,JLEV) = MAX(0.0_JPRB,PQI(JLON,JLEV)&
       & -PAUTOI(JLON,JLEV)*TSPHY)

      ZCLEAR = 1.0_JPRB - PNEB(JLON,JLEV)
      ZKDIFF = 2.E-5_JPRB * ZPREF / PAPRSF(JLON,JLEV)
      ZFACT3 = (ZSQTVIS * ZKDIFF)**ZEXP1
      ZFACT4 = RV * PT(JLON,JLEV) / ZKDIFF

       ! -----------------------
       ! For evaporation of rain
       ! -----------------------
      ZQSATW(JLON,JLEV) = FOQS(ZWORK1(JLON)/PAPRSF(JLON,JLEV))
      ZSSATW = 1.0_JPRB - PQ(JLON,JLEV)/ZQSATW(JLON,JLEV)

      ZCONDT = ( FOLH(PT(JLON,JLEV),0.0_JPRB)/PT(JLON,JLEV) )**2 /ZCDARV
      ZDIFFV = ZFACT4 / ZWORK1(JLON)

      ZCEV = ZSSATW * ZCLEAR * RNINTR&
       & / ZRHO(JLON,JLEV) / (ZCONDT + ZDIFFV)  
      ZCEV = MAX(0.0_JPRB,ZCEV)
      ZCEV1(JLON,JLEV) = ZCEV * ZCOEFF3 
      ZCEV2(JLON,JLEV) = ZCEV * ZCOEFF4 / ZFACT3

       ! -----------------------
       ! For sublimation of snow
       ! -----------------------
      ZQSATI(JLON,JLEV) = FOQS(ZWORK2(JLON)/PAPRSF(JLON,JLEV))
      ZSSATI = 1.0_JPRB - PQ(JLON,JLEV)/ZQSATI(JLON,JLEV)

      ZCONDT = ( FOLH(PT(JLON,JLEV),1.0_JPRB)/PT(JLON,JLEV) )**2 /ZCDARV
      ZDIFFV = ZFACT4 / ZWORK2(JLON)

      ZCSU = ZSSATI * ZCLEAR * ZNS(JLON,JLEV)&
       & / ZRHO(JLON,JLEV) / (ZCONDT + ZDIFFV)  
      ZCSU = MAX(0.0_JPRB,ZCSU)
      ZCSU1(JLON,JLEV) = ZCSU * ZCOEFF5
      ZCSU2(JLON,JLEV) = ZCSU * ZCOEFF6 / ZFACT3

       ! ------------------------
       ! For collection processes
       ! ------------------------
      ZCACC(JLON,JLEV) = ZCOEFF1 * ZWORK3(JLON)
      ZCRIM(JLON,JLEV) = ZCOEFF2 * ZWORK3(JLON)
      ZCAGG(JLON,JLEV) = ZCOEFF2B * ZWORK3(JLON) * ZEFFA(JLON,JLEV)

    ENDDO
  ENDDO

    ! =============================================
    ! PERFORM STATISTICAL ADVECTION OF PRECIPITATION
    ! =============================================

    !-- -- -- -- -- --
  DO JLEV=KTDIA,KFLEV  
    !-- -- -- -- -- --

    IF (JLEV < KFLEV) THEN
      ZDZL = ZDZL0
    ELSE
      ZDZL = ZDZL1
    ENDIF

      ! =================================================
      ! First computation of total rain and snow which fall  
      ! through the curent level. Only 3 terms at this stage :
      ! 1 ==> Initial contents
      ! 2 ==> Flux from the upper level
      ! 3 ==> Autoconversion flux 
      
      ! In this version there is only one falling speed, depending of 
      ! the nature of the precipitation (like ADVPRC)
      ! =================================================

    DO JLON = KIDIA, KFDIA
    
      ZDZ(JLON)   = ZFVEL(JLON,JLEV)*TSPHY
    
      ZWORK3(JLON) = MAX(0.0_JPRB,ZDPSG(JLON,JLEV)*ZQPR(JLON,JLEV) + &
       &              TSPHY*(PFPLSL(JLON,JLEV-1)) + ZAUTOL(JLON,JLEV))
      ZQPSTOT(JLON) = MAX(0.0_JPRB,ZDPSG(JLON,JLEV)*ZQPS(JLON,JLEV) + &
       &              TSPHY*(PFPLSN(JLON,JLEV-1)) + ZAUTOI(JLON,JLEV))

!  New formulation which does not take into account initial contents
! This implies a total independence to CFL criteria therefore to the layers thickness

!      ZWORK3(JLON) = MAX(0.0_JPRB,TSPHY*(PFPLSL(JLON,JLEV-1))+ZAUTOL(JLON,JLEV))
!      ZQPSTOT(JLON) = MAX(0.0_JPRB,TSPHY*(PFPLSN(JLON,JLEV-1))+ZAUTOI(JLON,JLEV))

    ENDDO

    DO JLON = KIDIA, KFDIA

      ZQR = ZWORK3(JLON) / ZDZ(JLON)
      ZQS = ZQPSTOT(JLON) / ZDZ(JLON)
      
      IF (LEVAPP) THEN

        ZWORK1(JLON) =  ZQR / ZNRHOW
        ZWORK2(JLON) =  ZQS / ZNS(JLON,JLEV)

      ENDIF

    ENDDO

    IF (LEVAPP) THEN
      DO JLON = KIDIA, KFDIA
        ZPOW1(JLON)=ZCEV2(JLON,JLEV)*ZWORK1(JLON)**ZEXP6
        ZPOW2(JLON)=ZCSU1(JLON,JLEV)*ZWORK2(JLON)**ZEXP4
      ENDDO
    ENDIF

!DEC$ IVDEP
    DO JLON = KIDIA, KFDIA

      ZTQEVAPPL = 0.0_JPRB
      ZTQEVAPPN = 0.0_JPRB
      ZQFPFPL   = 0.0_JPRB
      ZQFPFPN   = 0.0_JPRB
      ZTCOLLL   = 0.0_JPRB
      ZTCOLLN   = 0.0_JPRB
      ZQMLT     = 0.0_JPRB
      ZQMLTX    = 0.0_JPRB
      ZQFRZ     = 0.0_JPRB
      ZQFRZX    = 0.0_JPRB  
      ZACCR     = 0.0_JPRB

      IF (LEVAPP) THEN

           ! ----------------------------------------
           ! Evaporation/Sublimation of precipitation
           ! ----------------------------------------

        ZEVAPPL = ZCEV1(JLON,JLEV)*SQRT(ZWORK1(JLON)) + ZPOW1(JLON)
        ZEVAPPN = ZPOW2(JLON) + ZCSU2(JLON,JLEV)*ZWORK2(JLON)

        ZINT1 = 1.0_JPRB / MAX(ZEPS,ZEVAPPL+ZEVAPPN)

        IF (LLEVAPX) THEN
          ZSUBSA = REVASX*ZINT1*(1.0_JPRB-EXP(-1.0_JPRB/(REVASX*ZINT1)))
          ZEVAPPL = ZSUBSA*ZEVAPPL
          ZEVAPPN = ZSUBSA*ZEVAPPN
        ENDIF

        ZSUBSA = ZINT1 * ZEVAPPL * (ZQSATW(JLON,JLEV) - PQ(JLON,JLEV))
        ZTQEVAPPL = MAX(0.0_JPRB, MIN( ZWORK3(JLON),&
         & ZEVAPPL * ZDPSGDT(JLON,JLEV), ZSUBSA * ZDPSG(JLON,JLEV) ))   

        ZSUBSA = ZINT1 * ZEVAPPN * (ZQSATI(JLON,JLEV) - PQ(JLON,JLEV))
        ZTQEVAPPN = MAX(0.0_JPRB, MIN( ZQPSTOT(JLON),&
         & ZEVAPPN * ZDPSGDT(JLON,JLEV), ZSUBSA * ZDPSG(JLON,JLEV) ))   

      ENDIF

      ZQPRTOT1 = ZWORK3(JLON) - ZTQEVAPPL
      ZQPSTOT1 = ZQPSTOT(JLON) - ZTQEVAPPN

      ZQR = ZQPRTOT1 / ZDZ(JLON)
      ZQS = ZQPSTOT1 / ZDZ(JLON)

      IF (LCOLLEC) THEN

           ! ----------------------------------------
           ! Collection of cloud liquid water by rain
           ! ----------------------------------------

        ZACCR = ZQL(JLON,JLEV)*(1.0_JPRB-EXP(-ZCACC(JLON,JLEV)*ZQR*TSPHY))&
        &     * MAX(0.0_JPRB,SIGN(1.0_JPRB,PT(JLON,JLEV)-RTT))
        

           ! -------------------------------
           ! Collection of cloud ice by snow
           ! -------------------------------
        ZAGGR = ZQI(JLON,JLEV)*(1.0_JPRB-EXP(-ZCAGG(JLON,JLEV)*ZQS*TSPHY))

           ! ----------------------------------------
           ! Collection of cloud liquid water by snow
           ! ----------------------------------------
        ZRIMI = ZQL(JLON,JLEV)*(1.0_JPRB-EXP(-ZCRIM(JLON,JLEV)*ZQS*TSPHY))

           ! ----------------------------
           ! Sum up collection processes
           ! ----------------------------
        ZTCOLLL = MAX(0.0_JPRB, MIN(ZACCR+ZRIMI,ZQL(JLON,JLEV)) )&
         & * ZDPSG(JLON,JLEV)
        ZTCOLLN = MAX(0.0_JPRB, MIN(ZAGGR      ,ZQI(JLON,JLEV)) )&
         & * ZDPSG(JLON,JLEV)

      ENDIF

      ZQPRTOT2  = ZWORK3(JLON) + ZTCOLLL
      ZQPSTOT2  = ZQPSTOT(JLON) + ZTCOLLN

      IF (LLMELTS) THEN

           ! ----------------------------
           ! Snow melting
           ! ----------------------------
        ZLHFUS = FOLH(PT(JLON,JLEV),1.0_JPRB) - FOLH(PT(JLON,JLEV),0.0_JPRB)
        ZQMLTX = ZDPSG(JLON,JLEV) * PCP(JLON,JLEV)&
         & * MAX(0.0_JPRB,ZDELT(JLON,JLEV)) / ZLHFUS

         IF (.NOT. LSMOOTHMELT) THEN 
          ZQMLT = MIN ( ZQMLTX , ZQPSTOT2 - ZTQEVAPPN )
         ELSE
          ZQMLT=(ZQPSTOT2-ZTQEVAPPN)*(1+TANH(ZDELT(JLON,JLEV)/RSMOOTHMELT))/2.0_JPRB
         ENDIF
       ENDIF 
       IF (LLFREEZ) THEN  

           ! ----------------------------
           ! Rain freezing
           ! ----------------------------
        
        ZQFRZX = ZDPSG(JLON,JLEV) * PCP(JLON,JLEV)&
         & * MAX(0.0_JPRB,-ZDELT(JLON,JLEV)) / ZLHFUS
        
        ZQFRZ = MIN ( ZQFRZX , ZQPRTOT2 - ZTQEVAPPL ) 
        
      ENDIF

      
      PFPEVPL(JLON,JLEV) = PFPEVPL(JLON,JLEV-1)&
       & + ( ZTQEVAPPL - ZQMLT + ZQFRZ) / TSPHY
      PFPEVPN(JLON,JLEV) = PFPEVPN(JLON,JLEV-1)&
       & + ( ZTQEVAPPN + ZQMLT - ZQFRZ) / TSPHY

      PFPFPL (JLON,JLEV) = PFPFPL (JLON,JLEV-1)&
       & + ( ZTCOLLL + ZAUTOL(JLON,JLEV) ) / TSPHY
      PFPFPN (JLON,JLEV) = PFPFPN (JLON,JLEV-1)&
       & + ( ZTCOLLN + ZAUTOI(JLON,JLEV) ) / TSPHY


           ! ----------------------------
           ! Computation of fundamental proportions
           ! needed by the statistical algorithm
           ! (only YB formulation !)
           ! ----------------------------

! Rain and snow           
      ZDZS = ZALTIH(JLON,JLEV-1) - ZALTIH(JLON,JLEV)
      ZP1  = MIN(1._JPRB , ZDZ(JLON)/ZDZS)
      ZP2  = MAX(0._JPRB,1._JPRB - ZDZS/ZDZ(JLON))
      ZP3  = (ZP1 + ZP2)/2.0_JPRB
! Cloud liquid water      
      ZP1L  = MIN(1._JPRB , ZDZL/ZDZS)
      ZP2L  = MAX(0._JPRB,1._JPRB - ZDZS/MAX(ZEPS,ZDZL))
! Cloud ice
      ZP1I  = MIN(1._JPRB , ZDZI/ZDZS)
      ZP2I  = MAX(0._JPRB,1._JPRB - ZDZS/MAX(ZEPS,ZDZI))

      
! WARNING ! : Dans cette version pour coller a ADVPRC il n'y a pas de traitement separe de la neige 
!             et de la pluie. Ceci serait difficile dans ADVPRC mais trivial dans ADVPRCS      
    ! ================================================================
    ! COMPUTE FLUX ASSOCIATED TO FALLING OF PRECIPITATION
    ! ================================================================

      PFPLSL(JLON,JLEV) = (ZP1*ZDPSG(JLON,JLEV)*ZQPR(JLON,JLEV)&
      &                 + ZP2*TSPHY*PFPLSL(JLON,JLEV-1)&
      &                 + ZP3*(ZAUTOL(JLON,JLEV) + ZTCOLLL + ZQMLT))&
      &                 * MAX(0.0_JPRB,&
      &                (1._JPRB - (ZTQEVAPPL+ZQFRZ)/MAX(ZEPS,ZQPRTOT2))) / TSPHY
      

      PFPLSN(JLON,JLEV) = (ZP1*ZDPSG(JLON,JLEV)*ZQPS(JLON,JLEV)&
      &                 + ZP2*TSPHY*PFPLSN(JLON,JLEV-1)&
      &                 + ZP3*(ZAUTOI(JLON,JLEV) + ZTCOLLN + ZQFRZ))&
      &                 * MAX(0.0_JPRB,&
      &                (1._JPRB - (ZTQEVAPPN+ZQMLT)/MAX(ZEPS,ZQPSTOT2))) / TSPHY

      PSEDIQL(JLON,JLEV) = (ZP1L*ZDPSG(JLON,JLEV)*ZQL(JLON,JLEV)&
       &                  + ZP2L*TSPHY*PSEDIQL(JLON,JLEV-1) ) / TSPHY
       
      PSEDIQN(JLON,JLEV) = (ZP1I*ZDPSG(JLON,JLEV)*ZQI(JLON,JLEV)&
       &                  + ZP2I*TSPHY*PSEDIQN(JLON,JLEV-1) ) / TSPHY
       
    ENDDO ! JLON = KIDIA, KFDIA

    !-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  ENDDO ! LEV=1,KFLEV : end of statistical advection 
    !-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

!- - - - - - - - - - - - - - - - - - - - - - -
ENDIF  ! End of test on TSPHY > 0.0_JPRB
!- - - - - - - - - - - - - - - - - - - - - - -

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ADVPRCS',1,ZHOOK_HANDLE)
END SUBROUTINE ADVPRCS

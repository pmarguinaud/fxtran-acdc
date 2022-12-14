!****  YOMCOMPO
!
!     PURPOSE.
!     --------
!       MODULE CONTAINING VARIABLES FOR COMPOSITION VALID for GHG, AER and CHEMISTRY 
!
!     PARAMETER        DESCRIPTION                                  
!     ---------         -----------                                  !      
!
!     REFERENCE.
!     ----------
!                   
!     AUTHOR.
!     -------
!     2016-09-20   J. Flemming

MODULE YOMCOMPO

USE PARKIND1  ,ONLY : JPIM, JPRB

IMPLICIT NONE
SAVE

TYPE :: TCOMPO
!  mass diagnostics switch for global budget   
     LOGICAL    :: LCHEM_DIA
!  output of photolysis rates in extra ) 
     LOGICAL    :: LCHEM_DDFLX
!  if depostion as part of diffusion flux, do not add before , direct input to tracer diffusion    
     LOGICAL    :: LCOMPO_DDFLX_DIR
! variable tropopause (humidity)
    LOGICAL :: LCHEM_TROPO
! CO2 aircraft emissions 
    LOGICAL :: LGHG_ACCO2 
! CO2 surface fluxes  
    LOGICAL :: LCO2SFC
! CH4 surface fluxes  
    LOGICAL :: LCH4SFC
! aerosol surface fluxes  
    LOGICAL :: LAEROSFC
! fire emissions 
    LOGICAL :: LFIRE
! Use of injection height for bb emissions of aerosols
    LOGICAL :: LINJ           
! Use of injection height for bb emissions of Chemistry    
    LOGICAL :: LINJ_CHEM       
! Use of injection height for bb emissions of GHG
    LOGICAL :: LINJ_GHG       
! Climatological diurnal Cycle (Gauss) for VOC emissions (CHEM)    
    LOGICAL :: LCOMPO_DCVOC
! Climatological diurnal Cycle (Gauss) for Anthropogenic emissions (AER + CHEM)    
    LOGICAL :: LCOMPO_DCANT
! Climatological diurnal Cycle (cosine) for biomass burning (AER + CHEM)    
    LOGICAL :: LCOMPO_DCBB
! Climatological diurnal Cycle (cosine) for climatological dry deposition velocities (AER + CHEM)    
    LOGICAL :: LCOMPO_DCDD
! Use volcano heights for SO2 emission injection
    LOGICAL :: LVOLC_ALTI
! Elevated emissions (Power Gneration) for anthropogenic (SO2) emission 
    LOGICAL :: LANT_HIGH
! Use nitrate aerosol scheme
    LOGICAL :: LAERNITRATE
! Use aer resuspension
    LOGICAL :: LAERRESUSPENSION
! Use distinct SOA species in aerosol scheme
    LOGICAL :: LAERSOA
! Use nucleation in IFS-GLOMAP
    LOGICAL :: LAERNUCL
! Aersols scheme (aer/glomap)
    CHARACTER(LEN=10)    :: AERO_SCHEME

END TYPE TCOMPO 

!!TYPE(TCOMPO), POINTER :: YRCOMPO => NULL()
CHARACTER(LEN=10)    :: AERO_SCHEME_OP_OBS

END MODULE YOMCOMPO 

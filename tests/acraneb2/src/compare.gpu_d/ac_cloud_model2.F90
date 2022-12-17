SUBROUTINE AC_CLOUD_MODEL2(                          &
 & YDPHY,YDPHY3,KIDIA    ,KFDIA    ,KLON     ,KTDIA    ,KLEV     ,&
 & LDMASKS  ,PDELP    ,&
 & PNEB     ,PQI      ,PQL      ,PR       ,PAPRSF   ,&
 & PT       ,PDEOSA   ,PBSFSI   ,PBSFSL   ,PBSFTI   ,&
 & PBSFTL   ,PEOASI   ,PEOASL   ,PEOATI   ,PEOATL   ,&
 & PEODSI   ,PEODSL   ,PEODTI   ,PEODTL   ,PEOSIDIR ,&
 & PEOSLDIR ,PUSAI    ,PUSAL    ,PUSBI    ,PUSBL, YDSTACK     &
 & )
!$acc routine (AC_CLOUD_MODEL2) seq

! Purpose:
! --------
!   AC_CLOUD_MODEL2 - Computes cloud optical coefficients taking into
!   account liquid/ice water content and in case of solar absorption
!   also non-local saturation effect caused by cloud layers above.
!   This version is part of ACRANEB2 scheme, original ACRANEB uses
!   frozen version of AC_CLOUD_MODEL.

! Interface:
! ----------
! INPUT:
!   KIDIA   - initial index for horizontal loops
!   KFDIA   - final index for horizontal loops
!   KLON    - horizontal dimension of arrays
!   KTDIA   - initial index for vertical loops (usually 1)
!   KLEV    - vertical dimension of full level arrays
!   KJN     - dimension of arrays containing "daylight" intervals
!   KIIDIA  - array of indices marking start of "daylight" intervals
!   KIFDIA  - array of indices marking end of "daylight" intervals
!   KAUCR   - number of "daylight" intervals
!   PDELP   - pressure difference across layers
!   PNEB    - cloud fraction (0-1)
!   PQI     - specific mass of ice INSIDE CLOUD
!   PQL     - specific mass of liquid water INSIDE CLOUD
!   PR      - gas constant of air
!   PAPRSF  - full level pressure
!   PT      - temperature
!   PDEOSA  - total gaseous optical depths (solar descending)

! OUTPUT:
!   PBSF[X][Y] - back scatter fraction beta
!   PEOA[X][Y] - absorption coefficient k_abs
!   PEOD[X][Y] - scattering coefficient k_scat (D = diffuse)
!   PEOS[Y]DIR - SW extinction coefficient k_ext, not delta-scaled 
!   PUSA[Y]    - coefficient in numerator of upscatter fraction   (solar band)
!   PUSB[Y]    - coefficient in denominator of upscatter fraction (solar band)

!     [X] - spectral band: S - solar
!                          T - thermal
!     [Y] - water phase:   I - ice
!                          L - liquid

! Externals:
! ----------

! Method:
! -------


! Reference:
! ----------


! Author:
! -------
!   2006-02, J.-F. Geleyn, J. Masek (original AC_CLOUD_MODEL)

! Modifications:
! --------------
!   2006-06, R. Brozkova
!   Code optimization.
!
!   2009-07, K. Yessad
!   Remove CDLOCK + some cleanings.
!
!   2013-11, J. Masek
!   Ice clouds based on Edwards et al. 2007 data, unified saturation
!   formula for cloud solar absorption, revised geometry factors for
!   effective cloud optical depth. Upgraded to AC_CLOUD_MODEL2.
!
!   2014-11, J. Masek
!   New ice cloud optical properties, explicit conversion of cloud
!   water content to particle size, separated saturation for ice/liquid clouds,
!   SW gas-cloud overlap parameterisation.
!
!   2016-04, J. Masek
!   Unscaled SW extinction coefficients for true direct solar flux.
!
! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1 ,ONLY: JPIM     ,JPRB

USE YOMCST   ,ONLY: RG

USE YOMPHY   ,ONLY : TPHY

USE YOMPHY3  ,ONLY : TPHY3
USE STACK_MOD
#include "stack.h"

!-------------------------------------------------------------------------------
                   
IMPLICIT NONE

TYPE(TPHY)        ,INTENT(INOUT):: YDPHY
TYPE(TPHY3)       ,INTENT(INOUT):: YDPHY3
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
LOGICAL,          INTENT(IN)  :: LDMASKS(KLON)

REAL(KIND=JPRB),INTENT(IN)  :: PDELP   (KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PNEB    (KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PQI     (KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PQL     (KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PR      (KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PAPRSF  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PT      (KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PDEOSA  (KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PBSFSI  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PBSFSL  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PBSFTI  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PBSFTL  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEOASI  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEOASL  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEOATI  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEOATL  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEODSI  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEODSL  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEODTI  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEODTL  (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEOSIDIR(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PEOSLDIR(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PUSAI   (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PUSAL   (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PUSBI   (KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PUSBL   (KLON,KLEV)
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK

#include "abor1.intfb.h"

!-------------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: IAUCR
INTEGER(KIND=JPIM) :: JB,JLEV,JLEV2,JLON,JN

REAL(KIND=JPRB) :: ZARGI,ZARGL,ZA,ZB,ZCI,ZCL,ZGI0,ZGL0
REAL(KIND=JPRB) :: ZAI2L,ZDI2L,ZDE,ZRE,ZRHO,ZRRG,ZTRLI

temp (REAL(KIND=JPRB), ZIWC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZLWC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDEL0_EFFA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDEL0_EFFD, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDE1, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZRE1, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZDEL0, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZEOAI, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZEOAL, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZEODI, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZEODL, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZGI, (KLON,KLEV,YDPHY3%N_SPBAND))
temp (REAL(KIND=JPRB), ZGL, (KLON,KLEV,YDPHY3%N_SPBAND))

temp (LOGICAL, LLQI, (KLON,KLEV))
temp (LOGICAL, LLQL, (KLON,KLEV))
LOGICAL :: LLMASKS
REAL(KIND=JPRB) :: ZSIZE_,ZP_,ZQ_

YLSTACK = YDSTACK

alloc (ZIWC)
alloc (ZLWC)
alloc (ZDEL0_EFFA)
alloc (ZDEL0_EFFD)
alloc (ZDE1)
alloc (ZRE1)
alloc (ZDEL0)
alloc (ZEOAI)
alloc (ZEOAL)
alloc (ZEODI)
alloc (ZEODL)
alloc (ZGI)
alloc (ZGL)
alloc (LLQI)
alloc (LLQL)



JLON = KIDIA


!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------

! 1. Computation of cloud optical properties
! ------------------------------------------

IF ( .NOT.YDPHY%LCLSATUR ) THEN

  ! 1.1 Old scheme
  ! --------------
  ! Old scheme ignores dependency of cloud optical coefficients on
  ! liquid/ice water content. Only mean saturation effect is taken 
  ! into account.

  ! fill arrays with namelist values
  DO JLEV=KTDIA,KLEV
    

      ! absorption coefficient
      PEOASI(JLON,JLEV)=YDPHY3%EOASI
      PEOASL(JLON,JLEV)=YDPHY3%EOASN
      PEOATI(JLON,JLEV)=YDPHY3%EOATI
      PEOATL(JLON,JLEV)=YDPHY3%EOATN

      ! scattering coefficient
      PEODSI(JLON,JLEV)=YDPHY3%EODSI
      PEODSL(JLON,JLEV)=YDPHY3%EODSN
      PEODTI(JLON,JLEV)=YDPHY3%EODTI
      PEODTL(JLON,JLEV)=YDPHY3%EODTN

      ! back scatter fraction
      PBSFSI(JLON,JLEV)=YDPHY3%BSFSI
      PBSFSL(JLON,JLEV)=YDPHY3%BSFSN
      PBSFTI(JLON,JLEV)=YDPHY3%BSFTI
      PBSFTL(JLON,JLEV)=YDPHY3%BSFTN

      ! coefficients for computation of upscatter fraction
      PUSAI (JLON,JLEV)=YDPHY3%USAI
      PUSAL (JLON,JLEV)=YDPHY3%USAN
      PUSBI (JLON,JLEV)=YDPHY3%USBI
      PUSBL (JLON,JLEV)=YDPHY3%USBN

    
  ENDDO

ELSE

  ! 1.2 New scheme
  ! --------------
  ! New scheme assumes dependency of cloud optical coefficients on
  ! liquid/ice water content. It introduces solar cloud saturation
  ! depending on cloud layers above and below.

  ! auxiliary parameters
  ZTRLI=EXP(-250._JPRB)
  ZRRG =1._JPRB/RG
  ZAI2L=LOG(YDPHY3%FCM_DEL_AI/YDPHY3%FCM_DEL_AL)
  ZDI2L=LOG(YDPHY3%FCM_DEL_DI/YDPHY3%FCM_DEL_DL)

  ! logical keys for skipping points without cloud liquid/ice
  ! daand: add explicit loops
	DO JLEV=1,KLEV
	  
			LLQI(JLON,JLEV)=PQI(JLON,JLEV) > 0._JPRB
			LLQL(JLON,JLEV)=PQL(JLON,JLEV) > 0._JPRB
		
	ENDDO

  ! 1.2.1 Convert IWC/LWC to D_e/R_e
  ! --------------------------------

  ! determine IWC/LWC [g/m^3]
  DO JLEV=KTDIA,KLEV
    
      ZRHO=PAPRSF(JLON,JLEV)/(PR(JLON,JLEV)*PT(JLON,JLEV))
      ZIWC(JLON,JLEV)=ZRHO*PQI(JLON,JLEV)*1000._JPRB
      ZLWC(JLON,JLEV)=ZRHO*PQL(JLON,JLEV)*1000._JPRB
    
  ENDDO

  ! determine D_e/R_e [micron] and scale it for fitting
  DO JB=1,YDPHY3%N_SPBAND
    DO JLEV=KTDIA,KLEV
      

        ! effective dimension D_e of ice particles [micron]
        IF ( LLQI(JLON,JLEV) ) THEN
          ZDE=YDPHY3%FCM_IWC2DE(JB,0)+YDPHY3%FCM_IWC2DE(JB,1)*&
           & (ZIWC(JLON,JLEV)+YDPHY3%FCM_IWC2DE(JB,2))**YDPHY3%FCM_IWC2DE(JB,3)
          ZDE=MAX(YDPHY3%FCM_IWC2DE(JB,-2),MIN(YDPHY3%FCM_IWC2DE(JB,-1),ZDE))
          ZDE1(JLON,JLEV,JB)=LOG(MAX(ZDE,ZTRLI))
        ELSE
          ZDE1(JLON,JLEV,JB)=0._JPRB
        ENDIF

        ! effective radius R_e of water droplets [micron]
        IF ( LLQL(JLON,JLEV) ) THEN
          ZRE=YDPHY3%FCM_LWC2RE(JB,0)+YDPHY3%FCM_LWC2RE(JB,1)*&
           & (ZLWC(JLON,JLEV)+YDPHY3%FCM_LWC2RE(JB,2))**YDPHY3%FCM_LWC2RE(JB,3)
          ZRE=MAX(YDPHY3%FCM_LWC2RE(JB,-2),MIN(YDPHY3%FCM_LWC2RE(JB,-1),ZRE))
          ZRE1(JLON,JLEV,JB)=LOG(MAX(ZRE,ZTRLI))
        ELSE
          ZRE1(JLON,JLEV,JB)=0._JPRB
        ENDIF

      
    ENDDO
  ENDDO

  ! 1.2.2 Unsaturated cloud optical properties
  ! ------------------------------------------

  ! initialize arrays for the case of no clouds
  ! daand: add explicit loops
  DO JB=1,YDPHY3%N_SPBAND
		DO JLEV=1,KLEV
			
				ZEOAI(JLON,JLEV,JB)=0._JPRB
				ZEOAL(JLON,JLEV,JB)=0._JPRB
				ZEODI(JLON,JLEV,JB)=0._JPRB
				ZEODL(JLON,JLEV,JB)=0._JPRB
				ZGI  (JLON,JLEV,JB)=0._JPRB
				ZGL  (JLON,JLEV,JB)=0._JPRB
			
		ENDDO
	ENDDO
	
  ! loop through spectral bands
  DO JB=1,YDPHY3%N_SPBAND

    ! differentiate between solar/thermal bounds
    IF ( JB == 1 ) THEN
		  
			  LLMASKS=LDMASKS(JLON)
			
    ELSE
		  
			  LLMASKS=.TRUE.
			
    ENDIF

    ! Pade approximants for scaled k_abs, k_scat and g
    

! Interface:
! ----------
! INPUT:
!   PSIZE - scaled D_e or R_e
!   PP    - Pade coefficients in numerator 
!   PQ    - Pade coefficients in denominator

! OUTPUT:
!   POUT - scaled fitted quantity


DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZSIZE_=ZDE1(JLON,JLEV,JB)
      ZP_=YDPHY3%FCM_P_AI(JB,0)   +ZSIZE_*(YDPHY3%FCM_P_AI(JB,1)+ZSIZE_*(YDPHY3%FCM_P_AI(JB,2)+ZSIZE_*YDPHY3%FCM_P_AI(JB,3)))
      ZQ_=1.0_JPRB+ZSIZE_*(YDPHY3%FCM_Q_AI(JB,1)+ZSIZE_*(YDPHY3%FCM_Q_AI(JB,2)+ZSIZE_*YDPHY3%FCM_Q_AI(JB,3)))
      ZEOAI(JLON,JLEV,JB)=ZP_/ZQ_
    !ENDIF
  
ENDDO





    

! Interface:
! ----------
! INPUT:
!   PSIZE - scaled D_e or R_e
!   PP    - Pade coefficients in numerator 
!   PQ    - Pade coefficients in denominator

! OUTPUT:
!   POUT - scaled fitted quantity


DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZSIZE_=ZRE1(JLON,JLEV,JB)
      ZP_=YDPHY3%FCM_P_AL(JB,0)   +ZSIZE_*(YDPHY3%FCM_P_AL(JB,1)+ZSIZE_*(YDPHY3%FCM_P_AL(JB,2)+ZSIZE_*YDPHY3%FCM_P_AL(JB,3)))
      ZQ_=1.0_JPRB+ZSIZE_*(YDPHY3%FCM_Q_AL(JB,1)+ZSIZE_*(YDPHY3%FCM_Q_AL(JB,2)+ZSIZE_*YDPHY3%FCM_Q_AL(JB,3)))
      ZEOAL(JLON,JLEV,JB)=ZP_/ZQ_
    !ENDIF
  
ENDDO





    

! Interface:
! ----------
! INPUT:
!   PSIZE - scaled D_e or R_e
!   PP    - Pade coefficients in numerator 
!   PQ    - Pade coefficients in denominator

! OUTPUT:
!   POUT - scaled fitted quantity


DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZSIZE_=ZDE1(JLON,JLEV,JB)
      ZP_=YDPHY3%FCM_P_DI(JB,0)   +ZSIZE_*(YDPHY3%FCM_P_DI(JB,1)+ZSIZE_*(YDPHY3%FCM_P_DI(JB,2)+ZSIZE_*YDPHY3%FCM_P_DI(JB,3)))
      ZQ_=1.0_JPRB+ZSIZE_*(YDPHY3%FCM_Q_DI(JB,1)+ZSIZE_*(YDPHY3%FCM_Q_DI(JB,2)+ZSIZE_*YDPHY3%FCM_Q_DI(JB,3)))
      ZEODI(JLON,JLEV,JB)=ZP_/ZQ_
    !ENDIF
  
ENDDO





    

! Interface:
! ----------
! INPUT:
!   PSIZE - scaled D_e or R_e
!   PP    - Pade coefficients in numerator 
!   PQ    - Pade coefficients in denominator

! OUTPUT:
!   POUT - scaled fitted quantity


DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZSIZE_=ZRE1(JLON,JLEV,JB)
      ZP_=YDPHY3%FCM_P_DL(JB,0)   +ZSIZE_*(YDPHY3%FCM_P_DL(JB,1)+ZSIZE_*(YDPHY3%FCM_P_DL(JB,2)+ZSIZE_*YDPHY3%FCM_P_DL(JB,3)))
      ZQ_=1.0_JPRB+ZSIZE_*(YDPHY3%FCM_Q_DL(JB,1)+ZSIZE_*(YDPHY3%FCM_Q_DL(JB,2)+ZSIZE_*YDPHY3%FCM_Q_DL(JB,3)))
      ZEODL(JLON,JLEV,JB)=ZP_/ZQ_
    !ENDIF
  
ENDDO





    

! Interface:
! ----------
! INPUT:
!   PSIZE - scaled D_e or R_e
!   PP    - Pade coefficients in numerator 
!   PQ    - Pade coefficients in denominator

! OUTPUT:
!   POUT - scaled fitted quantity


DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZSIZE_=ZDE1(JLON,JLEV,JB)
      ZP_=YDPHY3%FCM_P_GI(JB,0)   +ZSIZE_*(YDPHY3%FCM_P_GI(JB,1)+ZSIZE_*(YDPHY3%FCM_P_GI(JB,2)+ZSIZE_*YDPHY3%FCM_P_GI(JB,3)))
      ZQ_=1.0_JPRB+ZSIZE_*(YDPHY3%FCM_Q_GI(JB,1)+ZSIZE_*(YDPHY3%FCM_Q_GI(JB,2)+ZSIZE_*YDPHY3%FCM_Q_GI(JB,3)))
      ZGI(JLON,JLEV,JB)=ZP_/ZQ_
    !ENDIF
  
ENDDO





    

! Interface:
! ----------
! INPUT:
!   PSIZE - scaled D_e or R_e
!   PP    - Pade coefficients in numerator 
!   PQ    - Pade coefficients in denominator

! OUTPUT:
!   POUT - scaled fitted quantity


DO JLEV=KTDIA,KLEV
	
		!IF ( LLMASKS(JLON) ) THEN
      ZSIZE_=ZRE1(JLON,JLEV,JB)
      ZP_=YDPHY3%FCM_P_GL(JB,0)   +ZSIZE_*(YDPHY3%FCM_P_GL(JB,1)+ZSIZE_*(YDPHY3%FCM_P_GL(JB,2)+ZSIZE_*YDPHY3%FCM_P_GL(JB,3)))
      ZQ_=1.0_JPRB+ZSIZE_*(YDPHY3%FCM_Q_GL(JB,1)+ZSIZE_*(YDPHY3%FCM_Q_GL(JB,2)+ZSIZE_*YDPHY3%FCM_Q_GL(JB,3)))
      ZGL(JLON,JLEV,JB)=ZP_/ZQ_
    !ENDIF
  
ENDDO






    DO JLEV=KTDIA,KLEV
      
        !IF ( LLMASKS(JLON) ) THEN

          ! unscale k_abs, k_scat, convert units to [1/Pa],
          ! unscale asymmetry factor
          IF ( LLQI(JLON,JLEV) ) THEN
            ZEOAI(JLON,JLEV,JB)=EXP(ZEOAI(JLON,JLEV,JB))*ZRRG
            ZEODI(JLON,JLEV,JB)=EXP(ZEODI(JLON,JLEV,JB))*ZRRG
            ZGI  (JLON,JLEV,JB)=0.5_JPRB/&
             & (1.0_JPRB+EXP(-2.0_JPRB*ZGI(JLON,JLEV,JB)))
          ENDIF
          IF ( LLQL(JLON,JLEV) ) THEN
            ZEOAL(JLON,JLEV,JB)=EXP(ZEOAL(JLON,JLEV,JB))*ZRRG
            ZEODL(JLON,JLEV,JB)=EXP(ZEODL(JLON,JLEV,JB))*ZRRG
            ZGL  (JLON,JLEV,JB)=0.5_JPRB/&
             & (1.0_JPRB+EXP(-2.0_JPRB*ZGL(JLON,JLEV,JB)))
          ENDIF

          ! unsaturated optical depth delta0
          ZDEL0(JLON,JLEV,JB)=PDELP(JLON,JLEV)*(&
           & PQI(JLON,JLEV)*(ZEOAI(JLON,JLEV,JB)+ZEODI(JLON,JLEV,JB))+&
           & PQL(JLON,JLEV)*(ZEOAL(JLON,JLEV,JB)+ZEODL(JLON,JLEV,JB)))

        !ENDIF
      
    ENDDO

  ! end of loop through spectral bands
  ENDDO

  ! 1.2.3 Solar saturation
  ! ----------------------

  ! solar band
  JB=1

  ! loop through levels
  DO JLEV=KTDIA,KLEV

    ! initialize effective cloud optical depth with local one
		
			!IF ( LLMASKS(JLON) ) THEN
        ZDEL0_EFFA(JLON,JLEV)=ZDEL0(JLON,JLEV,JB)
        ZDEL0_EFFD(JLON,JLEV)=ZDEL0(JLON,JLEV,JB)
      !ENDIF
    

    ! sum effective cloud optical depth: 1, ..., JLEV-1
!cdir outerunroll=8
    DO JLEV2=KTDIA,JLEV-1
      
        !IF ( LLMASKS(JLON) ) THEN
          ZB=(YDPHY3%FCM_B_AI*ZIWC(JLON,JLEV)+YDPHY3%FCM_B_AL*ZLWC(JLON,JLEV))/&
           & (ZIWC(JLON,JLEV)+ZLWC(JLON,JLEV)+ZTRLI)
          ZDEL0_EFFA(JLON,JLEV)=ZDEL0_EFFA(JLON,JLEV)+&
           & PNEB(JLON,JLEV2)*ZDEL0(JLON,JLEV2,JB)*ZB
          ZDEL0_EFFD(JLON,JLEV)=ZDEL0_EFFD(JLON,JLEV)+&
           & PNEB(JLON,JLEV2)*ZDEL0(JLON,JLEV2,JB)
        !ENDIF
      
    ENDDO

    ! sum effective cloud optical depth: JLEV+1, ..., KLEV
!cdir outerunroll=8
    DO JLEV2=JLEV+1,KLEV
      
        !IF ( LLMASKS(JLON) ) THEN
          ZB=(YDPHY3%FCM_B_BI*ZIWC(JLON,JLEV)+YDPHY3%FCM_B_BL*ZLWC(JLON,JLEV))/&
           & (ZIWC(JLON,JLEV)+ZLWC(JLON,JLEV)+ZTRLI)
          ZDEL0_EFFA(JLON,JLEV)=ZDEL0_EFFA(JLON,JLEV)+&
           & PNEB(JLON,JLEV2)*ZDEL0(JLON,JLEV2,JB)*ZB
          ZDEL0_EFFD(JLON,JLEV)=ZDEL0_EFFD(JLON,JLEV)+&
           & PNEB(JLON,JLEV2)*ZDEL0(JLON,JLEV2,JB)
        !ENDIF
      
    ENDDO

    ! saturation of k_abs
		
			!IF ( LLMASKS(JLON) ) THEN

        ! saturation factors
        ZA   =(YDPHY3%FCM_AI*ZIWC(JLON,JLEV)+YDPHY3%FCM_AL*ZLWC(JLON,JLEV))/&
         &    (ZIWC(JLON,JLEV)+ZLWC(JLON,JLEV)+ZTRLI)
        ZARGI=LOG(MAX((ZDEL0_EFFA(JLON,JLEV)+&
         &    ZA*PDEOSA(JLON,JLEV))/YDPHY3%FCM_DEL_AI,ZTRLI))
        ZARGL=ZARGI+ZAI2L
        ZCI  =EXP(-YDPHY3%FCM_NU_AI*LOG(1.0_JPRB+EXP(YDPHY3%FCM_MU_AI*ZARGI)))
        ZCL  =EXP(-YDPHY3%FCM_NU_AL*LOG(1.0_JPRB+EXP(YDPHY3%FCM_MU_AL*ZARGL)))

        ! saturated k_abs
        ZEOAI(JLON,JLEV,JB)=ZCI*ZEOAI(JLON,JLEV,JB)
        ZEOAL(JLON,JLEV,JB)=ZCL*ZEOAL(JLON,JLEV,JB)

      !ENDIF
    

    IF ( YDPHY3%FCM_NU_DI /= 0._JPRB .OR. YDPHY3%FCM_NU_DL /= 0._JPRB ) THEN

      ! saturation of k_scat
      
        !IF ( LLMASKS(JLON) ) THEN

          ! saturation factors
          ZARGI=LOG(MAX(ZDEL0_EFFD(JLON,JLEV)/YDPHY3%FCM_DEL_DI,ZTRLI))
          ZARGL=ZARGI+ZDI2L
          ZCI  =EXP(-YDPHY3%FCM_NU_DI*LOG(1.0_JPRB+EXP(YDPHY3%FCM_MU_DI*ZARGI)))
          ZCL  =EXP(-YDPHY3%FCM_NU_DL*LOG(1.0_JPRB+EXP(YDPHY3%FCM_MU_DL*ZARGL)))

          ! saturated k_scat
          ZEODI(JLON,JLEV,JB)=ZCI*ZEODI(JLON,JLEV,JB)
          ZEODL(JLON,JLEV,JB)=ZCL*ZEODL(JLON,JLEV,JB)

        !ENDIF
      

    ENDIF

  ! end of loop through levels
  ENDDO

  ! 1.2.4 Fill output arrays
  ! ------------------------

  ! solar band
  JB=1
  DO JLEV=KTDIA,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        PEOASI(JLON,JLEV)=ZEOAI(JLON,JLEV,JB)
        PEOASL(JLON,JLEV)=ZEOAL(JLON,JLEV,JB)
        PEODSI(JLON,JLEV)=ZEODI(JLON,JLEV,JB)
        PEODSL(JLON,JLEV)=ZEODL(JLON,JLEV,JB)
        PBSFSI(JLON,JLEV)=0.5_JPRB-0.375_JPRB*ZGI(JLON,JLEV,JB)
        PBSFSL(JLON,JLEV)=0.5_JPRB-0.375_JPRB*ZGL(JLON,JLEV,JB)

        ! extinction coefficients, not detla-scaled
        ZGI0=ZGI(JLON,JLEV,JB)/(1._JPRB-ZGI(JLON,JLEV,JB))
        ZGL0=ZGL(JLON,JLEV,JB)/(1._JPRB-ZGL(JLON,JLEV,JB))
        PEOSIDIR(JLON,JLEV)=PEOASI(JLON,JLEV)+&
         &                  PEODSI(JLON,JLEV)/(1._JPRB-ZGI0*ZGI0)
        PEOSLDIR(JLON,JLEV)=PEOASL(JLON,JLEV)+&
         &                  PEODSL(JLON,JLEV)/(1._JPRB-ZGL0*ZGL0)

        ! coefficients for computation of upscatter fraction
        PUSAI(JLON,JLEV)=2.0_JPRB*PBSFSI(JLON,JLEV)-1.0_JPRB
        PUSAL(JLON,JLEV)=2.0_JPRB*PBSFSL(JLON,JLEV)-1.0_JPRB
        PUSBI(JLON,JLEV)=0.0_JPRB
        PUSBL(JLON,JLEV)=0.0_JPRB
      !ENDIF
    
  ENDDO

  ! thermal band
  JB=2
  DO JLEV=KTDIA,KLEV
    
      PEOATI(JLON,JLEV)=ZEOAI(JLON,JLEV,JB)
      PEOATL(JLON,JLEV)=ZEOAL(JLON,JLEV,JB)
      PEODTI(JLON,JLEV)=ZEODI(JLON,JLEV,JB)
      PEODTL(JLON,JLEV)=ZEODL(JLON,JLEV,JB)
      PBSFTI(JLON,JLEV)=0.5_JPRB-0.375_JPRB*ZGI(JLON,JLEV,JB)
      PBSFTL(JLON,JLEV)=0.5_JPRB-0.375_JPRB*ZGL(JLON,JLEV,JB)
    
  ENDDO

ENDIF



! ------------------------------------------------------------------------------



! Private subroutines/functions
! -----------------------------

! P.1 Subroutine for evaluating Pade approximants
! -----------------------------------------------



END SUBROUTINE AC_CLOUD_MODEL2

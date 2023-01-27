MODULE SPP_MOD

! Purpose :
! -------
!    Define types for the SPP scheme, which represents model uncertainties
!    with Stochastically Perturbed Parameterisations.

! Interface :
! ---------
!    Empty.

! External :
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------

! Author :
! ------
!    M. Leutbecher & S.-J. Lock (ECMWF)
!    Original : December 2014

! Modifications :
! -------------
!    
!    SJ Lock : Jan-2016   Enabled SPP perturbations of clear-skies heating rates
!     S Lang : Jan-2017   Enable output of pattern and absolute time rnd number seed
!    SJ Lock : Oct 2017   Option to reduce frequency of SPP pattern updates
!    SJ Lock : Oct-2017   Enabled options for new microphysics perturbations
!    SJ Lock : Oct-2017   Enabled options for new microphysics perturbations
!   U Andrae : Dec 2020   Introduce SPP for HARMONIE-AROME
!  
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK

IMPLICIT NONE
SAVE

INTEGER(KIND=JPIM), PARAMETER :: JPSPPLABLEN=16, NWRMAX=255
!=============================================================================

!      1.    TYPE DEFINITIONS
!            ----------------

!      1.1   TYPE holding the configuration of the spp scheme     
TYPE TSPP_CONFIG
  LOGICAL :: LSPP=.FALSE.        !> activates stochastically perturbed parameterisation scheme
  LOGICAL :: LSPG_SPP=.FALSE.    !> activates SPG as pattern generator

  REAL(KIND=JPRB) :: SPGMU       !>
  REAL(KIND=JPRB) :: SPGLAMBDA   !>
  REAL(KIND=JPRB) :: SPGSIGMA    !>
  REAL(KIND=JPRB) :: SPGQ        !>
  REAL(KIND=JPRB) :: SPGADTMIN   !>
  REAL(KIND=JPRB) :: SPGADTMAX   !>
  REAL(KIND=JPRB) :: SPGTDT      !>

  INTEGER :: IEZDIAG_POS         !> location in EZDIAG for diagnostic fields

  ! AROME specific settings

  LOGICAL :: LPERT_PSIGQSAT     !> perturb saturation limit sensitivity VSIGQSAT
  LOGICAL :: LPERT_CLDDPTH      !> perturb threshold cloud thickness for stratocumulus/cumulus transition RFRMIN(19)
  LOGICAL :: LPERT_CLDDPTHDP    !> perturb threshold cloud thickness used in shallow/deep convection decision RFRMIN(20)
  LOGICAL :: LPERT_ICE_CLD_WGT  !> perturb cloud ice content impact on cloud thickness RFRMIN(21)
  LOGICAL :: LPERT_ICENU        !> perturb ice nuclei RFRMIN(9)
  LOGICAL :: LPERT_KGN_ACON     !> perturb Kogan autoconversion speed RFRMIN(10)
  LOGICAL :: LPERT_KGN_SBGR     !> perturb Kogan subgrid scale (cloud fraction) sensitivity RFRMIN(11)
  LOGICAL :: LPERT_RADGR        !> perturb graupel impact on radiation
  LOGICAL :: LPERT_RADSN        !> perturb snow impact on radiation
  LOGICAL :: LPERT_RFAC_TWOC    !> perturb top entrainment
  LOGICAL :: LPERT_RZC_H     	!> perturb stable condition length scale
  LOGICAL :: LPERT_RZL_INF     	!> perturb asymptotic free atmospheric length scale 
  LOGICAL :: LPERT_RSWINHF    	!> perturb SW radiation inomogeniety factor
  LOGICAL :: LPERT_RLWINHF    	!> perturb LW radiation inomogeniety factor

  LOGICAL :: LCORR_RZL_INF      !> Use RZC_H pattern for RZL_INF

  LOGICAL :: LLNN_MEAN1_PSIGQSAT=.FALSE.
  LOGICAL :: LLNN_MEAN1_CLDDPTH=.FALSE.
  LOGICAL :: LLNN_MEAN1_CLDDPTHDP=.FALSE.
  LOGICAL :: LLNN_MEAN1_ICE_CLD_WGT=.FALSE.
  LOGICAL :: LLNN_MEAN1_ICENU=.FALSE.
  LOGICAL :: LLNN_MEAN1_KGN_ACON=.FALSE.
  LOGICAL :: LLNN_MEAN1_KGN_SBGR=.FALSE.
  LOGICAL :: LLNN_MEAN1_RADGR=.FALSE.
  LOGICAL :: LLNN_MEAN1_RADSN=.FALSE.
  LOGICAL :: LLNN_MEAN1_RFAC_TWOC=.FALSE.
  LOGICAL :: LLNN_MEAN1_RZC_H=.FALSE.
  LOGICAL :: LLNN_MEAN1_RZL_INF=.FALSE.
  LOGICAL :: LLNN_MEAN1_RSWINHF=.FALSE.
  LOGICAL :: LLNN_MEAN1_RLWINHF=.FALSE.

  LOGICAL :: LUNIFORM_PSIGQSAT=.FALSE.
  LOGICAL :: LUNIFORM_CLDDPTH=.FALSE.
  LOGICAL :: LUNIFORM_CLDDPTHDP=.FALSE.
  LOGICAL :: LUNIFORM_ICE_CLD_WGT=.FALSE.
  LOGICAL :: LUNIFORM_ICENU=.FALSE.
  LOGICAL :: LUNIFORM_KGN_ACON=.FALSE.
  LOGICAL :: LUNIFORM_KGN_SBGR=.FALSE.
  LOGICAL :: LUNIFORM_RADGR=.FALSE.
  LOGICAL :: LUNIFORM_RADSN=.FALSE.
  LOGICAL :: LUNIFORM_RFAC_TWOC=.FALSE.
  LOGICAL :: LUNIFORM_RZC_H=.FALSE.
  LOGICAL :: LUNIFORM_RZL_INF=.FALSE.
  LOGICAL :: LUNIFORM_RSWINHF=.FALSE.
  LOGICAL :: LUNIFORM_RLWINHF=.FALSE.

  INTEGER(JPIM) :: NSEED_OFFS_PSIGQSAT       =0
  INTEGER(JPIM) :: NSEED_OFFS_CLDDPTH        =2**1
  INTEGER(JPIM) :: NSEED_OFFS_CLDDPTHDP      =2**2
  INTEGER(JPIM) :: NSEED_OFFS_ICE_CLD_WGT    =2**3
  INTEGER(JPIM) :: NSEED_OFFS_ICENU          =2**4
  INTEGER(JPIM) :: NSEED_OFFS_KGN_ACON       =2**5
  INTEGER(JPIM) :: NSEED_OFFS_KGN_SBGR       =2**6
  INTEGER(JPIM) :: NSEED_OFFS_RADGR          =2**7
  INTEGER(JPIM) :: NSEED_OFFS_RADSN          =2**8
  INTEGER(JPIM) :: NSEED_OFFS_RFAC_TWOC      =2**9
  INTEGER(JPIM) :: NSEED_OFFS_RZC_H          =2**10
  INTEGER(JPIM) :: NSEED_OFFS_RZL_INF        =2**12
  INTEGER(JPIM) :: NSEED_OFFS_RSWINHF        =2**13
  INTEGER(JPIM) :: NSEED_OFFS_RLWINHF        =2**14

  REAL(KIND=JPRB) :: CMPERT_PSIGQSAT, &
                     CMPERT_CLDDPTH, &
                     CMPERT_CLDDPTHDP, &
                     CMPERT_ICE_CLD_WGT, &
                     CMPERT_ICENU, &
                     CMPERT_KGN_ACON, &
                     CMPERT_KGN_SBGR, &
                     CMPERT_RADGR, &
                     CMPERT_RADSN, &
                     CMPERT_RFAC_TWOC, &
                     CMPERT_RZC_H, &
                     CMPERT_RZL_INF,&
                     CMPERT_RSWINHF,&
                     CMPERT_RLWINHF
		     
  REAL(KIND=JPRB) :: UNIFORM_OFFSET_PSIGQSAT, &
                     UNIFORM_OFFSET_CLDDPTH, &
                     UNIFORM_OFFSET_CLDDPTHDP, &
                     UNIFORM_OFFSET_ICE_CLD_WGT, &
                     UNIFORM_OFFSET_ICENU, &
                     UNIFORM_OFFSET_KGN_ACON, &
                     UNIFORM_OFFSET_KGN_SBGR, &
                     UNIFORM_OFFSET_RADGR, &
                     UNIFORM_OFFSET_RADSN, &
                     UNIFORM_OFFSET_RFAC_TWOC, &
                     UNIFORM_OFFSET_RZC_H, &
                     UNIFORM_OFFSET_RZL_INF,&
                     UNIFORM_OFFSET_RSWINHF,&
                     UNIFORM_OFFSET_RLWINHF

  REAL(KIND=JPRB) :: CLIP_PSIGQSAT(2), &
                     CLIP_CLDDPTH(2), &
                     CLIP_CLDDPTHDP(2), &
                     CLIP_ICE_CLD_WGT(2), &
                     CLIP_ICENU(2), &
                     CLIP_KGN_ACON(2), &
                     CLIP_KGN_SBGR(2), &
                     CLIP_RADGR(2), &
                     CLIP_RADSN(2), &
                     CLIP_RFAC_TWOC(2), &
                     CLIP_RZC_H(2), &
                     CLIP_RZL_INF(2), &
                     CLIP_RSWINHF(2), &
                     CLIP_RLWINHF(2)

  ! ECMWF specific settings

  LOGICAL :: LPERT_CFM           !> perturb transfer coefficient for momentum
  LOGICAL :: LPERT_TOFDC         !> perturb drag coefficient in turbulent orographic form drag scheme
  LOGICAL :: LPERT_HSDT          !> perturb stdev of subgrid orography
  LOGICAL :: LPERT_VDEXC_LEN_STABLE_BL !>  perturb vertical exchange coefficients through length scale in stable boundary layer

  ! parameterized convection
  LOGICAL :: LPERT_ENTRORG       !> perturb entrainment rate
  LOGICAL :: LPERT_ENTSHALP      !> perturb shallow entrainment (cave because of multiplication)
  LOGICAL :: LPERT_DETRPEN       !> perturb detrainment rate for penetrative convection
  LOGICAL :: LPERT_RPRCON        !> perturb conversion coefficient cloud to rain
  LOGICAL :: LPERT_RTAU          !> perturb adjustment time scale in CAPE closure
  LOGICAL :: LPERT_CUDUDV        !> perturb convective momentum transport

  ! large-scale cloud processes
  LOGICAL :: LPERT_RAMID         !> perturb  RH threshold for onset of stratiform cond.
  LOGICAL :: LPERT_RCLDIFF       !> perturb diffusion coeff. for evap. of turb. mixing
  LOGICAL :: LPERT_RCLCRIT       !> perturb critical cloud water content
  LOGICAL :: LPERT_RLCRITSNOW    !> perturb threshold value for snow autoconversion
  LOGICAL :: LPERT_RAINEVAP      !> perturb rain evaporation rate
  LOGICAL :: LPERT_SNOWSUBLIM    !> perturb snow sublimation rate
  LOGICAL :: LPERT_QSATVERVEL    !> perturb saturation due to adiabatic vertical velocity

  ! radiation-cloud interaction
  LOGICAL :: LPERT_ZDECORR       !> perturb vertical decorrelation height of cloud in McICA
  LOGICAL :: LPERT_ZSIGQCW       !> perturb fractional stdev of cloud water distribution in McICA
  LOGICAL :: LPERT_ZRADEFF       !> perturb effective radii  of cloud droplets (liquid/ice) in McICA
  LOGICAL :: LPERT_ZRADEFF2      !> perturb effective radii  of cloud liquid & ice independently in McICA
  LOGICAL :: LPERT_PHR           !> perturb clear-skies (LW/SW) heating rates due to gaseous absorption
  ! climatological aerosol
  LOGICAL :: LPERT_ZHS_VDAERO    !> perturb scale height of normalised vertical distribution of aerosol
  LOGICAL :: LPERT_DELTA_AERO    !> perturb 2D distribution of optical thickness of aerosol

  LOGICAL :: LLNN_MEAN1=.TRUE.   !> adjust mean of log-normal distribution to 1.
  LOGICAL :: LLNN_MEAN1_ZSIGQCW=.FALSE.   !> force mean of log-normal distribution to 1 for perturbations of ZSIGQCW regardless of LLNN_MEAN1
  LOGICAL :: LLNN_MEAN1_CFM=.FALSE.
  LOGICAL :: LLNN_MEAN1_TOFDC=.FALSE.
  LOGICAL :: LLNN_MEAN1_HSDT=.FALSE.
  LOGICAL :: LLNN_MEAN1_VDEXC_LEN_STABLE_BL=.FALSE.
  LOGICAL :: LLNN_MEAN1_ENTRORG=.FALSE.
  LOGICAL :: LLNN_MEAN1_ENTSHALP=.FALSE. 
  LOGICAL :: LLNN_MEAN1_DETRPEN=.FALSE.
  LOGICAL :: LLNN_MEAN1_RPRCON=.FALSE.
  LOGICAL :: LLNN_MEAN1_CUDUDV=.FALSE.
  LOGICAL :: LLNN_MEAN1_RTAU=.FALSE.
  LOGICAL :: LLNN_MEAN1_RAMID=.FALSE.
  LOGICAL :: LLNN_MEAN1_RCLDIFF=.FALSE. 
  LOGICAL :: LLNN_MEAN1_RCLCRIT=.FALSE.
  LOGICAL :: LLNN_MEAN1_RLCRITSNOW=.FALSE.
  LOGICAL :: LLNN_MEAN1_RAINEVAP=.FALSE.
  LOGICAL :: LLNN_MEAN1_SNOWSUBLIM=.FALSE.
  LOGICAL :: LLNN_MEAN1_QSATVERVEL=.FALSE.
  LOGICAL :: LLNN_MEAN1_ZDECORR=.FALSE.
  LOGICAL :: LLNN_MEAN1_ZRADEFFLP=.FALSE.
  LOGICAL :: LLNN_MEAN1_ZRADEFFIP=.FALSE.
  LOGICAL :: LLNN_MEAN1_PHRLW=.FALSE.
  LOGICAL :: LLNN_MEAN1_PHRSW=.FALSE.
  LOGICAL :: LLNN_MEAN1_ZHS_VDAERO=.FALSE.
  LOGICAL :: LLNN_MEAN1_DELTA_AERO=.FALSE.

  LOGICAL :: LRAMIDLIMIT1        !> limit perturbed value of RAMID to be <=1.

  LOGICAL :: LUSE_SETRAN=.TRUE.  !> use setran to randomize seed depending on start date, ensemble member number, ...
  LOGICAL :: LNSEED_OFFS0        !> set all seed offsets to 0 regardless of NSEED_OFFS_* given below

  ! write out pattern and reset seed
  LOGICAL            :: LRDPATINIT 
  LOGICAL            :: LWRPATTRUN
  INTEGER(KIND=JPIM) :: NWRPATTRUN
  INTEGER(KIND=JPIM), DIMENSION(NWRMAX) :: NHOUR_PATTRUN
  INTEGER(KIND=JPIM), DIMENSION(NWRMAX) :: NSTEP_PATTRUN
  LOGICAL            :: LRESETSEED 
  LOGICAL            :: LABSTIMSEED 
  INTEGER(KIND=JPIM) :: RESETSEEDFRQ
  INTEGER(KIND=JPIM) :: SHIFTSEED
  CHARACTER(LEN=256) :: SPP_RDPATINIT
  CHARACTER(LEN=256) :: SPP_WRPATTRUN

  INTEGER(JPIM) :: NSEEDFAC      !> seed factor for random numbers
  INTEGER(JPIM) :: NSEED_OFFS_CFM         =0
  INTEGER(JPIM) :: NSEED_OFFS_TOFDC       =2** 8  
  INTEGER(JPIM) :: NSEED_OFFS_HSDT        =2** 9  
  INTEGER(JPIM) :: NSEED_OFFS_VDEXC       =2**10 
  INTEGER(JPIM) :: NSEED_OFFS_ENTRORG     =2**11
  INTEGER(JPIM) :: NSEED_OFFS_ENTSHALP    =2**12
  INTEGER(JPIM) :: NSEED_OFFS_DETRPEN     =2**13
  INTEGER(JPIM) :: NSEED_OFFS_RPRCON      =2**14
  INTEGER(JPIM) :: NSEED_OFFS_RTAU        =2**15
  INTEGER(JPIM) :: NSEED_OFFS_RAMID       =2**16
  INTEGER(JPIM) :: NSEED_OFFS_RCLDIFF     =2**17
  INTEGER(JPIM) :: NSEED_OFFS_RCLCRIT     =2**18
  INTEGER(JPIM) :: NSEED_OFFS_RLCRITSNOW  =2**19
  INTEGER(JPIM) :: NSEED_OFFS_ZDECORR     =2**20
  INTEGER(JPIM) :: NSEED_OFFS_ZSIGQCW     =2**21
  INTEGER(JPIM) :: NSEED_OFFS_ZRADEFF1    =2**22
  INTEGER(JPIM) :: NSEED_OFFS_CUDU        =2**23
  INTEGER(JPIM) :: NSEED_OFFS_CUDV        =2**24
  INTEGER(JPIM) :: NSEED_OFFS_ZHS_VDAERO  =2**25
  INTEGER(JPIM) :: NSEED_OFFS_DELTA_AERO  =2**26
  INTEGER(JPIM) :: NSEED_OFFS_ZRADEFF2    =2**27
  INTEGER(JPIM) :: NSEED_OFFS_PHRLW       =2**28
  INTEGER(JPIM) :: NSEED_OFFS_PHRSW       =2**29
  INTEGER(JPIM) :: NSEED_OFFS_RAINEVAP    =2**30
  INTEGER(JPIM) :: NSEED_OFFS_SNOWSUBLIM  =2**30 + 2**8
  INTEGER(JPIM) :: NSEED_OFFS_QSATVERVEL  =2**30 + 2**9

  INTEGER(JPIM) :: NPATFR=1      !> frequency of pattern updates:
                                 !> >0: every NPATFR timesteps / <0: every -NPATFR hours

  REAL(KIND=JPRB) :: CMPERTOC    !> rel. magn. of perturb. of turb. transfer coeff. Ocean
  REAL(KIND=JPRB) :: CMPERTLA    !> rel. magn. of perturb. of turb. transfer coeff. Land
  REAL(KIND=JPRB) :: CMPERTTOFD  !> rel. magn. of perturb. of drag coeff. of TOFD scheme
  REAL(KIND=JPRB) :: SDPERTSO    !> rel. magn. of perturb. of st. dev. of subgrid orogr.
  REAL(KIND=JPRB) :: CMPERTLENBL !> rel. magn. of perturb. of length scale in stable boundary layer

  REAL(KIND=JPRB) :: CMPERT_ENTRORG     !> rel. magn. of perturb. of entrainment rate
  REAL(KIND=JPRB) :: CMPERT_ENTSHALP    !> rel. magn. of perturb. of shallow entrainment rate
  REAL(KIND=JPRB) :: CMPERT_DETRPEN     !> rel. magn. of perturb. of detrainment rate for pene. conv.
  REAL(KIND=JPRB) :: CMPERT_RPRCON      !> rel. magn. of perturb. of conv. coeff. cloud to rain
  REAL(KIND=JPRB) :: CMPERT_RTAU        !> rel. magn. of perturb. of adj. time scale in CAPE closure
  REAL(KIND=JPRB) :: CMPERT_CUDU        !> rel. magn. of perturb. of (deep) convective zonal momentum transport
  REAL(KIND=JPRB) :: CMPERT_CUDV        !> rel. magn. of perturb. of (deep) convective meridional momentum transport

  REAL(KIND=JPRB) :: CMPERT_RAMID       !> rel. magn. of perturb. of RH threshold for onset of stratiform cond.
  REAL(KIND=JPRB) :: CMPERT_RCLDIFF     !> rel. magn. of perturb. of diffusion coeff. for evap. of turb. mixing
  REAL(KIND=JPRB) :: CMPERT_RCLCRIT_SEA !> rel. magn. of perturb. of critical cloud water content for sea
  REAL(KIND=JPRB) :: CMPERT_RCLCRIT_LAND!> rel. magn. of perturb. of --------------- " -------------- land
  REAL(KIND=JPRB) :: CMPERT_RLCRITSNOW  !> rel. magn. of perturb. of threshold value for snow autoconversion
  REAL(KIND=JPRB) :: CMPERT_RAINEVAP    !> rel. magn. of perturb. of rain evaporation rate
  REAL(KIND=JPRB) :: CMPERT_SNOWSUBLIM  !> rel. magn. of perturb. of snow sublimation rate
  REAL(KIND=JPRB) :: CMPERT_QSATVERVEL  !> rel. magn. of perturb. of saturation due to adiabatic vertical velocity

  REAL(KIND=JPRB) :: CMPERT_ZDECORR     !> rel. magn. of perturb. of decorrelation height of cloud
  REAL(KIND=JPRB) :: CMPERT_ZSIGQCW     !> rel. magn. of perturb. of fractional stdev of cloud water distribution 
  REAL(KIND=JPRB) :: CMPERT_ZRADEFFLP   !> rel. magn. of perturb. of effective radius of cloud liquid droplets
  REAL(KIND=JPRB) :: CMPERT_ZRADEFFIP   !> rel. magn. of perturb. of effective radius of cloud ice    droplets
  REAL(KIND=JPRB) :: CMPERT_PHRLW_ST    !> rel. magn. of perturb. of heating rate due to gaseous absorption: LW, stratosphere
  REAL(KIND=JPRB) :: CMPERT_PHRLW_TR    !> rel. magn. of perturb. of ------------------"-------------------: LW,  troposphere
  REAL(KIND=JPRB) :: CMPERT_PHRSW_ST    !> rel. magn. of perturb. of ------------------"-------------------: SW, stratosphere
  REAL(KIND=JPRB) :: CMPERT_PHRSW_TR    !> rel. magn. of perturb. of ------------------"-------------------: SW,  troposphere
  REAL(KIND=JPRB) :: CMPERT_ZHS_VDAERO  !> rel. magn. of perturb. of aerosol scale height
  REAL(KIND=JPRB) :: CMPERT_DELTA_AERO  !> rel. magn. of perturb. of aerosol optical thickness

  REAL(KIND=JPRB) :: XPRESS_PHR_ST      !> pressure defining level of stratosphere, for heating rate perturbations

  REAL(KIND=JPRB) :: TAU         !> correlation time scale of patterns
  REAL(KIND=JPRB) :: SDEV        !> standard deviation of patterns
  REAL(KIND=JPRB) :: XLCOR       !> correlation length scale of patterns

END TYPE TSPP_CONFIG


TYPE TSPP_DATA
  INTEGER(JPIM) :: N2D     !> number of 2D patterns
  INTEGER(JPIM) :: N2DRAD  !> number of 2D patterns for McICA

  ! AROME specific settings
  INTEGER(JPIM) :: MP_PSIGQSAT, &
                   MP_CLDDPTH, &
                   MP_CLDDPTHDP, &
                   MP_ICE_CLD_WGT, &
                   MP_ICENU, &
                   MP_KGN_ACON, &
                   MP_KGN_SBGR, &
                   MP_RADGR, &
                   MP_RADSN, &
                   MP_RFAC_TWOC, &
                   MP_RZC_H, &
                   MP_RZL_INF, &
                   MP_RSWINHF, &
                   MP_RLWINHF

 
  ! ECMWF specific settings

  ! stochastic drag
  INTEGER(JPIM) :: MPCFM   !> index referring to CFM pattern
  INTEGER(JPIM) :: MPTOFDC !> index referring to TOFDC pattern
  INTEGER(JPIM) :: MPHSDT  !> index referring to HSDT pattern
  INTEGER(JPIM) :: MPVDEXC !> index referring to VDEXC pattern
  ! convection
  INTEGER(JPIM) :: MPENTRORG  !> index referring to ENTRORG pattern
  INTEGER(JPIM) :: MPENTSHALP !> index referring to ENTSHALP pattern
  INTEGER(JPIM) :: MPDETRPEN  !> index referring to DETRPEN  pattern
  INTEGER(JPIM) :: MPRPRCON   !> index referring to RPRCON pattern
  INTEGER(JPIM) :: MPRTAU     !> index referring to RTAU  pattern
  INTEGER(JPIM) :: MPCUDU     !> index referring to CUDU  pattern
  INTEGER(JPIM) :: MPCUDV     !> index referring to CUDV  pattern
  ! cloud
  INTEGER(JPIM) :: MPRAMID      !> index referring to RAMID pattern
  INTEGER(JPIM) :: MPRCLDIFF    !> index referring to RCLDIFF pattern
  INTEGER(JPIM) :: MPRCLCRIT    !> index referring to RCLCRIT pattern
  INTEGER(JPIM) :: MPRLCRITSNOW !> index referring to RLCRITSNOW pattern
  INTEGER(JPIM) :: MPRAINEVAP   !> index referring to RAINEVAP pattern
  INTEGER(JPIM) :: MPSNOWSUBLIM !> index referring to SNOWSUBLIM pattern
  INTEGER(JPIM) :: MPQSATVERVEL !> index referring to QSATVERVEL pattern
  ! radiation
  INTEGER(JPIM) :: MPZDECORR       !> index for ZDECORR pattern
  INTEGER(JPIM) :: MPZDECORRRAD    !> index for ZDECORR pattern in radiation code
  INTEGER(JPIM) :: MPZSIGQCW       !> index for ZSIGQCW pattern
  INTEGER(JPIM) :: MPZSIGQCWRAD    !> index for ZSIGQCW pattern in radiation code
  INTEGER(JPIM) :: MPZRADEFFLP     !> index for ZRADEFFLP pattern
  INTEGER(JPIM) :: MPZRADEFFLPRAD  !> index for ZRADEFFLP pattern in radiation code
  INTEGER(JPIM) :: MPZRADEFFIP     !> index for ZRADEFFIP pattern
  INTEGER(JPIM) :: MPZRADEFFIPRAD  !> index for ZRADEFFIP pattern in radiation code
  INTEGER(JPIM) :: MPPHRLW         !> index for PHRLW      pattern
  INTEGER(JPIM) :: MPPHRSW         !> index for PHRSW      pattern
  INTEGER(JPIM) :: MPZHSVDAERO     !> index for ZHS_VDAERO pattern
  INTEGER(JPIM) :: MPZHSVDAERORAD  !> index for ZHS_VDAERO pattern in radiation code
  INTEGER(JPIM) :: MPDELTAAERO     !> index for DELTA_AERO pattern
  INTEGER(JPIM) :: MPDELTAAERORAD  !> index for DELTA_AERO pattern in radiation code

  INTEGER(JPIM) :: MP_RAD0      !> first index for radiation related pattern in GP_ARP, ..
  INTEGER(JPIM) :: MP_RAD1      !> last  index for radiation related pattern in GP_ARP, ..

END TYPE TSPP_DATA



TYPE(TSPP_CONFIG) :: YSPP_CONFIG
TYPE(TSPP_DATA)   :: YSPP


END MODULE SPP_MOD

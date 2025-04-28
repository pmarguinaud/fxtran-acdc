SUBROUTINE MF_PHYS_PREP(YDGEOMETRY, YDCPG_BNDS, YDCPG_OPTS, YDARPHY, YDDYNA, YDCPG_DYN0, YDCPG_DYN9, &
& YDCPG_PHY0, YDCPG_PHY9, YDVARS)

!**** *MF_PHYS_PREP* .

!     Purpose.
!     --------
!         Does preparations for MF physics (some memory transfers for example)

!**   Interface.
!     ----------
!        *CALL* *MF_PHYS_PREP(...)*

!        Explicit arguments :
!        --------------------

!     INPUT:
!     ------
!      KST              : first element of work.
!      KEND             : last element of work.
!      KIBL             : index into YROROG types in YDGEOMETRY
!      POROGL,POROGM    : components of grad(orography).
!      PGMV             : upper air GMV variables at time t and t-dt.
!      PR0              : air constant "R" at t.
!      PR9              : air constant "R" at t-dt.
!      PGWFT0           : [gw] at full levels at t.
!      PGWFT9           : [gw] at full levels at t-dt.
!      PGWFL            : zonal comp grad(gw) at full levels at time t.
!      PGWFM            : merid comp grad(gw) at full levels at time t.
!      PNHPRE0F         : "pre" at full levels (time t).
!      PNHPRE9F         : "pre" at full levels (time t-dt).
!      PNHPRE0H         : "pre" at half levels (time t).
!      PNHPRE9H         : "pre" at half levels (time t-dt).
!      PRE0F            : "prehyd" at full levels at time t.
!      PRE9F            : "prehyd" at full levels at time t-dt.
!      PRE0             : "prehyd" at half levels at time t.
!      PRE9             : "prehyd" at half levels at time t-dt.
!      PXYB0            : contains pressure depth, "delta", "alpha" at time t.
!      PXYB9            : contains pressure depth, "delta", "alpha" at time t-dt.

!     OUTPUT:
!     -------
!      PW0              : "w" at full levels at time t.
!      PW9              : "w" at full levels at time t-dt.
!      PW0L             : zonal comp grad(w) at full levels at t.
!      PW0M             : merid comp grad(w) at full levels at t.
!      PRE0F_PHY        : input "pre" for AROME at full levels at time t.
!      PRE9F_PHY        : input "pre" for AROME at full levels at time t-dt.
!      PRE0_PHY         : input "pre" for AROME at half levels at time t.
!      PRE9_PHY         : input "pre" for AROME at half levels at time t-dt.
!      PREHYD0F_PHY     : input "prehyd" for phys. at full levels at t.
!      PREHYD9F_PHY     : input "prehyd" for phys. at full levels at t-dt.
!      PREHYD0_PHY      : input "prehyd" for phys. at half levels at t.
!      PREHYD9_PHY      : input "prehyd" for phys. at half levels at t-dt.
!      PXYB0_PHY        : contains pressure depth, "delta", "alpha" for physics input at t.
!      PXYB9_PHY        : contains pressure depth, "delta", "alpha" for physics input at t-dt.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------
!        K. Yessad (March 2008), after some CPG pieces of code.

!     Modifications.
!     --------------
!   K. Yessad (March 2009): correct false comments for LRWSDLG=T
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   F. Vana  22-Feb_2011:  Horizontal derivatives of w for 3D TKE shear
!   R. El Khatib 16-Mar-2012 Cleaning
!   K. Yessad (June 2017): Introduce NHQE model.
!   K. Yessad (Feb 2018): remove deep-layer formulations.
!     -------------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE CPG_OPTS_TYPE_MOD &
               & , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE
USE FIELD_VARIABLES_MOD, ONLY : FIELD_VARIABLES
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE CPG_TYPE_MOD       , ONLY : CPG_DYN_TYPE, CPG_PHY_TYPE
USE YOMCST   , ONLY : RG
USE YOMARPHY , ONLY : TARPHY
USE YOMDYNA  , ONLY : TDYNA

!     -------------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,   INTENT(IN)    :: YDGEOMETRY
TYPE(CPG_BNDS_TYPE),  INTENT(IN)    :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE),  INTENT(IN)    :: YDCPG_OPTS
TYPE(TARPHY)      ,   INTENT(IN)    :: YDARPHY
TYPE(TDYNA)       ,   INTENT(IN)    :: YDDYNA
TYPE(CPG_DYN_TYPE),   INTENT(IN)    :: YDCPG_DYN0
TYPE(CPG_DYN_TYPE),   INTENT(IN)    :: YDCPG_DYN9
TYPE(CPG_PHY_TYPE),   INTENT(INOUT) :: YDCPG_PHY0
TYPE(CPG_PHY_TYPE),   INTENT(INOUT) :: YDCPG_PHY9
TYPE(FIELD_VARIABLES),INTENT(IN)    :: YDVARS

!     ------------------------------------------------------------------
REAL(KIND=JPRB) :: ZUSG
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
INTEGER(KIND=JPIM) :: JLEV, JLON

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('MF_PHYS_PREP', 0, ZHOOK_HANDLE)

ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM, YDDIMV=>YDGEOMETRY%YRDIMV, YDVAB=>YDGEOMETRY%YRVERT_GEOM%YRVAB)
ASSOCIATE(LMPA=>YDARPHY%LMPA, NFLEVG=>YDDIMV%NFLEVG)
!     ------------------------------------------------------------------

!        1.    Input for AROME physics: transfer "w" and "pre"
!              -----------------------------------------------

! * Transfer "w" and "grad(w)"

ZUSG=1.0_JPRB/RG

!$ACDC PARALLEL,TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn,NAME=PREP {

DO JLEV = 1, NFLEVG
  DO JLON = YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA
    YDCPG_PHY0%W(JLON,JLEV)=YDCPG_DYN0%GWFT(JLON,JLEV)*ZUSG
  ENDDO
ENDDO

IF (LMPA.AND.(.NOT.YDDYNA%LTWOTL)) THEN
  DO JLEV = 1, NFLEVG
    DO JLON=YDCPG_BNDS%KIDIA,YDCPG_BNDS%KFDIA
      YDCPG_PHY9%W(JLON,JLEV)=YDCPG_DYN9%GWFT(JLON,JLEV)*ZUSG
    ENDDO
  ENDDO
ELSEIF (.NOT. YDDYNA%LTWOTL) THEN
  YDCPG_PHY9%W(:,:)=0.0_JPRB
ENDIF

IF (YDDYNA%LNHDYN) THEN
  DO JLEV = 1, NFLEVG
    DO JLON=YDCPG_BNDS%KIDIA,YDCPG_BNDS%KFDIA
      YDCPG_PHY0%WL(JLON,JLEV)=YDCPG_DYN0%GWFL(JLON,JLEV)*ZUSG
      YDCPG_PHY0%WM(JLON,JLEV)=YDCPG_DYN0%GWFM(JLON,JLEV)*ZUSG
    ENDDO
  ENDDO
ELSE
  YDCPG_PHY0%WL(:,:)=0.0_JPRB
  YDCPG_PHY0%WM(:,:)=0.0_JPRB
ENDIF

! * Transfer "pre"

YDCPG_PHY0%PREF(:,:)=0.0_JPRB
YDCPG_PHY0%PRE(:,:)=0.0_JPRB

IF (.NOT. YDDYNA%LTWOTL) THEN
  YDCPG_PHY9%PREF(:,:)=0.0_JPRB
  YDCPG_PHY9%PRE(:,:)=0.0_JPRB
ENDIF

IF (LMPA) THEN
  IF (YDDYNA%LNHDYN.AND.YDDYNA%LTWOTL) THEN
    YDCPG_PHY0%PREF(:,:)=YDCPG_DYN0%NHPREF(:,:)
    YDCPG_PHY0%PRE(:,:)=YDCPG_DYN0%NHPREH(:,:)
  ELSEIF (YDDYNA%LNHDYN.AND.(.NOT.YDDYNA%LTWOTL)) THEN
    YDCPG_PHY9%PREF(:,:)=YDCPG_DYN9%NHPREF(:,:)
    YDCPG_PHY9%PRE(:,:)=YDCPG_DYN9%NHPREH(:,:)
  ELSEIF ((.NOT.YDDYNA%LNHDYN).AND.YDDYNA%LTWOTL) THEN
    YDCPG_PHY0%PREF(:,:)=YDCPG_DYN0%PREF(:,:)
    YDCPG_PHY0%PRE(:,:)=YDCPG_DYN0%PRE(:,:)
  ELSEIF ((.NOT.YDDYNA%LNHDYN).AND.(.NOT.YDDYNA%LTWOTL)) THEN
    YDCPG_PHY9%PREF(:,:)=YDCPG_DYN9%PREF(:,:)
    YDCPG_PHY9%PRE(:,:)=YDCPG_DYN9%PRE(:,:)
  ENDIF
ENDIF

!     ------------------------------------------------------------------

!        2.    store variables based on "prehyd" 
!              ---------------------------------

! "t" variables.
YDCPG_PHY0%PREHYDF(:,:)=YDCPG_DYN0%PREF(:,:)
YDCPG_PHY0%PREHYD(:,:)=YDCPG_DYN0%PRE(:,:)
YDCPG_PHY0%XYB%DELP (:,:)=YDCPG_DYN0%XYB%DELP (:,:)
YDCPG_PHY0%XYB%RDELP(:,:)=YDCPG_DYN0%XYB%RDELP(:,:)
YDCPG_PHY0%XYB%LNPR (:,:)=YDCPG_DYN0%XYB%LNPR (:,:)
YDCPG_PHY0%XYB%ALPH (:,:)=YDCPG_DYN0%XYB%ALPH (:,:)

! "t-dt" variables.
IF (.NOT.YDDYNA%LTWOTL) THEN
  YDCPG_PHY9%PREHYDF(:,:)=YDCPG_DYN9%PREF(:,:)
  YDCPG_PHY9%PREHYD(:,:)=YDCPG_DYN9%PRE(:,:)
  YDCPG_PHY9%XYB%DELP (:,:)=YDCPG_DYN9%XYB%DELP (:,:)
  YDCPG_PHY9%XYB%RDELP(:,:)=YDCPG_DYN9%XYB%RDELP(:,:)
  YDCPG_PHY9%XYB%LNPR (:,:)=YDCPG_DYN9%XYB%LNPR (:,:)
  YDCPG_PHY9%XYB%ALPH (:,:)=YDCPG_DYN9%XYB%ALPH (:,:)
ENDIF

!$ACDC }

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('MF_PHYS_PREP', 1, ZHOOK_HANDLE)
END SUBROUTINE MF_PHYS_PREP


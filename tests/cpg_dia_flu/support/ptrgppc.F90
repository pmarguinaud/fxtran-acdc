MODULE PTRGPPC

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!*    Pointers for grid point array for PC schemes (obsolescent feature
!      which has not yet been moved into GMV, GMVS and GFL).
!     These pointers must later to be put in GMV+GMVS (new attributes
!      to create), and also GFL for MGPPCF_GFLP.
!     They are linked to the different PC schemes and they are no
!      longer purely NH pointers, so they have been renamed in Jan 2008.

!  NFGPPC       - number of (2D) fields in PC gridpoint buffer GPPCBUF

!  MGPPC        - whole GPPCBUF-pointer.
!  -- Pointers for LPC_FULL (hydrostatic or NH):
!  MGPPCF_U     - pointer for U.
!  MGPPCF_V     - pointer for V.
!  MGPPCF_T     - pointer for T.
!  MGPPCF_SPD   - pointer for pressure departure variable.
!  MGPPCF_SVD   - pointer for vertical divergence variable.
!  MGPPCF_SP    - pointer for log(prehyds).
!  -- Additional pointers for LPC_CHEAP (hydrostatic or NH):
!     (they are used to save interpolated quantities from the predictor
!     to the corrector step).
!  MGPPCF_CP    - pointer for continuity eqn (3D quantity).
!  MGPPCF_NHX   - pointer for NHX.
!  MGPPCF_UP    - pointer for split EC physics (U eqn).
!  MGPPCF_VP    - pointer for split EC physics (V eqn).
!  MGPPCF_TP    - pointer for split EC physics (T eqn).
!  MGPPCF_GFLP  - pointer for split EC physics (GFL eqn).
!  MGPPCF_BBC   - pointer for BBC quantity (LRDBBC=T).

!  The same names with a suffix "5": pointers for trajectory.

TYPE :: TPTRGPPC
INTEGER(KIND=JPIM) :: NFGPPC

INTEGER(KIND=JPIM) :: MGPPC

INTEGER(KIND=JPIM) :: MGPPCF_U
INTEGER(KIND=JPIM) :: MGPPCF_V
INTEGER(KIND=JPIM) :: MGPPCF_T
INTEGER(KIND=JPIM) :: MGPPCF_SPD
INTEGER(KIND=JPIM) :: MGPPCF_SVD
INTEGER(KIND=JPIM) :: MGPPCF_SP

INTEGER(KIND=JPIM) :: MGPPCF_CP
INTEGER(KIND=JPIM) :: MGPPCF_NHX
INTEGER(KIND=JPIM) :: MGPPCF_UP
INTEGER(KIND=JPIM) :: MGPPCF_VP
INTEGER(KIND=JPIM) :: MGPPCF_TP
INTEGER(KIND=JPIM) :: MGPPCF_GFLP
INTEGER(KIND=JPIM) :: MGPPCF_BBC

INTEGER(KIND=JPIM) :: MGPPC5

INTEGER(KIND=JPIM) :: MGPPCF_U5
INTEGER(KIND=JPIM) :: MGPPCF_V5
INTEGER(KIND=JPIM) :: MGPPCF_T5
INTEGER(KIND=JPIM) :: MGPPCF_SPD5
INTEGER(KIND=JPIM) :: MGPPCF_SVD5
INTEGER(KIND=JPIM) :: MGPPCF_SP5

CONTAINS
  
   
    
END TYPE TPTRGPPC

!!TYPE(TPTRGPPC), POINTER :: YRPTRGPPC => NULL()
  !---------------------------------------------------------------------

CONTAINS 
  



END MODULE PTRGPPC

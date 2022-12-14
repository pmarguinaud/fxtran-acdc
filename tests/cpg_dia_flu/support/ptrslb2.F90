MODULE PTRSLB2

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     NFLDSLB2     - Number of fields in semi-lagrangian buffer 2
!     MSLBUF2      - whole SLB2-pointer.
!     MSLB2DBBC1   - SLB2-pointer for intermediate quantity ((p/mRT)*Jacobian
!                    part of Laplacian) in Lth level for diagnostic BBC in NH.
!     MSLB2DPHI1   - SLB2-pointer for (p/m Rd T)_L for diagnostic BBC in NH.
!     MSLB2USI     - SLB2-pointer for t SI quantity in U-wind eqn.
!     MSLB2VSI     - SLB2-pointer for t SI quantity in V-wind eqn.
!     MSLB2TSI     - SLB2-pointer for t SI quantity in T eqn.
!     MSLB2PDSI    - SLB2-pointer for t SI quantity in Pcha eqn.
!     MSLB2VDSI    - SLB2-pointer for t SI quantity in dcha eqn.
!     MSLB2SPSI    - SLB2-pointer for t SI quantity in continuity eqn.
!     MSLB2VVEL    - SLB2-pointer for vertical velocity.
!     MSLB2URL     - SLB2-pointer for gp U-wind used in the SL traj research. 
!     MSLB2VRL     - SLB2-pointer for gp V-wind used in the SL traj research.
!     MSLB2WRL     - SLB2-pointer for gp "etadot"-wind used in the SL traj research.
!     MSLB2URL5    - SLB2-pointer for gp U-wind used in the SL traj res (TL).
!     MSLB2VRL5    - SLB2-pointer for gp V-wind used in the SL traj res (TL).
!     MSLB2WRL5    - SLB2-pointer for gp "etadot" used in the SL traj res (TL).
!     MSLB2USI5    - SLB2-pointer for t SI quantity in U-wind eqn
!                    (same as MSLB2USI but for trajectory).
!     MSLB2VSI5    - SLB2-pointer for t SI quantity in V-wind eqn
!                    (same as MSLB2VSI but for trajectory).
!     MSLB2U15     - SLB2-pointer for grid-point quantity in U-wind eqn
!                    (same as MSLB2U1 but for trajectory).
!     MSLB2V15     - SLB2-pointer for grid-point quantity in V-wind eqn
!                    (same as MSLB2V1 but for trajectory).
!     MSLB2T15     - SLB2-pointer for grid-point quantity in T eqn
!                    (same as MSLB2T1 but for trajectory).
!     MSLB2Q15     - SLB2-pointer for grid-point quantity in Q eqn
!                    (same as MSLB2Q1 but for trajectory).
!     MSLB2KAPPA   - SLB2-pointer for the KAPPA function controlling SLHD 
!     MSLB2KAPPAT  - SLB2-pointer for the KAPPA function controlling SLHD on T 
!                    (valid at t0)
!     MSLB2KAPPAM  - SLB2-pointer for horizontal momentum echange coef.
!                     used in 3D turbulence (valid at t+dt in F)
!     MSLB2KAPPAH  - SLB2-pointer for horizontal heat (scalar) echange coef.
!                     used in 3D turbulence (valid at t+dt in F)
!     MSLB2KAPPA5  - trajectory of the KAPPA function (MSLB2KAPPA)
!     MSLB2KAPPAT5 - trajectory of the KAPPA function for T (MSLB2KAPPAT)
!     MSLB2GWF     - SLB2-pointer to store full level "gw" at t or t-dt.
!     MSLB2GDW     - SLB2-pointer to store full level "g dw" at t or t-dt.
!     MSLB2GWS     - SLB2-pointer to store "g w_surf" at t or t-dt.
!     MSLB2STDDISU - SLB2-pointer for the zonal STDDIS coef. for COMAD 
!     MSLB2STDDISV - SLB2-pointer for the meridional STDDIS coef. for COMAD 
!     MSLB2STDDISW - SLB2-pointer for the vertical STDDIS coef. for COMAD 

TYPE :: TPTRSLB2
INTEGER(KIND=JPIM) :: NFLDSLB2
INTEGER(KIND=JPIM) :: MSLBUF2
INTEGER(KIND=JPIM) :: MSLB2DBBC1
INTEGER(KIND=JPIM) :: MSLB2DPHI1
INTEGER(KIND=JPIM) :: MSLB2USI
INTEGER(KIND=JPIM) :: MSLB2VSI
INTEGER(KIND=JPIM) :: MSLB2TSI
INTEGER(KIND=JPIM) :: MSLB2PDSI
INTEGER(KIND=JPIM) :: MSLB2VDSI
INTEGER(KIND=JPIM) :: MSLB2SPSI
INTEGER(KIND=JPIM) :: MSLB2VVEL
INTEGER(KIND=JPIM) :: MSLB2URL
INTEGER(KIND=JPIM) :: MSLB2VRL
INTEGER(KIND=JPIM) :: MSLB2WRL
INTEGER(KIND=JPIM) :: MSLB2URL5
INTEGER(KIND=JPIM) :: MSLB2VRL5
INTEGER(KIND=JPIM) :: MSLB2WRL5
INTEGER(KIND=JPIM) :: MSLB2USI5
INTEGER(KIND=JPIM) :: MSLB2VSI5
INTEGER(KIND=JPIM) :: MSLB2U15
INTEGER(KIND=JPIM) :: MSLB2V15
INTEGER(KIND=JPIM) :: MSLB2T15
INTEGER(KIND=JPIM) :: MSLB2Q15
INTEGER(KIND=JPIM) :: MSLB2KAPPA
INTEGER(KIND=JPIM) :: MSLB2KAPPAT
INTEGER(KIND=JPIM) :: MSLB2KAPPAM
INTEGER(KIND=JPIM) :: MSLB2KAPPAH
INTEGER(KIND=JPIM) :: MSLB2KAPPA5
INTEGER(KIND=JPIM) :: MSLB2KAPPAT5
INTEGER(KIND=JPIM) :: MSLB2GWF
INTEGER(KIND=JPIM) :: MSLB2GDW
INTEGER(KIND=JPIM) :: MSLB2GWS
INTEGER(KIND=JPIM) :: MSLB2STDDISU
INTEGER(KIND=JPIM) :: MSLB2STDDISV
INTEGER(KIND=JPIM) :: MSLB2STDDISW
!--------------------------------------------------------------
CONTAINS
  
   

END TYPE TPTRSLB2
!==============================================================

!!TYPE(TPTRSLB2), POINTER :: YRPTRSLB2 => NULL()
CONTAINS 
  


END MODULE PTRSLB2

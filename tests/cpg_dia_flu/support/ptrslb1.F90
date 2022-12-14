MODULE PTRSLB1

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     SEMI LAGRANGIAN BUFFER 1

!     Abbreviations:
!     "O" stands for "origin point", "M" stands for medium point.
!     "NH" stands for "non hydrostatic", "SI" stands for "semi-implicit". 

!     * NFLDSLB1  : Number of fields in semi-lagrangian buffer 1
!     * RPARSL1   : parities for extrapolar extensions for interpol. buffers.
!     * MSLBUF1   : start pointer for interpolation buffer 1.
!     * MSLB1U9   : pointer for U-wind eqn term to be interpolated at O.
!     * MSLB1V9   : pointer for V-wind eqn term to be interpolated at O.
!     * MSLB1T9   : pointer for T eqn term to be interpolated at O.
!     * MSLB1GFL9 : pointer for unified_treatment grid-point fields
!     * MSLB1GFLSP9 : pointer for spline rep. of gfl needed for spline int. at O.
!     * MSLB1PD9  : pointer for P-hat eqn term to be interpolated at O (NH).
!     * MSLB1VD9  : pointer for d-hat eqn term to be interpolated at O (NH).
!     * MSLB1NHX9 : pointer for "X" term eqn term to be interpolated at O (NH).
!     * MSLB1UR0  : pointer for U-wind to be interpolated for traj research.
!     * MSLB1VR0  : pointer for V-wind to be interpolated for traj research.
!     * MSLB1ZR0  : pointer for Z-wind to be interpolated for traj research.
!     * MSLB1WR0  : pointer for "etadot" to be interpolated for traj research.
!     * MSLB1UR00 : pointer for U-wind at t to be interpolated for traj research.
!     * MSLB1VR00 : pointer for V-wind at t to be interpolated for traj research.
!     * MSLB1ZR00 : pointer for Z-wind at t to be interpolated for traj research.
!     * MSLB1WR00 : pointer for "etadot" at t to be interpolated for traj research.
!     * MSLB1WRA  : pointer for "etadot" at the arrival point
!     * MSLB1UR9  : pointer for t-dt U-wind to be interpolated at O for 
!                   Coriolis term refined recalculation (L2TLFF=T).
!     * MSLB1VR9  : pointer for t-dt V-wind to be interpolated at O for
!                   Coriolis term refined recalculation (L2TLFF=T).
!     * MSLB1DBBC9: pointer for intermediate quantity ((p/mRT)*Jacobian part
!                   of Laplacian in Lth level) to be interpolated at O
!                   for diagnostic BBC in NH.
!     * MSLB1DPHI9: pointer for (p/m Rd T) to be interpolated at O for
!                   diagnostic BBC in NH.
!     * MSLB1GWS9 : pointer for (gw_s) for high-order horizontal interp.
!                   at O(L) for diagnostic BBC in NH.
!     * MSLB1U0   : pointer for U-wind eq term to be interpolated linearly at O.
!     * MSLB1V0   : pointer for V-wind eq term to be interpolated linearly at O.
!     * MSLB1T0   : pointer for T eqn term to be interpolated linearly at O.
!     * MSLB1PD0  : pointer for P-hat eqn term to be interpolated linearly at O.
!     * MSLB1VD0  : pointer for d-hat eqn term to be interpolated linearly at O.
!     * MSLB1C9   : pointer for 3D continuity eqn term to be interpolated at O.
!     * MSLB1SP9  : pointer for 2D continuity eqn term to be interpolated at O.
!     * MSLB1SP0  : pointer for 2D continuity eqn term to be interpolated
!                   linearly at O.
!     * MSLB1C0   : pointer for 3D continuity eqn term to be interpolated
!                   linearly at O.
!     * MSLB1UP9  : pointer for U-wind eqn term to be interpolated at O:
!                   tendency in split physics (if LSLPHY=.T. only). 
!     * MSLB1VP9  : pointer for V-wind eqn term to be interpolated at O:
!                   tendency in split physics (if LSLPHY=.T. only). 
!     * MSLB1TP9  : pointer for T eqn term to be interpolated at O:
!                   tendency in split physics (if LSLPHY=.T. only). 
!     * MSLB1GFLP9: pointer for unified_treatment grid-point fields
!                   tendency in split physics (if LSLPHY=.T. only). 
!     * MSLB1[X]9_SI: cf. MSLB1[X]9 ([X]=U,V,T,PD,VD,C) for cases where linear
!                   terms must be separately interpolated (contain separate
!                   linear terms).
!     * MSLB1[X]F9: pointers (applicable only when NSPLTHOI /= 0) for
!                   second part of the quantity to be interpolated at O
!                   by high order interpolation. ([X]=U,V,T,GFL,VD,GFLSP)
!     * MSLB1[X]9_NL: cf. MSLB1[X]9_NL ([X]=U,V,T,PD,VD,C) for cases where 
!                   nonlinear tendency must be separated if LSETTLS and LPC_CHEAP

TYPE :: TPTRSLB1
INTEGER(KIND=JPIM) :: NFLDSLB1

REAL(KIND=JPRB),ALLOCATABLE:: RPARSL1(:)

INTEGER(KIND=JPIM) :: MSLBUF1
INTEGER(KIND=JPIM) :: MSLB1U9
INTEGER(KIND=JPIM) :: MSLB1V9
INTEGER(KIND=JPIM) :: MSLB1T9
INTEGER(KIND=JPIM) :: MSLB1GFL9
INTEGER(KIND=JPIM) :: MSLB1GFLSP9
INTEGER(KIND=JPIM) :: MSLB1PD9
INTEGER(KIND=JPIM) :: MSLB1VD9
INTEGER(KIND=JPIM) :: MSLB1NHX9
INTEGER(KIND=JPIM) :: MSLB1UR0
INTEGER(KIND=JPIM) :: MSLB1VR0
INTEGER(KIND=JPIM) :: MSLB1ZR0
INTEGER(KIND=JPIM) :: MSLB1WR0
INTEGER(KIND=JPIM) :: MSLB1UR00
INTEGER(KIND=JPIM) :: MSLB1VR00
INTEGER(KIND=JPIM) :: MSLB1ZR00
INTEGER(KIND=JPIM) :: MSLB1WR00
INTEGER(KIND=JPIM) :: MSLB1WRA
INTEGER(KIND=JPIM) :: MSLB1UR9
INTEGER(KIND=JPIM) :: MSLB1VR9
INTEGER(KIND=JPIM) :: MSLB1DBBC9
INTEGER(KIND=JPIM) :: MSLB1DPHI9
INTEGER(KIND=JPIM) :: MSLB1GWS9
INTEGER(KIND=JPIM) :: MSLB1U0
INTEGER(KIND=JPIM) :: MSLB1V0
INTEGER(KIND=JPIM) :: MSLB1T0
INTEGER(KIND=JPIM) :: MSLB1PD0
INTEGER(KIND=JPIM) :: MSLB1VD0
INTEGER(KIND=JPIM) :: MSLB1C9
INTEGER(KIND=JPIM) :: MSLB1SP9
INTEGER(KIND=JPIM) :: MSLB1SP0
INTEGER(KIND=JPIM) :: MSLB1C0
INTEGER(KIND=JPIM) :: MSLB1UP9
INTEGER(KIND=JPIM) :: MSLB1VP9
INTEGER(KIND=JPIM) :: MSLB1TP9
INTEGER(KIND=JPIM) :: MSLB1GFLP9
INTEGER(KIND=JPIM) :: MSLB1U9_SI
INTEGER(KIND=JPIM) :: MSLB1V9_SI
INTEGER(KIND=JPIM) :: MSLB1T9_SI
INTEGER(KIND=JPIM) :: MSLB1PD9_SI
INTEGER(KIND=JPIM) :: MSLB1VD9_SI
INTEGER(KIND=JPIM) :: MSLB1C9_SI
INTEGER(KIND=JPIM) :: MSLB1UF9
INTEGER(KIND=JPIM) :: MSLB1VF9
INTEGER(KIND=JPIM) :: MSLB1TF9
INTEGER(KIND=JPIM) :: MSLB1GFLF9
INTEGER(KIND=JPIM) :: MSLB1VDF9
INTEGER(KIND=JPIM) :: MSLB1GFLSPF9
INTEGER(KIND=JPIM) :: MSLB1U9_NL
INTEGER(KIND=JPIM) :: MSLB1V9_NL
INTEGER(KIND=JPIM) :: MSLB1T9_NL
INTEGER(KIND=JPIM) :: MSLB1PD9_NL
INTEGER(KIND=JPIM) :: MSLB1VD9_NL
INTEGER(KIND=JPIM) :: MSLB1C9_NL
!--------------------------------------------------------------
CONTAINS
  
   

END TYPE TPTRSLB1
!==============================================================

!!TYPE(TPTRSLB1), POINTER :: YRPTRSLB1 => NULL()

CONTAINS 
  


END MODULE PTRSLB1

MODULE YEMLBC_GEO

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Defining specific geometry variables for LAM model: those linked with LBC and I+E zones.
!     These variables are set-up in SUEGEOLBC and should not be modified elsewhere.

!     ------------------------------------------------------------------

TYPE :: TELBC_GEO

  ! NIND_LIST,NIND_LEN: help arrays for memory transfers between
  !  (NPROMA,NGPBLKS)-dimensioned arrays.
  ! NCPLBLKS : number of active NPROMA-sized blocks (ie where the number of coupling points is bigger than zero)
  ! MPTRCPLBLK : index of the active coupling blocks among the NGPLBLKS model blocks

  INTEGER(KIND=JPIM), ALLOCATABLE :: NIND_LIST(:,:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NIND_LEN(:)
  INTEGER(KIND=JPIM) :: NCPLBLKS
  INTEGER(KIND=JPIM), ALLOCATABLE :: MPTRCPLBLK(:)

END TYPE TELBC_GEO

!TYPE(TELBC_GEO), POINTER :: YRGEOLBC => NULL()   ! moved to type_geometry.F90

!     ------------------------------------------------------------------
END MODULE YEMLBC_GEO

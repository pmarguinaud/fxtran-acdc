MODULE YOMLAP

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    Constants related to the Laplace space


!  NASN0(0:NSMAX)  : address in a spectral array of (n, m=0)
!  NASM0(0:NSMAX)  : address in a spectral array of (m, n=m)
!  NASM0G(0:NSMAX) : address in a global version of spectral array of (m, n=m)
!  NVALUE(NTPEC2)  : n value for each NSPEC2 spectral coeffient
!  MYMS(0:NUMP)    : actual wave numbers handled by this processor
!  NSPZERO(0:NSMAX): Index for the imaginary m=0 values
!  NSE0L(NUMP)     : Index array, used for example in Helmholtz operator and
!                    horizontal diffusion operator, to find the first element
!                    of the non-zero diagonals of these operators for each
!                    zonal wave number m. DM-local
!  RLAPDI(-1:NSMAX+2) :  eigen-values of the Laplace operator
!  RLAPIN(-1:NSMAX+2) :  eigen-values of its inverse

TYPE TLAP
  INTEGER(KIND=JPIM), ALLOCATABLE :: NASN0(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NASM0(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NASM0G(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NVALUE(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: MYMS(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NSPZERO(:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NSE0L(:)

  REAL(KIND=JPRB),    ALLOCATABLE :: RLAPDI(:)
  REAL(KIND=JPRB),    ALLOCATABLE :: RLAPIN(:)
END TYPE TLAP


END MODULE YOMLAP

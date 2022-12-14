MODULE YEMMP

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!  ---------------------------------------------------------------------
!  C. Fischer : 99-06-11

!* store extra setup to perform distributed work along the efficient
!* wavenumber kstar in aladin 3d-var 

!  NEPROCN: number of the processor which possesses global kstar
!  NUEMP  : number of kstars treated in this processor
!  MYENS  : array containing those kstars (in 0:nsmax) treated in this proc
!  NUEMPP : array containing the number of kstars treated proc by proc
!  NEALLNS: array containing in a row the values of kstars proc by proc
!  NEPTRNS: pointer into neallns


TYPE :: TEMMP
  INTEGER(KIND=JPIM), POINTER :: NEPROCN(:) => NULL()
  INTEGER(KIND=JPIM) :: NUEMP
  INTEGER(KIND=JPIM), POINTER :: MYENS(:) => NULL()
  INTEGER(KIND=JPIM), POINTER :: NUEMPP(:) => NULL()
  INTEGER(KIND=JPIM), POINTER :: NEALLNS(:) => NULL()
  INTEGER(KIND=JPIM), POINTER :: NEPTRNS(:) => NULL()
END TYPE TEMMP

!  ---------------------------------------------------------------------
END MODULE YEMMP

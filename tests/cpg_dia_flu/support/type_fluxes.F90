MODULE TYPE_FLUXES

!     Purpose.
!     --------
!      To define the descriptors of families of model fluxes.
!      "FLUXES_DESCRIPTOR" describes :
!      - the "FA" prefix name
!      - the "FA" suffix name
!      - the index values of full/half levels if the field is upper air
!      - the total number of levels
!      - the vectorial parity

!     Author.
!     -------
!        Ryad El Khatib *METEO-FRANCE*

!     Modifications.
!     --------------
!        Original : 2001-01-09
!        M.Hamrud   01-Oct-2003 CY28 Cleaning
!       R El Khatib 05-03-11 Cleanups

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM, JPRB
USE PARDIM    ,ONLY : JPMXLE

IMPLICIT NONE
SAVE

PRIVATE
PUBLIC FLUXES_DESCRIPTOR

TYPE FLUXES_DESCRIPTOR
  CHARACTER(LEN=4)   :: CLPREF(JPMXLE+1)
  CHARACTER(LEN=16)  :: CLSUFF
  INTEGER(KIND=JPIM) :: INUMLEV(JPMXLE+1)
  INTEGER(KIND=JPIM) :: ILEV
  INTEGER(KIND=JPIM) :: IPARITY
  LOGICAL            :: LACTIVE = .FALSE.
  INTEGER(KIND=JPIM) :: ILEVBOT = 0 ! Bottom level for input field in cpxfu.F90/cpcfu.F90
  INTEGER(KIND=JPIM) :: IOFFSET = 0 ! Offset in XFU/CFU buffer
END TYPE FLUXES_DESCRIPTOR

CONTAINS


!     ------------------------------------------------------------
END MODULE TYPE_FLUXES

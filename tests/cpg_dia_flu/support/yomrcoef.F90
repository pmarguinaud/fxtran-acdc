MODULE YOMRCOEF

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!   -----------------------------------------------------------------

!*   Logical switches for writing and reading the radiation coefficients
!    in file or in core

!    LRCOEF                       : switch for write out and read
!                                   the radiation coefficients in file

!*   Logical switch connected with the simplified radiation

!    LTLADDIA    : switch for using diabatic adjoint and then reading
!                  rad.coef. at each time step when LRAYSP
!    LGLOBRAD    : switch to compute the global mean for the thermal
!                  radiation matrix

!*   Number of arrays for radiation coefficients

!    NG3SR                        : number of 3D fields (NFLEVG)
!    NGMTR                        : number of matrices (0:NFLEVG,0:NFLEVG)
!    NLATWR                       : number of latitude to be written
!    NLATRD                       : number of latitude to be read

!*   Buffer for radiation coefficients

!    NLENGSRB                       : length of buffer for 3D solar radiation
!                                     coefficients and correction for thermal
!                                     radiation
!    NLENGTRB                       : length of buffer for matrix of thermal
!                                     radiation coefficients
!    SOLRAD(NLENGSRB)               : buffer for 3D fields
!    THERRAD(NSLBR/NGPBLKS/1,NLENGTRB): buffer for matrix
!                                     for 3D fields

!*   Buffer for simplified thermal radiation

!    TRWEIGHT(NFLEVG+1,NFLEVG+1) : buffer for sum of mean weights
!                                  for the whole domain
!    TRMATSUM(NFLEVG+1,NFLEVG+1) : buffer for sum of thermal radiation
!                                  matrices for the whole domain

TYPE :: TRCOEF
LOGICAL :: LRCOEF
LOGICAL :: LTLADDIA
LOGICAL :: LGLOBRAD

INTEGER(KIND=JPIM) :: NG3SR
INTEGER(KIND=JPIM) :: NGMTR
INTEGER(KIND=JPIM) :: NLATWR
INTEGER(KIND=JPIM) :: NLATRD

INTEGER(KIND=JPIM) :: NLENGSRB
INTEGER(KIND=JPIM) :: NLENGTRB

REAL(KIND=JPRB),ALLOCATABLE:: SOLRAD(:)
REAL(KIND=JPRB),ALLOCATABLE:: THERRAD(:,:)

REAL(KIND=JPRB),ALLOCATABLE:: TRWEIGHT(:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRMATSUM(:,:)
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TRCOEF
!============================================================================

!!TYPE(TRCOEF), POINTER :: YRRCOEF => NULL()


!   ----------------------------------------------------------------

CONTAINS



END MODULE YOMRCOEF

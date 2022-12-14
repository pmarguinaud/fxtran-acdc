MODULE YOMDPHY

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

TYPE :: TDPHY

!     ------------------------------------------------------------------

!     DIMENSION DES TABLEAUX POINT DE GRILLE PHYSIQUE

!     NCSS : number of deep soil layers

!     NVXP : number of variables in the generic EXTRP.
!     NVXP2: number of variables in the generic XTRP2.

!     NCXP : number of levels in EXTRP
!     NCSI : number of sea-ice levels
!     NCOM : number of ocean mix-layer model levels
!     NCSNEC: number of snow levels in EC physics (specific for ECMWF) 
!     NTILES: number of surface tiles

!     NVEXTR : number of variables in the generic VEXTRA (extra-fields)
!     NVEXTRRAD : number of variables in the radiation VEXTRA (extra-fields)
!     NVEXTRDYN : number of extra-fields comming from the dynamics
!     NVXTR2 : number of variables in the generic VEXTR2

!     NVECOUT : number of model variable put out via passive DDH pathway

!     NCEXTR : number of levels in the generic VEXTRA

!     NTSV : nombre de types de vegetations.
!     NTOZ1D: 1 si representation 1D des NVCLIS variables , 0 sinon
!     NTOZ2D: 1 si representation 2D des NVCLIS variables , 0 sinon
!     NTOZ3D: 1 si representation 3D des NVCLIS variables , 0 sinon
!     NTSSG : number of surface temperatures for subgrid diagnostics

!     LTPROF: .T. if more than 1 vertical layer in deep soil
!     LDIRCLSMOD: L to take CLS model equivalent directly from input file
!     LDIRSICMOD: L to take sea-ice model (CLS option) equivalent directly from input file

INTEGER(KIND=JPIM) :: NCSS
INTEGER(KIND=JPIM) :: NVXP
INTEGER(KIND=JPIM) :: NVXP2
INTEGER(KIND=JPIM) :: NCXP
INTEGER(KIND=JPIM) :: NCSI
INTEGER(KIND=JPIM) :: NCOM
INTEGER(KIND=JPIM) :: NCSNEC
INTEGER(KIND=JPIM) :: NTILES
INTEGER(KIND=JPIM) :: NVEXTR
INTEGER(KIND=JPIM) :: NVEXTRDI
INTEGER(KIND=JPIM) :: NVEXTRRAD
INTEGER(KIND=JPIM) :: NVEXTRDYN
INTEGER(KIND=JPIM) :: NVXTR2
INTEGER(KIND=JPIM) :: NVECOUT
INTEGER(KIND=JPIM) :: NCEXTR
INTEGER(KIND=JPIM) :: NVCLIS
INTEGER(KIND=JPIM) :: NTOZ1D
INTEGER(KIND=JPIM) :: NTOZ2D
INTEGER(KIND=JPIM) :: NTOZ3D
INTEGER(KIND=JPIM) :: NTSSG
LOGICAL            :: LTPROF
LOGICAL            :: LDIRCLSMOD
LOGICAL            :: LDIRSICMOD
CONTAINS
   
END TYPE TDPHY
!     --------------------------------------------------------------------------------

TYPE(TDPHY), POINTER :: YRDPHY => NULL()

!     ------------------------------------------------------------------
CONTAINS 
  


END MODULE YOMDPHY

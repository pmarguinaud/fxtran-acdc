MODULE YOMMP

!$ACDC methods 


USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

! ----------------------------------------------------------------------

TYPE :: TMP

!*    common block describing the partitioning of data

!     Abbreviation DM-ARR stands for "DM distribution for arrival geometry"
!     Abbreviation DM-DEP stands for "DM distribution for departure geometry"

! ----------------------------------------------------

!  nprocm(0:ncmax) :  gives process which is responsible for Legendre
!             transforms, and spectral space calculations for a
!             certain wave number m
!  numpp(n_regions_ns) : the number of wave numbers each a-set is responsible
!             for. As aspecial case NUMP = NUMPP(MYSETA).
!  nallms(0:max(nsmax,nmsmax)) :  wave numbers for all a-set concate-
!             nated together to give all wave numbers in a-set order.
!             Used when global spectral norms have to be gathered.
!  nptrms(n_regions_ns)  :  pointer to the first wave number of a given a-set
!             in nallms array.
!  mylats(1:ndgenl) if LMESSP else mylats(ndgsag:ndgeng) : mapping
!             between physical latitude number and local latitude number
!             in grid point space on this process. This is identical
!             for all processes within an a-set
!  nptrls(n_regions_ns) : pointer to first global latitude of each a-set
!             for which it performs the Fourier calculations
!  nptrlsf(n_regions_ns) : pointer to first global latitude of each a-set
!             for which it performs the Fourier calculations
!  nfrstlat(n_regions_ns) : first lat of each a-set in grid-point space
!  nfrstloff: offset for first lat of own a-set in grid-point space,
!             i.e. nfrstloff=nfrstlat(my_region_ns)-1
!  nlstlat(n_regions_ns) : last lat of each a-set in grid-point space
!  nptrfrstlat(n_regions_ns) : pointer to the first latitude of each a-set in
!             NSTA and NONL arrays
!  nptrlstlat(n_regions_ns) : pointer to the last latitude of each a-set in
!             NSTA and NONL arrays
!  nptrfloff    : offset for pointer to the first latitude of own a-set
!               NSTA and NONL arrays, i.e. nptrfrstlatf(my_region_ns)-1
!  nptrlat      : pointer to start of latitude in grid-point space
!  lsplitlat(ndglg) : true if latitude is split in grid point space
!              over two a-sets
!  myfrstactlat : first actual lat on this PE in grid-point space,
!                 it is nfrstlat(my_region_ns)
!  mylstactlat  : last actual lat on this PE in grid-point space,
!                 it is nlstlat(my_region_ns)
! ------------------------------------------------------------------
!  nptrsv(nprtrw+1) :  pointer to first spectral wave column to be
!             handled by each b-set. Used for semi-implicit calculations
!             and Jb vertical transforms, and only really if nprtrv>1.
!  nptrsvf(nprtrv+1) :  As nptrsv but for the case where full m-columns
!             have to be treated by one processor for the vertical
!             spectral calculations. This is the case if implicit
!             treatment of Coriolis terms is used and in other cases.
!  nptrmf(nprtrv+1)  :  Distribution of m-columns among b-sets used for
!             the full m-column cases where nptrsvf() is used.
!  nspstaf(0:nsmax) : pointer to where each m-column starts (used for
!             the full m-column cases where nptrsvf() is used.
!  numll(nprtrv+1) :  distribution of levels among b-sets for Legendre
!             transforms, FFT and horizontal diffusion.
!             To simplify coding numll(nprtrv+1) is defined to zero.
!  nptrll(nprtrv+1) :  defines the first level treated on each b-set
!             To simplify coding nptrll(nprtrv+1)=nptrll(nprtrv)
!  npsp    :  =1 if surface pressure is handled by this processor for
!             the Legendre Trasforms and FFT calculations. npsp is
!             the same for all processors within a b-set.
!  npsurf(nprtrv)  :  contains the npsp-values for each b-set
!  nbsetlev(nflevg) :  the b-set on which a level belongs. Please use
!              global indexing.
!  nbsetsp :  the b-set on which the surface pressure belongs.
!  mylevs(nflevl) :  mapping between local and global numbering for the
!             levels handled by this process.
!  nspec2v :  number of spectral columns treated by this process for
!             semi-implicit calculations and other vertical transforms
!  nspec2vf:  number of spectral columns treated by this process for
!             semi-implicit calculations for the full m-column cases.
!             See nptrsvf().
!  nspec2vddh: cf. nspec2v but for arrays only used in the DDH package
!  nspec2v_nh: cf. nspec2v but for arrays only used in the NH model
!  nsta(ndgsag:ndgeng+n_regions_ns-1,n_regions_ew) :  Position of first grid column
!             for the latitudes on a processor. The information is
!             available for all processors. The b-sets are distinguished
!             by the last dimension of nsta(). The latitude band for
!             each a-set is addressed by nptrfrstlat(jaset),
!             nptrlstlat(jaset), and nptrfloff=nptrfrstlat(my_region_ns) on
!             this processors a-set. Each split latitude has two entries
!             in nsta(,:) which necessitates the rather complex
!             addressing of nsta(,:) and the overdimensioning of nsta by
!             n_regions_ns.
!  nonl(ndgsag:ndgeng+n_regions_ns-1,n_regions_ew)  :  number of grid columns for
!             the latitudes on a processor. Similar to nsta() in data
!             structure.
!             belong to it in fourier space. Available for all n_regions_ew
!             processors within this processors a-set.
!  nglobalindex : mapping of local grid points to global grid points
!               : used for debugging
!  nglobalat    : global latitude index (i.e. in NLOENG) for each local grid point
!  nglobalproc  : global data structure containing proc distribution
!                 an ngptotg array that maps owning proc
!  nlocalindex  : global data structure containing local index
!                 an ngptotg array that maps the local index into a
!                 ngptot array for the owning proc
!  nlatgpp,nlongpp: global lat,lon indx of a jproma,jgpblks point in gp-space
! ------------------------------------------------------------------

INTEGER(KIND=JPIM), ALLOCATABLE :: NUMPP(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPROCM(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRMS(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NALLMS(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRLS(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRSV(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRSVF(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRMF(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NSPSTAF(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NUMLL(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRLL(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: MYLEVS(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPSURF(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NSTA(:,:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NONL(:,:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRFRSTLAT(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRLSTLAT(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NPTRLAT(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NFRSTLAT(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NLSTLAT(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NBSETLEV(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NGLOBALINDEX(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NGLOBALAT(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NGLOBALPROC(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NLOCALINDEX(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NLATGPP(:,:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NLONGPP(:,:)

LOGICAL, ALLOCATABLE :: LSPLITLAT(:)

INTEGER(KIND=JPIM), ALLOCATABLE :: MYLATS(:)

INTEGER(KIND=JPIM) :: NPSP
INTEGER(KIND=JPIM) :: NSPEC2V
INTEGER(KIND=JPIM) :: NSPEC2VF
INTEGER(KIND=JPIM) :: NSPEC2VDDH
INTEGER(KIND=JPIM) :: NSPEC2V_NH
INTEGER(KIND=JPIM) :: NSPEC2V_NHX
INTEGER(KIND=JPIM) :: NBSETSP
INTEGER(KIND=JPIM) :: NFRSTLOFF
INTEGER(KIND=JPIM) :: MYFRSTACTLAT
INTEGER(KIND=JPIM) :: MYLSTACTLAT
INTEGER(KIND=JPIM) :: NPTRFLOFF

!     ------------------------------------------------------------------

!*    Global view variables mainly used to handle reading and
!     writing of grib data

! NPOSSP    : Defines partitioning of global spectral fields among PEs
! NDIM0G    : Defines partitioning of global spectral fields among PEs 

INTEGER(KIND=JPIM), ALLOCATABLE :: NPOSSP(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: NDIM0G(:)

CONTAINS
PROCEDURE :: ACDC_COPY => ACDC_COPY_TMP
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TMP
PROCEDURE :: ACDC_HOST => ACDC_HOST_TMP
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TMP
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TMP
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TMP
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TMP
END TYPE TMP


! ----------------------------------------------------------------------

INTERFACE

MODULE SUBROUTINE ACDC_COPY_TMP (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TMP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TMP (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TMP), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TMP (SELF)

IMPLICIT NONE
CLASS (TMP), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TMP (SELF, KLUN)

IMPLICIT NONE
CLASS (TMP), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TMP (SELF, KLUN)

IMPLICIT NONE
CLASS (TMP), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TMP (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TMP),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TMP (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TMP), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

END MODULE YOMMP

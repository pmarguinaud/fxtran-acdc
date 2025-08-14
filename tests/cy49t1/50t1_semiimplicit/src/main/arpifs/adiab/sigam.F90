SUBROUTINE SIGAM (YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,PD,PT,PSP)

!$ACDC singleblock 

!**** *SIGAM_GP_OPENMP* - Solve hydrostatic operator in semi-implicit

!     Purpose.
!     --------
!           Operator gamma to compute p.

!**   Interface.
!     ----------
!        *CALL* *SIGAM(...)

!        Explicit arguments :
!        --------------------
!        YDCST         : structure containing model constants
!        YDGEOMETRY    : structure containing geometry
!        YDDYN         : structure containing dynamics
!        KLON : TOTAL NUMBER OF COLUMNS IN THE THREE PARAMETER ARRAYS 
!               (PD,PT,PSP)
!        KLEV : NUMBER OF VERTICAL LEVELS IN THE 3D PARAMETER ARRAYS,
!                AND PROCESSED
!        KDIA : FIRST COLUMN PROCESSED 
!        KFDIA: LAST COLUMN PROCESSED      
!
!
!           TYPICAL VALUES ARE  NDLSUR,1  FOR GRID POINT ARRAY
!                               1,NFLSUR  FOR SPECTRAL ARRAY

!        PD    : DIVERGENCE       (output)
!        PT    : TEMPERATURE      (input)
!        PSP   : SURFACE PRESSURE (input)
!        KNLON : NUMBER OF VERTICAL COLUMNS TREATED
!        KFLEVG: NUMBER OF ELEMENTS IN A VERTICAL COLUMN

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      Mats Hamrud and Philippe Courtier  *ECMWF*
!      Original : 87-10-15

!     Modifications.
!     --------------
!      Modified : 09-Oct-2007 by K. YESSAD: possibility to have a specific
!                 value of LVERTFE in the SI linear model.
!      F. Vana + NEC 28-Apr-2009: OpenMP
!      P. Smolikova and J. Vivoda (Oct 2013): new options for VFE-NH
!      G. Mozdzynski Oct 2012: OpenMP optimization
!      K. Yessad (Dec 2016): Prune obsolete options.
!      J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!      R.Brozkova + NEC 03-Mar-2021: Optimization for vector (NEC)
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST       , ONLY : TCST
USE YOMDYN       , ONLY : TDYN


!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PD(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSP(KLON)

!     ------------------------------------------------------------------
REAL(KIND=JPRB) :: ZSPHI(KLON,0:KLEV+1)
REAL(KIND=JPRB) :: ZOUT(KLON,KLEV)

REAL(KIND=JPRB) :: ZSPHIX(KLON)
INTEGER(KIND=JPIM) :: JLEV, JLON
CHARACTER(LEN=4):: CLOPER
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "verdisint.intfb.h"

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SIGAM',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------
ASSOCIATE(YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA,YDVFE=>YDGEOMETRY%YRVERT_GEOM%YRVFE, YDCVER=>YDGEOMETRY%YRVERT_GEOM%YRCVER)
ASSOCIATE(SIALPH=>YDDYN%SIALPH, SILNPR=>YDDYN%SILNPR, SIRPRG=>YDDYN%SIRPRG)
!     ------------------------------------------------------------------

!*       1.    SUM GEOPOTENTIAL, COMPUTES P AND PUT IT IN PD.
!              ----------------------------------------------

IF (YDCVER%LVERTFE) THEN

  CLOPER='IBOT'
  IF (YDCVER%LVFE_COMPATIBLE) CLOPER='INTG'

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC) 
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      ZSPHI(JLON,JLEV)=-YDCST%RD*PT(JLON,JLEV)*SILNPR(JLEV)*YDVETA%VFE_RDETAH(JLEV)
    ENDDO
  ENDDO
!$OMP END DO
!$OMP END PARALLEL

  ZSPHI(KIDIA:KFDIA,0)=0.0_JPRB
  ZSPHI(KIDIA:KFDIA,KLEV+1)=0.0_JPRB

  CALL VERDISINT(YDVFE,YDCVER,CLOPER,'11',KLON,KIDIA,KFDIA,KLEV,ZSPHI,ZOUT,KCHUNK=YDGEOMETRY%YRDIM%NPROMA)

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC) 
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      PD(JLON,JLEV)=ZOUT(JLON,JLEV)+PSP(JLON)*SIRPRG
    ENDDO
  ENDDO
!$OMP END DO
!$OMP END PARALLEL

ELSE
  
!$OMP PARALLEL PRIVATE(JLEV)
!$OMP DO SIMD 
  DO JLON=KIDIA,KFDIA
    ZSPHIX(JLON)=0.0_JPRB
    DO JLEV=KLEV,1,-1
      PD(JLON,JLEV)=ZSPHIX(JLON)+SIALPH(JLEV)*YDCST%RD*PT(JLON,JLEV)&
       & +PSP(JLON)*SIRPRG
      ZSPHIX(JLON)=ZSPHIX(JLON)+YDCST%RD*PT(JLON,JLEV)*SILNPR(JLEV)
    ENDDO
  ENDDO
!$OMP END DO SIMD
!$OMP END PARALLEL 

ENDIF

!      -----------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('SIGAM',1,ZHOOK_HANDLE)

END SUBROUTINE SIGAM

SUBROUTINE SITNU (YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,PD,PT,PSP)

!$ACDC singleblock 

!**** *SITNU_GP_OPENMP*   - Continuity equation for semi-implicit.

!     Purpose.
!     --------
!           Evaluate operators Tau and Nu in semi-implicit.

!**   Interface.
!     ----------
!        *CALL* *SITNU_GP_OPENMP(...)

!        Explicit arguments :
!        --------------------
!        YDCST         : structure containing model constants
!        YDGEOMETRY    : structure containing geometry
!        YDDYN         : structure containing dynamics
!        KLON : TOTAL NUMBER OF COLUMNS IN THE THREE PARAMETER ARRAYS 
!               (PD,PT,PSP)
!        KLEV : NUMBER OF VERTICAL LEVELS IN THE 3D PARAMETER ARRAYS,
!               AND PROCESSED
!        KDIA : FIRST COLUMN PROCESSED 
!        KFDIA: LAST COLUMN PROCESSED      
!
!
!        PD    : DIVERGENCE
!        PT    : TEMPERATURE
!        PSP   : SURFACE PRESSURE

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.   None.
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
!                 value of LVERTFE in the SI NH linear model.
!      F. Vana + NEC 28-Apr-2009: OpenMP + optimization
!      P. Smolikova and J. Vivoda (Oct 2013): new options for VFE-NH
!      G. Mozdzynski Oct 2012: OpenMP optimization
!      K. Yessad (Dec 2016): Prune obsolete options.
!      J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
!      R.Brozkova + NEC: Mar 2021: Optimization for vector (NEC)
!      R. El Khatib 28-Feb-2023 Bugfixes for open-mp
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PD(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSP(KLON) 

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZSDIV(KLON,0:KLEV+1)
REAL(KIND=JPRB) :: ZOUT(KLON,KLEV+1)

REAL(KIND=JPRB) :: ZSDIVX(KLON)
INTEGER(KIND=JPIM) :: JLEV, JLON
REAL(KIND=JPRB) :: ZREC
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "verdisint.intfb.h"

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SITNU',0,ZHOOK_HANDLE)

ASSOCIATE( YDVETA=>YDGEOMETRY%YRVERT_GEOM%YRVETA, YDVFE=>YDGEOMETRY%YRVERT_GEOM%YRVFE, YDCVER=>YDGEOMETRY%YRVERT_GEOM%YRCVER) 
ASSOCIATE(SIALPH=>YDDYN%SIALPH, SIDELP=>YDDYN%SIDELP, SILNPR=>YDDYN%SILNPR, SIRDEL=>YDDYN%SIRDEL, &
 & SIRPRN=>YDDYN%SIRPRN, SITLAF=>YDDYN%SITLAF, SITR=>YDDYN%SITR, YDDIMV=>YDGEOMETRY%YRDIMV)

!     ------------------------------------------------------------------

!*       1.    SUM DIVERGENCE AND COMPUTES TEMPERATURE.
!              ----------------------------------------

IF(YDCVER%LVERTFE) THEN

!$OMP PARALLEL PRIVATE(JLEV,JLON)
!$OMP DO SCHEDULE(STATIC) 
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      ZSDIV(JLON,JLEV)=PD(JLON,JLEV)*SIDELP(JLEV)*YDVETA%VFE_RDETAH(JLEV)
    ENDDO
  ENDDO
!$OMP END DO
!$OMP END PARALLEL

  IF (KLON>=1) THEN

!DEC$ IVDEP
    ZSDIV(KIDIA:KFDIA,0)=0.0_JPRB
    ZSDIV(KIDIA:KFDIA,KLEV+1)=0.0_JPRB

    CALL VERDISINT(YDVFE,YDCVER,'ITOP','11',KLON,KIDIA,KFDIA,KLEV,ZSDIV,ZOUT,KCHUNK=YDGEOMETRY%YRDIM%NPROMA)
  ENDIF

!$OMP PARALLEL PRIVATE(JLEV,JLON,ZREC)
!$OMP DO SCHEDULE(STATIC) 
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      ZREC=1.0_JPRB/SITLAF(JLEV)
      PT(JLON,JLEV)=YDCST%RKAPPA*SITR*ZOUT(JLON,JLEV)*ZREC
    ENDDO
  ENDDO
!$OMP END DO
!$OMP END PARALLEL

  DO JLON=KIDIA,KFDIA
    PSP(JLON)=ZOUT(JLON,KLEV+1)*SIRPRN
  ENDDO

ELSE

!$OMP PARALLEL PRIVATE(JLEV) 
!$OMP DO SIMD
  DO JLON=KIDIA,KFDIA
    ZSDIVX(JLON)=0.0_JPRB
    DO JLEV=1,KLEV
      PT(JLON,JLEV)=YDCST%RKAPPA*SITR*(SIRDEL(JLEV)*SILNPR(JLEV)*ZSDIVX(JLON)&
       & +SIALPH(JLEV)*PD(JLON,JLEV))
      ZSDIVX(JLON)=ZSDIVX(JLON)+PD(JLON,JLEV)*SIDELP(JLEV)
    ENDDO
    PSP(JLON)=ZSDIVX(JLON)*SIRPRN
  ENDDO
!$OMP END DO SIMD
!$OMP END PARALLEL 
 
ENDIF
!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('SITNU',1,ZHOOK_HANDLE)

END SUBROUTINE SITNU


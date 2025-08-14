SUBROUTINE SISKAPI(YDCST, YDGEOMETRY, YDDYN, YDDYNA, KLON, KLEV, KIDIA, KFDIA, PIN, POUT)

!$ACDC singleblock 


!**** *SISKAPI_GP_OPENMP* - Semi-implicit scheme: compute Sstar_kap**-1 X

!     Purpose.
!     --------
!           Semi-implicit scheme: compute Sstar_kap**-1 X
!           This routine is called in the linear system of NHQE model

!           Sstar_kap**-1 Z_l = [I - [ ((Cpd/Rd)-1)/Tstar ] tau]**-1 Z_l

!           For VFD we invert a triangular matrix.

!**   Interface.
!     ----------
!        *CALL* *SISKAPI_GP_OPENMP(...)

!        Explicit arguments :
!        --------------------
!        * INPUT:
!          YDGEOMETRY    : structure containing geometry
!          YDDYN         : structure containing dynamics
!          KLON : TOTAL NUMBER OF COLUMNS IN THE TWO PARAMETER ARRAYS 
!                 (PIN,POUT)
!          KLEV : NUMBER OF VERTICAL LEVELS IN THE 3D PARAMETER ARRAYS,
!                  AND PROCESSED
!          KDIA : FIRST COLUMN PROCESSED 
!          KFDIA: LAST COLUMN PROCESSED      
!
!
!          PIN   : INPUT ARRAY                                              (IN)
!
!        * OUPUT:
!          POUT  : OUPUT ARRAY                                              (OUT)
!
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
!        Documentation (IDSI)

!     Author.
!     -------
!      K. Yessad (METEO-FRANCE/CNRM/GMAP)
!      Original : march 2017

!     Modifications.
!     --------------
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE YOMCST       , ONLY : TCST
USE YOMDYN       , ONLY : TDYN
USE YOMDYNA      , ONLY : TDYNA
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
TYPE(TDYNA)       ,INTENT(IN)    :: YDDYNA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
REAL(KIND=JPRB)   ,INTENT(IN)    :: PIN(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: POUT(KLON,KLEV)

!     ------------------------------------------------------------------


INTEGER(KIND=JPIM) :: JLEV, JLON
REAL(KIND=JPRB) :: ZSUM(KLON)
REAL(KIND=JPRB) :: ZFAC

REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SISKAPI',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

ZFAC=0.0_JPRB
IF (YDDYNA%LNHQE.AND.YDDYN%LNHQE_SOLVER_SP) ZFAC=YDCST%RKAPPA-1.0_JPRB

!$OMP PARALLEL PRIVATE(JLEV) 
!$OMP DO SIMD
  DO JLON=KIDIA,KFDIA
    ZSUM(JLON)=0.0_JPRB
    DO JLEV=1,KLEV
      POUT(JLON,JLEV)=( &
       & PIN(JLON,JLEV)-ZFAC*(YDDYN%SILNPR_NH(JLEV)*YDDYN%SIRDEL_NH(JLEV))*ZSUM(JLON) ) &
       & /(1.0_JPRB+ZFAC*YDDYN%SIALPH_NH(JLEV))
      ZSUM(JLON)=ZSUM(JLON)+YDDYN%SIDELP_NH(JLEV)*POUT(JLON,JLEV)
    ENDDO
  ENDDO
!$OMP END DO SIMD
!$OMP END PARALLEL

!      -----------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SISKAPI',1,ZHOOK_HANDLE)
END SUBROUTINE SISKAPI

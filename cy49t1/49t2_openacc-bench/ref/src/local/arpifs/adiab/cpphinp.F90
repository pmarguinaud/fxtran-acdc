SUBROUTINE CPPHINP(YDGEOMETRY,YDMODEL,KLON,KLEV,KIDIA,KFDIA,&
 & PGEMU,PGELAM,&
 & PUT0,PVT0,PTT0L,PTT0M,PQT0,PQT0L,PQT0M,PQSLT0L,PQSLT0M,&
 & PRDELP0,PEVEL0,PCVGQSL,&
 & PMU0,PSOLO,PMU0LU,PMU0M,PMU0N,PCVGQ,PCVGT)

!$ACDC singlecolumn 


!**** *CPPHINP*  - ComPute PHysical INPut.

!     Purpose.
!     --------

!**   Interface.
!     ----------
!        *CALL* *CPPHINP*

!        Explicit arguments :
!        --------------------

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------

!     Modifications.
!     --------------
!      2002-05, K. YESSAD: consistency with finite element scheme.
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      2004-11, Y. Seity: Do not compute Convergence of Humidity for Arome
!      2005-10  Y. Bouteloup : Modification for computation of CVGQ
!        2011-03  Y. Seity: add PMU0N from ARPEGE-climat
!        Y.Bouteloup 21-Jan-2011 : Compute pmu0m as average of pmu0 instead of 
!                                  pmu0(mean time)
!      2011-05-10 E. Bazile: PMU0M=PMU0 for NRADFR=1
!      2021-03-17 Y. Bouteloup: Compute PCVGT for Tiedtke scheme
!      2022-04-13 J.M. Piriou : Sun eclipses (solar obscuration PSOLO).
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE TYPE_MODEL   , ONLY : MODEL
USE GEOMETRY_MOD , ONLY : GEOMETRY

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(MODEL)       ,INTENT(IN)    :: YDMODEL
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEMU(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGELAM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUT0(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVT0(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQT0(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQT0L(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQT0M(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTT0L(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTT0M(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQSLT0L(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQSLT0M(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP0(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVEL0(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVGQSL(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMU0(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSOLO(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMU0LU(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMU0M(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMU0N(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCVGQ(YDGEOMETRY%YRDIM%NPROMM,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCVGT(YDGEOMETRY%YRDIM%NPROMM,KLEV) 

!     ------------------------------------------------------------------

#include "abor1.intfb.h"

INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZED
REAL(KIND=JPRB) :: ZEPS,ZABSTSPHY,ZUSTSPHY
REAL(KIND=JPRB) :: Z1MU0M(KLON),Z2MU0M(KLON)
REAL(KIND=JPRB) :: ZCOS (KLON), ZSIN (KLON)

LOGICAL :: LLCOMPUTE_CVGQ

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
#include "smpos_parall.intfb.h"

IF (LHOOK) CALL DR_HOOK('CPPHINP',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,&
 &  YDDYN=>YDMODEL%YRML_DYN%YRDYN,YDPHY=>YDMODEL%YRML_PHY_MF%YRPHY,YDSIMPHL=>YDMODEL%YRML_PHY_MF%YRSIMPHL, &
 & YDRIP=>YDMODEL%YRML_GCONF%YRRIP,YDARPHY=>YDMODEL%YRML_PHY_MF%YRARPHY, &
  & YDERAD=>YDMODEL%YRML_PHY_RAD%YRERAD,YDERIP=>YDMODEL%YRML_PHY_RAD%YRERIP,YDPHY2=>YDMODEL%YRML_PHY_MF%YRPHY2, &
  & YDEPHY=>YDMODEL%YRML_PHY_EC%YREPHY)
ASSOCIATE(&
 & NCOMP_CVGQ=>YDDYN%NCOMP_CVGQ, &
 & LEPHYS=>YDEPHY%LEPHYS, &
 & NRADFR=>YDERAD%NRADFR, &
 & RCODECM=>YDERIP%RCODECM, RSIDECM=>YDERIP%RSIDECM, RCOVSRM=>YDERIP%RCOVSRM, &
 & RSIVSRM=>YDERIP%RSIVSRM, &
 & RCODEC=>YDRIP%RCODEC, RCODECF=>YDRIP%RCODECF, RCODECLU=>YDRIP%RCODECLU, &
 & RCODECN=>YDRIP%RCODECN, RCOVSR=>YDRIP%RCOVSR, RCOVSRF=>YDRIP%RCOVSRF, &
 & RCOVSRLU=>YDRIP%RCOVSRLU, RCOVSRN=>YDRIP%RCOVSRN, RSIDEC=>YDRIP%RSIDEC, &
 & RSIDECF=>YDRIP%RSIDECF, RSIDECLU=>YDRIP%RSIDECLU, RSIDECN=>YDRIP%RSIDECN, &
 & RSIVSR=>YDRIP%RSIVSR, RSIVSRF=>YDRIP%RSIVSRF, RSIVSRLU=>YDRIP%RSIVSRLU, &
 & RSIVSRN=>YDRIP%RSIVSRN, &
 & LMSE=>YDARPHY%LMSE, LMPA=>YDARPHY%LMPA, &
 & TSPHY=>YDPHY2%TSPHY, &
 & LSIMPH=>YDSIMPHL%LSIMPH, &
 & LRMU0M=>YDPHY%LRMU0M, LRAYLU=>YDPHY%LRAYLU, NSOLE=>YDPHY%NSOLE, LRAYFM=>YDPHY%LRAYFM, &
 & LMPHYS=>YDPHY%LMPHYS, LRAYFM15=>YDPHY%LRAYFM15)
!     ------------------------------------------------------------------

!*       1.1   Astronomy.

DO JLON=KIDIA,KFDIA
  ZCOS (JLON) = COS(PGELAM(JLON))
ENDDO
DO JLON=KIDIA,KFDIA
  ZSIN (JLON) = SIN(PGELAM(JLON)) 
ENDDO

!DEC$ IVDEP
DO JLON=KIDIA,KFDIA
  PMU0(JLON)=MIN(1.0_JPRB, MAX(RSIDEC*PGEMU(JLON)&
   & -RCODEC*RCOVSR*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZCOS (JLON)&
   & +RCODEC*RSIVSR*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZSIN (JLON)&
   & ,0.0_JPRB))
ENDDO

IF(LMSE)THEN
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA
    PMU0N(JLON)=MIN(1.0_JPRB, MAX(RSIDECN*PGEMU(JLON)&
     & -RCODECN*RCOVSRN*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZCOS (JLON)&
     & +RCODECN*RSIVSRN*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZSIN (JLON)&
     & ,0.0_JPRB))
  ENDDO
ELSE
  DO JLON=KIDIA,KFDIA
    PMU0N(JLON)=0.0
  ENDDO
ENDIF


!*       Lunar astronomy.

IF(LRAYLU) THEN
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA
    PMU0LU(JLON)=MAX( RSIDECLU*PGEMU(JLON)&
     & -RCODECLU*RCOVSRLU*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZCOS (JLON)&
     & +RCODECLU*RSIVSRLU*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZSIN (JLON)&
     & ,0.0_JPRB)  
  ENDDO
ENDIF

! Compute lunar parallax & lunar obscuration of the Sun.

IF(NSOLE <= 0) THEN
  ! No obscuration computed.
  PSOLO(:)=0._JPRB
ELSE
  ! Obscuration computed.
  CALL SMPOS_PARALL(YDGEOMETRY,YDMODEL,KIDIA,KFDIA,PGEMU,PGELAM,PSOLO)
ENDIF

IF (LEPHYS.OR.(LMPHYS.AND.LRAYFM.AND.(.NOT.LRMU0M))) THEN
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA
    PMU0M(JLON)=MAX( RSIDECM*PGEMU(JLON)&
     & -RCODECM*RCOVSRM*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZCOS (JLON)&
     & +RCODECM*RSIVSRM*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZSIN (JLON)&
     & ,0.0_JPRB)  
  ENDDO
  IF (NRADFR == 1 ) THEN
     DO JLON=KIDIA,KFDIA
        PMU0M(JLON)=PMU0(JLON)
     ENDDO
  ENDIF
ELSEIF (LMPHYS.AND.LRAYFM.AND.LRMU0M) THEN
!DEC$ IVDEP
  DO JLON=KIDIA,KFDIA
    Z1MU0M(JLON) = MAX( RSIDECM*PGEMU(JLON)&
     & -RCODECM*RCOVSRM*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZCOS (JLON)&
     & +RCODECM*RSIVSRM*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZSIN (JLON)&
     & ,0.0_JPRB)  
    Z2MU0M(JLON)=0.5_JPRB*(MAX( RSIDECF*PGEMU(JLON)&
     & -RCODECF*RCOVSRF*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZCOS (JLON)&
     & +RCODECF*RSIVSRF*SQRT(1.0_JPRB-PGEMU(JLON)**2)*ZSIN (JLON)&
     & ,0.0_JPRB)  + PMU0(JLON))
     PMU0M(JLON) = MAX(Z1MU0M(JLON),Z2MU0M(JLON))
  ENDDO

ELSEIF (LMPHYS.AND.LRAYFM15) THEN
  CALL ABOR1('RADIATION SCHEME FROM CYCLE 15 NO LONGER AVAILABLE')
ELSE
  DO JLON=KIDIA,KFDIA
    PMU0M(JLON)=0.0_JPRB
  ENDDO
ENDIF

!*    1.2   Convergence of humidity and physical wind components.

! ky: variable to be put later in a module, saying if the CVGQ
! calculation is required or not (its definition must be the same
! everywhere in the code).
! For the time being this calculation is needed if MF physics
! (other than AROME physics) is activated.
LLCOMPUTE_CVGQ=(LMPHYS.OR.LSIMPH).AND.(.NOT.LMPA)

IF (LLCOMPUTE_CVGQ) THEN

    ! Eulerian computation of temperature convergence
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      PCVGT(JLON,JLEV)=&
       & -PTT0L(JLON,JLEV)*PUT0(JLON,JLEV)&
       & -PTT0M(JLON,JLEV)*PVT0(JLON,JLEV)  
    ENDDO
  ENDDO
  IF ((NCOMP_CVGQ == 0) .OR. (NCOMP_CVGQ == 1)) THEN
  
    ! * Eulerian convergence:

    ! a/ Horizontal part of the moisture convergence:
    IF (NCOMP_CVGQ == 0) THEN
      DO JLEV=1,KLEV
        DO JLON=KIDIA,KFDIA
          PCVGQ(JLON,JLEV)=&
           & -PQT0L(JLON,JLEV)*PUT0(JLON,JLEV)&
           & -PQT0M(JLON,JLEV)*PVT0(JLON,JLEV)  
        ENDDO
      ENDDO
    ELSE
      DO JLEV=1,KLEV
        DO JLON=KIDIA,KFDIA
          PCVGQ(JLON,JLEV)=&
           & -PQSLT0L(JLON,JLEV)*PUT0(JLON,JLEV)&
           & -PQSLT0M(JLON,JLEV)*PVT0(JLON,JLEV)  
        ENDDO
      ENDDO
    ENDIF

    ! b/ Vertical part of the moisture convergence:
    IF (YDGEOMETRY%YRVERT_GEOM%YRCVER%LVERTFE) THEN

      ! * "pevel0" contains full-layer "etadot (d prehyd / d eta)"
      !   The current discretisation of the vertical advection of a variable X
      !   which is proposed is:
      !   [etadot d X/d eta](l) = 0.5 * [1 / Delta eta](l)
      !    * [etadot d prehyd/d eta](l) * (X(l+1)-X(l-1))
      !   Remark K.Y.: this discretisation is not fully consistent with the
      !    discretisation of a vertical advection provided by ECMWF between
      !    CY29 and CY29R2 (first coded in CPDYN, then transferred to CPEULDYN),
      !    and it is desirable (but not urgent) to update this discretisation
      !    in the future to make it consistent with CPEULDYN (in this case
      !    one must use routine VERDER to compute the vertical derivative
      !    [d X/d eta](l)).

      ! * Layer 1:

      DO JLON=KIDIA,KFDIA
        PCVGQ(JLON,1  )=PCVGQ(JLON,1  )+&
         & (PQT0(JLON,1)-PQT0  (JLON,2))&
         & *PEVEL0(JLON,1)*PRDELP0(JLON,1  )  
      ENDDO

      ! * Layers 2 to L-1:

      DO JLEV=2,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZED=0.5_JPRB*PEVEL0(JLON,JLEV)
 
          PCVGQ(JLON,JLEV  )=PCVGQ(JLON,JLEV  )+&
           & (PQT0(JLON,JLEV-1)-PQT0  (JLON,JLEV+1))&
           & *ZED*PRDELP0(JLON,JLEV  )  
        ENDDO
      ENDDO

      ! * Layer L:

      DO JLON=KIDIA,KFDIA
        PCVGQ(JLON,KLEV  )=PCVGQ(JLON,KLEV  )+&
         & (PQT0(JLON,KLEV-1)-PQT0  (JLON,KLEV))&
         & *PEVEL0(JLON,KLEV)*PRDELP0(JLON,KLEV  )  
      ENDDO

    ELSE

      ! * "pevel0" contains half-layer "etadot (d prehyd / d eta)":
      !   The discretisation of the vertical advection of a variable X writes:
      !   [etadot d X/d eta](l) =
      !    0.5 * [1 / Delta eta](l) *
      !    { [etadot d prehyd/d eta](lbar) * (X(l+1)-X(l))
      !    + [etadot d prehyd/d eta](lbar-1) * (X(l)-X(l-1)) }

      DO JLEV=1,KLEV-1
        DO JLON=KIDIA,KFDIA
          ZED=0.5_JPRB*PEVEL0(JLON,JLEV)
  
          PCVGQ(JLON,JLEV  )=PCVGQ(JLON,JLEV  )+&
           & (PQT0(JLON,JLEV  )-PQT0  (JLON,JLEV+1))&
           & *ZED*PRDELP0(JLON,JLEV  )  

          PCVGQ(JLON,JLEV+1)=PCVGQ(JLON,JLEV+1)+&
            & (PQT0(JLON,JLEV  )-PQT0  (JLON,JLEV+1))&
            & *ZED*PRDELP0(JLON,JLEV+1)  
        ENDDO
      ENDDO
    ENDIF
  
  ELSE
  
    ! * case NCOMP_CVGQ=2: Semi-Lagrangian convergence:

    ZEPS=1.E-12
    ZABSTSPHY=MAX(ZEPS,ABS(TSPHY))
    IF (TSPHY >=0) THEN
      ZUSTSPHY=1.0_JPRB/ZABSTSPHY
    ELSE
      ZUSTSPHY=-1.0_JPRB/ZABSTSPHY
    ENDIF

    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        PCVGQ(JLON,JLEV)=PCVGQSL(JLON,JLEV)*ZUSTSPHY
      ENDDO
    ENDDO
  ENDIF     

ENDIF

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('CPPHINP',1,ZHOOK_HANDLE)
END SUBROUTINE CPPHINP

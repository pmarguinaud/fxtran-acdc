SUBROUTINE SI_CCCOR (YDCST, YDGEOMETRY, YDDYN, YDDYNA, KLON, KLEV, KIDIA, KFDIA, PIN, POU, POU2)

!$ACDC singleblock

!**** *SI_CCCOR* - Semi-implicit scheme in the NH model.
!                  Does a multiplication by C**2 * COR when the constraint
!                  C1 is not matched.
!                  Optionally, does a multiplication by (g**2/(N**2 C**2)) Lv* A2
!                  for the case when the constraint C2 is not matched.

!     Purpose.
!     --------
!       This routine is called in the SI scheme (NH model)

!       Action 1/
!        It computes C**2 * COR where:
!         C = sqrt( R_dry Trsi Cp_dry/Cv_dry )
!         C**2 is the acoustic phase speed
!        and:
!         COR = (Cv_dry/(R_d**2 Trsi)) Gamma Tau
!               - (Cv_dry/(R_dry*Cp_dry)) Gamma
!               - (Cv_dry/(R_dry*Trsi)) Tau
!               + (Cv_dry/Cv_dry) Nu
!        This is equivalent to write:
!         C**2 * COR = (Cp_dry/R_dry) Gamma Tau - Trsi Gamma
!          - Cp_dry Tau + (R_d*Trsi) Nu
!        It does a multiplication by C**2 * COR.

!       Action 2/ (when argument POU2 is present)
!        It computes (g**2/(N**2 C**2)) Lv* A2 where:
!         A2 = S*G* - (Cp_dry/Cv_dry) S* - (Cp_dry/Cv_dry) G*
!         S* is (Cp_dry/ (R_dry Trsi)) Tau
!         G* is (1/R_dry) Gamma
!         g is the acceleration of gravity
!         N**2 = g**2/(Cp_dry Trsi) is the Brunt-Vaisala frequency

!       Remarks:
!        COR=0 in the continuous equations and in the discretisations
!         matching the constraint C1 (ex: NDLNPR=1 in finite differences).
!        (g**2/(N**2 C**2)) Lv* A2 = Id in the continuous case.

!**   Interface.
!     ----------
!        *CALL* *SI_CCCOR(...)

!        Explicit arguments :
!        --------------------
!        YDCST         : structure containing model constants
!        YDGEOMETRY    : structure containing geometry
!        YDDYN         : structure containing dynamics
!
!        KLON : TOTAL NUMBER OF COLUMNS IN THE THREE PARAMETER ARRAYS 
!               (PIN,POU,POU2)
!        KLEV : NUMBER OF VERTICAL LEVELS IN THE 3D PARAMETER ARRAYS,
!                AND PROCESSED
!        KDIA : FIRST COLUMN PROCESSED 
!        KFDIA: LAST COLUMN PROCESSED      
!
!
!         PIN      :  known input vector.                           (input)
!         POU      :  unknown output vector linked to COR.          (output)
!         POU2     :  unknown output vector linked to Laplacian.    (opt output)

!        Implicit arguments :
!        --------------------
!        none.

!     Method.
!     -------
!        See documentation about semi-implicite scheme

!     Externals.
!     ----------
!        None.

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS
!        ARPEGE documentation about semi-implicit scheme

!     Author.
!     -------
!        K. YESSAD (CNRM/GMAP)

!     Modifications.
!     --------------
!      P Smolikova and J Vivoda (Oct 2013): add POU2
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST       , ONLY : TCST
USE YOMDYN       , ONLY : TDYN
USE YOMDYNA      , ONLY : TDYNA

!      ----------------------------------------------------------------

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
REAL(KIND=JPRB)   ,INTENT(OUT)   :: POU(KLON,KLEV) 
REAL(KIND=JPRB),OPTIONAL,INTENT(OUT)  :: POU2(KLON,KLEV) 

!      ----------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JLON

REAL(KIND=JPRB) :: ZGAM(KLON,KLEV)
REAL(KIND=JPRB) :: ZTAU(KLON,KLEV)
REAL(KIND=JPRB) :: ZGAMTAU(KLON,KLEV)
REAL(KIND=JPRB) :: ZTAUGAM(KLON,KLEV)
REAL(KIND=JPRB) :: ZAUX(KLON,KLEV)
REAL(KIND=JPRB) :: ZNU(KLON)
REAL(KIND=JPRB) :: ZSP(KLON), ZFAC
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!      ----------------------------------------------------------------

#include "sigam.intfb.h"
#include "sitnu.intfb.h"
#include "siseve.intfb.h"

!      ----------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SI_CCCOR',0,ZHOOK_HANDLE)
ASSOCIATE(RD=>YDCST%RD, RCPD=>YDCST%RCPD,RCVD=>YDCST%RCVD, &
 & SITR=>YDDYN%SITR)
!      ----------------------------------------------------------------

!*       1.    COMPUTE C2*COR AND DO MULTIPLICATION
!              ------------------------------------

! * Computes ZTAU = Cp_dry * Tau * PIN and ZNU = Nu * PIN
CALL SITNU(YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,PIN,ZTAU,ZNU)

ZTAU(:,:)=ZTAU(:,:)*RCPD

! * Computes ZGAM = Gamma * PIN

ZSP(1:KLON)=0.0_JPRB

CALL SIGAM(YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,ZGAM,PIN,ZSP)

! * Computes ZGAMTAU = Cp_dry * Gamma * (Tau * PIN) = Gamma * ZTAU

ZSP(:)=0.0_JPRB

CALL SIGAM(YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,ZGAMTAU,ZTAU,ZSP)

ZFAC = 1.0_JPRB + YDGEOMETRY%YRVERT_GEOM%YRCVER%RFAC1

! * Computes POU:
DO JLEV=1,KLEV
  DO JLON=KIDIA,KFDIA
    POU(JLON,JLEV)=ZGAMTAU(JLON,JLEV)/RD * ZFAC &
     & -SITR*ZGAM(JLON,JLEV)-ZTAU(JLON,JLEV)*ZFAC+RD*SITR*ZNU(JLON)
  ENDDO
ENDDO

!      ----------------------------------------------------------------

!*       2.    COMPUTE POU2
!              ------------

IF (PRESENT(POU2)) THEN

  ! * Computes ZTAUGAM = Tau * ZGAM
  ZSP(:)=0.0_JPRB

  CALL SITNU(YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,ZGAM,ZTAUGAM,ZSP)

  ! * Computes (g**2/(N**2 C**2)) A2 * PIN:
  DO JLEV=1,KLEV
    DO JLON=KIDIA,KFDIA
      ZAUX(JLON,JLEV)=((RCVD/RD)*ZTAUGAM(JLON,JLEV) &
       & -ZTAU(JLON,JLEV)-SITR*ZGAM(JLON,JLEV))*RCPD/(RD*RD*SITR)
    ENDDO
  ENDDO

  ! * Apply the tridiagonal operator Lv* to get POU2
  CALL SISEVE(YDCST,YDGEOMETRY,YDDYN,YDDYNA,KLON,KLEV,KIDIA,KFDIA,ZAUX,POU2)

ENDIF

!       ----------------------------------------------------------------

END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('SI_CCCOR',1,ZHOOK_HANDLE)

END SUBROUTINE SI_CCCOR

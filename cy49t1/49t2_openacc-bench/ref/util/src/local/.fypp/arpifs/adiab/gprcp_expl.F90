SUBROUTINE GPRCP_EXPL (YDCST, YDCPG_BNDS, YDCPG_OPTS, PCP, PR, PKAP, YDVARS, KGFLTYP)

!$ACDC pointerparallel    


!**** *GPRCP_EXPL* - Computes Cp, R and R/Cp from Q

!     Purpose.
!     --------
!        Computes Cp, R and R/Cp from Q

!**   Interface.
!     ----------
!        *CALL* *GPRCP_EXPL(...)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KPROMA               - dimensioning.
!          KSTART               - start of work.
!          KPROF                - depth of work.
!          KFLEV                - number of layers.

!        OUTPUT:
!          PCP(KPROMA,KFLEV)    - CP
!          PR(KPROMA,KFLEV)     - R
!          PKAP(KPROMA,KFLEV)   - KAPPA

!        Implicit arguments :  Physical constants from YOMCST
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.  None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      Mats Hamrud and Philippe Courtier  *ECMWF*
!      Original : 88-02-04

!     Modifications.
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      Y.Seity  04-02-13 (Rain, Snow and Graupel)
!      M.Hamrud  15-Jan-2006  Revised GPRCP
!      K. Yessad (Jan 2011): more compact rewriting.
!      R. El Khatib 28-Aug-2014 Optimizations :
!       - compute R or CP only if required
!       - loop collapsing whenever possible, through pure array syntax
!      A. Geer      01-Oct-2015    For independence of observation operator in OOPS, 
!                                  allow calls without YGFL initialised. Removal
!                                  of all YGFL references will have to wait.
!      H Petithomme (Dec 2020): general rewrite for optimization
!      R. El Khatib 18-Aug-2023 Re-instate a universal and even more efficient vectorization
!     ------------------------------------------------------------------

USE PARKIND1, ONLY: JPIM, JPRB
USE YOMHOOK,  ONLY: LHOOK, JPHOOK, DR_HOOK

USE YOMCST,              ONLY: TCST
USE FIELD_VARIABLES_MOD, ONLY : FIELD_VARIABLES
USE CPG_OPTS_TYPE_MOD  , ONLY : CPG_BNDS_TYPE, CPG_OPTS_TYPE

IMPLICIT NONE

TYPE(TCST),                     INTENT(IN)    :: YDCST
TYPE(CPG_BNDS_TYPE),            INTENT(IN)    :: YDCPG_BNDS
TYPE(CPG_OPTS_TYPE),            INTENT(IN)    :: YDCPG_OPTS
INTEGER(KIND=JPIM),OPTIONAL,    INTENT(IN)    :: KGFLTYP
TYPE(FIELD_VARIABLES),          INTENT(INOUT) :: YDVARS
REAL(KIND=JPRB),OPTIONAL,TARGET,INTENT(OUT)   :: PCP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB),OPTIONAL,TARGET,INTENT(OUT)   :: PR(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
REAL(KIND=JPRB),OPTIONAL,       INTENT(OUT)   :: PKAP(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)

#include "abor1.intfb.h"

REAL(KIND=JPRB), POINTER :: ZR(:,:)
REAL(KIND=JPRB), POINTER :: ZCP(:,:)
LOGICAL :: LLCP

REAL(KIND=JPRB),TARGET :: ZCP0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG),ZR0(YDCPG_OPTS%KLON,YDCPG_OPTS%KFLEVG)
INTEGER(KIND=JPIM) :: IGFLTYP, JLON, JLEV, JFLD, IFLD

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK("GPRCP_EXPL",0,ZHOOK_HANDLE)

ASSOCIATE(KLON => YDCPG_OPTS%KLON, KST => YDCPG_BNDS%KIDIA, KEND => YDCPG_BNDS%KFDIA, KFLEVG => YDCPG_OPTS%KFLEVG)

IF (PRESENT (PR)) THEN
  ZR => PR
ELSEIF (PRESENT (PKAP)) THEN
  ZR => ZR0
ELSE
  CALL ABOR1 ('GPRCP_EXPL: EXPECTED PR OR PKAP')
ENDIF

LLCP = .TRUE.

IF (PRESENT(PCP)) THEN
  ZCP => PCP
ELSEIF (PRESENT(PKAP)) THEN
  ZCP => ZCP0
ELSE
  ZCP => NULL()
  LLCP = .FALSE.
ENDIF

IGFLTYP = 0
IF (PRESENT (KGFLTYP)) IGFLTYP = KGFLTYP 

IFLD = SIZE (YDVARS%GFL_PTR)


!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

ZR(:,:) = 0._JPRB
IF (LLCP) THEN
  ZCP(:,:) = 0._JPRB
ENDIF

!$ACDC }


SELECT CASE (IGFLTYP) 
  CASE (0)
    DO JFLD = 1, IFLD
      IF (YDVARS%GFL_PTR (JFLD)%YCOMP%LTHERMACT) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

        DO JLEV = 1, KFLEVG
!DIR$ IVDEP
!NEC$ IVDEP
          DO JLON = KST, KEND
            ZR(JLON,JLEV) = ZR(JLON,JLEV) + (YDVARS%GFL_PTR(JFLD)%YCOMP%R-YDCST%RD)*YDVARS%GFL_PTR(JFLD)%T0(JLON,JLEV)
          ENDDO
        ENDDO
        IF (LLCP) THEN
          DO JLEV = 1, KFLEVG
!DIR$ IVDEP
!NEC$ IVDEP
            DO JLON = KST, KEND
              ZCP(JLON,JLEV) = ZCP(JLON,JLEV) + (YDVARS%GFL_PTR(JFLD)%YCOMP%RCP-YDCST%RCPD)*YDVARS%GFL_PTR(JFLD)%T0(JLON,JLEV)
            ENDDO
          ENDDO
        ENDIF

!$ACDC }

      ENDIF
    ENDDO
  CASE (1)
    DO JFLD = 1, IFLD
      IF (YDVARS%GFL_PTR (JFLD)%YCOMP%LTHERMACT) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

        DO JLEV = 1, KFLEVG
!DIR$ IVDEP
!NEC$ IVDEP
          DO JLON = KST, KEND
            ZR(JLON,JLEV) = ZR(JLON,JLEV) + (YDVARS%GFL_PTR(JFLD)%YCOMP%R-YDCST%RD)*YDVARS%GFL_PTR(JFLD)%T1(JLON,JLEV)
          ENDDO
        ENDDO
        IF (LLCP) THEN
          DO JLEV = 1, KFLEVG
!DIR$ IVDEP
!NEC$ IVDEP
            DO JLON = KST, KEND
              ZCP(JLON,JLEV) = ZCP(JLON,JLEV) + (YDVARS%GFL_PTR(JFLD)%YCOMP%RCP-YDCST%RCPD)*YDVARS%GFL_PTR(JFLD)%T1(JLON,JLEV)
            ENDDO
          ENDDO
        ENDIF

!$ACDC }

      ENDIF
    ENDDO
  CASE (9)
    DO JFLD = 1, IFLD
      IF (YDVARS%GFL_PTR (JFLD)%YCOMP%LTHERMACT) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

        DO JLEV = 1, KFLEVG
!DIR$ IVDEP
!NEC$ IVDEP
          DO JLON = KST, KEND
            ZR(JLON,JLEV) = ZR(JLON,JLEV) + (YDVARS%GFL_PTR(JFLD)%YCOMP%R-YDCST%RD)*YDVARS%GFL_PTR(JFLD)%T9(JLON,JLEV)
          ENDDO
        ENDDO
        IF (LLCP) THEN
          DO JLEV = 1, KFLEVG
!DIR$ IVDEP
!NEC$ IVDEP
            DO JLON = KST, KEND
              ZCP(JLON,JLEV) = ZCP(JLON,JLEV) + (YDVARS%GFL_PTR(JFLD)%YCOMP%RCP-YDCST%RCPD)*YDVARS%GFL_PTR(JFLD)%T9(JLON,JLEV)
            ENDDO
          ENDDO
        ENDIF

!$ACDC }

      ENDIF
    ENDDO
  CASE DEFAULT
    CALL ABOR1 ('UNEXPECTED IGFLTYP')
END SELECT

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

DO JLEV = 1, KFLEVG
  DO JLON = KST, KEND
    ZR(JLON,JLEV) = YDCST%RD + ZR(JLON,JLEV)
  ENDDO
ENDDO
IF (LLCP) THEN
  DO JLEV = 1, KFLEVG
    DO JLON = KST, KEND
      ZCP(JLON,JLEV) = YDCST%RCPD + ZCP(JLON,JLEV)
    ENDDO
  ENDDO
ENDIF

!$ACDC }

IF (PRESENT(PKAP)) THEN

!$ACDC PARALLEL, TARGET=OpenMP/OpenMPSingleColumn/OpenACCSingleColumn {

  DO JLEV = 1, KFLEVG
    DO JLON = KST, KEND
      PKAP(JLON,JLEV)=ZR(JLON,JLEV)/ZCP(JLON,JLEV)
    ENDDO
  ENDDO

!$ACDC }

ENDIF

END ASSOCIATE

IF (LHOOK) CALL DR_HOOK("GPRCP_EXPL",1,ZHOOK_HANDLE)

END SUBROUTINE GPRCP_EXPL



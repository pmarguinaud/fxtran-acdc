SUBROUTINE GPHLWI(LDVERTFE,KFLEV,KPROMA,KST,KEND,PLNPR,PALPH,PRDETAH,PDETA_RATIO,PWW)

!**** *GPHLWI* - to half-levels interpolation weights

!     Purpose.
!     --------
!           Compute weights for interpolation of winds to
!           half levels (in non-hydrostatic dynamics)

!**   Interface.
!     ----------
!        *CALL* *GPHLWI()

!        Explicit arguments :
!        --------------------
!         INPUT:
!          KFLEV   - Nb of vertical levels.
!          KPROMA  - dimensioning.
!          KST     - start of work.
!          KEND    - depth of work.
!          PLNPR   - "delta" (log(prehyd) depth) at full levels.
!          PALPH   - "alpha" at full levels.

!         OUTPUT:
!          PWW     - vertical weight.

!        Implicit arguments : none.
!        --------------------

!     Method.
!     -------
!        Interpolation from full-levels using logaritmic pressure profile
!        Then modify values on the top and bottom using boundary condition
!        (at the bottom free-slip condition)
!        Store the weights of vertical interpolation

!     Externals.
!     ----------
!     Reference.
!     ----------
!        ARPEGE/ALADIN documentation

!     Author.
!     -------
!        Radmila Bubnova,  CNRM/GMAP/EXT

!     Modifications.
!     --------------
!        Original : November 1997
!        Modified 02-07-02 by C. Fischer : remove pww5 computation + intents
!        M.Hamrud 01-Oct-2003 CY28 Cleaning
!        K.Yessad 20-Mar-2006 Cleaning + optimisation
!        K.Yessad (Dec 2008): remove dummy CDLOCK
!     ----------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK

!     ------------------------------------------------------------------

IMPLICIT NONE

LOGICAL           ,INTENT(IN)    :: LDVERTFE
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLNPR(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALPH(KPROMA,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDETAH(KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDETA_RATIO(KFLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWW(KPROMA,0:KFLEV)

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GPHLWI',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

IF (LDVERTFE) THEN
  DO JLEV=1, KFLEV-1
    DO JROF=KST,KEND
        PWW(JROF,JLEV)= (PDETA_RATIO(JLEV)*PRDETAH(JLEV))/&
         & ( (PDETA_RATIO(JLEV+1)*PRDETAH(JLEV+1))+&
         &   (PDETA_RATIO(JLEV)*PRDETAH(JLEV)) )
    ENDDO
  ENDDO
ELSE
  DO JLEV=1, KFLEV-1
    DO JROF=KST,KEND
      PWW(JROF,JLEV)=(PLNPR(JROF,JLEV+1)-PALPH(JROF,JLEV+1))&
       & /(PLNPR(JROF,JLEV+1)-PALPH(JROF,JLEV+1)+PALPH(JROF,JLEV))
    ENDDO
  ENDDO  
ENDIF
!     ------------------------------------------------------------------

DO JROF=KST,KEND
  !* Bottom half level:
  PWW(JROF,KFLEV) = 1.0_JPRB
ENDDO

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GPHLWI',1,ZHOOK_HANDLE)
END SUBROUTINE GPHLWI

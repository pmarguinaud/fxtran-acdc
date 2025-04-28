!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRSO(YDPHY0,KIDIA,KFDIA,KLON,KLEV,KTDIA,KSGST,PGEMU,PMU0,PAPHIF,PDELP,PFRSO)

!**** *ACRSO * - Reduce SOlar fluxes, depending on solar zenithal angle.

!-----------------------------------------------------------------------
! -   INPUT ARGUMENTS.
!     ----------------

! - PHYSICS DIMENSIONNING PARAMETERS

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".
! KTDIA      : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL).
! KSGST      : DIMENSION POUR LE RAYONNEMENT.

! - PHYSICS VARIABLES.

! - 2D (0:KLEV) .

! - 2D (1:KLEV) .

! PAPHIF     : FULL LEVEL GEOPOTENTIAL
! PDELP      : PRESSURE DIFFERENCE OVER THE LAYER

! - 1D .

! PGEMU      : sinus de la latitude.
! PMU0       : sinus de la hauteur du Soleil au-dessus de l'horizon.

!-----------------------------------------------------------------------

! -   OUTPUT ARGUMENTS
!     ----------------

! - 2D (0:KLEV) .

! - 2D (1:KLEV) .

! - 1D (DIAGNOSTIQUE) .

!-----------------------------------------------------------------------

! -   INPUT/OUTPUT ARGUMENTS
!     ----------------------

! - 2D (0:KLEV) .

! PFRSO      : SOLAR RADIATION FLUX.

! - 2D (1:KLEV) .

!-----------------------------------------------------------------------

! METHOD.
!     --------

! AUTHOR.
!     -------
! 2014-05-15, J.M. Piriou.

! MODIFICATIONS.
!     --------------

!-----------------------------------------------------------------------


USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMPHY0   , ONLY : TPHY0

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TPHY0)       ,INTENT(IN)    :: YDPHY0
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KSGST

REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEMU(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMU0(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFRSO(KLON,0:KLEV,KSGST+1)

REAL(KIND=JPRB)    :: ZRSO(KLON,KLEV)
REAL(KIND=JPRB) :: ZTEND(KLON,KLEV)
REAL(KIND=JPRB) :: ZFRAC
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
INTEGER(KIND=JPIM) :: JLON,JLEV

IF (LHOOK) CALL DR_HOOK('ACRSO',0,ZHOOK_HANDLE)
ASSOCIATE(GRSO=>YDPHY0%GRSO)
!-------------------------------------------------
! Reducing factor for net solar radiation.
!-------------------------------------------------
! ZRSO: reducing factor for net solar radiation, function of solar zenital angle.
! If GRSO=1., no reduction is done.
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZFRAC=MAX(0._JPRB,MIN(1._JPRB,(PAPHIF(JLON,JLEV)-115000._JPRB)/45000._JPRB))
    ZRSO(JLON,JLEV)=ZFRAC+(1._JPRB-ZFRAC)*(GRSO+PMU0(JLON)*(1._JPRB-GRSO))
  ENDDO
ENDDO

! Modify flux.
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZTEND(JLON,JLEV)=-(PFRSO(JLON,JLEV,1)-PFRSO(JLON,JLEV-1,1))/PDELP(JLON,JLEV)
    ZTEND(JLON,JLEV)=ZRSO(JLON,JLEV)*ZTEND(JLON,JLEV)
  ENDDO
ENDDO

! Recompute flux from modified tendency. This is done upwards in order to let
! surface flux unchanged.
DO JLEV=KLEV,KTDIA,-1
  DO JLON=KIDIA,KFDIA
    PFRSO(JLON,JLEV-1,1)=PFRSO(JLON,JLEV,1)+ZTEND(JLON,JLEV)*PDELP(JLON,JLEV)
  ENDDO
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACRSO',1,ZHOOK_HANDLE)
END SUBROUTINE ACRSO



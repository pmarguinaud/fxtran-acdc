!OPTIONS XOPT(NOEVAL)
SUBROUTINE TRIDIFV1 ( YDPHY,YDPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT  2D .
 & PA,PB,PC,PF ,PDFDT,PIPOI,PY,&
 ! - OUTPUT 2D .
 & PCFA,PCFB)

!**** *TRIDIFV1 * - Resolution of the tridiag of ACDIFV1

!     Sujet.
!     ------
!       Tridiag resolution

!**   Interface.
!     ----------
!        *CALL* *TRIDIFV1*

!-----------------------------------------------------------------------

! -   Input arguments.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : Start of horizontal loops
! KFDIA      : End of horizopntal loops
! KLON       : Horizontal Array size
! KTDIA      : Start of vertical loops (1)
! KLEV       : Vertical array size


! - 2D (KLON,KLEV) .

! PA         : Left subdiagonal term of matrix
! PB         : Diagonal term of matrix
! PC         : Right subdiagonal term of matrix
! PY         : Right member of the linear system


!-----------------------------------------------------------------------

! -   Output arguments
!     --------------------

! - 2D (KLON,KLEV) .

! PCFA       : Upgraded right subdiagonal term of matrix
! PCFB       : Upgraded right member of the linear system



!-----------------------------------------------------------------------


!     Externes.
!     ---------

!     Methode.
!     --------

!  Gauss elimination

!     Auteur.
!     -------
!        2011-08, Yves Bouteloup

!     Modifications.
!     --------------
!        JF. Gueremy 2015-11 : GCVIMPT implication factor for convective
!                              transport in combined (cv-turb) computation
!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMPHY   , ONLY : TPHY
USE YOMPHY0  , ONLY : TPHY0
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK


!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(TPHY)        ,INTENT(IN)    :: YDPHY
TYPE(TPHY0)       ,INTENT(IN)    :: YDPHY0
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PA(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PB(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PY(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PF(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDFDT(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PIPOI(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFA(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFB(KLON,KLEV) 


!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZMUL(KLON),ZY(KLON,KLEV)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

INTEGER(KIND=JPIM) :: JLEV, JLON


!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIDIFV1',0,ZHOOK_HANDLE)
ASSOCIATE(LEDMFI=>YDPHY%LEDMFI, GCVIMPT=>YDPHY0%GCVIMPT)
!-----------------------------------------------------------------------

! Construction of the right member of the linear system


IF (LEDMFI) THEN   ! <== Mass flux AND Eddy difusivity
   DO JLON=KIDIA,KFDIA
      ZY(JLON,KTDIA) =  PY(JLON,KTDIA)&
          &                           + PF(JLON,KTDIA-1)*PIPOI(JLON,KTDIA)&
          &                           - PF(JLON,KTDIA  )*PIPOI(JLON,KTDIA)&
          &                           -  PDFDT(JLON,KTDIA)*0.5_JPRB*GCVIMPT*PY(JLON,KTDIA  )*PIPOI(JLON,KTDIA)&
          &            -  PDFDT(JLON,KTDIA)*0.5_JPRB*GCVIMPT*PY(JLON,KTDIA+1)*PIPOI(JLON,KTDIA)
   ENDDO                    

   DO JLEV=KTDIA+1,KLEV-1
     DO JLON=KIDIA,KFDIA
       ZY(JLON,JLEV) =  PY(JLON,JLEV)&
          &                          + PF(JLON,JLEV-1)*PIPOI(JLON,JLEV)&
          &                          - PF(JLON,JLEV  )*PIPOI(JLON,JLEV)&
          &                          +  PDFDT(JLON,JLEV-1)*0.5_JPRB*GCVIMPT*PY(JLON,JLEV-1)*PIPOI(JLON,JLEV)&
          &                          +  PDFDT(JLON,JLEV-1)*0.5_JPRB*GCVIMPT*PY(JLON,JLEV  )*PIPOI(JLON,JLEV)&
          &                          -  PDFDT(JLON,JLEV  )*0.5_JPRB*GCVIMPT*PY(JLON,JLEV  )*PIPOI(JLON,JLEV)&
          &           -  PDFDT(JLON,JLEV  )*0.5_JPRB*GCVIMPT*PY(JLON,JLEV+1)*PIPOI(JLON,JLEV)
     ENDDO                 
   ENDDO

   DO JLON=KIDIA,KFDIA
      ZY(JLON,KLEV) =  PY(JLON,KLEV)&
          &                         + PF(JLON,KLEV-1)*PIPOI(JLON,KLEV)&
          &                         +  PDFDT(JLON,KLEV-1)*0.5_JPRB*GCVIMPT*PY(JLON,KLEV-1)*PIPOI(JLON,KLEV)&
          &           +  PDFDT(JLON,KLEV-1)*0.5_JPRB*GCVIMPT*PY(JLON,KLEV  )*PIPOI(JLON,KLEV)   
   ENDDO                   

ELSE            ! <== Eddy difusivity only 

  DO JLEV=KTDIA,KLEV  
    DO JLON=KIDIA,KFDIA
      ZY(JLON,JLEV) = PY(JLON,JLEV)
    ENDDO
  ENDDO    

ENDIF
!     ------------------------------------------------------------------
!     I -  TOP ELIMINATION 

DO JLON=KIDIA,KFDIA
    ZMUL(JLON)        = 1._JPRB/PB(JLON,KTDIA)
    PCFA(JLON,KTDIA)  = -ZMUL(JLON)*PC(JLON,KTDIA)                                    
    PCFB(JLON,KTDIA)  = ZMUL(JLON)*ZY(JLON,KTDIA)                                   
ENDDO    

!   II -  ELIMINATION FOR A STANDARD LEVEL

DO JLEV=KTDIA+1,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZMUL(JLON)       = 1._JPRB/(PB(JLON,JLEV)+PA(JLON,JLEV)*PCFA(JLON,JLEV-1))
    PCFA(JLON,JLEV)  = -ZMUL(JLON)*PC(JLON,JLEV)
    PCFB(JLON,JLEV)  = ZMUL(JLON)*(ZY(JLON,JLEV)-PA(JLON,JLEV)*PCFB(JLON,JLEV-1))
  ENDDO
ENDDO

!  III -  COEFFICIENTS AT LAST LEVEL

DO JLON=KIDIA,KFDIA
  ZMUL(JLON)       = 1._JPRB/(PB(JLON,KLEV)+PA(JLON,KLEV)*PCFA(JLON,KLEV-1))
  PCFA(JLON,KLEV)  = -ZMUL(JLON)*PC(JLON,KLEV)
  PCFB(JLON,KLEV)  =  ZMUL(JLON)*(ZY(JLON,KLEV)-PA(JLON,KLEV)*PCFB(JLON,KLEV-1))
ENDDO


!     ------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('TRIDIFV1',1,ZHOOK_HANDLE)
END SUBROUTINE TRIDIFV1

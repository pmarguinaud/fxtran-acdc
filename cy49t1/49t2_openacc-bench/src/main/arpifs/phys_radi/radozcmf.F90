SUBROUTINE RADOZCMF (YDCST, YDEOZOC, KIDIA , KFDIA , KLON , KLEV,&
 & PAPRS , PGEMU, POZON )

!**** *RADOZCMF* - COMPUTES DISTRIBUTION OF OZONE FROM CLIMATOLOGY

!     PURPOSE.
!     --------

!**   INTERFACE.
!     ----------

!        EXPLICIT ARGUMENTS :
!        --------------------

!        IMPLICIT ARGUMENTS :   NONE
!        --------------------

!     METHOD.
!     -------

!     EXTERNALS.
!     ----------

!          NONE

!     REFERENCE.
!     ----------

!        SEE RADIATION'S PART OF THE MODEL'S DOCUMENTATION AND
!        ECMWF RESEARCH DEPARTMENT DOCUMENTATION OF THE "I.F.S"

!     AUTHOR.
!     -------
!     F. BOUYSSEL  Meteo-France    2008/10/01
!     Adaptation of radozc to work on rotated and stretched geometry

!     MODIFICATIONS.
!     --------------
!     F. BOUYSSEL 2010-01-19   Optimisation

!-----------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY : TCST
USE YOEOZOC  , ONLY : TEOZOC

IMPLICIT NONE

TYPE(TCST)         ,INTENT(IN)   :: YDCST
TYPE(TEOZOC)       ,INTENT(IN)   :: YDEOZOC
INTEGER(KIND=JPIM) ,INTENT(IN)   :: KIDIA 
INTEGER(KIND=JPIM) ,INTENT(IN)   :: KFDIA 
INTEGER(KIND=JPIM) ,INTENT(IN)   :: KLON 
INTEGER(KIND=JPIM) ,INTENT(IN)   :: KLEV 
REAL(KIND=JPRB)    ,INTENT(IN)   :: PAPRS(KLON,KLEV+1) 
REAL(KIND=JPRB)    ,INTENT(IN)   :: PGEMU(KLON) 
REAL(KIND=JPRB)    ,INTENT(OUT)  :: POZON(KLON,KLEV) 
!     -----------------------------------------------------------------

!*       0.1   ARGUMENTS.
!              ----------

!     ----------------------------------------------------------------- 

!*       0.2   LOCAL ARRAYS.
!              -------------

REAL(KIND=JPRB) :: ZOZLT(KLON,0:35), ZOZON(KLON,KLEV+1)
REAL(KIND=JPRB) :: ZRRR(KLON,0:34), ZSILAT(KLON)

INTEGER(KIND=JPIM) :: INLA(KLON)
INTEGER(KIND=JPIM) :: JC, JLEV, JLON, I1

REAL(KIND=JPRB) :: Z1, Z2
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------

!*         1.     LATITUDE INDEX WITHIN OZONE CLIMATOLOGY
!                 ---------------------------------------

IF (LHOOK) CALL DR_HOOK('RADOZCMF',0,ZHOOK_HANDLE)

DO JLON=KIDIA,KFDIA
  INLA(JLON)=1+INT(18.0_JPRB/YDCST%RPI*ASIN(PGEMU(JLON))+9.0_JPRB)
  ZSILAT(JLON)=(PGEMU(JLON)-YDEOZOC%RSINC(INLA(JLON)))/(YDEOZOC%RSINC(INLA(JLON)+1)-YDEOZOC%RSINC(INLA(JLON)))
ENDDO

!     ------------------------------------------------------------------

!*         2.     LATITUDE INTERPOLATED FIELD
!                 ---------------------------

DO JC=0,35
  DO JLON=KIDIA,KFDIA
    Z1=REAL(INLA(JLON))
    Z2=MIN(ABS((Z1-1._JPRB)*(Z1-18._JPRB)),1.0_JPRB)
    I1=INLA(JLON)
    ZOZLT(JLON,JC)=(1.0_JPRB-Z2)*YDEOZOC%ROZT(I1,JC) &
     & + Z2*(YDEOZOC%ROZT(I1,JC)+ZSILAT(JLON)*(YDEOZOC%ROZT(I1+1,JC)-YDEOZOC%ROZT(I1,JC)))
  ENDDO
ENDDO

!     ------------------------------------------------------------------

!*         3.     VERTICAL INTERPOLATION 
!                 ----------------------

DO JC=0,34
  DO JLON=KIDIA,KFDIA
    ZRRR(JLON,JC)=(1.0_JPRB/(YDEOZOC%RPROC(JC)-YDEOZOC%RPROC(JC+1)))*(ZOZLT(JLON,JC)-ZOZLT(JLON,JC+1))
  ENDDO
ENDDO

DO JLEV=1,KLEV+1
  DO JLON=KIDIA,KFDIA
    ZOZON(JLON,JLEV)=0.0_JPRB
  ENDDO
ENDDO

DO JC=0,34
  DO JLEV=1,KLEV+1
    DO JLON=KIDIA,KFDIA
      ZOZON(JLON,JLEV)=ZOZON(JLON,JLEV) &
       & +(ZOZLT(JLON,JC+1)+(PAPRS(JLON,JLEV)-YDEOZOC%RPROC(JC+1))*ZRRR(JLON,JC)) &
       & *MAX(0.0_JPRB,SIGN(1.0_JPRB,PAPRS(JLON,JLEV)-YDEOZOC%RPROC(JC))) &
       & *MAX(0.0_JPRB,SIGN(1.0_JPRB,YDEOZOC%RPROC(JC+1)-PAPRS(JLON,JLEV)))
    ENDDO
  ENDDO
ENDDO

DO JLEV=1,KLEV+1
  DO JLON=KIDIA,KFDIA
    ZOZON(JLON,JLEV)=ZOZON(JLON,JLEV) &
     & +ZOZLT(JLON,35)*MAX(0.0_JPRB,SIGN(1.0_JPRB,PAPRS(JLON,JLEV)-YDEOZOC%RPROC(35)))
  ENDDO
ENDDO

! INTEGRATION IN THE VERTICAL:

DO JLEV=1,KLEV
  DO JLON=KIDIA,KFDIA
    POZON(JLON,JLEV)=(PAPRS(JLON,JLEV+1)-PAPRS(JLON,JLEV))&
     & *(ZOZON(JLON,JLEV)+ZOZON(JLON,JLEV+1))*0.5_JPRB  
  ENDDO
ENDDO

!     -----------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('RADOZCMF',1,ZHOOK_HANDLE)
END SUBROUTINE RADOZCMF

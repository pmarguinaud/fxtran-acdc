SUBROUTINE ACCLDIA(YDCST, LDXCLP, LDXTGST, LDXXGST,YDPHY,YDPHY2,YDTOPH,KIDIA,KFDIA,KLON,KLEV,&
                   & PUCLS,PVCLS,PU,PV,PCAPE,PDCAPE,PTKE,PAPHIFM,POROG,&
                   & PUGST,PVGST,PBLH,KCLPH)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMPHY   , ONLY : TPHY
USE YOMPHY2  , ONLY : TPHY2
USE YOMCST   , ONLY : TCST
USE YOMTOPH  , ONLY : TTOPH

!**** *ACCLDIA*  - Compute some PBL Diags  -

!     PURPOSE.
!     --------
!        To Compute some PBl diags 
!                   * Wind gusts from PBL wind and Tke
!                   * Height of PBL
!**   INTERFACE.
!     ----------
!       *CALL* *ACCLDIA*

!        EXPLICIT ARGUMENTS
!        --------------------
!            INPUT :
!        KIDIA   : start of work
!        KFDIA   : end of work
!        KLON    : depth of work
!        KLEV    : number of levels
!        PUCLS   : x-CLS wind              (KLON)
!        PVCLS   : y-CLS wind              (KLON)
!        PU      : x-wind                  (KLON,KLEV)
!        PV      : y-wind                  (KLON,KLEV)
!        PTKE    : TKE                     (KLON,KLEV)
!        PCAPE   : CAPE                    (KLON)
!        PDCAPE  : downward CAPE           (KLON)
!        PAPHIFM : full level geopotential (KLON,KLEV)
!        POROG   : orography times g       (KLON)
!            OUTPUT:
!        PUGST   : x-wind gust             (KLON)
!        PVGST   : y-wind gust             (KLON)
!        PBLH    : PBL height              (KLON)
!        KCLPH   : level of PBL            (KLON)

!        IMPLICIT ARGUMENTS
!        --------------------
!           NONE

!     METHOD.
!     -------
!        Consider That winds gust are // to cls wind. Consider Tke to
!        increment the cls wind to obtain wind gust. Formulation first
!        implemented and tested in the meso-Nh diags for the 1999
!        storms.

!     EXTERNALS.
!     ----------
   
!     REFERENCE.
!     ----------
!        None

!     AUTHOR.
!     -------
!        Gwenaelle Hello *METEO-FRANCE*

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 07-04-18
!        S. Riette    : 2009-03-25 HTKERAF and FACRAF are introduced
!        E. Bazile et Y. Seity : 2009-07-16 vectorisation
!        E. Bazile et Y. Seity : 2010-05-16 add PBL Height
!        2011-06: M. Jerczynski - some cleaning to meet norms
!        2018-06: J.M. Piriou : convective wind gusts (FACRAFCV, GCAPERAF, FACRAFDCAPE).
!        2020-10: R. Brozkova : usage with TOUCANS not touching PBL height
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.

! End modifications
!-------------------------------------------------------------------------------

IMPLICIT NONE

TYPE(TPHY)        ,INTENT(IN):: YDPHY
TYPE(TPHY2)       ,INTENT(IN):: YDPHY2
TYPE(TTOPH)       ,INTENT(IN):: YDTOPH
TYPE(TCST)        ,INTENT(IN):: YDCST
LOGICAL           ,INTENT(IN):: LDXCLP
LOGICAL           ,INTENT(IN):: LDXTGST
LOGICAL           ,INTENT(IN):: LDXXGST

INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA,KFDIA,KLON,KLEV
REAL(KIND=JPRB), INTENT(IN) :: PUCLS(KLON), PVCLS(KLON), POROG(KLON)
REAL(KIND=JPRB), INTENT(OUT) :: PUGST(KLON), PVGST(KLON), PBLH(KLON)
REAL(KIND=JPRB), INTENT(IN) :: PTKE(KLON,KLEV), PAPHIFM(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PU(KLON,KLEV), PV(KLON,KLEV), PCAPE(KLON), PDCAPE(KLON)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KCLPH(KLON)

INTEGER (KIND=JPIM) :: JLON, JLEV, ILEVM1
INTEGER (KIND=JPIM) :: JJLEVM1(KLON),JLEVM1(KLON),ICM(KLON,KLEV),ILEVBI(KLON)
REAL(KIND=JPRB) :: ZCVGUSTS
REAL(KIND=JPRB) :: ZALPHA, ZVCLS, ZZ,  ZEPS, ZECTBLH, ZHBOT, ZHTOP

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ACCLDIA',0,ZHOOK_HANDLE)
ASSOCIATE(LCOEFKTKE => YDPHY%LCOEFKTKE, &
 & LRAFTKE=>YDPHY2%LRAFTKE, HTKERAF=>YDPHY2%HTKERAF, &
 & FACRAF=>YDPHY2%FACRAF,&
 & FACRAFCV=>YDPHY2%FACRAFCV, &
 & FACRAFDCAPE=>YDPHY2%FACRAFDCAPE, &
 & GCAPERAF=>YDPHY2%GCAPERAF, &
 & RG=>YDCST%RG, &
 & NT850=>YDTOPH%NT850, &
 & NT950=>YDTOPH%NT950, &
 & NTCOET=>YDTOPH%NTCOET)
!      ----------------------------------------------------------------
 
! 1. Computation of gusts

IF ((LDXTGST .OR. LDXXGST).AND.LRAFTKE) THEN
ZEPS=1.E-6_JPRB
JLEVM1(:)=0

! Recherche du niveau JLEV juste au dessus de HTKERAF
DO JLEV=KLEV,1,-1
   DO JLON=KIDIA, KFDIA
      ZZ=PAPHIFM(JLON,JLEV)-POROG(JLON) - HTKERAF*RG
      JJLEVM1(JLON)=MAX(0._JPRB,SIGN(1._JPRB,ZZ))*JLEV
      JLEVM1(JLON)=MAX(JJLEVM1(JLON),JLEVM1(JLON))
   ENDDO
ENDDO


DO JLON=KIDIA, KFDIA
!  Cas ou HTKERAF est en dessous du niveau KLEV donc extrapolation
   JLEVM1(JLON)=MIN(JLEVM1(JLON),KLEV-1)
     ! Turbulent gusts.
   ZVCLS=MAX(ZEPS,PUCLS(JLON)**2+PVCLS(JLON)**2)
   ZALPHA = PTKE(JLON,JLEVM1(JLON))*(HTKERAF*RG+POROG(JLON)-PAPHIFM(JLON,JLEVM1(JLON)+1))-&
           & PTKE(JLON,JLEVM1(JLON)+1)*(HTKERAF*RG+POROG(JLON)-PAPHIFM(JLON,JLEVM1(JLON)  ))
   ZALPHA = MAX(ZEPS,ZALPHA / ( PAPHIFM(JLON,JLEVM1(JLON)) - PAPHIFM(JLON,JLEVM1(JLON)+1) ))
   ZALPHA = ZALPHA/ZVCLS
   ! Convective gusts, if CAPE > given threshold.
   ZCVGUSTS=MAX(0._JPRB,SQRT(PU(JLON,NT850)**2+PV(JLON,NT850)**2) &
      & -SQRT(PU(JLON,NT950)**2+PV(JLON,NT950)**2)) &
      & *MAX(0._JPRB,SIGN(1._JPRB,PCAPE(JLON)-GCAPERAF)) &
      & *MIN(1._JPRB,SQRT(MAX(0._JPRB,PCAPE(JLON)/(5._JPRB*GCAPERAF))))
   ! Gust from 3 processes: turbulence (FACRAF), convective transport (FACRAFCV), 
   ! precipitation evaporation in the PBL (FACRAFDCAPE).
   ZALPHA =1.0_JPRB + FACRAF * SQRT(ZALPHA) + FACRAFCV * ZCVGUSTS/SQRT(ZVCLS) &
     & + FACRAFDCAPE * SQRT(2._JPRB*MAX(0._JPRB,-PDCAPE(JLON)))/SQRT(ZVCLS)
   PUGST(JLON)=ZALPHA * PUCLS(JLON)
   PVGST(JLON)=ZALPHA * PVCLS(JLON)
ENDDO
ENDIF

! 2. Computation of PBL Height

! do not overwrite diagnosed PBL height for TOUCANS (LCOEFKTKE=.T.)
IF (LDXCLP.AND..NOT.LCOEFKTKE) THEN

  ! CALCUL DE LA HAUTEUR DE COUCHE LIMITE:
  ! PREMIER NIVEAU EN PARTANT DE LA SURFACE OU LA TKE <0.01

  !- - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Compute the INDEX array ILEVBI from the lowest
  ! half level (KLEV-1) to the "Top-PBL" half level :
  !- - - - - - - - - - - - - - - - - - - - - - - - - -
  ! ILEVBI(JLON) = 1 from KLEV-1 to the "Top-PBL"
  ! ILEVBI(JLON) = 0 above the "Top-PBL" half level
  !- - - - - - - - - - - - - - - - - - - - - - - - - -
  ! the case "MAX(JLEV, ILEVBI(JLON))" avoid the
  ! detection of the other "PBL" located above
  ! the first one close to the ground.
  !- - - - - - - - - - - - - - - - - - - - - - - - - -
  DO JLON=KIDIA,KFDIA
    ILEVBI(JLON)=0
  ENDDO
  ICM(:,:)=0
  ZECTBLH=0.01_JPRB
  !NTCOET = 1 by default in the setup 
  DO JLEV=NTCOET,KLEV
    DO JLON=KIDIA,KFDIA
      ICM(JLON,JLEV)=INT(MAX(0.0_JPRB,SIGN(1.0_JPRB, PTKE(JLON,JLEV)-ZECTBLH)))
    ENDDO
  ENDDO
  DO JLEV=KLEV,NTCOET,-1
    DO JLON=KIDIA,KFDIA
      ILEVM1=MAX(NTCOET, JLEV-1)
      IF ( (ICM(JLON, JLEV ) == 1).AND.(ICM(JLON,ILEVM1) == 0) ) THEN
        ILEVBI(JLON) = MAX(JLEV, ILEVBI(JLON))
      ENDIF
    ENDDO
  ENDDO
  DO JLON=KIDIA,KFDIA
    ILEVBI(JLON)=ILEVBI(JLON)*MAX(ICM(JLON, KLEV),ICM(JLON, KLEV-1))
    IF ((ICM(JLON, KLEV ) == 0).AND.(ILEVBI(JLON) == 0) ) ILEVBI(JLON)=KLEV
  ENDDO
  DO JLON=KIDIA,KFDIA
    KCLPH(JLON)=MAX(1,ILEVBI(JLON))
  ENDDO
  DO JLON=KIDIA,KFDIA
    IF ((ILEVBI(JLON) > 1).AND.(ILEVBI(JLON) < KLEV)) THEN
      ZHBOT=(PAPHIFM(JLON,ILEVBI(JLON))-POROG(JLON))/RG
      ZHTOP=(PAPHIFM(JLON,ILEVBI(JLON)-1)-POROG(JLON))/RG
      PBLH(JLON)=ZHBOT+(ZHTOP - ZHBOT)/&
       & (PTKE(JLON,ILEVBI(JLON)-1) - PTKE(JLON,ILEVBI(JLON)))*&
       & (ZECTBLH - PTKE(JLON,ILEVBI(JLON)))
    ELSEIF (ILEVBI(JLON) == KLEV) THEN
      PBLH(JLON)=(PAPHIFM(JLON,KLEV)-POROG(JLON))/RG
    ELSEIF (ILEVBI(JLON) == 0) THEN
      PBLH(JLON)=(PAPHIFM(JLON,NTCOET)-POROG(JLON))/RG
    ELSE
      PBLH(JLON)=(PAPHIFM(JLON,ILEVBI(JLON))-POROG(JLON))/RG
    ENDIF
  ENDDO

  !WRITE(NULOUT,*)'sous accldia PBLH',MINVAL(PBLH),MAXVAL(PBLH)
 
ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACCLDIA',1,ZHOOK_HANDLE)

END SUBROUTINE ACCLDIA


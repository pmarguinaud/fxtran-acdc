SUBROUTINE DPRECIPS(YDCST, YDPRECIPS,KIDIA,KFDIA,KLON,KLEV,POROG,PCLSTPW,PDIAGH,PAPHIFM,PDZZ,PTPW,&
        &PQCM,PFPLSL,PFPLSN,PFPLSG,PDPRECIPS)

!**** *DPRECIPS*   -  Compute precipitation type diagnostic

!     Purpose.
!     --------
!           Compute precipitation type diagnostic (PDPRECIPS), each time step 

!**   Interface.
!     ----------
!        *CALL* *DPRECIPS(...)

!        Explicit arguments :
!        --------------------
!----
! 0D :
!----
! KIDIA, KFDIA : START/END OF HORIZONTAL LOOP  (IST,IEND IN *CPG*).
! KLON : HORIZONTAL DIMENSION                  (KLON IN *APL_AROME*)
! KLEV : END OF VERTICAL LOOP AND VERTICAL DIMENSION(NFLEVG IN *CPG*)
!----
! 1D :
!----
! POROG      :  SURFACE GEOPOTENTIAL (mgp) 
! PCLSTPW : 2m T'w
! PGIAGH : Hail diagnostic
!----
! 2D :
!----
! PAPHIFM : RG*Full levels height (in m)
! PDZZ : Full levels depth (in m)
! PTPW : T'w 3D field
! PQCM        : SPECIFIC HUMIDITY OF CLOUD WATER
! PFPLSL      : SURFACE PRECIPITATION FLUX LIQUID
! PFPLSN      : SURFACE PRECIPITATION FLUX SNOW
! PFPLSG      : SURFACE PRECIPITATION FLUX GRAUPEL
! ------
! INOUT :
! ------
! PDPRECIPS   : precipitation type diagnostic :
!    0: no precipitation
!    1: rain   / pluie
!    3: freezing rain / pluie verglacante
!    5: dry snow / neige seche 
!    6: wet snow / neige humide
!    7: rain now mixture / pluie et neige melees
!    8: ice pellets/ granules de glace
!    9: graupel   / gresil
!   10: hail      / grele
!   11: drizzle/ bruine
!   12: freezing drizzle / bruine verglacante
!  193: moist snow / neige mouillee
!                                 
!        Implicit arguments :
!        --------------------
!        COMMON YOMDPRECIPS

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        Documentation ARPEGE/AROME

!     Authors.
!     -------
!      I.Etchevers Y. Seity.
!      Original : 2018-07-17

!     Modifications.
!     --------------
!        2019-10, I. Etchevers : optimization and cleaning
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!        2023-08, R. Brozkova  : further optimization
!
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK,  JPHOOK
USE YOMDPRECIPS  , ONLY : TDPRECIPS
USE YOMCST     , ONLY : TCST

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE (TCST), INTENT (IN) :: YDCST
TYPE(TDPRECIPS), INTENT(IN), TARGET ::  YDPRECIPS

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROG(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLSTPW(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDIAGH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIFM(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDZZ(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTPW(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQCM(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFPLSL(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFPLSN(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFPLSG(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDPRECIPS(KLON)


INTEGER (KIND=JPIM):: JLON, JLEV, JL,JLEVH,JLEVB, INDIH, INDI1, INDI2
INTEGER (KIND=JPIM)::ITOP(KLON),JLEVM1(KLON),JJLEVM1(KLON),I1(KLON), I2(KLON)
REAL(KIND=JPRB) :: ZZ,ZALPHA,ZINVG
REAL(KIND=JPRB) :: ZRATIO(KLON),ZLIQ(KLON),ZPROFNEG(KLON)
REAL(KIND=JPRB) :: ZICE(KLON),ZTOT(KLON),ZHEIGHT(KLON),ZHF(KLON,KLEV)
REAL(KIND=JPRB) :: ZAPOS(KLON),ZANEG(KLON),ZDCLWC(KLON)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!     ------------------------------------------------------------------


!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DPRECIPS',0,ZHOOK_HANDLE)

ASSOCIATE(RG=>YDCST%RG, RTT=>YDCST%RTT, HDPRECIPS=>YDPRECIPS%HDPRECIPS, HDCLWC=>YDPRECIPS%HDCLWC, &
 & RDHAIL1=>YDPRECIPS%RDHAIL1, RDHAIL2=>YDPRECIPS%RDHAIL2, &
 & RDSEUIL1=>YDPRECIPS%RDSEUIL1, RDSEUIL2=>YDPRECIPS%RDSEUIL2, RDSEUIL3=>YDPRECIPS%RDSEUIL3, &
 & RDSEUIL4=>YDPRECIPS%RDSEUIL4, RDSEUIL5=>YDPRECIPS%RDSEUIL5, RDCLWC=>YDPRECIPS%RDCLWC, &
 & RPRECSEUIL=>YDPRECIPS%RPRECSEUIL, RHTOP=>YDPRECIPS%RHTOP,RTPW=>YDPRECIPS%RTPW,  &
 & RAWARM=>YDPRECIPS%RAWARM, RACOLD=>YDPRECIPS%RACOLD, &
 & RAWARM2=>YDPRECIPS%RAWARM2, RACOLD2=>YDPRECIPS%RACOLD2 )

!     -------------------
!*       0.    Initialisations
!     -------------------


ZINVG=1._JPRB/RG

DO JLON=KIDIA,KFDIA
  ZLIQ(JLON)=MAX(PFPLSL(JLON),0._JPRB)
  ZICE(JLON)=MAX(PFPLSN(JLON),0._JPRB)+MAX(PFPLSG(JLON),0._JPRB)
  ZTOT(JLON)=ZLIQ(JLON)+ZICE(JLON)
  ZTOT(JLON)=MAX(ZTOT(JLON),1.E-8_JPRB)
  ZRATIO(JLON)=ZLIQ(JLON)/ZTOT(JLON)
  ZHEIGHT(JLON)=POROG(JLON)*ZINVG
ENDDO

! Preparations
ITOP(:)=1._JPIM
I1(KIDIA:KFDIA)=1._JPIM
I2(KIDIA:KFDIA)=1._JPIM
ZPROFNEG(:)=0._JPRB
ZANEG(:)=0._JPRB
ZAPOS(:)=0._JPRB

DO JLEV=KLEV,1,-1
  DO JLON=KIDIA,KFDIA
    ZHF(JLON,JLEV)=PAPHIFM(JLON,JLEV)*ZINVG
! first level index > 4000m above ground
    INDIH=INT(MAX(0._JPRB,SIGN(1._JPRB,ZHF(JLON,JLEV)-RHTOP-ZHEIGHT(JLON))))
    ITOP(JLON)=MAX(ITOP(JLON),JLEV*INDIH)

! first level index above ground where T'w > RTT in the layer limited by 4000 m
    I1(JLON)=MAX(I1(JLON),ITOP(JLON),JLEV*INT(MAX(0._JPRB,SIGN(1._JPRB,PTPW(JLON,JLEV)-RTT)))) 

! first level index above I1 where T'w < RTT in the layer from I1 to 4000 m
    I2(JLON)=MIN(I1(JLON),MAX(I2(JLON),ITOP(JLON),JLEV*INT(MAX(0._JPRB,SIGN(1._JPRB,RTT-PTPW(JLON,JLEV))))))

! integral from the ground up to the level 4000 m above ground 
    ZPROFNEG(JLON)=ZPROFNEG(JLON)+(1-INDIH)*MAX(0._JPRB,SIGN(1._JPRB,PTPW(JLON,JLEV)-RTT))

! integral from ground up to the level I1+1 securized by MIN with KLEV
    INDI1=MAX(0,SIGN(1,JLEV-MIN(I1(JLON)+1,KLEV)))
    ZANEG(JLON)=ZANEG(JLON)+INDI1*PDZZ(JLON,JLEV)*(RTT-PTPW(JLON,JLEV))

! integral from the level I1 up to the level I2+1, securized by MIN with I1
    INDI2=MAX(0,SIGN(1,JLEV-MIN(I2(JLON)+1,I1(JLON))))* &
        & MAX(0,SIGN(1,I1(JLON)-JLEV))
    ZAPOS(JLON)=ZAPOS(JLON)+INDI2*PDZZ(JLON,JLEV)*(PTPW(JLON,JLEV)-RTT)
  ENDDO
ENDDO

! computation of qc(HDCLWC) 
JLEVM1(:)=0
! level JLEV just above HDCLWC
DO JLEV=KLEV,1,-1
   DO JLON=KIDIA, KFDIA
      ZZ=ZHF(JLON,JLEV) - HDCLWC -ZHEIGHT(JLON)
      JJLEVM1(JLON)=MAX(0._JPRB,SIGN(1._JPRB,ZZ))*JLEV
      JLEVM1(JLON)=MAX(JJLEVM1(JLON),JLEVM1(JLON))
   ENDDO
ENDDO

DO JLON=KIDIA, KFDIA
   JLEVH=JLEVM1(JLON)
   JLEVB=MIN(JLEVM1(JLON)+1,KLEV)
   IF(JLEVH == JLEVB) THEN
       ZDCLWC(JLON)=PQCM(JLON,JLEV)
   ELSE
       ZALPHA=(ZHF(JLON,JLEVH)-HDCLWC-ZHEIGHT(JLON))/&
        &(ZHF(JLON,JLEVH)-ZHF(JLON,JLEVB))
       ZDCLWC(JLON)=PQCM(JLON,JLEVB)*ZALPHA+PQCM(JLON,JLEVH)*(1-ZALPHA)
   ENDIF
ENDDO


! Determining the type of precipitation

DO JLON=KIDIA,KFDIA

IF (ZTOT(JLON)>= RPRECSEUIL) THEN

!     -------------------
!*       1.    Hail
!     -------------------
  IF (PDIAGH(JLON)>= RDHAIL1) THEN
     IF (PDIAGH(JLON)>= RDHAIL2) THEN
        PDPRECIPS(JLON)=10._JPRB ! Hail
     ELSE 
        PDPRECIPS(JLON)=9._JPRB ! Small Hail 
     ENDIF   

!     -------------------
!*       2.    T'w2m < 0 
!     -------------------
! diag freezing drizzle

  ELSEIF (PCLSTPW(JLON) < RTPW) THEN

    IF (ZPROFNEG(JLON)==0._JPRB) THEN

       IF (ZDCLWC(JLON)>RDCLWC) THEN
          PDPRECIPS(JLON)=12._JPRB ! Freezing drizzle
       ELSEIF (PCLSTPW(JLON) <= RTPW-2) THEN
          PDPRECIPS(JLON)=5._JPRB  ! Dry snow
       ELSE
          PDPRECIPS(JLON)=6._JPRB  ! Wet snow
       ENDIF

! diag freezing rain       

    ELSEIF (ZAPOS(JLON)>=RAWARM) THEN
      IF (ZANEG(JLON)< RACOLD)  THEN
         PDPRECIPS(JLON)=3._JPRB   ! Freezing rain
      ELSE
         PDPRECIPS(JLON)=8._JPRB   ! Ice pellets 
      ENDIF
    ELSEIF (ZAPOS(JLON)>=RAWARM2) THEN
      IF (ZANEG(JLON)< RACOLD2)  THEN
         PDPRECIPS(JLON)=7._JPRB      ! Rain snow mixture
      ELSE
         PDPRECIPS(JLON)=8._JPRB   ! Ice pellets
      ENDIF
    ELSE
       PDPRECIPS(JLON)=6._JPRB  ! Wet snow
    ENDIF

!     -------------------
!*       3.    T'w2m >= 0 
!     -------------------

  ELSEIF (ZRATIO(JLON) > RDSEUIL4) THEN
    IF (ZLIQ(JLON) > RDSEUIL5) THEN
       PDPRECIPS(JLON)=1._JPRB    ! Rain
    ELSE 
       PDPRECIPS(JLON)=11._JPRB    ! Drizzle
    ENDIF
  ELSEIF (ZRATIO(JLON) > RDSEUIL3) THEN
    PDPRECIPS(JLON)=7._JPRB       ! Rain snow mixture
  ELSEIF (ZRATIO(JLON) > RDSEUIL2) THEN
    PDPRECIPS(JLON)=193._JPRB     ! Moist snow
  ELSEIF (ZRATIO(JLON) > RDSEUIL1) THEN
    PDPRECIPS(JLON)=6._JPRB       ! Wet snow
  ELSE
    PDPRECIPS(JLON)=5._JPRB       ! Dry snow
  
  ENDIF

ELSE
 PDPRECIPS(JLON)=0._JPRB ! No significant precipitation

ENDIF 

ENDDO 


END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('DPRECIPS',1,ZHOOK_HANDLE)
END SUBROUTINE DPRECIPS

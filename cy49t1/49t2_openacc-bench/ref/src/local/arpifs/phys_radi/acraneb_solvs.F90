!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_SOLVS(YDPHY,LDNUMX,&
 & KLON,KTDIA,KLEV,KJN,KIIDIA,KIFDIA,KAUCR,&
! - INPUT 1D
 & PALB,PALBDIR,PCLCT,PFRSODIRT,PFRSODIRS_C,PFRSODIRS_CUN,&
 & PNEB,PB1,PB2,PB3,PB4,&
 & PA1C,PA1CUN,PA2C,PA3C,PA4C,PA5C,PA1N,PA1NUN,PA2N,PA3N,PA4N,PA5N,&
! - OUTPUT 2D
 & PFRSO,&
! - OUTPUT 1D
 & PFRSODIFS,PFRSODIRS,PFRSODIRS_UN,PFRSODIRS_TRUE)

!$ACDC singlecolumn  


! Purpose:
! --------
!   ACRANEB_SOLVS - Adding system solver (solar band).

! Interface:
! ----------
! INPUT:
!   LDNUMX          - former LRNUMX, false for random overlaps
!   KLON            - horizontal dimension of arrays
!   KTDIA           - initial index for vertical loops (usually 1)
!   KLEV            - vertical dimension of full level arrays
!   KJN             - dimension of arrays containing "daylight" intervals
!   KIIDIA          - array of indices marking start of "daylight" intervals
!   KIFDIA          - array of indices marking end of "daylight" intervals
!   KAUCR           - number of "daylight" intervals
!   PALB            - diffuse surface albedo
!   PALBDIR         - direct (parallel) surface albedo
!   PCLCT           - total cloud cover
!   PFRSODIRT       - direct solar flux at model top (incoming radiation)
!   PFRSODIRS_C     - direct solar flux at surface, clearsky, delta-scaled
!   PFRSODIRS_CUN   - direct solar flux at surface, clearsky, delta-unscaled
!   PNEB            - cloud fraction
!   PB1             - 1st intermediate storage for cloud geometry
!   PB2             - 2nd intermediate storage for cloud geometry
!   PB3             - 3rd intermediate storage for cloud geometry
!   PB4             - 4th intermediate storage for cloud geometry
!   PA1C            - clearsky parallel transmissivity
!   PA1CUN          - clearsky parallel transmissivity, not delta-scaled
!   PA2C            - clearsky parallel/diffuse transmissivity
!   PA3C            - clearsky parallel/diffuse reflectivity
!   PA4C            - clearsky diffuse transmissivity
!   PA5C            - clearsky diffuse reflectivity
!   PA1N            - cloudy parallel transmissivity
!   PA1NUN          - cloudy parallel transmissivity, not delta-scaled
!   PA2N            - cloudy parallel/diffuse transmissivity
!   PA3N            - cloudy parallel/diffuse reflectivity
!   PA4N            - cloudy diffuse transmissivity
!   PA5N            - cloudy diffuse reflectivity
! OUTPUT:
!   PFRSO          - net solar flux (positive downwards)
!   PFRSODIFS      - downward diffuse  solar flux at surface
!   PFRSODIRS      - direct (parallel) solar flux at surface, delta-scaled
!   PFRSODIRS_UN   - direct (parallel) solar flux at surface, delta-unscaled
!   PFRSODIRS_TRUE - direct (parallel) solar flux at surface, true

! Externals:
! ----------

! Method:
! -------

! Reference:
! ----------

! Author:
! -------
!   1989-12, J.-F. Geleyn (original ACRANEB)

! Modifications:
! --------------
!   2009-10, T. Kral
!   Externalized from ACRANEB.

!   2013-11, J. Masek
!   Backphasing to cy38t1.

!   2016-04, J. Masek
!   Computation of true direct solar flux.

!   2016-09, J. Masek
!   Proper naming of variables containing delta-unscaled values,
!   changes needed for sunshine duration.

!   2019-09, J. Masek
!   Key LRNUMX changed to dummy argument LDNUMX, enabling efficient
!   clearsky calculations. Final evaluation of fluxes moved here from
!   ACRANEB2. Avoided INOUT arrays.

! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM    ,JPRB
USE YOMPHY    ,ONLY : TPHY
USE YOMHOOK   ,ONLY : LHOOK   ,DR_HOOK, JPHOOK

IMPLICIT NONE

TYPE(TPHY)        ,INTENT(IN) :: YDPHY
LOGICAL           ,INTENT(IN) :: LDNUMX
INTEGER(KIND=JPIM),INTENT(IN) :: KLON 
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KJN
INTEGER(KIND=JPIM),INTENT(IN) :: KIIDIA(KJN)
INTEGER(KIND=JPIM),INTENT(IN) :: KIFDIA(KJN)
INTEGER(KIND=JPIM),INTENT(IN) :: KAUCR

REAL(KIND=JPRB),INTENT(IN)  :: PALB(KLON)
REAL(KIND=JPRB),INTENT(IN)  :: PALBDIR(KLON)
REAL(KIND=JPRB),INTENT(IN)  :: PCLCT(KLON)
REAL(KIND=JPRB),INTENT(IN)  :: PFRSODIRT(KLON)
REAL(KIND=JPRB),INTENT(IN)  :: PFRSODIRS_C(KLON)
REAL(KIND=JPRB),INTENT(IN)  :: PFRSODIRS_CUN(KLON)
REAL(KIND=JPRB),INTENT(IN)  :: PNEB(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PB1(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PB2(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PB3(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PB4(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA1C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA1CUN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA2C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA3C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA4C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA5C(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA1N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA1NUN(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA2N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA3N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA4N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)  :: PA5N(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PFRSO(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT) :: PFRSODIFS(KLON)
REAL(KIND=JPRB),INTENT(OUT) :: PFRSODIRS(KLON)
REAL(KIND=JPRB),INTENT(OUT) :: PFRSODIRS_UN(KLON)
REAL(KIND=JPRB),INTENT(OUT) :: PFRSODIRS_TRUE(KLON)

! local real scalars
REAL(KIND=JPRB) :: ZAL,ZALP,ZTD1,ZTD2,ZTD3,ZTD4,ZTD5,ZTD6,ZTD7,&
                 & ZTDS1,ZTDS2,ZTDS3,ZTUS1,ZFPC_TRUE

! local real 1D arrays
REAL(KIND=JPRB) :: ZAL1(KLON),ZAL2(KLON),ZBE1(KLON),ZBE2(KLON),&
                 & ZDE1(KLON),ZDE2(KLON),ZGA1(KLON),ZGA2(KLON)

! local real 2D arrays
REAL(KIND=JPRB) :: ZA1C  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZA1CUN(KLON,  KLEV)
REAL(KIND=JPRB) :: ZA2C  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZA3C  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZA4C  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZA5C  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU1  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU2  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU3  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU4  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU5  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU6  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU7  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU8  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZTU9  (KLON,  KLEV)
REAL(KIND=JPRB) :: ZFDC  (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZFMC  (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZFPC  (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZFPCUN(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZFDN  (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZFMN  (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZFPN  (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZFPNUN(KLON,0:KLEV)

! local integer scalars
INTEGER(KIND=JPIM) :: JLEV, JLON, JN

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ACRANEB_SOLVS',0,ZHOOK_HANDLE)
ASSOCIATE(LRTRUEBBC=>YDPHY%LRTRUEBBC,LRTRUEDIR=>YDPHY%LRTRUEDIR)
! - TEMPORAIRE(S) 1D

! ZAL1      : weight of the M outgoing C flux in the M ingoing C flux
! ZAL2      : weight of the D outgoing C flux in the D ingoing C flux
! ZBE1      : weight of the M outgoing C flux in the M ingoing N flux
! ZBE2      : weight of the D outgoing C flux in the D ingoing N flux
! ZGA1      : weight of the M outgoing N flux in the M ingoing C flux
! ZGA2      : weight of the D outgoing N flux in the D ingoing C flux
! ZDE1      : weight of the M outgoing N flux in the M ingoing N flux
! ZDE2      : weight of the D outgoing N flux in the D ingoing N flux

! - TEMPORAIRE(S) 2D (1:KLEV)

! ZA1C      : clearsky parallel transmissivity
! ZA1CUN    : clearsky parallel transmissivity, not delta-scaled
! ZA2C      : clearsky parallel/diffuse transmissivity
! ZA3C      : clearsky parallel/diffuse reflectivity
! ZA4C      : clearsky diffuse transmissivity
! ZA5C      : clearsky diffuse reflectivity
! ZFDC      : downward diffuse  flux, clearsky
! ZFMC      : upward   diffuse  flux, clearsky
! ZFPC      : direct (parallel) flux, clearsky, delta-scaled
! ZFPCUN    : direct (parallel) flux, clearsky, delta-unscaled
! ZFDN      : downward diffuse  flux, cloudy
! ZFMN      : upward   diffuse  flux, cloudy
! ZFPN      : direct (parallel) flux, cloudy, delta-scaled
! ZFPNUN    : direct (parallel) flux, cloudy, delta-unscaled
! ZTU1      : 1st superdiagonal coeff. for the adding system
! ZTU2      : 2nd superdiagonal coeff. for the adding system
! ZTU3      : 3rd superdiagonal coeff. for the adding system
! ZTU4      : 4th superdiagonal coeff. for the adding system
! ZTU5      : 5th superdiagonal coeff. for the adding system
! ZTU6      : 6th superdiagonal coeff. for the adding system
! ZTU7      : 7th superdiagonal coeff. for the adding system
! ZTU8      : 8th superdiagonal coeff. for the adding system
! ZTU9      : 9th superdiagonal coeff. for the adding system

! initialize fluxes, apply upper boundary condition
!-------------------------------------------------------------------------------

ZFDC  (:,:)=0._JPRB
ZFPC  (:,:)=0._JPRB
ZFPCUN(:,:)=0._JPRB
IF (LDNUMX) THEN
  ZFDN  (:,:)=0._JPRB
  ZFPN  (:,:)=0._JPRB
  ZFPNUN(:,:)=0._JPRB
ENDIF
DO JN=1,KAUCR
  DO JLON=KIIDIA(JN),KIFDIA(JN)
    ZFPC  (JLON,KTDIA-1)=PFRSODIRT(JLON)
    ZFPCUN(JLON,KTDIA-1)=PFRSODIRT(JLON)
  ENDDO
ENDDO

! first layer, elimination (easy)
!-------------------------------------------------------------------------------

DO JN=1,KAUCR
  DO JLON=KIIDIA(JN),KIFDIA(JN)
    IF (LDNUMX) THEN
      ZAL1(JLON)=PB2(JLON,KTDIA)
      ZAL2(JLON)=PB1(JLON,KTDIA)
      ZBE1(JLON)=1._JPRB-PB2(JLON,KTDIA)
      ZBE2(JLON)=1._JPRB-PB1(JLON,KTDIA)
      ZGA1(JLON)=1._JPRB-PB4(JLON,KTDIA)
      ZGA2(JLON)=1._JPRB-PB3(JLON,KTDIA)
      ZDE1(JLON)=PB4(JLON,KTDIA)
      ZDE2(JLON)=PB3(JLON,KTDIA)
    ELSE
      ZA1C(JLON,KTDIA)=PA1C(JLON,KTDIA)*PB1(JLON,KTDIA)&
       & +PA1N(JLON,KTDIA)*PNEB(JLON,KTDIA)
      ZA1CUN(JLON,KTDIA)=PA1CUN(JLON,KTDIA)*PB1(JLON,KTDIA)&
       & +PA1NUN(JLON,KTDIA)*PNEB(JLON,KTDIA)
      ZA2C(JLON,KTDIA)=PA2C(JLON,KTDIA)*PB1(JLON,KTDIA)&
       & +PA2N(JLON,KTDIA)*PNEB(JLON,KTDIA)
      ZA3C(JLON,KTDIA)=PA3C(JLON,KTDIA)*PB1(JLON,KTDIA)&
       & +PA3N(JLON,KTDIA)*PNEB(JLON,KTDIA)
      ZA4C(JLON,KTDIA)=PA4C(JLON,KTDIA)*PB1(JLON,KTDIA)&
       & +PA4N(JLON,KTDIA)*PNEB(JLON,KTDIA)
      ZA5C(JLON,KTDIA)=PA5C(JLON,KTDIA)*PB1(JLON,KTDIA)&
       & +PA5N(JLON,KTDIA)*PNEB(JLON,KTDIA)
    ENDIF

    IF (LDNUMX) THEN
      ZFMC(JLON,KTDIA-1)=PA3C(JLON,KTDIA)*ZAL2(JLON)*ZFPC(JLON,KTDIA-1)
      ZFPC(JLON,KTDIA)=PA1C(JLON,KTDIA)*ZAL2(JLON)*ZFPC(JLON,KTDIA-1)
      ZFPCUN(JLON,KTDIA)=PA1CUN(JLON,KTDIA)*ZAL2(JLON)*ZFPCUN(JLON,KTDIA-1)
      ZFDC(JLON,KTDIA)=PA2C(JLON,KTDIA)*ZAL2(JLON)*ZFPC(JLON,KTDIA-1)
      ZFMN(JLON,KTDIA-1)=PA3N(JLON,KTDIA)*ZBE2(JLON)*ZFPC(JLON,KTDIA-1)
      ZFPN(JLON,KTDIA)=PA1N(JLON,KTDIA)*ZBE2(JLON)*ZFPC(JLON,KTDIA-1)
      ZFPNUN(JLON,KTDIA)=PA1NUN(JLON,KTDIA)*ZBE2(JLON)*ZFPCUN(JLON,KTDIA-1)
      ZFDN(JLON,KTDIA)=PA2N(JLON,KTDIA)*ZBE2(JLON)*ZFPC(JLON,KTDIA-1)
      ZTU1(JLON,KTDIA)=0._JPRB
      ZTU2(JLON,KTDIA)=ZAL1(JLON)*PA4C(JLON,KTDIA)
      ZTU3(JLON,KTDIA)=ZGA1(JLON)*PA4C(JLON,KTDIA)
      ZTU4(JLON,KTDIA)=ZBE1(JLON)*PA4N(JLON,KTDIA)
      ZTU5(JLON,KTDIA)=ZDE1(JLON)*PA4N(JLON,KTDIA)
      ZTU6(JLON,KTDIA)=ZAL1(JLON)*PA5C(JLON,KTDIA)
      ZTU7(JLON,KTDIA)=ZGA1(JLON)*PA5C(JLON,KTDIA)
      ZTU8(JLON,KTDIA)=ZBE1(JLON)*PA5N(JLON,KTDIA)
      ZTU9(JLON,KTDIA)=ZDE1(JLON)*PA5N(JLON,KTDIA)
    ELSE
      ZFMC(JLON,KTDIA-1)=ZA3C(JLON,KTDIA)*ZFPC(JLON,KTDIA-1)
      ZFPC(JLON,KTDIA)=ZA1C(JLON,KTDIA)*ZFPC(JLON,KTDIA-1)
      ZFPCUN(JLON,KTDIA)=ZA1CUN(JLON,KTDIA)*ZFPCUN(JLON,KTDIA-1)
      ZFDC(JLON,KTDIA)=ZA2C(JLON,KTDIA)*ZFPC(JLON,KTDIA-1)
      ZTU2(JLON,KTDIA)=ZA4C(JLON,KTDIA)
      ZTU6(JLON,KTDIA)=ZA5C(JLON,KTDIA)
    ENDIF

  ENDDO
ENDDO

! loop over the layers, preliminary computations and then elimination
!-------------------------------------------------------------------------------

DO JLEV=KTDIA+1,KLEV

  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)

      IF (LDNUMX) THEN
        ZAL1(JLON)=PB2(JLON,JLEV)
        ZAL2(JLON)=PB1(JLON,JLEV)
        ZBE1(JLON)=1._JPRB-PB2(JLON,JLEV)
        ZBE2(JLON)=1._JPRB-PB1(JLON,JLEV)
        ZGA1(JLON)=1._JPRB-PB4(JLON,JLEV)
        ZGA2(JLON)=1._JPRB-PB3(JLON,JLEV)
        ZDE1(JLON)=PB4(JLON,JLEV)
        ZDE2(JLON)=PB3(JLON,JLEV)
      ELSE
        ZA1C(JLON,JLEV)=PA1C(JLON,JLEV)*PB1(JLON,JLEV)&
         & +PA1N(JLON,JLEV)*PNEB(JLON,JLEV)
        ZA1CUN(JLON,JLEV)=PA1CUN(JLON,JLEV)*PB1(JLON,JLEV)&
         & +PA1NUN(JLON,JLEV)*PNEB(JLON,JLEV)
        ZA2C(JLON,JLEV)=PA2C(JLON,JLEV)*PB1(JLON,JLEV)&
         & +PA2N(JLON,JLEV)*PNEB(JLON,JLEV)
        ZA3C(JLON,JLEV)=PA3C(JLON,JLEV)*PB1(JLON,JLEV)&
         & +PA3N(JLON,JLEV)*PNEB(JLON,JLEV)
        ZA4C(JLON,JLEV)=PA4C(JLON,JLEV)*PB1(JLON,JLEV)&
         & +PA4N(JLON,JLEV)*PNEB(JLON,JLEV)
        ZA5C(JLON,JLEV)=PA5C(JLON,JLEV)*PB1(JLON,JLEV)&
         & +PA5N(JLON,JLEV)*PNEB(JLON,JLEV)
      ENDIF

    ENDDO
  ENDDO

  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)

      IF (LDNUMX) THEN
        ZTD1=1._JPRB/(1._JPRB-PA5C(JLON,JLEV)*(ZAL2(JLON)&
         & *ZTU6(JLON,JLEV-1)+ZGA2(JLON)*ZTU8(JLON,JLEV-1)))
        ZFMC(JLON,JLEV-1)=ZTD1*(PA5C(JLON,JLEV)*(ZAL2(JLON)&
         & *ZFDC(JLON,JLEV-1)+ZGA2(JLON)*ZFDN(JLON,JLEV-1))&
         & +PA3C(JLON,JLEV)*(ZAL2(JLON)*ZFPC(JLON,JLEV-1)&
         & +ZGA2(JLON)*ZFPN(JLON,JLEV-1)))
        ZTU1(JLON,JLEV)=ZTD1*PA5C(JLON,JLEV)*(ZAL2(JLON)&
         & *ZTU7(JLON,JLEV-1)+ZGA2(JLON)*ZTU9(JLON,JLEV-1))
        ZTU2(JLON,JLEV)=(ZTD1*PA4C(JLON,JLEV))*ZAL1(JLON)
        ZTU3(JLON,JLEV)=(ZTD1*PA4C(JLON,JLEV))*ZGA1(JLON)
        ZTD2=PA5N(JLON,JLEV)*(ZBE2(JLON)*ZTU6(JLON,JLEV-1)&
         & +ZDE2(JLON)*ZTU8(JLON,JLEV-1))
        ZTD3=1._JPRB/(1._JPRB-PA5N(JLON,JLEV)*(ZBE2(JLON)&
         & *ZTU7(JLON,JLEV-1)+ZDE2(JLON)*ZTU9(JLON,JLEV-1))&
         & -ZTD2*ZTU1(JLON,JLEV))
        ZFMN(JLON,JLEV-1)=ZTD3*(PA5N(JLON,JLEV)*(ZBE2(JLON)&
         & *ZFDC(JLON,JLEV-1)+ZDE2(JLON)*ZFDN(JLON,JLEV-1))&
         & +ZTD2*ZFMC(JLON,JLEV-1)+PA3N(JLON,JLEV)*(ZBE2(JLON)&
         & *ZFPC(JLON,JLEV-1)+ZDE2(JLON)*ZFPN(JLON,JLEV-1)))
        ZTU4(JLON,JLEV)=ZTD3*(PA4N(JLON,JLEV)&
         & *ZBE1(JLON)+ZTD2*ZTU2(JLON,JLEV))
        ZTU5(JLON,JLEV)=ZTD3*(PA4N(JLON,JLEV)&
         & *ZDE1(JLON)+ZTD2*ZTU3(JLON,JLEV))
        ZFPC(JLON,JLEV)=PA1C(JLON,JLEV)*(ZAL2(JLON)&
         & *ZFPC(JLON,JLEV-1)+ZGA2(JLON)*ZFPN(JLON,JLEV-1))
        ZFPCUN(JLON,JLEV)=PA1CUN(JLON,JLEV)*(ZAL2(JLON)&
         & *ZFPCUN(JLON,JLEV-1)+ZGA2(JLON)*ZFPNUN(JLON,JLEV-1))
        ZFPN(JLON,JLEV)=PA1N(JLON,JLEV)*(ZBE2(JLON)&
         & *ZFPC(JLON,JLEV-1)+ZDE2(JLON)*ZFPN(JLON,JLEV-1))
        ZFPNUN(JLON,JLEV)=PA1NUN(JLON,JLEV)*(ZBE2(JLON)&
         & *ZFPCUN(JLON,JLEV-1)+ZDE2(JLON)*ZFPNUN(JLON,JLEV-1))
        ZTD4=PA4C(JLON,JLEV)*(ZAL2(JLON)*ZTU6(JLON,JLEV-1)&
         & +ZGA2(JLON)*ZTU8(JLON,JLEV-1))
        ZTD5=PA4C(JLON,JLEV)*(ZAL2(JLON)*ZTU7(JLON,JLEV-1)&
         & +ZGA2(JLON)*ZTU9(JLON,JLEV-1))
        ZFDC(JLON,JLEV)=PA4C(JLON,JLEV)*(ZAL2(JLON)*ZFDC(JLON,JLEV-1)&
         & +ZGA2(JLON)*ZFDN(JLON,JLEV-1))+ZTD4*ZFMC(JLON,JLEV-1)&
         & +ZTD5*ZFMN(JLON,JLEV-1)+PA2C(JLON,JLEV)*(ZAL2(JLON)&
         & *ZFPC(JLON,JLEV-1)+ZGA2(JLON)*ZFPN(JLON,JLEV-1))
        ZTU6(JLON,JLEV)=PA5C(JLON,JLEV)*ZAL1(JLON)&
         & +ZTD4*ZTU2(JLON,JLEV)+ZTD5*ZTU4(JLON,JLEV)
        ZTU7(JLON,JLEV)=PA5C(JLON,JLEV)*ZGA1(JLON)&
         & +ZTD4*ZTU3(JLON,JLEV)+ZTD5*ZTU5(JLON,JLEV)
        ZTD6=PA4N(JLON,JLEV)*(ZBE2(JLON)*ZTU6(JLON,JLEV-1)&
         & +ZDE2(JLON)*ZTU8(JLON,JLEV-1))
        ZTD7=PA4N(JLON,JLEV)*(ZBE2(JLON)*ZTU7(JLON,JLEV-1)&
         & +ZDE2(JLON)*ZTU9(JLON,JLEV-1))
        ZFDN(JLON,JLEV)=PA4N(JLON,JLEV)*(ZBE2(JLON)*ZFDC(JLON,JLEV-1)&
         & +ZDE2(JLON)*ZFDN(JLON,JLEV-1))+ZTD6 *ZFMC(JLON,JLEV-1)&
         & +ZTD7*ZFMN(JLON,JLEV-1)+PA2N(JLON,JLEV)*(ZBE2(JLON)&
         & *ZFPC(JLON,JLEV-1)+ZDE2(JLON)*ZFPN(JLON,JLEV-1))
        ZTU8(JLON,JLEV)=PA5N(JLON,JLEV)*ZBE1(JLON)+ZTD6&
         & *ZTU2(JLON,JLEV)+ZTD7*ZTU4(JLON,JLEV)
        ZTU9(JLON,JLEV)=PA5N(JLON,JLEV)*ZDE1(JLON)+ZTD6&
         & *ZTU3(JLON,JLEV)+ZTD7*ZTU5(JLON,JLEV)
      ELSE
        ZTD1=1._JPRB/(1._JPRB-ZA5C(JLON,JLEV)*ZTU6(JLON,JLEV-1))
        ZFMC(JLON,JLEV-1)=ZTD1*(ZA5C(JLON,JLEV)*ZFDC(JLON,JLEV-1)&
         & +ZA3C(JLON,JLEV)*ZFPC(JLON,JLEV-1))
        ZTU2(JLON,JLEV)=ZTD1*ZA4C(JLON,JLEV)
        ZFPC(JLON,JLEV)=ZA1C(JLON,JLEV)*ZFPC(JLON,JLEV-1)
        ZFPCUN(JLON,JLEV)=ZA1CUN(JLON,JLEV)*ZFPCUN(JLON,JLEV-1)
        ZTD4=ZA4C(JLON,JLEV)*ZTU6(JLON,JLEV-1)
        ZFDC(JLON,JLEV)=ZA4C(JLON,JLEV)*ZFDC(JLON,JLEV-1)+ZTD4&
         & *ZFMC(JLON,JLEV-1)+ZA2C(JLON,JLEV)*ZFPC(JLON,JLEV-1)
        ZTU6(JLON,JLEV)=ZA5C(JLON,JLEV)+ZTD4*ZTU2(JLON,JLEV)
      ENDIF ! LDNUMX

    ENDDO
  ENDDO

ENDDO

! surface treatment, elimination and back-substitution
!-------------------------------------------------------------------------------

! TODO: INCLUDE ALBEDO IN ZAC(N)3 & ZAC(N)5 ARRAYS, THIS REQUIRES TO 
!       INCREASE DIMENSION OF ZAC(N) TO 0:KLEV

IF ( LRTRUEBBC ) THEN

  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZAL      =PALB   (JLON)
      ZALP     =PALBDIR(JLON)
      ZTDS1    =1._JPRB/(1._JPRB-ZAL*ZTU6(JLON,KLEV))
      ZFPC_TRUE=ZFPCUN(JLON,KLEV)+(1._JPRB-PCLCT(JLON))*&
       & (PFRSODIRS_C(JLON)-PFRSODIRS_CUN(JLON))
      ZFMC(JLON,KLEV)=ZTDS1*(ZAL*(ZFDC(JLON,KLEV)+ZFPC(JLON,KLEV)-&
       & ZFPC_TRUE)+ZALP*ZFPC_TRUE)
      IF (LDNUMX) THEN
        ZTUS1=ZTDS1*ZAL*ZTU7(JLON,KLEV)
        ZTDS2=ZAL*ZTU8(JLON,KLEV)
        ZTDS3=1._JPRB/(1._JPRB-ZAL*ZTU9(JLON,KLEV)-ZTDS2*ZTUS1)
        ZFMN(JLON,KLEV)=ZTDS3*(ZAL*(ZFDN(JLON,KLEV)+ZFPN(JLON,KLEV)-&
         & ZFPNUN(JLON,KLEV))+ZTDS2*ZFMC(JLON,KLEV)+ZALP*ZFPNUN(JLON,KLEV))
        ZFMC(JLON,KLEV)=ZFMC(JLON,KLEV)+ZTUS1*ZFMN(JLON,KLEV)
      ENDIF
    ENDDO
  ENDDO

ELSE

  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      ZAL =PALB   (JLON)
      ZALP=PALBDIR(JLON)
      ZTDS1=1._JPRB/(1._JPRB-ZAL*ZTU6(JLON,KLEV))
      ZFMC(JLON,KLEV)=ZTDS1*(ZAL*ZFDC(JLON,KLEV)+ZALP*ZFPC(JLON,KLEV))
      IF (LDNUMX) THEN
        ZTUS1=ZTDS1*ZAL*ZTU7(JLON,KLEV)
        ZTDS2=ZAL*ZTU8(JLON,KLEV)
        ZTDS3=1._JPRB/(1._JPRB-ZAL*ZTU9(JLON,KLEV)-ZTDS2*ZTUS1)
        ZFMN(JLON,KLEV)=ZTDS3*(ZAL*ZFDN(JLON,KLEV)+&
         & ZTDS2*ZFMC(JLON,KLEV)+ZALP*ZFPN(JLON,KLEV))
        ZFMC(JLON,KLEV)=ZFMC(JLON,KLEV)+ZTUS1*ZFMN(JLON,KLEV)
      ENDIF
    ENDDO
  ENDDO

ENDIF

! back-substitution layer by layer
!-------------------------------------------------------------------------------

!cdir unroll=8
DO JLEV=KLEV,KTDIA,-1
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)

      IF (LDNUMX) THEN
        ZFDN(JLON,JLEV)=ZFDN(JLON,JLEV)+ZTU8(JLON,JLEV)&
         & *ZFMC(JLON,JLEV)+ZTU9(JLON,JLEV)*ZFMN(JLON,JLEV)
        ZFDC(JLON,JLEV)=ZFDC(JLON,JLEV)+ZTU6(JLON,JLEV)&
         & *ZFMC(JLON,JLEV)+ZTU7(JLON,JLEV)*ZFMN(JLON,JLEV)
        ZFMN(JLON,JLEV-1)=ZFMN(JLON,JLEV-1)+ZTU4(JLON,JLEV)&
         & *ZFMC(JLON,JLEV)+ZTU5(JLON,JLEV)*ZFMN(JLON,JLEV)
        ZFMC(JLON,JLEV-1)=ZFMC(JLON,JLEV-1)+ZTU2(JLON,JLEV)&
         & *ZFMC(JLON,JLEV)+ZTU3(JLON,JLEV)*ZFMN(JLON,JLEV)&
         & +ZTU1(JLON,JLEV)*ZFMN(JLON,JLEV-1)
      ELSE
        ZFDC(JLON,JLEV)=ZFDC(JLON,JLEV)+ZTU6(JLON,JLEV)&
         & *ZFMC(JLON,JLEV)
        ZFMC(JLON,JLEV-1)=ZFMC(JLON,JLEV-1)+ZTU2(JLON,JLEV)&
         & *ZFMC(JLON,JLEV)
      ENDIF

    ENDDO
  ENDDO
ENDDO

! initializing fluxes to zero for security in the night-time case
!-------------------------------------------------------------------------------

PFRSO       (:,:)=0._JPRB
PFRSODIFS     (:)=0._JPRB
PFRSODIRS     (:)=0._JPRB
PFRSODIRS_UN  (:)=0._JPRB
PFRSODIRS_TRUE(:)=0._JPRB

! evaluation of fluxes
!-------------------------------------------------------------------------------

! net flux
DO JLEV=KTDIA-1,KLEV
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PFRSO(JLON,JLEV)=ZFPC(JLON,JLEV)+ZFDC(JLON,JLEV)-ZFMC(JLON,JLEV)
      IF (LDNUMX) THEN
        PFRSO(JLON,JLEV)=PFRSO(JLON,JLEV)+ZFPN(JLON,JLEV)&
         & +ZFDN(JLON,JLEV)-ZFMN(JLON,JLEV)
      ENDIF
    ENDDO
  ENDDO
ENDDO

! no net flux divergence above layer KTDIA
DO JLEV=0,KTDIA-2
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PFRSO(JLON,JLEV)=PFRSO(JLON,KTDIA-1)
    ENDDO
  ENDDO
ENDDO

! direct flux (delta-unscaled and true)
DO JN=1,KAUCR
  DO JLON=KIIDIA(JN),KIFDIA(JN)
    PFRSODIRS_UN(JLON)=ZFPCUN(JLON,KLEV)
    IF (LDNUMX) THEN
      PFRSODIRS_UN(JLON)=PFRSODIRS_UN(JLON)+ZFPNUN(JLON,KLEV)
    ENDIF
    PFRSODIRS_TRUE(JLON)=PFRSODIRS_UN(JLON)+(1._JPRB-PCLCT(JLON))* &
     & (PFRSODIRS_C(JLON)-PFRSODIRS_CUN(JLON))
  ENDDO
ENDDO

! direct and downward diffuse fluxes
DO JN=1,KAUCR
  DO JLON=KIIDIA(JN),KIFDIA(JN)
    PFRSODIFS(JLON)=ZFDC(JLON,KLEV)
    PFRSODIRS(JLON)=ZFPC(JLON,KLEV)
    IF (LDNUMX) THEN
      PFRSODIFS(JLON)=PFRSODIFS(JLON)+ZFDN(JLON,KLEV)
      PFRSODIRS(JLON)=PFRSODIRS(JLON)+ZFPN(JLON,KLEV)
    ENDIF
  ENDDO
ENDDO
IF ( LRTRUEDIR ) THEN
  DO JN=1,KAUCR
    DO JLON=KIIDIA(JN),KIFDIA(JN)
      PFRSODIFS(JLON)=PFRSODIFS(JLON)+PFRSODIRS(JLON)-PFRSODIRS_TRUE(JLON)
      PFRSODIRS(JLON)=                                PFRSODIRS_TRUE(JLON)
    ENDDO
  ENDDO
ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACRANEB_SOLVS',1,ZHOOK_HANDLE)
END SUBROUTINE ACRANEB_SOLVS

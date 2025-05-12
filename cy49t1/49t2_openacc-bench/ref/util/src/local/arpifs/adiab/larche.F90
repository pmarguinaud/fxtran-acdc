SUBROUTINE LARCHE(YDTCCO,YDTSCO,KPROMA,KST,KEND,KFLEV,&
 & KSTTYP,PDSTRET,PC2M1,PC2P1,PI,PDEPI,&
 & PLOCEN,PMUCEN,&
 & PSCO,KROT,PCCO,PRCOLON,PRSILON,PGECLO,PGEMU,PGESLO,PGSQM2)  

!$ACDC singlecolumn  --process-pointers


!**** *LARCHE  -  semi-LAgrangian scheme:
!                 Research of the Coordinates (of the medium or origin point).

!     Purpose.
!     --------
!       Computes the longitude and latitude of the interpolation
!       point from its cartesian coordinates.
!       Then computes the vector displacement matrix
!                        I po pq I
!                        I       I
!                        I-pq po I
!       from the interpolation point to the grid point.

!**   Interface.
!     ----------
!        *CALL* *LARCHE(...)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KPROMA   - horizontal dimension.
!          KST   - first element of arrays where computations are performed.
!          KEND    - depth of work.
!          KFLEV    - vertical dimension.
!          KSTTYP   - 1: Not tilted pole;  2: Tilted pole.
!          PDSTRET  - 2*c (where c is the stretching factor).
!          PC2M1    - c*c-1.
!          PC2P1    - c*c+1.
!          PI       - number PI
!          PDEPI    - 2 * PI
!          PLOCEN   - geographic longitude of the stretching pole.
!          PMUCEN   - sinus of the geographic latitude of the stretching pole.
!          PSCO     - information about geographic position of interpol. point.
!          KROT     - KROT=1: computation of the elements po and pq
!                     of the wind displacement matrix.
!                     KROT=0: no computation.

!        OUTPUT:
!          PCCO     - information about comput. space position of interpol. point.

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
!        K. YESSAD, after the subroutine LAGINT0 written by Maurice IMBARD
!        Alain CRAPLET and Michel ROCHAS  METEO FRANCE/EERM/CRMD

!     Modifications.
!     --------------
!        Original : FEBRUARY 1992.
!        J. HAGUE : JANUARY  2003.   MASS Vector Functions       
!        D.SALMOND : JUNE  2003.    Remove vdiv for adjoint test
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        K.Yessad : Aug 2005 : Cleaning
!        K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!        K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!        G.Mozdzynski (Feb 2011): OOPS cleaning, use of derived types TGSGEOM and TCSGEOM
!        T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!        R. El Khatib 02-Sep-2014 Optimization :
!            N_VMASS < 0 used as an option where non-vectorizing ACOS and ASIN
!            are isolated in a specific loop
!        F. Vana: May 2018 : consistent and optimized code through the NL/TL/AD models
!                            for KSTTYP=1 & .NOT.LLSTR
!     ------------------------------------------------------------------

USE PARKIND1    , ONLY : JPIM, JPRB, JPRD
USE YOMHOOK     , ONLY : LHOOK, DR_HOOK, JPHOOK
USE INTDYNSL_MOD, ONLY : TSCO,TCCO

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TCCO)        ,INTENT(IN)    :: YDTCCO
TYPE(TSCO)        ,INTENT(IN)    :: YDTSCO
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTTYP 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDSTRET
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC2M1 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC2P1
REAL(KIND=JPRB)   ,INTENT(IN)    :: PI 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDEPI 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLOCEN
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMUCEN
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSCO(KPROMA,KFLEV,YDTSCO%NDIM)
INTEGER(KIND=JPIM),INTENT(IN)    :: KROT 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCCO(KPROMA,KFLEV,YDTCCO%NDIM)
REAL(KIND=JPRD)   ,INTENT(IN)    :: PRCOLON(KPROMA)
REAL(KIND=JPRD)   ,INTENT(IN)    :: PRSILON(KPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGECLO(KPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEMU(KPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGESLO(KPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGSQM2(KPROMA)


!     ------------------------------------------------------------------

LOGICAL :: LLSTR

!Local copy of fields in high precision:
REAL(KIND=JPRD)  :: ZPDSTRET
REAL(KIND=JPRD)  :: ZPC2M1 
REAL(KIND=JPRD)  :: ZPC2P1
REAL(KIND=JPRD)  :: ZPI 
REAL(KIND=JPRD)  :: ZPDEPI 
REAL(KIND=JPRD)  :: ZPSCO(KPROMA,KFLEV,YDTSCO%NDIM)

REAL(KIND=JPRD) :: ZTMP1(KPROMA)
REAL(KIND=JPRD) :: ZTMP2(KPROMA)
REAL(KIND=JPRD) :: ZTMP4(KPROMA)
REAL(KIND=JPRD) :: ZTMP5(KPROMA)
REAL(KIND=JPRD) :: ZTMP6(KPROMA)
REAL(KIND=JPRD) :: ZTMP7(KPROMA)
REAL(KIND=JPRD) :: ZTMP8(KPROMA)
REAL(KIND=JPRD) :: ZTMP9(KPROMA)
REAL(KIND=JPRD) :: ZZA  (KPROMA)

INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRD) :: Z1ST, Z1STT, ZCA, ZCOSCO, ZCOSL2, ZCOSLA,&
 & ZCOSLR, ZDECO, ZDI, ZEPS, ZEPS1, ZFACT1, ZFACT2, ZMU, ZP, &
 & ZPCLAP, ZPCLOP, ZPSLAP, ZPSLOP, ZQ, ZSA, &
 & ZSINCO, ZSINLA, ZXX, ZYY, ZZP, ZZW, &
 & ZZX, ZZY, ZZZ, ZANGLE, ZPIH, ZPIH52

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LARCHE',0,ZHOOK_HANDLE)

! Transfer SP input variables to DP local vars
ZPDSTRET=PDSTRET
ZPC2M1=PC2M1 
ZPC2P1=PC2P1
ZPI=PI
ZPDEPI=PDEPI
ZPSCO(KST:KEND,1:KFLEV,1:YDTSCO%NDIM)=PSCO(KST:KEND,1:KFLEV,1:YDTSCO%NDIM)

!     ------------------------------------------------------------------

!*       1.    COMPUTATION OF LAT LON OF THE INTERPOLATION POINT.
!              IF KROT=1 COMPUTATION OF THE WIND DISPLACEMENT MATRIX
!              FROM THE INTERPOLATION POINT TO THE FINAL POINT.
!              ( T FOR LATITUDE THETA, L FOR LONGITUDE LAMBDA).
!              PCCO(.,.,YDTCCO%M_RQX)= ( 1 / (1+cos(PHI)) )
!                  *( cos(TG)*cos(T) + (1+sin(TG)*sin(T))*cos(L-LG) )
!              PCCO(.,.,YDTCCO%M_RQY)= (-1 / (1+cos(PHI)) )
!                  *( sin(TG)+sin(T) ) * sin(L-LG)
!     ------------------------------------------------------------------

ZEPS=100._JPRD*EPSILON(ZEPS)
ZEPS1=100.0_JPRD*TINY(1.0_JPRD)
LLSTR=(ABS(0.5_JPRD*PDSTRET-1.0_JPRD)>ZEPS1)
ZANGLE = SQRT(0.5_JPRD)
ZPIH=0.5_JPRD*PI
ZPIH52=2.5_JPRD*PI  ! PDEPI+ZPIH


IF(KSTTYP == 1 .AND.(.NOT.LLSTR)) THEN  ! Cheap and simple code for ECMWF

!   ---- standard vector code
  DO JLEV=1,KFLEV
!DIR$ NOFUSION
!DIR$ PREFERVECTOR
    DO JROF=KST,KEND
      ZCOSLR=SQRT(ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)*ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)&
       &         +ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)*ZPSCO(JROF,JLEV,YDTSCO%M_SINCO))  
      ZCOSLR=MAX(ZEPS,ZCOSLR)
      ZCOSCO= ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)
      ZSINCO= ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)
      ZCOSLA= ZCOSLR
      ZTMP7(JROF)=ZCOSLA
      ZXX   = PRCOLON(JROF)*ZCOSCO-PRSILON(JROF)*ZSINCO
      ZYY   = PRSILON(JROF)*ZCOSCO+PRCOLON(JROF)*ZSINCO
      ZTMP5(JROF)   = SIGN(1.0_JPRD,ZYY)
      ZTMP8(JROF)   = SIGN(1.0_JPRD,ZXX)
      ZTMP6(JROF)   = MAX(-1.0_JPRD,MIN(1.0_JPRD,ZPSCO(JROF,JLEV,YDTSCO%M_SINLA)))
      ZTMP4(JROF)   = MAX(-1.0_JPRD,MIN(1.0_JPRD,ZXX/ZCOSLA))
      ZZA  (JROF)   = MAX(-1.0_JPRD,MIN(1.0_JPRD,ZYY/ZCOSLA))
    ENDDO
!DIR$ PREFERVECTOR
    DO JROF=KST,KEND
      ZTMP6(JROF)=ASIN(ZTMP6(JROF))
      ZTMP4(JROF)=ACOS(ZTMP4(JROF))
      ZTMP9(JROF)=ASIN(ZZA(JROF))
    ENDDO
    DO JROF=KST,KEND
      PCCO(JROF,JLEV,YDTCCO%M_RLAT)=ZTMP6(JROF)
      ! Essential form to stay consistent with the linear models
      PCCO(JROF,JLEV,YDTCCO%M_RLON)=MERGE(MOD(PI+ZTMP5(JROF)*(ZTMP4(JROF)-PI),PDEPI),&
       & MOD(ZPIH52+ZTMP8(JROF)*(ZTMP9(JROF)-ZPIH),PDEPI),ABS(ZZA(JROF)) >= ZANGLE )
    ENDDO

    IF (KROT == 1) THEN
!DIR$ PREFERVECTOR
      DO JROF=KST,KEND
        ZDECO =1.0_JPRD/(1.0_JPRD+ZPSCO(JROF,JLEV,YDTSCO%M_COPHI))
        Z1ST  =1.0_JPRD/ZTMP7(JROF)
        PCCO(JROF,JLEV,YDTCCO%M_RQX)=(PGSQM2(JROF)*ZTMP7(JROF)    &
          & +(1.0_JPRD+PGEMU(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA))&
          & *ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)*Z1ST )*ZDECO  
        PCCO(JROF,JLEV,YDTCCO%M_RQY)=-(PGEMU(JROF)+ZPSCO(JROF,JLEV,YDTSCO%M_SINLA))&
          & *(ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)*Z1ST)*ZDECO  
      ENDDO
    ENDIF

  ENDDO

ELSEIF(KSTTYP == 1 .AND. LLSTR) THEN

!        POLE STRETCHING, NO POLE TILTING.

    DO JLEV=1,KFLEV

      DO JROF=KST,KEND
        ZCOSLR=SQRT(ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)*ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)&
         & +ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)*ZPSCO(JROF,JLEV,YDTSCO%M_SINCO))  
        ZTMP5(JROF)=MAX(ZEPS,ZCOSLR)
        ZFACT1 = 1.0_JPRD/(ZPC2P1-ZPC2M1*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA))
        ZFACT2 = ZPDSTRET*ZFACT1
        ZCOSCO= ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)*ZFACT2
        ZSINCO= ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)*ZFACT2
        ZCOSLA= ZTMP5(JROF)*ZFACT2
        ZSINLA= (ZPC2P1*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA)-ZPC2M1)*ZFACT1
        ZXX   = PRCOLON(JROF)*ZCOSCO-PRSILON(JROF)*ZSINCO
        ZTMP4(JROF) = PRSILON(JROF)*ZCOSCO+PRCOLON(JROF)*ZSINCO
        ZTMP1(JROF) = MAX(-1.0_JPRD,MIN(1.0_JPRD,ZSINLA))
        ZTMP2(JROF) = MAX(-1.0_JPRD,MIN(1.0_JPRD,ZXX/ZCOSLA))
      ENDDO

      DO JROF=KST,KEND
        PCCO(JROF,JLEV,YDTCCO%M_RLAT)=ASIN(ZTMP1(JROF))
        PCCO(JROF,JLEV,YDTCCO%M_RLON)=ZPI+SIGN(1.0_JPRD,ZTMP4(JROF))*(ACOS(ZTMP2(JROF))-ZPI)
      ENDDO
!     Even mod may not vectorize - let a chance for the compiler to fuse loops
      DO JROF=KST,KEND
        PCCO(JROF,JLEV,YDTCCO%M_RLON)=MOD(PCCO(JROF,JLEV,YDTCCO%M_RLON),ZPDEPI)
      ENDDO

      IF (KROT == 1) THEN
        DO JROF=KST,KEND
          ZDECO =1.0_JPRD/(1.0_JPRD+ZPSCO(JROF,JLEV,YDTSCO%M_COPHI))
          Z1ST  =1.0_JPRD/ZTMP5(JROF)
          PCCO(JROF,JLEV,YDTCCO%M_RQX)=(PGSQM2(JROF)*ZTMP5(JROF)&
           & +(1.0_JPRD+PGEMU(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA))&
           & *ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)*Z1ST )*ZDECO  
          PCCO(JROF,JLEV,YDTCCO%M_RQY)=-(PGEMU(JROF)+ZPSCO(JROF,JLEV,YDTSCO%M_SINLA))&
           & *(ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)*Z1ST)*ZDECO  
        ENDDO
      ENDIF

    ENDDO

ELSEIF(KSTTYP == 2) THEN

!        POLE STRETCHING AND TILTING.

  ZPSLAP = PMUCEN
  ZPCLAP = SQRT(1.0_JPRD-REAL(PMUCEN,JPRD)*REAL(PMUCEN,JPRD))
  ZPSLOP = SIN(REAL(PLOCEN,JPRD))
  ZPCLOP = COS(REAL(PLOCEN,JPRD))

!   ---- standard vector code

  DO JLEV=1,KFLEV

    DO JROF=KST,KEND

      ZZY   = PGESLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)+PGECLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)
      ZZX   = PGECLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)-PGESLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)
      ZZZ   = ZZX*ZPCLOP+ZPSLOP*ZZY
      ZZP   = ZPSLAP*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA)+ZPCLAP*ZZZ
      ZDI= 1.0_JPRD/(ZPC2P1-ZPC2M1*ZZP)
      ZYY   = ZPDSTRET*ZDI*(-ZPCLOP*ZZY+ZPSLOP*ZZX)
      ZXX   = ZPDSTRET*ZDI*(ZPCLAP*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA)-ZPSLAP*ZZZ)
      ZCOSLA= SQRT(ZXX*ZXX+ZYY*ZYY)
      ZCOSLA= MAX(ZEPS,ZCOSLA)
      ZSINLA=-(ZPC2M1-ZPC2P1*ZZP)*ZDI
      ZTMP4(JROF)   = ZYY
      ZTMP1(JROF)   = MAX(-1.0_JPRD,MIN(1.0_JPRD,ZSINLA))
      ZTMP2(JROF)   = MAX(-1.0_JPRD,MIN(1.0_JPRD,ZXX/ZCOSLA))

    ENDDO

    DO JROF=KST,KEND
      PCCO(JROF,JLEV,YDTCCO%M_RLAT)=ASIN(ZTMP1(JROF))
      PCCO(JROF,JLEV,YDTCCO%M_RLON)=ZPI+SIGN(1.0_JPRD,ZTMP4(JROF))*(ACOS(ZTMP2(JROF))-ZPI)
    ENDDO
!   Even mod may not vectorize - let a chance for the compiler to fuse loops
    DO JROF=KST,KEND
      PCCO(JROF,JLEV,YDTCCO%M_RLON)=MOD(PCCO(JROF,JLEV,YDTCCO%M_RLON),ZPDEPI)
    ENDDO

    IF (KROT == 1) THEN
      DO JROF=KST,KEND

      ZZY   = PGESLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)+PGECLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)
      ZZX   = PGECLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)-PGESLO(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)
      ZCOSL2= ZZY*ZZY+ZZX*ZZX
      ZCOSLR= SQRT(ZCOSL2)
      ZCOSLR= MAX(ZEPS,ZCOSLR)
      ZZZ   = ZZX*ZPCLOP+ZPSLOP*ZZY
      ZZP   = ZPSLAP*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA)+ZPCLAP*ZZZ
      ZDI= 1.0_JPRD/(ZPC2P1-ZPC2M1*ZZP)
      ZYY   = ZPDSTRET*ZDI*(-ZPCLOP*ZZY+ZPSLOP*ZZX)
      ZXX   = ZPDSTRET*ZDI*(ZPCLAP*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA)-ZPSLAP*ZZZ)
      ZCOSLA= SQRT(ZXX*ZXX+ZYY*ZYY)
      ZCOSLA= MAX(ZEPS,ZCOSLA)

      ZDECO =1.0_JPRD/(1.0_JPRD+ZPSCO(JROF,JLEV,YDTSCO%M_COPHI))
      Z1ST  =1.0_JPRD/ZCOSLR
      Z1STT =Z1ST/ZCOSLA

      ZP = (PGSQM2(JROF)*ZCOSLR&
       & +(1.0_JPRD+PGEMU(JROF)*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA))&
       & *ZPSCO(JROF,JLEV,YDTSCO%M_COSCO)*Z1ST )*ZDECO
      ZQ =-(PGEMU(JROF)+ZPSCO(JROF,JLEV,YDTSCO%M_SINLA))*(ZPSCO(JROF,JLEV,YDTSCO%M_SINCO)*Z1ST )*ZDECO
      ZCA=ZPDSTRET*ZDI*(ZPSLAP*ZCOSL2-ZPCLAP*ZPSCO(JROF,JLEV,YDTSCO%M_SINLA)*ZZZ)*Z1STT
      ZSA=-ZYY*ZPCLAP*Z1STT
      PCCO(JROF,JLEV,YDTCCO%M_RQX)=ZP*ZCA+ZQ*ZSA
      PCCO(JROF,JLEV,YDTCCO%M_RQY)=ZQ*ZCA-ZP*ZSA

      ENDDO
    ENDIF

  ENDDO

ENDIF

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LARCHE',1,ZHOOK_HANDLE)

END SUBROUTINE LARCHE

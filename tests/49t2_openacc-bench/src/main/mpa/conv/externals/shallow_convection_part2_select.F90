SUBROUTINE SHALLOW_CONVECTION_PART2_SELECT &
                                   (CVP_SHAL, CVPEXT, CST, D, NSV,   &
                                    CONVPAR, KICE, OSETTADJ, PTADJS, &
                                    PPABST, PZZ, PTT, PRVT, PRCT,    &
                                    PRIT, OCH1CONV, KCH1,  PCH1,     &
                                    PRDOCP, PTHT, PSTHV, PSTHES,     &
                                    ISDPL, ISPBL, ISLCL, PSTHLCL,    &
                                    PSTLCL, PSRVLCL, PSWLCL, PSZLCL, &
                                    PSTHVELCL, GTRIG1, PUMF, PTHC,   &
                                    PRVC, PRCC, PRIC, ICTL, IMINCTL, &
                                    PPCH1TEN, KCOUNT)


USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, JPHOOK, DR_HOOK
USE MODD_CONVPAR, ONLY : CONVPAR_T
USE MODD_CONVPAR_SHAL, ONLY : CONVPAR_SHAL
USE MODD_CONVPAREXT, ONLY: CONVPAREXT
USE MODD_CST, ONLY: CST_T
USE MODD_DIMPHYEX, ONLY: DIMPHYEX_T
USE MODD_NSV, ONLY: NSV_T

IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
TYPE(CONVPAR_SHAL)                              ,INTENT(IN)   :: CVP_SHAL
TYPE(CONVPAREXT)                                ,INTENT(IN)   :: CVPEXT
TYPE(CST_T)                                     ,INTENT(IN)   :: CST
TYPE(DIMPHYEX_T)                                ,INTENT(IN)   :: D
TYPE(NSV_T)                                     ,INTENT(IN)   :: NSV
TYPE(CONVPAR_T)                                 ,INTENT(IN)   :: CONVPAR
INTEGER                                         ,INTENT(IN)   :: KICE     ! flag for ice ( 1 = yes,
                                                         !                0 = no ice )
LOGICAL                                         ,INTENT(IN)   :: OSETTADJ ! logical to set convective
                                                         ! adjustment time by user
REAL                                            ,INTENT(IN)   :: PTADJS   ! user defined adjustment time
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(IN)   :: PPABST   ! grid scale pressure at t
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(IN)   :: PZZ      ! height of model layer (m)
                                                       ! tendency (K/s)
                                                       ! they are given a value of
                                                       ! 0 if no convection
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(IN)   :: PTT      ! grid scale temperature at t
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(IN)   :: PRVT     ! grid scale water vapor "
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(IN)   :: PRCT     ! grid scale r_c  "
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(IN)   :: PRIT     ! grid scale r_i "
                                                         ! velocity (m/s)
!
LOGICAL                                         ,INTENT(IN)   :: OCH1CONV ! include tracer transport
INTEGER                                         ,INTENT(IN)   :: KCH1     ! number of species
REAL               ,DIMENSION(D%NIT,D%NKT,KCH1) ,INTENT(IN)   :: PCH1     ! grid scale chemical species

REAL                                            ,INTENT(IN)   :: PRDOCP  ! R_d/C_p
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(IN)   :: PTHT,PSTHV,PSTHES  ! grid scale theta, theta_v
INTEGER            ,DIMENSION(D%NIT)            ,INTENT(IN)   :: ISDPL   ! index for parcel departure level
INTEGER            ,DIMENSION(D%NIT)            ,INTENT(IN)   :: ISPBL   ! index for source layer top
INTEGER            ,DIMENSION(D%NIT)            ,INTENT(IN)   :: ISLCL   ! index for lifting condensation level
REAL               ,DIMENSION(D%NIT)            ,INTENT(IN)   :: PSTHLCL ! updraft theta at LCL/L
REAL               ,DIMENSION(D%NIT)            ,INTENT(IN)   :: PSTLCL  ! updraft temp. at LCL
REAL               ,DIMENSION(D%NIT)            ,INTENT(IN)   :: PSRVLCL ! updraft rv at LCL
REAL               ,DIMENSION(D%NIT)            ,INTENT(IN)   :: PSWLCL  ! updraft w at LCL
REAL               ,DIMENSION(D%NIT)            ,INTENT(IN)   :: PSZLCL  ! LCL height
REAL               ,DIMENSION(D%NIT)            ,INTENT(IN)   :: PSTHVELCL! envir. theta_v at LCL
LOGICAL            ,DIMENSION(D%NIT)            ,INTENT(IN)   :: GTRIG1  ! logical mask for convection
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(OUT)  :: PUMF    ! updraft mass flux (kg/s)
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(OUT)  :: PTHC    ! conv. adj. grid scale theta
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(OUT)  :: PRVC    ! conv. adj. grid scale r_w
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(OUT)  :: PRCC    ! conv. adj. grid scale r_c
REAL               ,DIMENSION(D%NIT,D%NKT)      ,INTENT(OUT)  :: PRIC    ! conv. adj. grid scale r_i
INTEGER            ,DIMENSION(D%NIT)            ,INTENT(OUT)  :: ICTL    ! index for cloud top level
INTEGER            ,DIMENSION(D%NIT)            ,INTENT(OUT)  :: IMINCTL ! min between index for cloud top level
                                                        ! and lifting condensation level
REAL               ,DIMENSION(D%NIT,D%NKT,KCH1) ,INTENT(OUT)  :: PPCH1TEN
INTEGER                                         ,INTENT(IN)   :: KCOUNT
!
END SUBROUTINE SHALLOW_CONVECTION_PART2_SELECT


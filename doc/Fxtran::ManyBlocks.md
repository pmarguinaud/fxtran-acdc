# NAME

[Fxtran::ManyBlocks](../lib/Fxtran/ManyBlocks.pm)

# DESCRIPTION

This module provides methods for transforming `NPROMA` vector routines (processing a single block) into 
routines processing several `NPROMA` blocks. Dimensions of all `NPROMA` arguments are extended 
with a block dimension. Array dummy arguments are dimensioned with implicit shapes, because 
actual arguments may be non-contiguous array sections (this is because of the extra block dimension).

The result of the transformation is a routine executing on the CPU but spawning OpenACC kernels. 
Each kernel is actually a single-column kernel, and may invoke single-column routines.

On entry of the result routine, all constants and `NPROMA` arrays are considered to be
present on the device.

Kernel boundaries can be delimited by the user, using `!$ACDC PARALLEL` directives, but
the option `--max-statements-per-parallel`, when non zero will detect parallel loops and
will try to merge them so that the number of statements per kernel does not exceed the limit
provided by the option.

Using appropriate options, it is possible to use a stack object to allocate `NPROMA` temporaries
but it is not mandatory.

Eventually, note that the option `--array-slice-to-address` transforms array section arguments
in kernels into array element, so that PGI does not copy full array descriptors. This is possible
because all dimensions but the last one (the one on the block dimension) are contiguous.

# EXAMPLE

For instance, the following routine:

    SUBROUTINE CUMASTRN &
      & (PPLDARE, PPLRG,    YDTHF,   YDCST, YDML_PHY_SLIN,     YDML_PHY_EC, YGFL,&
      & YDCHEM,   YDSPP_CONFIG, YDPERTPAR, &
      ...
      & PENTH,    PMFLXR,   PMFLXS,   PRAIN,    PLRAIN,    PRSUD,&
      & PMFU,     PMFD,     PLGLAC, &
      & PMFUDE_RATE,        PMFDDE_RATE,    PCAPE,   PWU, PWMEAN, PVDISCU, PDISS, &
      & KTRAC,    PCEN,     PTENC,    PSCAV, PSCAV0 )  

    !$ACDC manyblocks --array-slice-to-address

    IMPLICIT NONE

    REAL(KIND=JPRB)   ,INTENT(IN)    :: PPLDARE                      ! Constants  scalar arguments
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PPLRG                        ! Constants  scalar arguments
    TYPE(TTHF)                         ,INTENT(IN):: YDTHF           ! Constants  scalar arguments
    TYPE(TCST)                         ,INTENT(IN):: YDCST           ! Constants  scalar arguments
    TYPE(MODEL_PHYSICS_ECMWF_TYPE)     ,INTENT(IN):: YDML_PHY_EC     ! Constants  scalar arguments
    TYPE(TCHEM)       ,INTENT(IN)    :: YDCHEM                       ! Constants  scalar arguments
    TYPE(TSPP_CONFIG) ,INTENT(IN)    :: YDSPP_CONFIG                 ! Constants  scalar arguments
    TYPE(TPERTPAR)    ,INTENT(IN)    :: YDPERTPAR                    ! Constants  scalar arguments
    TYPE(MODEL_PHYSICS_SIMPLINEAR_TYPE),INTENT(IN):: YDML_PHY_SLIN   ! Constants  scalar arguments
    TYPE(TYPE_GFLD)   ,INTENT(IN)    :: YGFL                         ! Constants  scalar arguments
    INTEGER(KIND=JPIM),INTENT(IN)    :: KLON                         ! Constants  scalar arguments
    INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV                         ! Constants  scalar arguments
    INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA                        ! Constants  scalar arguments
    INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA                        ! Constants  scalar arguments
    INTEGER(KIND=JPIM),INTENT(IN)    :: KTRAC                        ! Constants  scalar arguments
    LOGICAL           ,INTENT(IN)    :: LDMCAPEA                     ! Constants  scalar arguments
    LOGICAL           ,INTENT(IN)    :: LDLAND(KLON)
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSPHY
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PLCRIT_AER(KLON,KLEV)    ! NPROMA array arguments
    ...            !
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PQHFL(KLON,KLEV+1)       ! NPROMA array arguments
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PAHFS(KLON,KLEV+1)       ! NPROMA array arguments
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PAP(KLON,KLEV)           ! NPROMA array arguments
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPH(KLON,KLEV+1)        ! NPROMA array arguments
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEO(KLON,KLEV)          ! NPROMA array arguments
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOH(KLON,KLEV+1)       ! NPROMA array arguments
    REAL(KIND=JPRB)   ,INTENT(IN)    :: PGAW(KLON)               ! NPROMA array arguments

    ...

    REAL(KIND=JPRB) ::         ZTENH(KLON,KLEV),       ZQENH(KLON,KLEV),&    ! NPROMA temporaries
     & ZTENH2(KLON,KLEV),      ZQENH2(KLON,KLEV),      ZQSENH(KLON,KLEV),&   ! NPROMA temporaries
     & ZTD(KLON,KLEV),         ZQD(KLON,KLEV)                                ! NPROMA temporaries

    ...

    DO JK=NJKT2,KLEV                                                                  ! Calculations
    !DIR$ LOOP_INFO EST_TRIPS(16)                                                     ! Calculations
      DO JL=KIDIA,KFDIA                                                               ! Calculations
        IF(LDCUM(JL).AND.JK >= KCBOT(JL)) THEN                                        ! Calculations
          ZDZ=(PAPH(JL,JK+1)-PAPH(JL,JK))                                             ! Calculations
          ZDHPBL(JL)=ZDHPBL(JL)+(RLVTT*PTENQ(JL,JK)+RCPD*PTENT(JL,JK))*ZDZ            ! Calculations
        ZCAPPBL(JL)=ZCAPPBL(JL)+(PTENT(JL,JK)+RETV*PTEN(JL,JK)*PTENQ(JL,JK))*ZDZ      ! Calculations
        ENDIF                                                                         ! Calculations
      ENDDO                                                                           ! Calculations
    ENDDO                                                                             ! Calculations

is transformed into:

    SUBROUTINE CUMASTRN_MANYBLOCKS (PPLDARE, PPLRG, YDTHF, YDCST, YDML_PHY_SLIN, YDML_PHY_EC, YGFL, YDCHEM&
    &, YDSPP_CONFIG, YDPERTPAR, KIDIA, KFDIA, KLON, KLEV, PDX, LDTDKMF, LDMCAPEA, LDLAND, PTSPHY, PTEN&
    ...
    &, PMFLXS, PRAIN, PLRAIN, PRSUD, PMFU, PMFD, PLGLAC, PMFUDE_RATE, PMFDDE_RATE, PCAPE, PWU, PWMEAN&
    &, PVDISCU, PDISS, KTRAC, PCEN, PTENC, PSCAV, PSCAV0, LDACC, KGPBLKS) ! LDACC for running on device, KGPBLKS is the number of blocks

    REAL (KIND=JPRB), INTENT (IN)::PPLDARE
    REAL (KIND=JPRB), INTENT (IN)::PPLRG
    TYPE (TTHF), INTENT (IN)::YDTHF
    TYPE (TCST), INTENT (IN)::YDCST
    TYPE (MODEL_PHYSICS_ECMWF_TYPE), INTENT (IN)::YDML_PHY_EC
    TYPE (TCHEM), INTENT (IN)::YDCHEM
    TYPE (TSPP_CONFIG), INTENT (IN)::YDSPP_CONFIG
    TYPE (TPERTPAR), INTENT (IN)::YDPERTPAR
    TYPE (MODEL_PHYSICS_SIMPLINEAR_TYPE), INTENT (IN)::YDML_PHY_SLIN
    TYPE (TYPE_GFLD), INTENT (IN)::YGFL
    INTEGER (KIND=JPIM), INTENT (IN)::KLON
    INTEGER (KIND=JPIM), INTENT (IN)::KLEV
    INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
    INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
    INTEGER (KIND=JPIM), INTENT (IN)::KTRAC
    LOGICAL, INTENT (IN)::LDMCAPEA
    LOGICAL, INTENT (IN)::LDLAND (:, :) ! (KLON)
    REAL (KIND=JPRB), INTENT (IN)::PTSPHY
    REAL (KIND=JPRB), INTENT (IN)::PLCRIT_AER (:, :, :) ! (KLON, KLEV)  ! Add an extra block dimension, use implicit shapes
    REAL (KIND=JPRB), INTENT (INOUT)::PTEN (:, :, :) ! (KLON, KLEV)     ! Add an extra block dimension, use implicit shapes
    REAL (KIND=JPRB), INTENT (INOUT)::PQEN (:, :, :) ! (KLON, KLEV)     ! Add an extra block dimension, use implicit shapes
    REAL (KIND=JPRB), INTENT (IN)::PUEN (:, :, :) ! (KLON, KLEV)        ! Add an extra block dimension, use implicit shapes

    !$ACC DATA &
    !$ACC&CREATE (ICTOP0, IDPL, IDTOP, ILAB, LLDCUM, LLDDRAF, LLDDRAF3, LLO2, LLSCVFLAG, &      ! Create NPROMA temporaries on the device
    !$ACC&        ZCAPDCYCL, ZCAPDCYCLLD, ZCAPDCYCLSE, ZCAPE, ZCAPE2, ZCAPPBL, ZDHPBL, &        ! Create NPROMA temporaries on the device
    !$ACC&        ZDMFDE, ZDMFDP, ZDMFDPC, ZDMFEN, ZDMFUP, ZDMFUPC, ZDPMEL, ZDQCV, ZDX, &       ! Create NPROMA temporaries on the device
    ...
    !$ACC&        ZUV2, ZVD, ZVU, ZWUBASE, ZXTAU) &                                             ! Create NPROMA temporaries on the device
    !$ACC&IF (LDACC) &
    !$ACC&PRESENT (KBOTSC, KCBOT, KCBOT_LIG, KCTOP, KCTOP_LIG, KTYPE, LDCUM, LDCUM_LIG, &       ! Constant & NPROMA arrays are on device
    !$ACC&         LDLAND, LDSC, LDSHCV, PAHFS, PAP, PAPH, PCAPE, PCEN, PCUCONVCA, PDISS, &     ! Constant & NPROMA arrays are on device
    ...
    !$ACC&         YDSPP_CONFIG, YDTHF, YGFL)                                                   ! Constant & NPROMA arrays are on device

    ...

    !$ACC PARALLEL LOOP GANG &  ! OpenACC single-column kernel
    !$ACC&IF (LDACC) &          ! OpenACC single-column kernel
    !$ACC&PRIVATE (JBLK) &      ! OpenACC single-column kernel
    !$ACC&VECTOR_LENGTH (KLON)  ! OpenACC single-column kernel
    

    ! Several (3) NPROMA kernels merged into a single one; kernel quoted in original routine is in 
    ! the middle position (loop from NJKT2 to KLEV)

    DO JBLK = 1, KGPBLKS
      
      !$ACC LOOP VECTOR &
      !$ACC&PRIVATE (IKB, ITOPM2, JK, JL, ZDZ, ZPBMPT) 
    
      DO JL = KIDIA, MERGE (KLON, KFDIA, JBLK < KGPBLKS)
    
        ZDHPBL (JL, JBLK)=0.0_JPRB
        IDTOP (JL, JBLK)=0
        ZCAPPBL (JL, JBLK)=0.
        ZKHVFL (JL, JBLK)=(-PAHFS (JL, KLEV+1, JBLK)*ZORCPD-YDCST%RETV*PTEN&
        & (JL, KLEV, JBLK)*PQHFL (JL, KLEV+1, JBLK))/(PPLRG*PPLDARE)
        ZKHFL (JL, JBLK)=(-PAHFS (JL, KLEV+1, JBLK)-YDCST%RLVTT*PQHFL (JL, KLEV+1, JBLK))/(PPLRG*PPLDARE)
    
        DO JK=YDML_PHY_EC%YRECUMF%NJKT2, KLEV
    
          IF (LDCUM (JL, JBLK).AND.JK>=KCBOT (JL, JBLK)) THEN
            ZDZ=(PAPH (JL, JK+1, JBLK)-PAPH (JL, JK, JBLK))
            ZDHPBL (JL, JBLK)=ZDHPBL (JL, JBLK)+(YDCST%RLVTT*PTENQ &
            &(JL, JK, JBLK)+YDCST%RCPD*PTENT (JL, JK, JBLK))*ZDZ
            ZCAPPBL (JL, JBLK)=ZCAPPBL (JL, JBLK)+(PTENT (JL, JK, JBLK)+YDCST&
            &%RETV*PTEN (JL, JK, JBLK)*PTENQ (JL, JK, JBLK))*ZDZ
          ENDIF
         
        ENDDO
    
        IF (LDCUM (JL, JBLK)) THEN
          IKB=KCBOT (JL, JBLK)
          ITOPM2=ICTOP0 (JL, JBLK)
          ZPBMPT=PAPH (JL, IKB, JBLK)-PAPH (JL, ITOPM2, JBLK)
    
          IF (ZPBMPT>=YDML_PHY_EC%YRECUMF%RDEPTHS) THEN
            KTYPE (JL, JBLK)=1
          ELSE
            KTYPE (JL, JBLK)=2
          ENDIF
    
        ELSE 
          KTYPE (JL, JBLK)=0
        ENDIF
      
      ENDDO
    
    ENDDO

# MORE EXAMPLES

See for instance:

- [cubasen.F90](../tests/49t2_openacc-manyblocks-auto/src/main/arpifs/phys_ec/cubasen.F90) 
/
[cubasen\_manyblocks.F90](../tests/49t2_openacc-manyblocks-auto/ref/manyblocks/src/local/arpifs/phys_ec/cubasen_manyblocks.F90).
- [cucalln\_mf.F90](../tests/49t2_openacc-manyblocks-auto/src/main/arpifs/phys_ec/cucalln_mf.F90) 
/
[cucalln\_mf\_manyblocks.F90](../tests/49t2_openacc-manyblocks-auto/ref/manyblocks/src/local/arpifs/phys_ec/cucalln_mf_manyblocks.F90).

# SEE ALSO

[Fxtran::SingleColumn](Fxtran%3A%3ASingleColumn.md), [Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

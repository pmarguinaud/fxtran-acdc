# NAME

[Fxtran::SingleColumn](../lib/Fxtran/SingleColumn.pm)

# DESCRIPTION

This transformation takes an `NPROMA` routine (processing a single `NPROMA` block),
and transform it into a routine processing a single column belonging to an `NPROMA`
block.

This means that threads belonging to the same wrap can be called in parallel on
the same `NPROMA` block, each thread being assigned a single column for 
processing.

Some temporary arrays can be scalarized (ie transformed into scalars).

Temporary memory has to be managed by a special allocator (threads belonging to 
the same wrap have to share temporary `NPROMA` arrays).

Eventually, the result of the transformation (which is a routine) is marked with 
an OpenACC `!$acc routine seq` directive.

# EXAMPLE

The following routine:

    SUBROUTINE ACTKE ( YDCST, YDLDDH, YDMDDH, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV,      &   
    & PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP, PR, PT, PU, PV, PQ, PQICONV,  PQLCONV, PLSCPE, PCD, PCH, PGZ0, &
    & PTS, PQS, PQICE, PQLI, PECT, PPRODTH,  PNLAB, PNLABCVP, PKTROV, PKQROV, PKQLROV, PKUROV, PXTROV,    &   
    & PXUROV, PNBVNO,  PNEBS, PQCS, PNEBS0, PQCS0, PCOEFN, PFECT, PFECTI, PECT1, PTPRDY, PEDR,  YDDDH,    &   
    & YDML_PHY_FORCING)
    
    !$ACDC singlecolumn 
    
    TYPE(TCST)                  ,INTENT(IN)     :: YDCST                          ! constant data
    TYPE(TLDDH)                 ,INTENT(IN)     :: YDLDDH                         ! constant data
    TYPE(TMDDH)                 ,INTENT(IN)     :: YDMDDH                         ! constant data
    TYPE(MODEL_PHYSICS_MF_TYPE) ,INTENT(IN)     :: YDML_PHY_MF                    ! constant data
    INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KIDIA                          ! scalar argument
    INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KFDIA                          ! scalar argument
    INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KLON                           ! scalar argument
    INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KTDIAT                         ! scalar argument
    INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KTDIAN                         ! scalar argument
    INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KLEV                           ! scalar argument
    REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPHI(KLON,0:KLEV)             ! NPROMA data
    REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPHIF(KLON,KLEV)              ! NPROMA data
    ...
    REAL(KIND=JPRB)             ,INTENT(OUT)    :: PECT1(KLON,KLEV)               ! NPROMA data
    REAL(KIND=JPRB)             ,INTENT(OUT)    :: PTPRDY(KLON,KLEV)              ! NPROMA data
    REAL(KIND=JPRB)             ,INTENT(OUT)    :: PEDR(KLON,KLEV)                ! NPROMA data
    TYPE(TYP_DDH)               ,INTENT(INOUT)  :: YDDDH
    TYPE(MODEL_PHYSICS_FORCING_TYPE), INTENT(IN) :: YDML_PHY_FORCING
    
    REAL(KIND=JPRB) :: ZDET(KLON,KLEV)                                            ! temporary NPROMA data
    REAL(KIND=JPRB) :: ZKCLS(KLON), ZECTCLS(KLON)                                 ! temporary NPROMA data
    REAL(KIND=JPRB) :: ZTABHL(KLON,0:KLEV),ZTABFL(KLON,KLEV),ZDIFFAR(KLON,KLEV)   ! temporary NPROMA data
    
    IF (LECTFL) THEN

       CALL FL2HL ( KIDIA, KFDIA, KLON, 1, KLEV,&        ! call to another NPROMA routine
              & PAPRS, PAPRSF, PECT, ZECT, 1)

    ELSE
       DO JLEV = 1, KLEV
          DO JLON = KIDIA,KFDIA
             ZECT(JLON,JLEV)=PECT(JLON,JLEV)             ! calculations
          ENDDO
       ENDDO
    ENDIF

    DO JLEV=KTDIAT,KLEV
      DO JLON=KIDIA,KFDIA
        ZECT(JLON,JLEV) = MAX( ECTMIN, ZECT(JLON,JLEV) ) ! calculations
        ZDELPSG(JLON,JLEV)=PDELP(JLON,JLEV)/RG
      ENDDO
    ENDDO

is transformed into:

    SUBROUTINE ACTKE_OPENACC (YDCST, YDLDDH, YDMDDH, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIAT&
    &, KTDIAN, KLEV, PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP, PR, PT, PU, PV, PQ, PQICONV, PQLCONV&
    &, PLSCPE, PCD, PCH, PGZ0, PTS, PQS, PQICE, PQLI, PECT, PPRODTH, PNLAB, PNLABCVP, PKTROV&
    &, PKQROV, PKQLROV, PKUROV, PXTROV, PXUROV, PNBVNO, PNEBS, PQCS, PNEBS0, PQCS0, PCOEFN&
    &, PFECT, PFECTI, PECT1, PTPRDY, PEDR, YDDDH, YDML_PHY_FORCING, YDSTACK)
    
    !$ACC ROUTINE (ACTKE_OPENACC) SEQ                               ! OpenACC directive
    
    TYPE (TCST), INTENT (IN)::YDCST
    TYPE (TLDDH), INTENT (IN)::YDLDDH
    TYPE (TMDDH), INTENT (IN)::YDMDDH
    TYPE (MODEL_PHYSICS_MF_TYPE), INTENT (IN)::YDML_PHY_MF
    INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
    INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
    INTEGER (KIND=JPIM), INTENT (IN)::KLON
    INTEGER (KIND=JPIM), INTENT (IN)::KTDIAT
    INTEGER (KIND=JPIM), INTENT (IN)::KTDIAN
    INTEGER (KIND=JPIM), INTENT (IN)::KLEV
    REAL (KIND=JPRB), INTENT (IN)::PAPHI (KLON, 0:KLEV)             ! NPROMA arguments
    REAL (KIND=JPRB), INTENT (IN)::PAPHIF (KLON, KLEV)              ! NPROMA arguments
    ...
    REAL (KIND=JPRB), INTENT (OUT)::PTPRDY (KLON, KLEV)             ! NPROMA arguments
    REAL (KIND=JPRB), INTENT (OUT)::PEDR (KLON, KLEV)               ! NPROMA arguments
    
    fxtran_acdc_temp (REAL (KIND=JPRB), ZKCLS, (KLON))              ! temporary NPROMA data
    fxtran_acdc_temp (REAL (KIND=JPRB), ZDIFFAR, (KLON, KLEV))      ! temporary NPROMA data
    fxtran_acdc_temp (REAL (KIND=JPRB), ZTABFL, (KLON, KLEV))       ! temporary NPROMA data
    fxtran_acdc_temp (REAL (KIND=JPRB), ZTABHL, (KLON, 0:KLEV))     ! temporary NPROMA data
    fxtran_acdc_temp (REAL (KIND=JPRB), ZFCORTKE, (KLON, 0:KLEV))   ! temporary NPROMA data
    fxtran_acdc_temp (REAL (KIND=JPRB), ZFDIFF, (KLON, 0:KLEV))     ! temporary NPROMA data
    
    IF (KIND (ZPHI3) == 8) THEN                                     ! allocation of NPROMA temporary data
        fxtran_acdc_alloc8 (ZPHI3)
    ELSEIF (KIND (ZPHI3) == 4) THEN
        fxtran_acdc_alloc4 (ZPHI3)
    ELSE
        STOP 1
    ENDIF
    
    IF (KIND (ZLMECT) == 8) THEN                                    ! allocation of NPROMA temporary data
        fxtran_acdc_alloc8 (ZLMECT)
    ELSEIF (KIND (ZLMECT) == 4) THEN
        fxtran_acdc_alloc4 (ZLMECT)
    ELSE
        STOP 1
    ENDIF
    
    ...

    JLON = KIDIA                                                    ! column index to be processed
    
    IF (YDML_PHY_MF%YRPHY%LECTFL) THEN

      CALL FL2HL_OPENACC (KIDIA, KFDIA, KLON, 1, KLEV, PAPRS, PAPRSF, PECT, ZECT, 1, YDSTACK=YLSTACK)  ! Call to another NPROMA single-column routine
    
    ELSE
    
      DO JLEV=1, KLEV
    
        ZECT (JLON, JLEV)=PECT (JLON, JLEV)                                                             ! Single column calculations
    
      ENDDO
    
    ENDIF
    
    DO JLEV=KTDIAT, KLEV                                                                                ! Single column calculations
    
      ZECT (JLON, JLEV)=MAX (YDML_PHY_MF%YRPHY0%ECTMIN, ZECT (JLON, JLEV))
      ZDELPSG (JLON, JLEV)=PDELP (JLON, JLEV)/YDCST%RG
    
    ENDDO

# MORE EXAMPLES

See for instance:

- [lascaw.F90](../tests/49t2_openacc-bench/src/main/arpifs/interpol/lascaw.F90)
/
[lascaw\_openacc.F90](../tests/49t2_openacc-bench/ref/util/src/local/arpifs/interpol/lascaw_openacc.F90)
- [shallow\_mf.F90](../tests/49t2_openacc-bench/src/main/phyex/turb/shallow_mf.F90)
/
[shallow\_mf\_openacc.F90](../tests/49t2_openacc-bench/ref/util/src/local/phyex/turb/shallow_mf_openacc.F90)

# SEE ALSO

[Fxtran::Stack](Fxtran%3A%3AStack.md), [Fxtran::Loop](Fxtran%3A%3ALoop.md), [Fxtran::ReDim](Fxtran%3A%3AReDim.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022

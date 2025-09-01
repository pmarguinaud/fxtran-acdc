package Fxtran::SingleColumn;

=head1 NAME

Fxtran::SingleColumn

=head1 DESCRIPTION

This transformation takes an C<NPROMA> routine (processing a single C<NPROMA> block),
and transform it into a routine processing a single column belonging to an C<NPROMA>
block.

This means that threads belonging to the same wrap can be called in parallel on
the same C<NPROMA> block, each thread being assigned a single column for 
processing.

Some temporary arrays can be scalarized (ie transformed into scalars).

Temporary memory has to be managed by a special allocator (threads belonging to 
the same wrap have to share temporary C<NPROMA> arrays).

Eventually, the result of the transformation (which is a routine) is marked with 
an OpenACC C<!$acc routine seq> directive.

=head1 EXAMPLE

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

=head1 MORE EXAMPLES

See for instance:

=over 4

=item

L<lascaw.F90|url:../tests/49t2_openacc-bench/src/main/arpifs/interpol/lascaw.F90>
/
L<lascaw_openacc.F90|url:../tests/49t2_openacc-bench/ref/util/src/local/arpifs/interpol/lascaw_openacc.F90>

=item

L<shallow_mf.F90|url:../tests/49t2_openacc-bench/src/main/phyex/turb/shallow_mf.F90>
/
L<shallow_mf_openacc.F90|url:../tests/49t2_openacc-bench/ref/util/src/local/phyex/turb/shallow_mf_openacc.F90>

=back

=head1 SEE ALSO

L<Fxtran::Stack>, L<Fxtran::Loop>, L<Fxtran::ReDim>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

use FileHandle;
use Data::Dumper;
use Getopt::Long;
use File::stat;
use File::Path;
use File::Copy;
use File::Basename;

use strict;

use Fxtran::Common;
use Fxtran;
use Fxtran::Stack;
use Fxtran::Loop;
use Fxtran::ReDim;
use Fxtran::Subroutine;
use Fxtran::Call;
use Fxtran::Canonic;
use Fxtran::DrHook;
use Fxtran::Include;
use Fxtran::Pointer;
use Fxtran::Print;
use Fxtran::Interface;
use Fxtran::Module;


sub arraySliceToAddress
{
  my ($pu, %opts) = @_;

  my $style = $opts{style};
  
  my @nproma = $style->nproma ();
  
  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my %ptr;
  
  my @decl = &F ('./T-decl-stmt[./attribute[string(attribute-N)="INTENT"]]', $dp);
  
  my $last = $decl[-1];
  
  $dp->insertAfter (&t ("\n"), $last);
  
  for my $decl (reverse (@decl))
    {
      my ($en_decl) = &F ('./EN-decl-LT/EN-decl', $decl);
      next unless (my ($as) = &F ('./array-spec', $en_decl));
  
      my @ss = &F ('./shape-spec-LT/shape-spec', $as, 1);
      next unless (grep { $ss[0] eq $_ } @nproma);
  
      my ($n) = &F ('./EN-N/N/n/text()', $en_decl, 1);
      my $stmt = &Fxtran::stmt ($en_decl);
      
      my ($t) = &F ('./_T-spec_', $stmt, 1);
  
      my $temp = &s ("fxtran_acdc_temp ($t, ${n}_PTR, " . $as->textContent . ")");
  
      $dp->insertAfter ($_, $last)
        for ($temp, &t ("\n"));
      
      $ptr{$n}++;
  
      $as->unbindNode ();
  
      my $assoc;
  
      if (&F ('./attribute[string(attribute-N)="OPTIONAL"]', $decl))
        {
          ($assoc) = &fxtran::parse (fragment => << "EOF", fopts => [qw (-construct-tag)]);
IF (PRESENT ($n)) THEN
fxtran_acdc_assoc (${n}_PTR, ${n})
ELSE
fxtran_acdc_nullptr (${n}_PTR)
ENDIF
EOF

          &Fxtran::Canonic::makeCanonicReferences ($assoc);
  
        }
      else
        {
          $assoc = &s ("fxtran_acdc_assoc (${n}_PTR, ${n})");
        }
  
      $ep->insertBefore ($_, $ep->firstChild) 
        for (&t ("\n"), $assoc, &t ("\n"));
  
    }
  
  $dp->insertAfter (&t ("\n"), $last);
  
  for my $expr (&F ('.//named-E', $ep))
    {
      my ($n) = &F ('./N/n/text()', $expr);
      next unless ($ptr{$n->textContent});
  
      my $expr = &Fxtran::expr ($n); 
      $expr = &Fxtran::expr ($expr);
  
      if ($expr && ($expr->nodeName eq 'named-E') 
         && ((&F ('./N', $expr, 1))[0] eq 'PRESENT') 
         && (&F ('./R-LT/function-R', $expr)))
        { 
          next;
        }
  
      $n->setData ($n->textContent . '_PTR');
    }

}


sub addValueAttribute
{
  my $d = shift;

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  my @intent = &F ('./T-decl-stmt'    
                 . '[_T-spec_/intrinsic-T-spec[string(T-N)="REAL" or string(T-N)="INTEGER" or string(T-N)="LOGICAL"]]' # Only REAL/INTEGER/LOGICAL
                 . '[not(.//array-spec)]'                                                                              # Without dimensions
                 . '//attribute[string(intent-spec)="IN"]'                                                             # Only arguments
                 , $dp); 

  for my $intent (@intent)
    {
      for my $x (&t (' '), &n ('<attribute><attribute-N>VALUE</attribute-N></attribute>'), &t (', '))
        {
          $intent->parentNode->insertAfter ($x, $intent);
        }
    }

}

sub processSingleModule
{
  my ($d, %opts) = @_;

  my $find = $opts{find};

  my @pu = &F ('./program-unit', $d);

  for my $pu (@pu)
    {
      &processSingleRoutine ($pu, %opts);
    }

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  if ($opts{'process-interfaces'})
    {
      my @pu = &F ('./interface-construct/program-unit', $dp);
     
      for my $pu (@pu)
        {
          &processSingleInterface ($pu, %opts);
        }
    }

  &Fxtran::Module::addSuffix ($d, $opts{'suffix-singlecolumn'});
}

sub processSingleInterface
{
  my ($d, %opts) = @_;

  my $find = $opts{find};

  my $end = $d->lastChild;

  &Fxtran::ReDim::reDim ($d, style => $opts{style}, 'redim-arguments' => $opts{'redim-arguments'});
  
  if ($opts{'value-attribute'})
    {
      &addValueAttribute ($d);
    }
  
  &Fxtran::Subroutine::addSuffix ($d, $opts{'suffix-singlecolumn'});
  
  $opts{pragma}->insertRoutineSeq ($d);
  
  &Fxtran::Stack::addStack 
  (
    $d, 
    skip => sub { $opts{style}->noComputeRoutine (@_) },
    stack84 => $opts{stack84},
    style => $opts{style},
    'stack-method' => $opts{'stack-method'},
  );

}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  # Process ABORT sections

  for my $abort (&F ('.//abort-section', $pu))
    {    
      $_->unbindNode () for ($abort->childNodes ());
      $abort->appendChild ($_) 
        for (&s ('CALL ABOR1 ("ERROR: WRONG SETTINGS")'), &t ("\n"));
    }    

  # Process SKIP sections

  for my $skip (&F ('.//skip-section', $pu))
    {    
      $skip->unbindNode ();
    }    

  my $find = $opts{find};

  my @pointer;

  unless ($opts{dummy})
    {
     
      @pointer = &Fxtran::Pointer::setPointersDimensions ($pu, 'no-check-pointers-dims' => $opts{'no-check-pointers-dims'})
        if ($opts{'process-pointers'});
      
      &Fxtran::Loop::removeNpromaLoops ($pu, style => $opts{style}, pointer => \@pointer);
      
      &Fxtran::ReDim::reDim ($pu, style => $opts{style}, 'redim-arguments' => $opts{'redim-arguments'});
      
      
      if ($opts{'value-attribute'})
        {
          &addValueAttribute ($pu);
        }

    }

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-singlecolumn'});
  
  unless ($opts{dummy})
    {
      &Fxtran::Call::addSuffix 
      (
        $pu, 
        suffix => $opts{'suffix-singlecolumn-called'}, 
        match => sub { ! $opts{style}->noComputeRoutine (@_) },
        'merge-interfaces' => $opts{'merge-interfaces'},
      );
    }
  
  $opts{pragma}->insertRoutineSeq ($pu);

  &Fxtran::Stack::addStack 
  (
    $pu, 
    skip => sub { $opts{style}->noComputeRoutine (@_) },
    stack84 => $opts{stack84},
    style => $opts{style},
    pointer => \@pointer,
    'stack-method' => $opts{'stack-method'},
  );

  if ($opts{dummy})
    {
      &Fxtran::Interface::intfbBody ($pu->ownerDocument ());
      my ($end) = &F ('./end-subroutine-stmt', $pu);  
      my $ep = &n ('<execution-part/>');
      my $abort = &s ('CALL FXTRAN_ACDC_ABORT ("ERROR : WRONG SETTINGS")');
      $ep->appendChild ($abort);
      $end->parentNode->insertBefore ($_, $end) for ($ep, &t ("\n"));
    }
  else
    {
      &Fxtran::Pointer::handleAssociations ($pu, pointers => \@pointer)
        if ($opts{'process-pointers'});
      
      &Fxtran::DrHook::remove ($pu) unless ($opts{drhook});
      

      unless ($opts{'merge-interfaces'})
        {
          &Fxtran::Include::removeUnusedIncludes ($pu) 
            if ($opts{style}->removeUnusedIncludes ());
        }

      $opts{style}->handleMessages ($pu, %opts);

      &Fxtran::Print::useABOR1_ACC ($pu);
      &Fxtran::Print::changeWRITEintoPRINT ($pu);
      &Fxtran::Print::changePRINT_MSGintoPRINT ($pu);
    }

  &arraySliceToAddress ($pu, %opts)
    if ($opts{'array-slice-to-address'});

}

1;

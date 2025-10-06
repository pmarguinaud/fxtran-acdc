package Fxtran::TopLevel;

=head1 NAME

Fxtran::TopLevel

=head1 DESCRIPTION

The purpose of this module is to transform top-level routines such as F<cpg_drv.F90> so that
generated parallel grid-point routines be called. 

This module does differents things:

=head2 switch

There is a C<--switch> option to the transformation. The following sequence of statements:

  SUBROUTINE CPG_DRV (...)

  !$ACDC toplevel --switch LLPARALLEL

  ...

  LLPARALLEL = .FALSE.

is transformed into:

  LLPARALLEL=FXTRAN_ACDC_LPARALLELMETHOD ('PARALLEL','CPG_DRV')

  ...

so that the user can choose to enable the grid-point parallel routines.

=head2 COPY directive

A block delimited by this directive:

  ...

  !$ACDC COPY, IF=LLPARALLEL {
  
  LLPERSISTENT = LLPARALLEL
  
  CALL YLCPG_DYN0%INIT (..., PERSISTENT=LLPERSISTENT)
  
  CALL YLCPG_PHY0%INIT (..., PERSISTENT=LLPERSISTENT)

  ...

  !$ACDC }
  
is transformed into:
  
  CALL YLCPG_DYN0%INIT (..., PERSISTENT=LLPERSISTENT)

  IF (LLPARALLEL) THEN
    CALL ACDC_COPY (YLCPG_DYN0)
  ENDIF

  CALL YLCPG_PHY0%INIT (..., PERSISTENT=LLPERSISTENT)

  IF (LLPARALLEL) THEN
    CALL ACDC_COPY (YLCPG_PHY0)
  ENDIF

  ...

so that scoped data structures be copied on the device.

=head2 PARALLEL directive

A block delimited by the C<PARALLEL> directive:

  IF (LLPARALLEL) THEN
  
  !$ACDC PARALLEL {
  
    CALL CPG (...)
  
  !$ACDC }
  
  ELSE
  
  ...
  
  ENDIF

is tranformed into:

  IF (LLPARALLEL) THEN

    CALL YFXTRAN_ACDC_STACK%INIT (YDCPG_OPTS%KLON, YDCPG_OPTS%KFLEVG, YDCPG_OPTS%KGPBLKS)

    CALL CPG_PARALLEL (...)
   
    IF (FXTRAN_ACDC_LSYNCHOST ('CPG_DRV')) THEN
      CALL HOST (YDA_EXTRA)
      CALL HOST (YDA_GFLPC)
      ...
    ENDIF
  ELSE
    ...
  ENDIF

The result is that the parallel version of F<cpg.F90> is called, and optionally, involved
Field API backed objects are synchronized on the CPU.

=head2 WIPE directive

A block delimited by this directive:

  !$ACDC WIPE, IF=LLPARALLEL {
  
  ...

  CALL YLCPG_PHY0%FINAL
  CALL YLCPG_DYN0%FINAL
  
  !$ACDC }

is transformed into:

  IF (LLPARALLEL) THEN
    CALL ACDC_WIPE (YLCPG_PHY0)
  ENDIF

  CALL YLCPG_PHY0%FINAL

  IF (LLPARALLEL) THEN
    CALL ACDC_WIPE (YLCPG_DYN0)
  ENDIF

  CALL YLCPG_DYN0%FINAL

So that scoped data structures be removed from the device before being destroyed.

=head1 ROUTINES

This is the list of IAL routines which are instrumented using the toplevel method:

=over 4

=item

F<cpg_drv.F90>

=item

F<cpglag_drv.F90>

=item

F<lapinea_drv.F90>

=item

F<lapineb_drv.F90>

=item

F<scan2m_ctvtot_drv.F90>

=item

F<scan2m.F90>

=item

F<scan2m_ctvtot_drv.F90>

=back

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 SEE ALSO

L<Fxtran::Generate>

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Decl;

sub if
{
  my ($cond, $stmt) = @_;

  return &s ($stmt) unless (defined ($cond));

  my ($if_block) = &Fxtran::parse (fragment => << "EOF", fopts => [qw (-construct-tag -line-length 5000 -canonic)]);
IF ($cond) THEN
$stmt
ENDIF
EOF

  return $if_block;
}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  my $types = $opts{'types-field-api'};

  &Fxtran::Decl::use ($pu, "USE FXTRAN_ACDC_PARALLELMETHOD_MOD", "USE FXTRAN_ACDC_STACK_MOD");

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  my ($NAME) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);

  if (my $switch = $opts{switch})
    {
      if (my ($E2) = &F ('./a-stmt[./E-1/named-E[string(N)="?"]]/E-2/ANY-E', $switch, $ep))
        {
          $E2->replaceNode (&e ("FXTRAN_ACDC_LPARALLELMETHOD ('PARALLEL','$NAME')"));
        }
    }

  for my $par (&F ('.//parallel-section', $pu))
    {
      my %type;

      for my $call (&F ('.//call-stmt', $par))
        {
          my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
          $proc->setData ($proc->textContent . '_PARALLEL');

          my @arg = &F ('./arg-spec/arg/named-E/N', $call, 1);

          my %sync2type;

          for my $arg (@arg)
            {
              next unless (my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $arg, $dp));

              # Skip local variables (these will be destroyed)

              next unless (&F ('./attribute[string(attribute-N)="INTENT"]', $decl));

              # Skip non structure variables

              next unless (my ($tn) = &F ('./_T-spec_/derived-T-spec/T-N', $decl, 1));

              # Keep only variables with Field API content

              next unless ($types->{'types'}{$tn});

              $type{$tn}++;

              $sync2type{$arg} = $tn;
            }

          my ($if) = &Fxtran::parse (fragment => << "EOF");
IF (FXTRAN_ACDC_LSYNCHOST ('$NAME')) THEN
ENDIF
EOF

          my ($if_block) = &F ('./if-block', $if);

          for my $sync (sort keys (%sync2type))
            {
              my $method = ($sync2type{$sync} =~ m/^FIELD_\w+_ARRAY/o) ? 'HOST' : "$opts{'method-prefix'}HOST";
              $if_block->insertBefore ($_, $if_block->lastChild) for (&s ("CALL $method ($sync)"), &t ("\n"));
            }

          $call->parentNode->insertAfter ($_, $call) for ($if, &t ("\n"));
        }
 
      &Fxtran::Decl::use ($pu, map { "USE UTIL_${_}_MOD" } grep { !/^FIELD_\w+_ARRAY/o } sort keys (%type));
      &Fxtran::Decl::use ($pu, "USE FIELD_ARRAY_UTIL_MODULE");

      $par->insertBefore ($_, $par->firstChild) for (&t ("\n"), &s ("CALL YFXTRAN_ACDC_STACK%INIT (YDCPG_OPTS%KLON, YDCPG_OPTS%KFLEVG, YDCPG_OPTS%KGPBLKS)"), &t ("\n"));
      $par->appendChild ($_) for (&t ("\n"), &s ("CALL YFXTRAN_ACDC_STACK%FINAL ()"), &t ("\n"));

    }

  my %name2type;

  for my $copy (&F ('.//copy-section', $pu))
    {
      my ($cond) = &F ('./@if', $copy, 1);

      for my $call (&F ('.//call-stmt', $copy))
        {
          my ($proc) = &F ('./procedure-designator/named-E', $call);
          my ($name) = &F ('./N', $proc, 1);
          my ($method) = &F ('./R-LT/component-R/ct', $proc, 1);

          next unless ($name && $method && ($method eq 'INIT'));
 
          unless ($name2type{$name})
            {
              my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $name, $dp);
              my ($type) = &F ('./_T-spec_/derived-T-spec/T-N', $decl, 1);
              $name2type{$name} = $type;
            }

          $call->parentNode->insertAfter ($_, $call) for (&if ($cond, "CALL ACDC_COPY ($name)"), &t ("\n"));
        }
    }

  for my $wipe (&F ('.//wipe-section', $pu))
    {
      my ($cond) = &F ('./@if', $wipe, 1);

      for my $call (&F ('.//call-stmt', $wipe))
        {
          my ($proc) = &F ('./procedure-designator/named-E', $call);
          my ($name) = &F ('./N', $proc, 1);
          my ($method) = &F ('./R-LT/component-R/ct', $proc, 1);
          next unless ($name && $method && ($method eq 'FINAL'));
 
          unless ($name2type{$name})
            {
              my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $name, $dp);
              my ($type) = &F ('./_T-spec_/derived-T-spec/T-N', $decl, 1);
              $name2type{$name} = $type;
            }

          $call->parentNode->insertBefore ($_, $call) for (&if ($cond, "CALL ACDC_WIPE ($name)"), &t ("\n"));
        }
    }

  for my $type (sort values (%name2type))
    {
      &Fxtran::Decl::use ($pu, "USE UTIL_${type}_MOD");
    }

}

1;

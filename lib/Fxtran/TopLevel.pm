package Fxtran::TopLevel;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

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

  &Fxtran::Decl::use ($pu, "USE FXTRAN_ACDC_PARALLELMETHOD_MOD", "USE FXTRAN_ACDC_STACK_MOD");

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  if (my $switch = $opts{switch})
    {
      if (my ($E2) = &F ('./a-stmt[./E-1/named-E[string(N)="?"]]/E-2/ANY-E', $switch, $ep))
        {
          my ($name) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);
          $E2->replaceNode (&e ("FXTRAN_ACDC_LPARALLELMETHOD ('PARALLEL','$name')"));
        }
    }

  for my $par (&F ('.//parallel-section', $pu))
    {
      for my $call (&F ('.//call-stmt', $par))
        {
          my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
          $proc->setData ($proc->textContent . '_PARALLEL');
        }

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

package Pointer::Parallel::UpdateView;

#
# Copyright 2023 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Pointer::Parallel;
use Fxtran;
use Data::Dumper;

sub getDefaultWhere
{
  'host';
}

sub onlySimpleFields
{
  1;
}

sub requireUtilMod
{
  1;
}

sub makeParallel
{
  shift;
  my ($par1, $t, %opts) = @_;

  my $style = $par1->getAttribute ('style') || 'ARPIFS';
  my $FILTER = $par1->getAttribute ('filter');

  if ($FILTER)
    {
      die;
    }

  my ($do) = &F ('./do-construct', $par1);
  my $do_jblk = $do->firstChild;

  my $indent = &Fxtran::getIndent ($do_jblk);

  my %update;

  my %nonblocked = map { ($_, 1) } @{ $opts{'types-fieldapi-non-blocked'} };

  for my $expr (&F ('.//named-E', $par1))
    {
      my ($N) = &F ('./N', $expr, 1);
      my $s = $t->{$N};
      next if ($s->{skip});


      if ($s->{object})
        {
          my $ts = $s->{ts};
          my ($tn) = &F ('./T-N', $ts, 1);
          $update{$N}++ unless ($nonblocked{$tn});
        }
    }

  my ($prep) = &F ('./prep', $par1);

  my $init = &s ('CALL YLCPG_BNDS%INIT (YDCPG_OPTS)');

  $par1->insertBefore ($init, $do);
  $par1->insertBefore (&t ("\n"), $do);


  my @prep_stmt = &F ('./ANY-stmt', $prep);
  my $first = $prep_stmt[0];

  for my $N (sort keys (%update))
    {
      $do->insertAfter ($_, $do_jblk) for (&s ("CALL $N%UPDATE_VIEW (JBLK)"), 
                                           &t ("\n" . (' ' x ($indent + 2))));
      
      $prep->insertAfter ($_, $first) for (&s ("CALL HOST ($N)"), 
                                           &t ("\n" . (' ' x $indent)));
      
    }

  return $par1;
}


1;

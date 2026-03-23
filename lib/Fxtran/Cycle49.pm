package Fxtran::Cycle49;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Cycle49

=head1 DESCRIPTION

Simplification rules specific to IFS cycle 49.  Provides a C<simplify> method
that rewrites a set of named Boolean or string variables to their known
compile-time values for that cycle, removing dead code branches.  Additional
variables can be overridden at run time via the C<set-variables> option.

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;
use Fxtran::Construct;
use Data::Dumper;

sub simplify
{

=head2 simplify

Rewrites named Boolean and string variables to their known compile-time values
for IFS cycle 49, removing dead code branches.  Additional variables can be
substituted at run time via the C<set-variables> option.

=cut

  shift;
  my $d = shift;
  my %opts = @_;

  my $zero = &e ('1');
  $zero->firstChild->firstChild->replaceNode (&t ('0'));

  &Fxtran::Construct::apply 
  ($d, 
    '//named-E[string(N)="LMUSCLFA"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="YDSPP_CONFIG%LSPP"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="OCH1CONV"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="YDLDDH%LFLEXDIA"]',                      => &e ('.FALSE.'),
    '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LFLEXDIA"]'     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_U"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_V"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_TH"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_RV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_SV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="LLSIMPLE_DGEMM"]'                        => &e ('.TRUE.'),
    '//named-E[string(.)="LLVERINT_ON_CPU"]'                       => &e ('.FALSE.'),
  );

  if (my $set = $opts{'set-variables'})
    {
      my %set;

      while (my ($k, $v) = each (%$set))
        {
          $k = '//named-E[string(.)="' . $k . '"]';
          $v = $v eq '0' ? $zero : &e ($v);
          $set{$k} = $v;
        }
      
      &Fxtran::Construct::apply ($d, %set);

    }

}

1;

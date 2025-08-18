package Fxtran::Formatter::Associate;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Formatter::block);

use Data::Dumper;

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{
  my $class = shift;
  my ($stmt, $indent) = @_;

  $stmt = $class->canonic ($stmt);

  my @t = &F ('./associate-LT/associate', $stmt);

  @t = grep { $_->nodeName ne '#text' } @t;
  
  my %n2t;

  for my $t (@t)
    {
      my ($n) = &F ('./associate-N', $t, 1);
      $n2t{$n} = $t;
    }
  
  @t = @n2t{sort keys (%n2t)};

  $stmt = "ASSOCIATE ( &\n$indent  " .  join ("\n$indent, ", map { $_->textContent . " & " } @t) . "\n$indent)";

  $stmt = $class->reparse ($stmt);

  return $stmt;
}

sub repack
{
  my $class = shift;
  my ($stmt, $indent) = @_;
  my @associate = &F ('./associate-LT/associate', $stmt, 1);

  @associate = sort (@associate);

  $class->repackCallLikeStatement ("ASSOCIATE (", @associate, ")", $indent);
}

1;

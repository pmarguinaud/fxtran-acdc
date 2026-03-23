package Fxtran::Formatter::Associate;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Formatter::Associate

=head1 DESCRIPTION

Formatter for Fortran C<ASSOCIATE> statements. Inherits from
L<Fxtran::Formatter::block>. The C<expand> method rewrites the statement so
that each selector appears on its own line sorted alphabetically by associate
name. The C<repack> method reformats a canonical statement back into a compact
multi-line form respecting the line-length limit.

=head1 FUNCTIONS

=cut

use base qw (Fxtran::Formatter::block);

use Data::Dumper;

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{

=head2 expand

Takes a Fortran C<ASSOCIATE> statement node and an indentation string. Parses
the selector list, sorts the selectors alphabetically by associate name, and
rewrites the statement so that each selector appears on its own continuation
line. Returns the re-parsed statement node.

=cut

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

=head2 repack

Takes an expanded C<ASSOCIATE> statement node and an indentation string.
Extracts the sorted associate selectors and reassembles them into a compact
multi-line form that respects the project line-length limit, via
C<repackCallLikeStatement>.

=cut

  my $class = shift;
  my ($stmt, $indent) = @_;
  my @associate = &F ('./associate-LT/associate', $stmt, 1);

  @associate = sort (@associate);

  $class->repackCallLikeStatement ("ASSOCIATE (", @associate, ")", $indent);
}

1;

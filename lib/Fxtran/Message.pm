package Fxtran::Message;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Message

=head1 DESCRIPTION

Simple diagnostic output module.  Formats and prints human-readable warning
or error messages, optionally accompanied by the text of the Fortran statement
that triggered the message.  The C<error> function additionally terminates the
program after printing the message.

=head1 FUNCTIONS

=cut

use Text::Wrap;

use strict;

sub message
{
  my $width = 80;

  my ($mess, $stmt) = @_;

  local $Text::Wrap::columns = $width;

  print "-" x $width, "\n\n";

  if ($stmt)
    {
      print &wrap ('', '', $mess . ':') . "\n\n";

      local $Text::Wrap::separator = " &\n";

      print &wrap ('   ', ' & ', $stmt->textContent), "\n" x 2;
    }
  else
    {
      print &wrap ('', '', $mess) . "\n\n";
    }

}

sub error
{
  &message (@_);
  die "\n";
}

1;

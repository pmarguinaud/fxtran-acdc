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

=cut

use Text::Wrap;

use strict;

sub message
{

=head2 message

Print a formatted diagnostic message to standard output.  The output is
preceded by a line of 80 dashes.  If a second argument C<$stmt> (a DOM node)
is given, the message is printed as a label followed by the wrapped text
content of the statement; otherwise only the wrapped message text is printed.

=cut

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

=head2 error

Print a diagnostic message via C<message> and then terminate the program by
calling C<die> with a bare newline (so that no additional "at file line N"
suffix is appended).

=cut

  &message (@_);
  die "\n";
}

1;

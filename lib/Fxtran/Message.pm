package Fxtran::Message;

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

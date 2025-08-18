package Fxtran::Message;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

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

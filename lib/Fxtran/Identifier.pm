package Fxtran::Identifier;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Identifier

=head1 DESCRIPTION

Provides utilities for renaming Fortran identifiers inside a parsed document.
Both expression references (C<named-E>) and declaration names (C<EN-N>) are
updated consistently.

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;
use Fxtran::Inline;

sub rename
{

=head2 rename

Renames Fortran identifiers in the parsed document according to a mapping hash.
Updates both expression references (C<named-E>) and declaration names (C<EN-N>).
In a program unit which already declares the new name, the declaration of the old
name is removed instead.

=cut

  my $d = shift;

  my %map = @_;

  while (my ($k, $v) = each (%map))
    {

      my @expr = &F ('.//named-E[string(N)="?"]/N/n/text()', $k, $d);

      for (@expr)
        {
          $_->setData ($v);
        }

      my @en_decl = &F ('.//EN-N[string(N)="?"]/N/n/text()', $k, $d);

      for (@en_decl)
        {
          my ($pu) = &F ('ancestor::program-unit[1]', $_);
          if ($pu && &F ('./specification-part/declaration-part//EN-N[string(N)="?"]', $v, $pu))
            {
              &Fxtran::Inline::removeEntity (&F ('ancestor::EN-decl[1]', $_));
            }
          else
            {
              $_->setData ($v);
            }
        }
    }
}


=head1 SEE ALSO

L<Fxtran::Decl>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;

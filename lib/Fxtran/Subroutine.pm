package Fxtran::Subroutine;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Subroutine

=head1 DESCRIPTION

Utilities for manipulating Fortran SUBROUTINE program units in a parsed
document.  C<addSuffix> appends a string to the subroutine name in both the
SUBROUTINE and END SUBROUTINE statements as well as in any DR_HOOK call
strings.  C<rename> applies a user-supplied transformation function to the
subroutine name and updates DR_HOOK strings accordingly.  C<getInterface>
locates and parses the interface block for a named subroutine.

=head1 FUNCTIONS

=cut

use FileHandle;
use Data::Dumper;

use strict;

use Fxtran;
  
sub addSuffix
{

=head2 addSuffix

Appends C<$suffix> to the subroutine name in both the SUBROUTINE and END
SUBROUTINE statements of the program unit C<$pu>, and updates any DR_HOOK
call string literals to include the same suffix.

=cut

  my ($pu, $suffix) = @_;

  my @sn = &F ('./subroutine-stmt/subroutine-N/N/n/text()|./end-subroutine-stmt/subroutine-N/N/n/text()', $pu);

  for my $sn (@sn) 
    {
      $sn->setData ($sn->data . $suffix);
    }

  return unless (my ($ep) = &F ('./execution-part', $pu));

  my @drhook_name = &F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]/arg-spec/arg/string-E/S/text()', $ep);

  for (@drhook_name)
    {
      (my $str = $_->data) =~ s/(["'])$/$suffix$1/go;
      $_->setData ($str);
    }

}

sub rename
{

=head2 rename

Renames a subroutine by applying the user-supplied transformation function
C<$sub> to the current name and updating the SUBROUTINE, END SUBROUTINE, and
DR_HOOK string nodes accordingly.

=cut

  my ($d, $sub) = @_;

  my @name = (
               &F ('./subroutine-stmt/subroutine-N/N/n/text()', $d),
               &F ('./end-subroutine-stmt/subroutine-N/N/n/text()', $d),
             );
  my $name = $name[0]->textContent;

  my $name1 = $sub->($name);

  for (@name)
    {   
      $_->setData ($name1);
    }   

  my @drhook = &F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]', $d);

  for my $drhook (@drhook)
    {   
      next unless (my ($S) = &F ('./arg-spec/arg/string-E/S/text()', $drhook));
      my $str = $S->textContent;
      $str =~ s/$name/$name1/;
      $S->setData ($str);
    }   
  
}

my %intf;

sub getInterface
{

=head2 getInterface

Locates the interface file for the named subroutine via the C<$find> helper,
reads it, and returns the parsed program-unit or interface-construct node.
Dies if the interface file cannot be found or opened.

=cut

  my ($name, $find) = @_;

  my $intf;

  unless ($intf = $intf{$find}{$name})
    {
      my $file = $find->getInterface (name => $name);
      $file or die ("Could not find interface for $name");
      my $code = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$file"); $fh or die ("Cannot open $file"); <$fh> };
      my @code = &Fxtran::parse (fragment => $code);
      ($intf) = grep { $_->nodeName =~ m/^(?:program-unit|interface-construct)$/o } @code;
      $intf{$find}{$name} = $intf;
    }

  return $intf;
}

=head1 SEE ALSO

L<Fxtran::Module>, L<Fxtran::Interface>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;

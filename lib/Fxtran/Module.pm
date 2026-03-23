package Fxtran::Module;

#
# Copyright 2024 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Module

=head1 DESCRIPTION

Utilities for manipulating Fortran MODULE program units in a parsed document.
Provides functions to append a suffix to a module name (updating both the
MODULE and END MODULE statements) and to rename a module using a user-supplied
transformation function.

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;
use FileHandle;
  
sub addSuffix
{

=head2 addSuffix

Appends C<$suffix> to the module name in both the MODULE and END MODULE
statements of the parse tree node C<$d>.

=cut

  my ($d, $suffix) = @_;

  my @sn = &F ('./module-stmt/module-N/N/n/text()|./end-module-stmt/module-N/N/n/text()', $d);

  for my $sn (@sn) 
    {
      $sn->setData ($sn->data . $suffix);
    }

}

sub rename
{

=head2 rename

Renames a module by applying the user-supplied transformation function C<$sub>
to the current module name, then updating the MODULE and END MODULE statement
nodes in the parse tree.

=cut

  my ($d, $sub) = @_;

  $d = &getProgramUnit ($d);

  my @name = (
               &F ('./module-stmt/module-N/N/n/text()', $d),
               &F ('./end-module-stmt/module-N/N/n/text()', $d),
             );
  my $name = $name[0]->textContent;

  my $name1 = $sub->($name);

  for (@name)
    {   
      $_->setData ($name1);
    }   

}

1;

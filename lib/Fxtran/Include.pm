package Fxtran::Include;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Include

=head1 DESCRIPTION

Manages Fortran INCLUDE directives in a parsed document.  Provides functions
to remove interface-block includes that are no longer referenced, to insert new
include directives into the declaration part of a program unit, and to inline
(load and expand) include files that appear after a CONTAINS statement.

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;
use Fxtran::Scope;
use Fxtran::Canonic;


sub removeUnusedIncludes
{

=head2 removeUnusedIncludes

Removes interface-block include directives from the declaration part that are no
longer referenced by any call statement in the execution part.

=cut

  my $d = shift;

  my ($dp) = &F ('./specification-part/declaration-part', $d);
  my ($ep) = &F ('./execution-part', $d);

  for my $include (&F ('./include', $dp))
    {   
      my ($filename) = &F ('./filename', $include, 2); 
      my $name = $filename;
      next if ($name =~ m/\.func\.h$/o);
      next if ($name eq 'fxtran_acdc_stack.h');
      for ($name)
        {
           s/\.intfb\.h$//o;
           s/\.h$//o;
        }
      $name = uc ($name);
      next if (&F ('.//call-stmt[string(procedure-designator)="?"]', $name, $ep));
      my $next = $include->nextSibling;
      $next->unbindNode () if ($next->textContent eq "\n");
      $include->unbindNode (); 
    }   
}

sub addInclude
{

=head2 addInclude

Inserts one or more include directives into the declaration part of a program
unit, placing them after any existing include or at the end of the declaration
part.

=cut

  my $pu = shift;

  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my $X = &n ('<C/>');

  if (my ($inc) = &F ('./include', $dp))
    {
      $dp->insertAfter ($X, $inc);
    }
  else
    {
      $dp->appendChild ($X);
    }

  for my $file (@_)
    {
      my $include = &n ('<include>#include "<filename>' . lc ($file) . '</filename>"</include>');
      $dp->insertAfter ($include, $X);
      $dp->insertAfter (&t ("\n"), $X);
    }

  $X->unbindNode ();
}

sub loadContainedIncludes
{

=head2 loadContainedIncludes

Inlines include files that appear after a CONTAINS statement: each such include
is parsed, canonicalised, and its program units are inserted in place of the
include directive.

=cut

  my $d = shift;
  my %opts = @_;

  my $find = $opts{find};
  my @fopts = @{ $opts{fopts} || [] };

  my @include = &F ('.//include-stmt[preceding-sibling::contains-stmt', $d);
  for my $include (@include)
    {   
      my ($filename) = &F ('./filename', $include, 2); 
      for ($filename)
        {
          s/^"//o;
          s/"$//o;
        }

      my $f = $filename;

      $filename = $find->resolve (file => $filename);

      $filename or die ("Cannot find include file `$f'");

      my $text = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$filename"); <$fh> };
      my $di = &Fxtran::parse (string => $text, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include), @fopts]);

      &Fxtran::Canonic::makeCanonic ($di, %opts);

      my @pu = &F ('./object/file/program-unit', $di);
      for my $pu (@pu)
        {
          $include->parentNode->insertBefore ($pu, $include);
          $include->parentNode->insertBefore (&t ("\n"), $include);
        }
      $include->unbindNode (); 
    }   
}


1;

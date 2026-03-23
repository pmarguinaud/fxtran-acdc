package Fxtran::Pointer::Object;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Pointer::Object

=head1 DESCRIPTION

Provides utilities for working with derived-type object fields in the context
of FIELD API pointer transformations. Functions look up type declarations,
determine whether a component belongs to an object, and translate field
expressions and component paths from their original names (e.g. C<PT0>,
C<DM0>) to their FIELD API field counterparts (e.g. C<FT0>, C<F_PT0>).

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;
use Data::Dumper;
use Storable;

{

my %decl;

sub getObjectDecl
{

=head2 getObjectDecl

Look up and cache the parsed declaration node for a given type component key.
Dies unless the key is found in the type hash or C<allowConstant> is set.

=cut

  my ($key, $types, %opts) = @_;

  my $h = $types;

  unless ($decl{$key}) 
    {
      unless ($h->{$key})
        {
          if ($opts{allowConstant})
            {
              return;
            }
          else
            {
              die $key;
            }
        }
      ($decl{$key}) = &s ($h->{$key});
    }

  return $decl{$key};
}

my %type;

sub getObjectType
{

=head2 getObjectType

Return and cache the type name (C<T-N> text) for the given symbol table entry.

=cut

  my ($s, $obj) = @_;

  unless ($type{$obj})
    {
      ($type{$obj}) = &F ('./T-N', $s->{ts}, 1);
    }

  return $type{$obj};
}

}

sub isField
{

=head2 isField

Return true when the component path C<@ctl> of symbol C<$s> is backed by a
FIELD API entry in the C<$types> hash.

=cut

  my ($types, $s, @ctl) = @_;

  my $ts = $s->{ts};

  die unless ($ts->textContent =~ m/^(?:TYPE|CLASS)\s*\(\s*(\w+)\s*\)$/o);
  
  my $t = $1;

  my $k = join ('%', $t, @ctl);

  return $types->{$k};
}

sub asFromDecl
{

=head2 asFromDecl

Extract the C<array-spec> node from a declaration node, or return C<undef>.

=cut

  my $decl = shift;

  my ($as) = &F ('.//EN-decl/array-spec', $decl);

  return $as;
}

sub getFieldFromExpr
{

=head2 getFieldFromExpr

Clone a named-E expression and rewrite its last component name to the
corresponding FIELD API name (e.g. C<PT0> -> C<FT0>, C<P*> -> C<F_*>),
stripping trailing array and parentheses references.

=cut

  my ($expr) = @_;

  my @Ctl = &F ('./R-LT/component-R/ct/text()', $expr);
  my $ctl = @Ctl ? $Ctl[-1]->textContent : undef;

  my ($obj) = &F ('.//N', $expr, 1);

  my %ydvars = map { ($_, 1) } qw (YDVARS YDGEOMVARS);

  if (($ydvars{$obj}) && $ctl && ($ctl eq 'RCP'))
     {
     }
  elsif (($ydvars{$obj}) && $ctl && ($ctl eq 'LT1'))
     {
     }
  elsif (($ydvars{$obj}) && $ctl && ($ctl eq 'P'))
     {
       $ctl = 'FT0';
     }
  elsif ($ctl =~ m/^(?:T[019]|(?:DM|DL)[019]?|PC_PH)$/o)
    {
      $ctl = 'F' . $ctl; 
    }
  elsif ($obj eq 'YDMF_PHYS_SURF') 
    {
      if ($ctl =~ m/^P(\w+)_T[019]$/o)
        {
          $ctl =~ s/^P/F_/o;
        }
      else
        {
          $ctl =~ s/^P/F_/o;
        }
    }
  else
    {
      $ctl = 'F_' . $ctl; 
    }

  my $e = $expr->cloneNode (1);
  my ($RLT) = &F ('./R-LT', $e);

  @Ctl = &F ('./component-R/ct/text()', $RLT);
  if ($ctl)
    {
      $Ctl[-1]->setData ($ctl);
    }

  @Ctl = &F ('./ANY-R', $RLT);

  while (my $Ctl = pop (@Ctl))
    {
      last unless (($Ctl->nodeName eq 'array-R') or ($Ctl->nodeName eq 'parens-R'));
      $Ctl->unbindNode ();
    }

  if ($RLT->lastChild->nodeName eq '#text')
    {
      $RLT->lastChild->unbindNode ();
    }

  return $e;
}

sub getFieldFromObjectComponents
{

=head2 getFieldFromObjectComponents

Build a new C<named-E> XML node for the FIELD API counterpart of an object
component path, applying the same naming rules as C<getFieldFromExpr>.

=cut

  my ($obj, @ctl) = @_;

  my %ydvars = map { ($_, 1) } qw (YDVARS YDGEOMVARS);

  if (($ydvars{$obj}) && @ctl && ($ctl[-1] eq 'RCP'))
     {
     }
  elsif (($ydvars{$obj}) && @ctl && ($ctl[-1] eq 'LT1'))
     {
     }
  elsif ($ctl[-1] =~ m/^(?:T[019]|(?:DM|DL)[019]?|PC_PH)$/o)
    {
      $ctl[-1] = 'F' . $ctl[-1]; 
    }
  elsif ($obj eq 'YDMF_PHYS_SURF') 
    {
      if ($ctl[-1] =~ m/^P(\w+)_T[019]$/o)
        {
          $ctl[-1] =~ s/^P/F_/o;
        }
      else
        {
          $ctl[-1] =~ s/^P/F_/o;
        }
    }
  else
    {
      $ctl[-1] = 'F_' . $ctl[-1]; 
    }

  my $n =  &n ("<named-E><N><n>$obj</n></N><R-LT>" 
             . join ('', map { "<component-R>%<ct>$_</ct></component-R>" } @ctl)
             . "</R-LT></named-E>");

  return $n;
}

1;

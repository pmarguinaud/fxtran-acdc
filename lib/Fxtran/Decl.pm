package Fxtran::Decl;

=head1 NAME

Fxtran::Decl

=head1 DESCRIPTION

This module provides functions for manipulating data declaration.

=head1 FUNCTIONS

=cut

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Scope;

sub forceSingleDecl
{

=head2 forceSingleDecl

This function transforms statements such as:

  REAL :: X, Y, Z

into:

  REAL :: X
  REAL :: Y 
  REAL :: Z

=cut


  my $d = shift;

# Select all entity lists with several entities

  my @en_decl_lst = &F ('.//EN-decl-LT[count(./EN-decl)>1]', $d);

  for my $en_decl_lst (@en_decl_lst)
    {
      my $stmt = &Fxtran::stmt ($en_decl_lst);
      my @en_decl = &F ('./EN-decl', $en_decl_lst);
      for my $en_decl (@en_decl)
        {
          my $s = $stmt->cloneNode (1);
          my ($l) = &F ('.//EN-decl-LT', $s);
          for ($l->childNodes ())
            {
              $_->unbindNode ();
            }
          $l->appendChild ($en_decl->cloneNode (1));
          $stmt->parentNode->insertAfter ($s, $stmt);
          $stmt->parentNode->insertAfter (&t ("\n"), $stmt);
        }
      $stmt->unbindNode ();
    }

}

sub declare
{

=head2 declare

This routine takes a program unit as argument, and statements to add
in the declaration part. These statements can be strings or XML nodes. 

Only statements declaring a single entity are accepted. Statements
redeclaring already declared entities are ignored.

=cut

  my $d = shift;

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  my @stmt = map { ref ($_) ? $_ : &s ($_) } @_;

  my %N_d = map { ($_, 1) } &F ('./T-decl-stmt//EN-N', $dp, 1);

  for my $stmt (@stmt)
    {
      my @N = &F ('.//EN-N', $stmt, 1);
      die if (scalar (@N) > 1);
      next if ($N_d{$N[0]});
      $dp->appendChild ($_) for (&t ("\n"), $stmt);
    }

}

sub use
{

=head2 use

This routine takes a program unit as argument, and use statements to 
add in the use part. These statements can be either strings or XML nodes.

Statements importing existing modules are ignored.

=cut

  my $d = shift;

  my ($up) = &F ('./specification-part/use-part', $d);

  my @stmt = map { ref ($_) ? $_ : &s ($_) } @_;

  my %U_d = map { ($_, 1) } &F ('.//module-N', $up, 1);

  for my $stmt (@stmt)
    {
      my ($U) = &F ('.//module-N', $stmt, 1);
      next if ($U_d{$U});
      $up->appendChild ($_) for (&t ("\n"), $stmt);
    }


}

sub include
{

=head2 include

This method takes a program unit as argument, and include statements to
add in the declaration part.

Already included files are not included again.

=cut

  my ($d, $include) = @_;

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  my ($filename) = &F ('filename', $include, 2);

  return if (&F ('./include[string(filename)="?"]', $filename, $dp));

  my $base;

  my @include = &F ('./include', $d);

  if (@include)
    {
      $dp->insertAfter ($_, $include[0]) for (&t ("\n"), $include);
    }
  else
    {
      $dp->appendChild ($_) for (&t ("\n"), $include);
    }
}

sub addAttributes
{

=head2 addAttributes

Add attributes to a declaration statement. For instance:

  REAL :: X

can be added a POINTER attribute:

  REAL, POINTER :: X

=cut

  my ($stmt, @attr) = @_; 
  my $ts = $stmt->firstChild;

  my %attr = map { ($_, 1) } (&F ('.//attribute-N', $stmt, 1));
  for my $attr (@attr)
    {   
      next if ($attr{$attr});
      $stmt->insertAfter (&n ("<attribute><attribute-N>$attr</attribute-N></attribute>"), $ts);
      $stmt->insertAfter (&t (', '), $ts);
    }   
}

sub removeAttributes
{

=head2 removeAttributes

Remove attributes from a declaration statement. For instance:

  REAL, POINTER :: X

can be removed its POINTER attribute:

  REAL :: X

=cut

  my ($stmt, @attr) = @_; 

  my @v; 

  for my $attr (@attr)
    {   
      next unless (my ($x) = &F ('.//attribute-N[string(.)="?"]', $attr, $stmt));
      push @v, $x->parentNode;
      $x->parentNode->previousSibling->unbindNode (); 
      $x->parentNode->unbindNode (); 
    }

  return @v;
}


=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

1;

package Decl;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Scope;
use Data::Dumper;

sub forceSingleDecl
{

# Single declaration statement per entity

  my $d = shift;

# Select all entity lists with several entities

  my @en_decl_lst = &F ('.//EN-decl-LT[count(./EN-decl)>1]', $d);

  for my $en_decl_lst (@en_decl_lst)
    {
      my $stmt = &Fxtran::stmt ($en_decl_lst);
      my $indent = &Fxtran::getIndent ($stmt);
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
          $stmt->parentNode->insertAfter (&t ("\n" . (' ' x $indent)), $stmt);
        }
      $stmt->unbindNode ();
    }

}

sub declare
{
  my $d = shift;
  my @stmt = map { ref ($_) ? $_ : &s ($_) } @_;

  my %N_d = map { ($_, 1) } &F ('./T-decl-stmt//EN-N', $d, 1);

  my $noexec = &Scope::getNoExec ($d);

  for my $stmt (@stmt)
    {
      my @N = &F ('.//EN-N', $stmt, 1);
      die if (scalar (@N) > 1);
      next if ($N_d{$N[0]});
      $noexec->parentNode->insertBefore ($stmt, $noexec);
      $noexec->parentNode->insertBefore (&t ("\n"), $noexec);
    }

}

sub use
{
  my $d = shift;
  my @stmt = map { ref ($_) ? $_ : &s ($_) } @_;

  my %U_d = map { ($_, 1) } &F ('.//module-N', $d, 1);

  my ($use) = &F ('.//use-stmt', $d);
  
  unless ($use)
    {
      ($use) = &F ('.//subroutine-stmt', $d);
    }

  for my $stmt (@stmt)
    {
      my ($U) = &F ('.//module-N', $stmt, 1);
      next if ($U_d{$U});
      $use->parentNode->insertAfter ($stmt, $use);
      $use->parentNode->insertAfter (&t ("\n"), $use);
    }


}

sub include
{
  my ($d, $include) = @_;

  my ($filename) = &F ('filename', $include, 2);

  return if (&F ('./include[string(filename)="?"]', $filename, $d));

  my $base;

  my @include = &F ('./include', $d);

  if (@include)
    {
      $base = $include[0];
    }
  else
    {
      $base = &Scope::getNoExec ($d);
    }

  $base->parentNode->insertBefore ($include, $base);
  $base->parentNode->insertBefore (&t ("\n"), $base);
  

}

sub changeIntent
{
  my ($d, %intent) = @_;

  my @en_decl = &F ('.//EN-decl[' . join (' or ', map { "string(EN-N)='$_'" } sort keys (%intent)) . ']', $d);

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);
      my ($intent) = &F ('.//intent-spec/text()', $stmt); 
      $intent->setData ($intent{$N});
    }

}

sub getAttributes
{
  my ($stmt, @attr) = @_; 

  my @v; 

  for my $attr (@attr)
    {   
      my ($v) = &F ('.//attribute-N[string(.)="?"]', $attr, $stmt);
      push @v, $v; 
    }   

  return @v; 
}

sub addAttributes
{
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


1;

package Decl;

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

  my %N_d = map { ($_, 1) } &F ('.//EN-N', $d, 1);

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

1;

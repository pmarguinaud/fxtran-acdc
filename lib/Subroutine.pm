package Subroutine;

use strict;
use Fxtran;
  
sub addSuffix
{
  my ($d, $suffix) = @_;

  my @sn = &F ('./object/file/program-unit/subroutine-stmt/subroutine-N/N/n/text()|./object/file/program-unit/end-subroutine-stmt/subroutine-N/N/n/text()', $d);

  for my $sn (@sn) 
    {
      $sn->setData ($sn->data . $suffix);
    }

  my @drhook_name = &F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]/arg-spec/arg/string-E/S/text()', $d);

  for (@drhook_name)
    {
      (my $str = $_->data) =~ s/(["'])$/$suffix$1/go;
      $_->setData ($str);
    }

}


1;

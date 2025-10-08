package Fxtran::UserCode;

=head1 NAME

Fxtran::UserCode

=head1 DESCRIPTION

Instead of generating code for a routine, look for user provided code (option C<user-code>).

The user provided code is renamed according using the appropriate routine name and suffix.

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Util;

sub getUserCode
{
  my ($F90, $F90out, $method, %opts) = @_;

  my $find = $opts{find};
  
  my $f90orig = $F90;
  my $f90user = $find->resolve (file => $opts{'user-code'});
  
  my $d_orig = &Fxtran::parse (location => $f90orig, fopts => [qw (-line-length 5000 -no-include -no-cpp -construct-tag)], dir => $opts{tmp});
  my $d_user = &Fxtran::parse (location => $f90user, fopts => [qw (-line-length 5000 -no-include -no-cpp -construct-tag)], dir => $opts{tmp});
  
  if (my $suffix = $opts{"suffix-$method"})
    {     
      my @pu_orig = &F ('./object/file/program-unit', $d_orig);
      my @pu_user = &F ('./object/file/program-unit', $d_user);
  
      die ("Program unit mismatch")
        unless (scalar (@pu_orig) == scalar (@pu_user));
  
      for my $i (0 .. $#pu_user)
        {
          my $pu_orig = $pu_orig[$i];
          my $pu_user = $pu_user[$i];
  
          my $n_orig;
          my $n_user;
  
          for my $sn (&F ('./subroutine-stmt/subroutine-N/N/n/text()|./end-subroutine-stmt/subroutine-N/N/n/text()', $pu_orig)) 
            {
              $n_orig = $sn->data;
            }
  
          for my $sn (&F ('./subroutine-stmt/subroutine-N/N/n/text()|./end-subroutine-stmt/subroutine-N/N/n/text()', $pu_user)) 
            {
              $n_user = $sn->data;
              $sn->setData ($n_orig . $suffix);
            }
  
          for my $s (&F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]'
                       . '/arg-spec/arg/string-E/S/text()[contains(string(.),"?")]', 
                         $n_user, $pu_user))
            {
              (my $str = $s->data) =~ s/(["'])$/$suffix$1/go;
              $s->setData ($str);
            }
        }
    }     
  
  &Fxtran::Util::updateFile ($F90out, $d_user->textContent);

}


1;

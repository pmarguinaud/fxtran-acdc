package Fxtran::UserCode;

=head1 NAME

Fxtran::UserCode

=head1 DESCRIPTION

Instead of generating code for a routine, look for user provided code (option C<user-code>).

The user provided code is renamed according using the appropriate routine name and suffix.
=cut

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Util;

sub getUserCode
{

=head2 getUserCode

Load user-provided replacement code instead of generating it automatically.
Both the original source file (C<$F90>) and the user-supplied file (resolved
via the C<user-code> option) are parsed.  When a method suffix is configured,
every program-unit name in the user file is renamed to match the original
name with the suffix appended, and any C<DR_HOOK> string literals that
reference the routine name are updated accordingly.  The resulting text is
written to C<$F90out> and, if C<FXTRAN_F90_COMMAND> is set in the
environment, a dry-run compilation is performed to validate the output.

=cut

  my ($F90, $F90out, $method, %opts) = @_;

  my @fxtran_f90_command = do
  {
    my $cmd;
    if ($cmd = $ENV{FXTRAN_F90_COMMAND})
      {
        $cmd = eval ($cmd);
      }
    @{ $cmd || [] }
  };

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

  if (@fxtran_f90_command)
    {
      &Fxtran::Util::runCommand (cmd => [@fxtran_f90_command, qw (--dryrun --tmp 0 --dir . -- f90 -c), $F90out]);
    }
}



=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut
1;

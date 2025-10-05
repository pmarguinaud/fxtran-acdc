package Fxtran::TopLevel::SemiImplicit;

use Data::Dumper;
use FileHandle;

use strict;

use base qw (Fxtran::TopLevel::Spectral);

use Fxtran::Decl;
use Fxtran;

sub processSingleRoutine
{
  __PACKAGE__->processSingleRoutineMethod (@_);
}

sub renameProc
{
  my $class = shift;
  my ($pu, $proc, %opts) = @_;

  next unless ((my $tt = $proc->textContent) =~ m/^SP\w+SI$/o);
  $proc->setData ($tt . $opts{'suffix-semiimplicit'});

  return 1;
}

1;

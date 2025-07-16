package Fxtran::BitRepro;

use strict;

use Fxtran::Intrinsic;
use Fxtran::Call;
use Fxtran::Subroutine;
use Fxtran::Module;
use Fxtran;


sub makeBitReproducible
{
  my ($d, %opts) = @_;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      my $stmt = $pu->firstChild;
      (my $kind = $stmt->nodeName) =~ s/-stmt$//o;

      if ($kind eq 'module')
        {
          &processSingleModule ($pu, %opts);
        }
      elsif ($kind eq 'subroutine')
        {
          &processSingleRoutine ($pu, %opts);
        }
      else
        {
          die;
        }

    }

}

sub processSingleModule
{
  my ($d, %opts) = @_;

  for my $pu (&F ('./program-unit', $d))
    {
      &processSingleRoutine ($pu, %opts);
    }

  &Fxtran::Module::addSuffix ($d, $opts{'suffix-bitrepro'});

}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  &Fxtran::Intrinsic::makeBitReproducible ($pu, %opts);

  my ($ep) = &F ('./execution-part', $pu);

  &Fxtran::Call::addSuffix 
  (
    $pu,
    section => $ep,
    suffix => $opts{'suffix-bitrepro'},
    'merge-interfaces' => $opts{'merge-interfaces'},
    match => sub { my $proc = shift; ($proc ne 'ABOR1') && ($proc ne 'PRINT_MSG') },
    contained => 1,
  );

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-bitrepro'});

  for my $pu (&F ('./program-unit', $pu))
    {
      &processSingleRoutine ($pu, %opts);
    }

}

1;

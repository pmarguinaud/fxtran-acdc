package Fxtran::Style::ACRANEB2;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Style::MFPHYS);

use File::Basename;
use Fxtran;

use strict;

sub preProcessForOpenACC
{
  my $class = shift;

  my $d = shift;
  my %args = @_;

  $class->SUPER::preProcessForOpenACC ($d, %args);

  my ($pu) = &F ('object/file/program-unit', $d);

  my ($ep) = &F ('./execution-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my ($file) = &F ('./object/file/@name', $d, 2);

  $file = &basename ($file);

# 'FileHandle'->new (">1.$file")->print (&Fxtran::Canonic::indent ($d));

  # Change KJN -> KLON

  my @kjn = &F ('.//named-E[string(N)="KJN"]', $pu);

  for my $kjn (@kjn)
    {
      $kjn->replaceNode (&e ('KLON'));
    }

  # Remove loops on JN

  my @do = &F ('.//do-construct[./do-stmt[string(do-V)="JN"]]', $pu);

  for my $do (@do)
    {
      my @c = $do->childNodes ();
      my @d = (splice (@c, 0, 2), splice (@c, -2, 2));

      for (@d)
        {
          $_->unbindNode ();
        }

     my $p = $do->parentNode ();

      for (@c)
        {
          $p->insertBefore ($_, $do);
        }
      
      $do->unbindNode ();
    }

  # Remove IIDIA/IFDIA initialization

  my @assign = &F ('.//a-stmt[./E-1/named-E[string(N)="?" or string(N)="?"]]', 'IIDIA', 'IFDIA', $ep);

  for (@assign)
    {
      $_->unbindNode ();
    }


  # Use KIDIA/KFDIA instead of IIDIA/IFDIA (expressions)

  for my $T (qw (I F))
    {
      for my $e (&F ('.//named-E[string(N)="?"]', "I${T}DIA", $ep))
        {
          $e->replaceNode (&e ("K${T}DIA"));
        }
    }

  # Use KIDIA/KFDIA instead of KIIDIA/KIFDIA (expressions)

  for my $T (qw (I F))
    {
      for my $e (&F ('.//named-E[string(N)="?"]', "KI${T}DIA", $ep))
        {
          $e->replaceNode (&e ("K${T}DIA"));
        }
    }

  # Make KIIDIA/KIFDIA arguments scalars

  for my $N (qw (KIIDIA KIFDIA))
    {
      if (my ($as) = &F ('./T-decl-stmt//EN-decl[string(EN-N)="?"]/array-spec', $N, $dp))
        {
          $as->unbindNode ();
        }
    }

  # Change KIIDIA/KIFDIA into KIDIA/KFDIA (arguments), unless KIDIA/KFDIA are already arguments of the routine
  
  my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $pu);

  unless (grep { $_->textContent eq 'KIDIA' } @arg)
    {
      for my $arg (@arg)
        {
          if ($arg->textContent =~ m/^KI([IF])DIA$/o)
            {
              my $T = $1;
              $arg->setData ("K${T}DIA");
            }
        }
      my @en_decl = &F ('./T-decl-stmt//EN-decl[string(EN-N)="KIIDIA" or string(EN-N)="KIFDIA"]/EN-N/N/n/text()', $dp);
      for my $en_decl (@en_decl)
        {
          (my $n = $en_decl->textContent) =~ s/^KI/K/o;
          $en_decl->setData ($n);
        }
    }

  
# 'FileHandle'->new (">2.$file")->print (&Fxtran::Canonic::indent ($d));
}

sub matchDocument
{
  shift;
  my $d = shift;

  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KLON"]', $d));
  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KJN"]', $d));

  return 1 if (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KIIDIA"]', $d));
  return 1 if (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt/EN-decl-LT/EN-decl[string(EN-N)="IIDIA"]', $d));
  
  return;
}

1;

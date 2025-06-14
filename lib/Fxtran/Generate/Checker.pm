package Fxtran::Generate::Checker;

use Data::Dumper;

use Fxtran;

use strict;

sub reportStatement
{
  use Text::Wrap;

  my $width = 80;

  my ($mess, $stmt) = @_;

  local $Text::Wrap::columns = $width;

  print "-" x $width, "\n\n";

  print &wrap ('', '', $mess . ':') . "\n\n";

  local $Text::Wrap::separator = " &\n";

  print &wrap ('   ', ' & ', $stmt->textContent), "\n" x 2;
}

sub checkVariables
{
  my ($pu, %opts) = @_;

  my $style = $opts{style};

  my @nproma = $style->nproma ();

  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);

  for my $decl (&F ('./T-decl-stmt', $dp))
    {
      my ($N) = &F ('./EN-decl-LT/EN-decl/EN-N', $decl, 1);

      my $dummy = grep { $N eq $_ } @arg;

      my @ss = &F ('./EN-decl-LT/EN-decl/array-spec/shape-spec-LT/shape-spec', $decl);
      my ($ts) = &F ('./_T-spec_/ANY-T-spec', $decl);
      my ($intent) = &F ('./attribute/intent-spec', $decl, 1); $intent ||= '';
      my ($pointer) = &F ('./attribute[string(attribute-N)="POINTER"]', $decl);
      my ($allocatable) = &F ('./attribute[string(attribute-N)="ALLOCATABLE"]', $decl);
   
      if ((! $intent) && $dummy)
        {
          &reportStatement ("Dummy argument $N must have the INTENT attribute", $decl);
        }

      if (($ts->nodeName eq 'derived-T-spec') && ($intent ne 'IN') && $dummy)
        {
          &reportStatement ("Dummy argument $N must have the INTENT (IN) attribute", $decl);
        }

      if (($ts->nodeName eq 'derived-T-spec') && (! $dummy))
        {
          &reportStatement ("Local variable with derived type $N is forbidden", $decl);
        }

      if (($ts->nodeName eq 'derived-T-spec') && scalar (@ss))
        {
          &reportStatement ("Derived type array $N is forbidden", $decl);
        }

      if ($dummy && (scalar (@ss) == 0) && ($intent ne 'IN') && $intent && ($ts->nodeName ne 'derived-T-spec'))
        {
          &reportStatement ("Dummy argument $N with intent $intent is forbidden", $decl);
        }
     
      if (@ss)
        {
          if ((! $allocatable) && (! $pointer) && grep { $_->textContent eq ':' } @ss)
            {
              &reportStatement ("Assumed shape array $N is forbidden", $decl);
            }
          elsif (! grep { $ss[0]->textContent eq $_ } @nproma)
            {
              my @expr = &F ('./ANY-bound/ANY-E', $ss[0]);
              &reportStatement ("Non NPROMA array $N with unknown compile-time size is forbidden", $decl)
                if (grep { $_->nodeName eq 'named-E' } @expr);
            }
        }

      if ($allocatable)
        {
          &reportStatement ("Allocatable variable $N is forbidden", $decl);
        }

      if ($pointer)
        {
          &reportStatement ("Pointer variable $N is forbidden", $decl);
        }
    }
}

sub checkArraySyntax
{
  my ($pu, %opts) = @_;

  my ($ep) = &F ('./execution-part', $pu);

  my %seen;

  for my $ar (&F ('.//array-R[./section-subscript-LT/section-subscript/text()[string(.)=":"]]', $pu))
    {
      my $stmt = &Fxtran::stmt ($ar);
      next if ($seen{$stmt->unique_key ()}++);
      if ($stmt->nodeName eq 'a-stmt')
        {
          my ($E2) = &F ('./E-2/ANY-E', $stmt);
          if (($E2->nodeName ne 'named-E') && ($E2->nodeName ne 'litteral-E'))
            {
              &reportStatement ("Assignment statement is forbidden : array syntax is limited to array copy or initialization", $stmt);
            }
        }
      elsif ($stmt->nodeName eq 'call-stmt')
        {
          my @ss = &F ('./section-subscript-LT/section-subscript', $ar);

          my $restrict;

          while (my $ss = shift (@ss))
            {
              my ($dd) = &F ('./text()[string(.)=":"]', $ss);
              my $full = $ss->textContent eq ':';

              if ($restrict)
                {
                  if ($dd)
                    {
                      &reportStatement ("Call statement is forbidden because of non-contiguous array section", $stmt);
                      last;
                    }
                }
              else
                {
                  if ($dd)
                    {
                      $restrict = 1 unless ($full);
                    }
                  else
                    {
                      $restrict = 1;
                    }
                }
                 
            }
        }
    }
}

sub checkExpressions
{
  my ($pu, %opts) = @_;

  my $style = $opts{style};

  my @nproma = $style->nproma ();
  my $jlon = $style->jlon ();
  my $kidia = $style->kidia ();
  my $kfdia = $style->kfdia ();

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my @N;
  
  for my $decl (&F ('./T-decl-stmt[./EN-decl-LT/EN-decl/array-spec]', $dp))
    {
      my ($en_decl) = &F ('./EN-decl-LT/EN-decl', $decl);
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($ss) = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl);
      push @N, $N, if (grep { $_ eq $ss->textContent } @nproma);
    }


  for my $expr (&F ('.//named-E[./R-LT/array-R]', $ep))
    {
      my ($N) = &F ('./N', $expr, 1);
      next unless (grep { $N eq $_ } @N);
      my $stmt = &Fxtran::stmt ($expr);
      my @ss = &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $expr, 1);

      next if ($ss[0] eq ':');

      if ($ss[0] =~ m/:/o) 
        {
          for my $nproma (@nproma)
            {
              goto OK if ($ss[0] eq "1:$nproma");
            }
          if ($ss[0] ne "$kidia:$kfdia")
            {
              &reportStatement ("NPROMA dimensioned array should be indexed with $kidia:$kfdia", $stmt);
            }
OK:
        }
      elsif ($ss[0] ne $jlon)
        {
          &reportStatement ("NPROMA dimensioned array should be indexed with $jlon", $stmt);
        }
    }

}

sub singlecolumn
{
  shift;
  my ($d, %opts) = @_;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      &checkVariables ($pu, %opts);
      &checkArraySyntax ($pu, %opts);
      &checkExpressions ($pu, %opts);
    }
}

sub pointerparallel
{
  shift;
  my ($d, %opts) = @_;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      &checkVariables ($pu, %opts);
      &checkArraySyntax ($pu, %opts);
      &checkExpressions ($pu, %opts);
    }
}

sub manyblocks
{
  shift;
  my ($d, %opts) = @_;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      &checkVariables ($pu, %opts);
      &checkArraySyntax ($pu, %opts);
      &checkExpressions ($pu, %opts);
    }
}

sub singleblock
{
  shift;
  my ($d, %opts) = @_;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      &checkVariables ($pu, %opts);
      &checkArraySyntax ($pu, %opts);
      &checkExpressions ($pu, %opts);
    }
}

1;

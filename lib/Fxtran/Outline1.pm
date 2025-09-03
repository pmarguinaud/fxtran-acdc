package Fxtran::Outline1;

=head1 NAME

Fxtran::Outline1

=head1 DESCRIPTION

The purpose of this module is to move sections of code from a 
subroutine into external routines. These code sections are 
replaced by a call to the external routine.

=head1 EXAMPLE

For instance the following routine:

  SUBROUTINE SUBR (YD, P, K)
  
  USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK
  USE YOMTT,ONLY:TT
  
  IMPLICIT NONE
  
  TYPE (TT), INTENT (INOUT)::YD
  REAL, INTENT (OUT)::P (K)
  INTEGER, INTENT (IN)::K
  
  INTEGER::I, J
  REAL :: ZZ
  
  REAL (KIND=JPHOOK) :: ZHOOK_HANDLE
  
  IF (LHOOK) CALL DR_HOOK ('SUBR', 0, ZHOOK_HANDLE)
  
  ZZ = 34.
  
  !$ACDC PARALLEL {
  
  IF (YD%LL) THEN
    P (:)=12.
  ENDIF
  
  !$ACDC }
  
  !$ACDC PARALLEL {
  
  DO I = 1, 100
    YD%ZZ (I) = 12. + ZZ
  ENDDO
  
  !$ACDC }
  
  IF (LHOOK) CALL DR_HOOK ('SUBR', 1, ZHOOK_HANDLE)
  
  END SUBROUTINE

is transformed into:

  SUBROUTINE SUBR (YD, P, K)
  USE YOMHOOK,ONLY:LHOOK, DR_HOOK, JPHOOK
  USE YOMTT,ONLY:TT
  
  IMPLICIT NONE
  
  TYPE (TT), INTENT (INOUT)::YD
  REAL, INTENT (OUT)::P (K)
  INTEGER, INTENT (IN)::K
  INTEGER::J
  
  REAL::ZZ
  REAL (KIND=JPHOOK)::ZHOOK_HANDLE
  #include "subr_outline_000.intfb.h"
  #include "subr_outline_001.intfb.h"
  IF (LHOOK) CALL DR_HOOK ('SUBR', 0, ZHOOK_HANDLE)
  ZZ=34.
  CALL SUBR_OUTLINE_000 (K, P, YD)
  CALL SUBR_OUTLINE_001 (ZZ, YD)
  IF (LHOOK) CALL DR_HOOK ('SUBR', 1, ZHOOK_HANDLE)
  END SUBROUTINE

and the following two routines are created in separate files, as well as
their interfaces:

  SUBROUTINE SUBR_OUTLINE_000 (K, P, YD) 
  USE YOMTT,ONLY:TT
  USE YOMHOOK,ONLY:LHOOK, JPHOOK, DR_HOOK
  
  IMPLICIT NONE
  
  INTEGER, INTENT (IN)::K
  REAL, INTENT (OUT)::P (K) 
  TYPE (TT), INTENT (INOUT)::YD
  REAL (KIND=JPHOOK)::ZHOOK_HANDLE
  IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_000', 0, ZHOOK_HANDLE)
  
  IF (YD%LL) THEN
    P (:)=12.
  ENDIF
  
  IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_000', 1, ZHOOK_HANDLE)
  END SUBROUTINE
  
  SUBROUTINE SUBR_OUTLINE_001 (PZ, YD)
  USE YOMTT,ONLY:TT
  USE YOMHOOK,ONLY:LHOOK, JPHOOK, DR_HOOK
  
  IMPLICIT NONE
  
  REAL, INTENT (INOUT)::PZ
  TYPE (TT), INTENT (INOUT)::YD
  INTEGER::I 
  REAL (KIND=JPHOOK)::ZHOOK_HANDLE
  IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_001', 0, ZHOOK_HANDLE)
  
  DO I=1, 100
    YD%ZZ (I)=12.+PZ
  ENDDO
  
  IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_001', 1, ZHOOK_HANDLE)
  END SUBROUTINE

=head1 MORE EXAMPLES

Examples are provided with test data; for instance:

L<apl_arpege.F90|url:../tests/49t2_openacc-outline1/src/main/arpifs/phys_dmn/apl_arpege.F90>

and:

L<apl_arpege.F90|url:../tests/49t2_openacc-outline1/ref/outline1/src/local/arpifs/phys_dmn/apl_arpege.F90>,
L<apl_arpege_zde2mr.F90|url:../tests/49t2_openacc-outline1/ref/outline1/src/local/arpifs/phys_dmn/apl_arpege_zde2mr.F90>,
L<apl_arpege_zbay_qrconv.F90|url:../tests/49t2_openacc-outline1/ref/outline1/src/local/arpifs/phys_dmn/apl_arpege_zbay_qrconv.F90>,
etc ...

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;
use FileHandle;
use File::Basename;
use List::MoreUtils qw (uniq);

use strict;

use Fxtran;
use Fxtran::Intrinsic;
use Fxtran::Include;
use Fxtran::Canonic;
use Fxtran::Directive;
use Fxtran::Inline;
use Fxtran::Finder;
use Fxtran::Util;

use click;

sub variableDependencies
{
  my ($pu, $n, %opts) = @_;

  my ($up) = &F ('./specification-part/use-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my ($decl, $use, $include, @n, $intrinsic);

  # Declaration

  if (($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string (EN-N)="?"]]', $n, $dp))
    {
      @n = grep { $_ ne $n } &uniq (&F ('.//n', $decl, 1));
      goto FOUND;
    }

  # Imported from use
  goto FOUND if (($use) = &F ('./use-stmt[./rename-LT/rename[string(use-N)="?"]]', $n, $up));

  # Interface 
  goto FOUND if (($include) = &F ('./include[string(filename)="?" or string(filename)="?"]', lc ($n) . '.h', lc ($n) . '.intfb.h', $dp));

  # Statement function from include file
  if ($n =~ m/^F/o) 
    {
      my $find = $opts{find};
      for my $inc (&F ('./include[contains(string(filename),".func.h")]', $dp))
        {
          my ($file) = &F ('./filename', $inc, 2);
          $file = $find->resolve (file => $file);
          my $text = uc (&Fxtran::Util::slurp ($file));

          if (index ($text, $n) >= 0)
            {
              $include = $inc;
              goto FOUND;
            }
        }
    }

FOUND:

  my $stmt = ($decl or $use or $include);

  unless ($stmt)
    {
      die ("Declaration of `$n' was not found") 
        unless (&Fxtran::Intrinsic::isIntrinsic ($n));
      $intrinsic = 1;
    }

  return {
           stmt => $stmt,                 # Declaration statement for this symbol (may be a T-decl-stmt, a use-stmt or an include node)
           name => \@n,                   # Other symbols we depend on for declaring this symbol
           intrinsic => $intrinsic,
         };
}

sub getVariables
{
  my $pu = shift;
  my %opts = @_;

  my @n_expr = &F ('.//named-E/N/n', $pu, 1); my %n_expr; $n_expr{$_}++ for (@n_expr);
  my @n_rename = &F ('.//rename', $pu, 1); 

  my @n = &uniq (@n_expr, @n_rename);
  
  my %var;
  
  for my $n (@n)
    {
      $var{$n} = &variableDependencies ($pu, $n, %opts);
      $var{$n}{count} = $n_expr{$n} if (exists $n_expr{$n}); # Number of occurences of symbol in $pu
    }

  # Rank symbols : 
  # - symbols with rank 0 do not depend on other symbols
  # - symbols with rank 1 depend on a single other symbol
  # - etc.
  
  my $doRank;
  
  $doRank = sub
  {
    my $n = shift;
  
    (my $var = $var{$n}) or die ("Unknonw symbol `$n'");
  
    if (exists ($var->{rank}))
      {
        return $var->{rank};
      }
  
    my $rank = 0;
  
    for my $n (@{ $var->{name} })
      { 
        my $r = $doRank->($n);
        $rank = $r if ($r > $rank);
      }
  
    $var->{rank} = 1 + $rank;
  
    return $var->{rank};
  };
  
  $doRank->($_) for (@n);

  return \%var;
}

sub outline
{
# outline $sect from $pu
# $VAR is the hash returned by getVariables

  my $pu = shift;
  my %args = @_;

  my ($sect, $sectName, $VAR) = @args{qw (section sectionName variables)};

  my @nn_expr = &F ('.//named-E/N', $sect, 1); my %nn_expr; $nn_expr{$_}++ for (@nn_expr); # Number of occurences of each symbol
  my @nn = &uniq (@nn_expr);

  # function statement includes : when present, we need to force YDCST presence

  my @func = &F ('./specification-part/declaration-part/include[contains(filename, ".func.h")]', $pu);

  if (@func && $VAR->{YDCST})
    {
      push @nn, 'YDCST';
    }

  # Select symbols used in section + their dependencies from $VAR
  
  my %nn;

  my $doSelect;
  
  $doSelect = sub
  {
    my $n = shift;
  
    return if ($nn{$n});
  
    $nn{$n} = 1;
  
    (my $var = $VAR->{$n}) or die;
  
    for my $n (@{ $var->{name} })
      {
        $doSelect->($n);
      }
  };
  
  $doSelect->($_) for (@nn);
  
  @nn = sort { ($VAR->{$a}{rank} <=> $VAR->{$b}{rank}) || ($a cmp $b) } keys (%nn);

  # Select statements for outlining
  
  my (@use, @include, @declArg, @declLocal);
  
  my %seen; # Statements already visited

  my @args; # Arguments of outlined routine

  my %do = map { ($_, 1) } (&F ('.//do-V', $sect, 1), &F ('.//do-construct//a-stmt/E-1/named-E[not (./R-LT/array-R)]/N', $sect, 1));

  for my $nn (@nn)
    {
      (my $var = $VAR->{$nn}) or die;

      next if ($var->{intrinsic});

      (my $stmtPu = $var->{stmt}) or die &Dumper ([$nn, $var]);

      next if ($seen{$stmtPu->unique_key ()}++);
  
      if (exists $nn_expr{$nn})
        {
          $var->{count} = $var->{count} - $nn_expr{$nn};
        }

      my $stmt = $stmtPu->cloneNode (1);

      my $stmtType = $stmt->nodeName;

      if ($stmtType eq 'use-stmt')
        {
          push @use, $stmt;
        }
      elsif ($stmtType eq 'include')
        {
          push @include, $stmt;

          if ($var->{count} == 0)
            {
              # Remove unneeded includes
              $stmtPu->unbindNode ();
            }
        }
      elsif ($stmtType eq 'T-decl-stmt')
        {
          my $isPuArg = &F ('./attribute[string(attribute-N)="INTENT"]', $stmt);
          if ($do{$nn})
            {
              push @declLocal, $stmt;
              if ($var->{count} == 0)
                {
                  # Remove unneeded local
                  $stmtPu->unbindNode ();
                }
            }
          elsif ((! $isPuArg) && ($var->{count} == 0))
            {
              push @declLocal, $stmt;
              $stmtPu->unbindNode (); # Remove unneeded local
            }
          else
            {
              # Add INTENT (INOUT) for arguments without INTENT (local variables of $pu)
              unless ($isPuArg)
                {
                  my ($ts) = &F ('./_T-spec_', $stmt);

                  my $INTENT = 'INOUT';

                  if (my ($param) = &F ('./attribute[string(attribute-N)="PARAMETER"]', $stmt))
                    {
                      # Remove PARAMETER attribute
                      $param->previousSibling->unbindNode; # Remove preceding comma
                      $param->unbindNode;

                      # Remove PARAMETER value
                      my ($init) = &F ('.//init-E', $stmt);
                      $init->previousSibling->unbindNode;
                      $init->unbindNode; # Remove =
                     
                      $INTENT = 'IN';
                    }
                  $stmt->insertAfter ($_, $ts) 
                    for (&n ("<attribute><attribute-N>INTENT</attribute-N> (<intent-spec>$INTENT</intent-spec>)</attribute>"), &t (', '));
                }
              push @declArg, $stmt;
              push @args, $nn;
            }
        }
      else
        {
          die $stmt->textContent;
        }
    }

  # Increment argument count use (in $pu)

  for my $nn (@args)
    {
      (my $var = $VAR->{$nn}) or die;
      $var->{count} = $var->{count} + 1;
    }

  # Create the call statement before renaming arguments
  
  my $call = &s ("CALL $sectName (" . join (', ', @args) . ')');
    
  # Rename arguments using DOCTOR norm

  if (1)
  {
    for my $argo (@args)
      {
        my $argn = $argo;

        if (($argn =~ s/^YL/YD/o) || ($argn =~ s/^Z/P/o) || ($argn =~ s/^LL/LD/o) || ($argn =~ s/^I/K/o))
          {
            next if ($nn{$argn});

            for my $x (@declArg, @declLocal, $sect)
              {
                my @n = &F ('.//N/n[string(.)="?"]/text()', $argo, $x);
                for (@n)
                  {
                    $_->setData ($argn);
                  }
              }
          }


        $argo = $argn;
      }
  }

  # Dump routine definition + interface
  
  my ($body, $intf) = ('', '');

  push @use, &s ("USE YOMHOOK, ONLY : LHOOK, JPHOOK, DR_HOOK");
  push @declLocal, &s ("REAL (KIND=JPHOOK) :: ZHOOK_HANDLE");
      
  for my $buf (\$body, \$intf)
    {
      $$buf .= "SUBROUTINE $sectName (" . join (', ', @args) . ")\n\n";
     
      for (@use, &t (''), &t ("IMPLICIT NONE"), &t (''), @declArg, &t (''))
        {
          $$buf .= $_->textContent . "\n";
        }
    }

  for (@include, @func, &t (''), @declLocal, &t (''))
    {
      $body .= $_->textContent . "\n";
    }
  
  $body .= << "EOF";

IF (LHOOK) CALL DR_HOOK ('$sectName',0,ZHOOK_HANDLE)

${ my $t = $sect->textContent; \$t  }

IF (LHOOK) CALL DR_HOOK ('$sectName',1,ZHOOK_HANDLE)

EOF

  for my $buf (\$body, \$intf)
    {
      $$buf .= "END SUBROUTINE\n";
    }

  # Replace section by the CALL statement 

  &Fxtran::Include::addInclude ($pu, lc ($sectName) . '.intfb.h');

  $sect->replaceNode ($call);

  &printCanonic ($args{dir} . '/' . lc ($sectName) . '.F90',     $body, 0);
  &printCanonic ($args{dir} . '/' . lc ($sectName) . '.intfb.h', $intf, 1);

}

sub printCanonic
{
  use File::Temp;

  my ($file, $text, $intf) = @_;

  my $fh;

  $fh = 'File::Temp'->new (DIR => '.', SUFFIX => '.F90', UNLINK => 1, TEMPLATE => '.XXXXXX');

  $fh->print ($text);

  $fh->close ();

  my $d = &Fxtran::parse (location => $fh->filename (), fopts => [qw (-construct-tag -no-include -line-length 5000 -canonic)]);

  $fh = 'FileHandle'->new (">$file");
  $fh->print ("INTERFACE\n") if ($intf);
  $fh->print (&Fxtran::Canonic::indent ($d));
  $fh->print ("END INTERFACE\n") if ($intf);
  $fh->close ();
}

&click (<< "EOF");
  dir=s                     -- Dump result in this directory                                                                                -- .
  tmp=s                     -- Temporary directory for processing                                                                           -- .
  inline-contained          -- Inline contained routines
EOF
sub outline1
{
  my ($opts, @args) = @_;

  &fxtran::setOptions (qw (Fragment -construct-tag -no-include -line-length 5000));
  &fxtran::setOptions (qw (Statement -line-length 5000));
  
  $opts->{find} = 'Fxtran::Finder'->new ();

  my ($F90) = @args;
  
  my $d = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -line-length 500 -directive ACDC -canonic)], dir => $opts->{tmp});
  
  &Fxtran::Canonic::makeCanonic ($d, %$opts);
  
  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');
  
  if ($opts->{'inline-contained'})
    {
      my @pu = &F ('./object/file/program-unit', $d);
      for my $pu (@pu)
        {
          &Fxtran::Inline::inlineContainedSubroutines ($pu, skipDimensionCheck => 1);
        }
    }
  
  my ($pu) = &F ('./object/file/program-unit', $d);
  
  my ($puName) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);
  
  my $vars = &Fxtran::Outline1::getVariables ($pu, %$opts);
  
  my @par = &F ('.//parallel-section', $d);
  
  my %parName;
  
  for my $i (0 .. $#par)
    {
      my $par = $par[$i];
  
      my $parName = $par->getAttribute ('name') || sprintf ('OUTLINE_%3.3d', $i);
  
      die ("Duplicate section name `$parName'") 
        if ($parName{$parName}++);
  
      &Fxtran::Outline1::outline ($pu, section => $par, sectionName => $puName. '_' . $parName, 
                                  variables => $vars, dir => $opts->{dir});
    }
  
  'FileHandle'->new (">$opts->{dir}/" . &basename ($F90))->print (&Fxtran::Canonic::indent ($d));

}

1;

package Outline1;

use strict;
use Fxtran;
use Intrinsic;
use Include;

use Data::Dumper;
use List::MoreUtils qw (uniq);

sub variableDependencies
{
  my ($pu, $n) = @_;

  my ($up) = &F ('./specification-part/use-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my ($decl, $use, $include);

  ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string (EN-N)="?"]]', $n, $dp);

  my $use;

  if (! $decl)
    {
      ($use) = &F ('./use-stmt[./rename-LT/rename[string(use-N)="?"]]', $n, $up);
    }

  if ((! $decl) && (! $use))
    {
      ($include) = &F ('./include[string(filename)="?" or string(filename)="?"]', lc ($n) . '.h', lc ($n) . '.intfb.h', $dp);
    }

  my @n;

  if ($decl)
    {
      @n = grep { $_ ne $n } &uniq (&F ('.//n', $decl, 1));
    }

  my $stmt = ($decl or $use or $include);

  my $intrinsic;

  unless ($stmt)
    {
      die ("Declaration of `$n' was not found") 
        unless (&Intrinsic::isIntrinsic ($n));
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

  my @n_expr = &F ('.//named-E/N/n', $pu, 1); my %n_expr; $n_expr{$_}++ for (@n_expr);
  my @n_rename = &F ('.//rename', $pu, 1); 

  my @n = &uniq (@n_expr, @n_rename);
  
  my %dep;
  
  for my $n (@n)
    {
      $dep{$n} = &variableDependencies ($pu, $n);
      $dep{$n}{count} = $n_expr{$n} if (exists $n_expr{$n}); # Number of occurences of symbol in $pu
    }

  # Rank symbols : 
  # - symbols with rank 0 do not depend on other symbols
  # - symbols with rank 1 depend on a single other symbol
  # - etc.
  
  my $doRank;
  
  $doRank = sub
  {
    my $n = shift;
  
    (my $dep = $dep{$n}) or die;
  
    if (exists ($dep->{rank}))
      {
        return $dep->{rank};
      }
  
    my $rank = 0;
  
    for my $n (@{ $dep->{name} })
      { 
        my $r = $doRank->($n);
        $rank = $r if ($r > $rank);
      }
  
    $dep->{rank} = 1 + $rank;
  
    return $dep->{rank};
  };
  
  $doRank->($_) for (@n);

  return \%dep;
}

sub outline
{
# outline $sect from $pu
# $DEP is the hash returned by getVariables

  my ($pu, $sect, $sectName, $DEP) = @_;

  my @nn_expr = &F ('.//named-E/N', $sect, 1); my %nn_expr; $nn_expr{$_}++ for (@nn_expr); # Number of occurences of each symbol
  my @nn = &uniq (@nn_expr);

  # Select symbols used in section + their dependencies from $DEP
  
  my %nn;

  my $doSelect;
  
  $doSelect = sub
  {
    my $n = shift;
  
    return if ($nn{$n});
  
    $nn{$n} = 1;
  
    (my $dep = $DEP->{$n}) or die;
  
    for my $n (@{ $dep->{name} })
      {
        $doSelect->($n);
      }
  };
  
  $doSelect->($_) for (@nn);
  
  @nn = sort { $DEP->{$a}{rank} <=> $DEP->{$b}{rank} } keys (%nn);

  # Select statements for outlining
  
  my (@use, @include, @declArg, @declLocal);
  
  my %seen; # Statements already visited

  my @args; # Arguments of outlined routine

  my %do = map { ($_, 1) } (&F ('.//do-V', $sect, 1), &F ('.//do-construct//a-stmt/E-1/named-E[not (./R-LT/array-R)]/N', $sect, 1));

  for my $nn (@nn)
    {
      (my $dep = $DEP->{$nn}) or die;
      next if ($dep->{intrinsic});

      (my $stmtPu = $dep->{stmt}) or die &Dumper ([$nn, $dep]);

      next if ($seen{$stmtPu->unique_key ()}++);
  
      if (exists $nn_expr{$nn})
        {
          $dep->{count} = $dep->{count} - $nn_expr{$nn};
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

          if ($dep->{count} == 0)
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
              if ($dep->{count} == 0)
                {
                  # Remove unneeded local
                  $stmtPu->unbindNode ();
                }
            }
          elsif ((! $isPuArg) && ($dep->{count} == 0))
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
                  $stmt->insertAfter ($_, $ts) 
                   for (&n ('<attribute><attribute-N>INTENT</attribute-N> (<intent-spec>INOUT</intent-spec>)</attribute>'), &t (', '));
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

  
  # Rename arguments using DOCTOR norm
  {
    for my $argo (@args)
      {
        my $argn = $argo;
     
        if (($argn =~ s/^YL/YD/o) || ($argn =~ s/^Z/P/o) || ($argn =~ s/^LL/LD/o) || ($argn =~ s/^I/K/o))
          {
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
  
  my ($fhBody, $fhIntf);

  $fhBody = 'FileHandle'->new ('>' . lc ($sectName) . '.F90');
  $fhIntf = 'FileHandle'->new ('>' . lc ($sectName) . '.intfb.h');

  $fhIntf->print ("INTERFACE\n");

  push @use, &s ("USE YOMHOOK, ONLY : LHOOK, JPHOOK, DR_HOOK");
  push @declLocal, &s ("REAL (KIND=JPHOOK) :: ZHOOK_HANDLE");
      
  for my $fh ($fhBody, $fhIntf)
    {
      $fh->print ("SUBROUTINE $sectName (", join (', ', @args) . ")\n\n");
     
      for (@use, &t (''), &t ("IMPLICIT NONE"), &t (''), @declArg, &t (''))
        {
          $fh->print ($_->textContent, "\n");
        }
    }

  
  for (@include, &t (''), @declLocal, &t (''))
    {
      $fhBody->print ($_->textContent, "\n");
    }
  
  $fhBody->print (<< "EOF");

IF (LHOOK) CALL DR_HOOK ('$sectName',0,ZHOOK_HANDLE)

${ my $t = $sect->textContent; \$t  }

IF (LHOOK) CALL DR_HOOK ('$sectName',1,ZHOOK_HANDLE)

EOF

  for my $fh ($fhBody, $fhIntf)
    {
      $fh->print ("END SUBROUTINE\n");
    }

  $fhIntf->print ("END INTERFACE\n");

  for my $fh ($fhBody, $fhIntf)
    {
      $fh->close ();
    }

  # Replace section by a CALL statement 

  my $call = &s ("CALL $sectName (" . join (', ', @args) . ')');

  &Include::addInclude ($pu, lc ($sectName) . '.intfb.h');

  $sect->replaceNode ($call);

}

1;

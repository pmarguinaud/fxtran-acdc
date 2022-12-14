package Outline;

use strict;
use Fxtran;
use Associate;
use Directive;
use Decl;
use Scope;
use Intrinsic;

use Data::Dumper;

sub sortArgs
{
  my ($d, @args) = @_;

  my @dum = &F ('.//dummy-arg-LT/arg-N', $d, 1);

  my %dum = map { ($dum[$_], $_) } (0 .. $#dum);

  my @d = grep {   defined ($dum{$_}) } @args;
  my @l = grep { ! defined ($dum{$_}) } @args;

  @d = sort { $dum{$a} <=> $dum{$b} } @d;
  @l = sort { $a cmp $b } @l;

  return (@d, @l);
}


sub renameVariables
{
  my ($Nl, $local, $param) = @_;

  my %N2M;
  
  my %S = qw (Z P LL LD I K J K N K L LD YL YD R P Y Y);
  my @S = qw (Z LL I J N L YL R Y);
  
  my %M;

  for my $N (@{ $Nl })
    {
      for my $v (values (%S))
        {
          if (index ($N, $v) == 0)
            {
              $N2M{$N} = $N;
              $M{$N} = 1;
              last;
            }
        }
    }
  
  for my $N (@{ $Nl })
    {
      next if ($N2M{$N});
      for my $k (@S)
        {
          if (index ($N, $k) == 0)
            {
              my $v = $S{$k};
              my $M = $N;
              $M =~ s/^$k/$v/ unless ($local->{$N} || $param->{$N});
              while ($M{$M})
                {
                  $M .= '_';
                }
              $N2M{$N} = $M;
  
              last;
            }
        }
    }
  
  return \%N2M;
} 


my @PARKIND = qw (SIGN MAX MIN MOD REAL EXP ASIN FOLH SQRT PRESENT 
                    ABS TINY SUM JPIM JPRB ATAN2 COS SIN);
my %PARKIND = map { ($_, 1) } @PARKIND;

sub outlineSection
{
  my $d = shift;
  my %args = @_;

  my ($s) = @args{qw (section)};

  my $NAME = $args{name};

  ($NAME) = &F ('./@name', $s, 1) unless ($NAME);

  my $name = lc ($NAME);

  my $include = &n ("<include>#include &quot;<filename>$name.intfb.h</filename>&quot;</include>");

  # Add include for outlined subroutine

  my ($inc_target) = &F ('.//include[last()]', $d);

  unless ($inc_target)
    {
      $inc_target = &Scope::getNoExec ($d);
    }

  die unless ($inc_target);

  $inc_target->parentNode->insertAfter ($include, $inc_target);
  $inc_target->parentNode->insertAfter (&t ("\n"), $inc_target);
  
  # Subroutine template

  my ($o) = &Fxtran::parse (string => << "EOF");
SUBROUTINE $NAME ()

USE PARKIND1, ONLY : JPIM, JPRB

!

IMPLICIT NONE

!

END SUBROUTINE
EOF
  
  &Associate::resolveAssociates ($s, outer => 1);
  
  my (%N, %do, %call); # Symbols, do variables, called subroutines

  my @expr = &F ('.//named-E', $s);
  for my $expr (@expr)
    {
      my ($n) = &F ('./N/n', $expr, 1);
      next if (&Intrinsic::isIntrinsic ($n) || $PARKIND{$n});
      if ($expr->parentNode->nodeName eq 'do-V')
        {
          $do{$n} = 1;
        }
      elsif ($expr->parentNode->nodeName eq 'procedure-designator')
        {
          $call{$n} = 1;
        }
      $N{$n} = 1;
    }

  # Look for scalars modified in loops; these should not be passed as arguments
  
  my %sc;

  my @sc = &F ('.//EN-N[not(array-spec)]', $d, 1);
  
  for (&F ('.//do-construct//a-stmt/E-1[' . join (' or ', map { 'string(.)="' . $_ . '"' } @sc) . ']', $d, 1))
    {
      $sc{$_}++;
    }
  

  my @N = &sortArgs ($d, grep { (! $do{$_}) && (! $sc{$_}) && (! $call{$_}) } keys (%N));
  
  my %priv = map { ($_, 1) } @{ $args{local} || [] }; 
  my %local; # Local to outline (not used in original subroutine)
  my %param; # Parameters

  for my $N (@N)
    {
      my @expr_d = &F ('.//named-E[string(N)="?"]', $N, $d); 
      my @expr_n = &F ('.//named-E[string(N)="?"]', $N, $s);
      $local{$N} = @expr_d > @expr_n ? 0 : 1;
      if (my ($decl) = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]', $N, $d))
        {
           my ($intent) = &F ('./attribute[string(attribute-N)="INTENT"]', $decl);
           if ($intent)
             {
               $local{$N} = 0;
             }
           my ($parameter) = &F ('./attribute[string(attribute-N)="PARAMETER"]', $decl);
           if ($parameter)
             {
               $param{$N} = 1;
             }
        }
      else
        {
          die $N;
        }
    }
  
  for (keys (%priv))
    {
      $local{$_} = 1;
    }


  my %rank = map { ($N[$_], $_) } (0 .. $#N);
  
  @N = sort { ($local{$a} <=> $local{$b}) or ($rank{$a} <=> $rank{$b}) } @N;
  
  my $N2M = &renameVariables (\@N, \%local, \%param);
  
  # Targets for use & declarations insertion

  my ($C1, $C2) = &F ('.//C', $o);
  
  # Insert section contents

  for my $node (reverse (&F ('./node()', $s)))
    {
      $C2->parentNode->insertAfter ($node, $C2);
    }

  # Replace section by call statement in original subroutine

  my $call = "CALL $NAME (" . join (', ', map { $_ } grep { ! ($local{$_} || $param{$_}) } @N) . ')';

  $call = &s ($call);

  if ($args{keep_section})
    {
      $s->appendChild (&t ("\n"));
      $s->appendChild ($call);
      $s->appendChild (&t ("\n"));
    }
  else
    {
      $s->replaceNode ($call);
    }

  my @decl;

  my ($dummy_arg_LT) = &F ('.//dummy-arg-LT', $o);

  my $narg = 0;
  for my $N (@N)
    {
      $N2M->{$N} or next;
  
      # Variable has to be passed by argument

      if (! ($local{$N} || $param{$N}))
        {
          $dummy_arg_LT->appendChild (&t (", ")) if ($narg++);
          $dummy_arg_LT->appendChild (&n ("<arg-N><N><n>$N2M->{$N}</n></N></arg-N>"));
        }
  
      my @n = &F ('.//named-E[string(N)="?"]/N/n/text()', $N, $o);
      for my $n (@n)
        {
          $n->replaceNode (&t ($N2M->{$N}));
        }
  
      # Copy declaration from original subroutine
      if (my ($decl) = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]', $N, $d))
        {
          $decl = $decl->cloneNode (1);
          my ($n) = &F ('.//EN-decl[string(EN-N)="?"]/EN-N/N/n/text()', $N, $decl);
          my @ts = &F ('.//derived-T-spec/T-N', $decl, 1);
  
          for my $ts (@ts)
            {
              my ($use) = &F ('.//use-stmt[.//use-N[string(.)="?"]]', $ts, $d);
              die $ts unless ($use);
              $use = $use->cloneNode (1);
              &Decl::use ($o, $use);
            }
  
          $n->replaceNode (&t ($N2M->{$N}));
          $C2->parentNode->insertBefore ($decl, $C2);
          $C2->parentNode->insertBefore (&t ("\n"), $C2);

        }
      else # Variable from module : guess its type from DOCTOR rules
        {
          if ($N =~ m/^[KIJMN]/o)
            {
              push @decl, "INTEGER (KIND=JPIM), INTENT (IN) :: $N2M->{$N}";
            }
          elsif ($N =~ m/^L/o)
            {
              push @decl, "LOGICAL, INTENT (IN) :: $N2M->{$N}";
            }
          elsif ($N =~ m/^R/o)
            {
              push @decl, "REAL (KIND=JPRB), INTENT (IN) :: $N2M->{$N}";
            }
          else
            {
              die $N;
            }
        }
  
    }

  &Decl::declare ($o, @decl);
  
  # Add required interfaces in new routine

  for my $call (sort keys (%call))
    {
      $C2->parentNode->insertBefore (&n ("<include>#include &quot;<filename>" . lc ($call) . ".intfb.h</filename>&quot;</include>"), $C2); 
      $C2->parentNode->insertBefore (&t ("\n"), $C2);
    }
  
  $C2->parentNode->insertBefore (&t ("\n"), $C2);
  
  # Declare do variables

  for my $v (sort keys (%do))
    {
      $C2->parentNode->insertBefore (&Fxtran::parse (statement => "INTEGER (KIND=JPIM) :: $v"), $C2);
      $C2->parentNode->insertBefore (&t ("\n"), $C2);
    }
  
  # Remove targets
  $C1->unbindNode ();
  $C2->unbindNode ();
  
  # Remove unused variables in original routine
  
  for my $N (@N)
    {
      next unless ($local{$N} && (! $priv{$N}));
      my ($en_decl) = &F ('.//EN-decl[string(EN-N)="?"]', $N, $d);
      die $N unless ($en_decl);
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($intent) = &F ('./attribute[string(attribute-N)="INTENT"]', $stmt);
      unless ($intent)
        {
          $stmt->unbindNode ();
        }
    }
  
  return [$o, $call, $include];
}


sub outline
{
  my $d = shift;
  
  my @s = &Directive::parseDirectives ($d);

  my @o;

  for my $s (@s)
    {
      push @o, &outlineSection ($d, section => $s);
    }

  return @o;
}

1;

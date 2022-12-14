package Inline;
#
use strict;
use FileHandle;
use Data::Dumper;
use Fxtran;
use Ref;
use Decl;
use DrHook;

sub replaceDummyArgumentByActual
{
  my ($e, $a) = @_;

  if ($a->nodeName eq 'named-E')
    {
      return &replaceDummyArgumentByActualNamedE ($e, $a);
    }
  elsif ($a->nodeName eq 'op-E')
    {
      return &replaceDummyArgumentByActualOpE ($e, $a);
    }
  elsif ($a->nodeName eq 'literal-E')
    {
      die $a->toString if (&F ('./R-LT', $e));
      $e->replaceNode ($a->cloneNode (1));
    }

}

sub replaceDummyArgumentByActualOpE
{
  my ($e, $a) = @_;

  $a = &n ('<parens-E>(' . $a->textContent . ')</parens-E>');

  my @re = &F ('./R-LT/ANY-R', $e);

  die $a->toString if (@re);

  $e->replaceNode ($a);

}

sub resolveArrayRef
{
  my ($ra, $re) = @_;

# print &Dumper ([$ra->textContent, $ra->toString, $re->textContent, $re->toString]);

  $ra = $ra->cloneNode (1);

  my $ee = sub
  {
    my $r = shift;
    if ($r->nodeName eq 'parens-R')
      {
        &Ref::parensToArrayRef ($r);
      }
    if ($r->nodeName eq 'array-R')
      {
        return &F ('./section-subscript-LT/section-subscript', $r);
      }
    else
      {
       die $r;
      }
  };

  my @ele = $ee->($re); #print &Dumper ([map { $_->toString } @ele]);
  my @ssa = $ee->($ra); #print &Dumper ([map { $_->toString } @ssa]);

  for (my ($ia, $ie) = (0, 0); $ia < @ssa; $ia++)
    {
      if ($ssa[$ia]->textContent eq ':')
        {
          $ssa[$ia]->replaceNode ($ele[$ie]->cloneNode (1));
          $ie++;
        }
    }
    
  

# print &Dumper ([$ra->textContent]);

  $re->replaceNode ($ra);
}

sub replaceDummyArgumentByActualNamedE
{
  my ($e, $a) = @_;

  my ($ne) = &F ('./N/n/text()', $e);
  my ($na) = &F ('./N/n/text()', $a);

  my @re = &F ('./R-LT/ANY-R', $e);
  my @ra = &F ('./R-LT/ANY-R', $a);

  my $se = $e->toString; my $te = $e->textContent;
  my $sa = $a->toString; my $ta = $a->textContent;

  # Use actual argument name

  $ne->replaceNode (&t ($na->textContent));

  return unless (@ra);

  my ($rlte) = &F ('./R-LT', $e);
  unless ($rlte)
    {
      $e->appendChild ($rlte = &n ('<R-LT/>'));
    }

  my $re = $re[0];
  while (my $ra = shift (@ra))
    {
      if ((scalar (@ra) == 0) && ($ra->nodeName =~ m/^(?:parens-R|array-R)$/o) 
                              && ($re->nodeName =~ m/^(?:parens-R|array-R)$/o))
        {
          &resolveArrayRef ($ra, $re);
        }
      elsif ($ra->nodeName eq 'component-R')
        {
          if ($re)
            {
              $rlte->insertBefore ($ra->cloneNode (1), $re);
            }
          else
            {
              $rlte->appendChild ($ra->cloneNode (1));
            }
        }
      else
        {
          die &Dumper ([$e->textContent, $a->textContent]);
        }
    }

}

sub removeStmt
{
  my $stmt = shift;
  # Remove everything until eol
 
  my @n;
  for (my $n = $stmt; $n; $n = $n->nextSibling)
    {
      push @n, $n;
      if ($n->nodeName eq '#text')
        {
          last if ($n->data =~ m/\n/o);
        }
    }
  
  # Remove everything from start of line

  for (my $n = $stmt->previousSibling; $n; $n = $n->previousSibling)
    {
      if ($n->nodeName eq '#text')
        {
          if ($n->data =~ m/\n/o)
            {
              my $t = $n->data;
              $t =~ s/\n\s*$/\n/o;
              $n->setData ($t);
              last;
            }
        }
      else
        {
          last;
        }
      push @n, $n;
    }
  
  for (@n)
    {
      $_->unbindNode ();
    }
}

sub inlineSingleCall
{
  my ($d1, $d2, $s2, $n2, $call, %opts) = @_;


  my @da = &F ('./dummy-arg-LT/arg-N/N/n/text()', $s2, 1);

  # Dummy arguments to actual arguments
  my %da2aa;
  
  {
    my @arg = &F ('.//arg-spec/arg', $call);

    my $i = 0;
    for my $arg (@arg)
      {
        if (my ($argn) = &F ('./arg-N/k/text()', $arg, 1))
          {            
            my ($argv) = &F ('./ANY-E', $arg);
            $da2aa{$argn} = $argv;
          }
        else
          {
            my ($aa) = &F ('./named-E', $arg);
            $da2aa{$da[$i++]} = $aa;
          }
      }
  }
  
  # Remove dummy arguments declaration
  
  for my $da (@da)
    {
      my @en_decl = &F ('.//EN-decl/EN-N/N/n[text()="?"]', $da, $d2);
      for my $en_decl (@en_decl)
        {
          my ($stmt) = &Fxtran::stmt ($en_decl);
          &removeStmt ($stmt);
        }
    }
  
  # Replace simple IFs by IF constructs
   
  use Construct;
  &Construct::changeIfStatementsInIfConstructs ($d2);

  # Replace dummy arguments by actual arguments
  for my $da (@da)
    {
      my $aa = $da2aa{$da};

      # Handle PRESENT attribute & intrinsic
      my @present = &F ('.//named-E[translate(string(.)," ","")="PRESENT(?)"]', $da, $d2);

      for my $present (@present)
        {
          my $flag = $aa ? &TRUE () : &FALSE ();
          $present->replaceNode ($flag);
        }

      if ($aa)
        {
          my @e = &F ('.//named-E[./N/n/text()="?"]', $da, $d2);
          for my $e (@e)
            {
              &replaceDummyArgumentByActual ($e, $aa);
            }
        }
    }

  # Simplify subroutine (with PRESENT (...) expressions replaced by their values
  &Construct::apply ($d2);

  my @use = &F ('.//use-stmt', $d2);
  my @decl = &F ('.//T-decl-stmt', $d2);
  
  if (@use || @decl)
    {
      if ($opts{inlineDeclarations})
        {
          for my $use (@use)
            {
              &Decl::use ($d1, $use->cloneNode (1));
              &removeStmt ($use);
            }
          for my $decl (@decl)
            {
              &Decl::declare ($d1, $decl->cloneNode (1));
              &removeStmt ($decl);
            }
        }
    }

  # Remove possible IMPLICIT NONE statement
  
  for (&F ('.//implicit-none-stmt', $d2))
    {
      $_->unbindNode (); # Should move includes to d1
    }
  
  my @node = &F ('descendant-or-self::program-unit/node()', $d2);
  
  # Drop subroutine && end subroutine statements

  shift (@node);
  pop (@node);
  
  # Insert statements from inlined routine + a few comments
  
  $call->parentNode->insertAfter (&t ("\n"), $call);
  if ($opts{comment})
    {
      $call->parentNode->insertAfter (&n ("<C>!----- END INLINE $n2</C>"), $call);
      $call->parentNode->insertAfter (&t ("\n"), $call);
    }
  
  for my $node (reverse @node)
    {
      $call->parentNode->insertAfter ($node, $call);
    }
  
  if ($opts{comment})
    {
      # Comment old code (CALL)
      my @c = split (m/\n/o, $call->textContent ());
      for my $i (reverse (0 .. $#c))
        {
          my $c = $c[$i];
          $c = "! $c";
          $c = &t ($c);
          $c = $c->toString ();
          $call->parentNode->insertAfter (&t ("\n"), $call);
          $call->parentNode->insertAfter (&n ("<C>" . $c . "</C>"), $call);
        }
  
      $call->parentNode->insertAfter (&t ("\n"), $call);
      $call->parentNode->insertAfter (&t ("\n"), $call);

      $call->parentNode->insertAfter (&n ("<C>!----- BEGIN INLINE $n2</C>"), $call);
      $call->parentNode->insertAfter (&t ("\n"), $call);
    }
  

  # Remove CALL statement 

  $call->unbindNode ();
  
}

sub inlineContainedSubroutine
{
  my ($d1, $n2, %opts) = @_;

  # Subroutine to be inlined
  my ($D2) = &F ('.//program-unit[./subroutine-stmt[./subroutine-N/N/n/text()="?"]]', $n2, $d1);
  my ($S2) = &F ('.//subroutine-stmt', $D2);
  
  # Subroutine calls to be replaced by subroutine contents
  my @call = &F ('.//call-stmt[./procedure-designator/named-E/N/n/text()="?"]', $n2, $d1);
  
  for my $call (@call)
    {
      &inlineSingleCall ($d1, $D2->cloneNode (1), $S2->cloneNode (1), $n2, $call, %opts);
    }
}


sub inlineContainedSubroutines
{
  my ($d1, %opts) = @_;

  my @n2 = &F ('.//program-unit//program-unit/subroutine-stmt/subroutine-N/N/n/text()', $d1);

  for my $n2 (@n2)
    {
      &inlineContainedSubroutine ($d1, $n2, %opts);
    }
  
  for (&F ('.//program-unit//program-unit', $d1))
    {
      $_->unbindNode ();
    }
  
  for (&F ('.//program-unit//contains-stmt', $d1))
    {
      $_->unbindNode ();
    }
}

sub suffixVariables
{
  my ($d, %opts) = @_;
  my $suffix = $opts{suffix};

  my @en_decl = &F ('.//T-decl-stmt//EN-decl/EN-N/N/n/text()', $d);

  my @skip = qw (JLON JLEV);
  my %skip = map { ($_, 1) } @skip;
  
  my %N;
  for my $en_decl (@en_decl)
    {
      my $N = $en_decl->data;
      next if ($skip{$N});
      $N{$N} = $N . $suffix;
      $en_decl->setData ($N . $suffix);
    }

  for my $expr 
    (&F ('./subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $d), 
     &F ('.//named-E/N/n/text()', $d))
    {
      my $N = $expr->data;
      next unless ($N{$N});
      $expr->setData ($N{$N});
    }

  return \%N;
}

sub inlineExternalSubroutine
{
  my ($d1, $d2, %opts) = @_;

  my ($D2) = &F ('./object/file/program-unit', $d2);
  my ($S2) = &F ('./subroutine-stmt', $D2);
  my ($n2) = &F ('./subroutine-N/N/n/text()', $S2);
  
  # Subroutine calls to be replaced by subroutine contents
  my @call = &F ('.//call-stmt[./procedure-designator/named-E/N/n/text()="?"]', $n2, $d1);

  my @rename;

  for my $i (0 .. $#call)
    {
      my $call = $call[$i];
      my $suffix = "_$n2";
      $suffix .= sprintf ('%3.3d', $i) if (scalar (@call) > 1);
      my $DD2 = $D2->cloneNode (1);
      &DrHook::remove ($DD2);
      my ($SS2) = &F ('./subroutine-stmt', $DD2);
  
      push @rename, &suffixVariables ($DD2, suffix => $suffix);

      &inlineSingleCall ($d1, $DD2, $SS2, $n2, $call, %opts, inlineDeclarations => 1);
    }

  my @include = &F ('./object/file/program-unit/include[string(filename)="?"]', lc ($n2) . '.intfb.h', $d1);
  &removeStmt ($_) for (@include);

  my %N;

  for my $rename (@rename)
    {
      for my $N (sort keys (%$rename)) 
        {
          push @{ $N{$N} }, $rename->{$N};
        }
    }

  for my $N (sort keys (%N))
    {
      my @N = @{ $N{$N} };
      my @en_decl = map { &F ('./object/file/program-unit/T-decl-stmt//EN-decl[string(EN-N)="?"]', $_, $d1) } @N;
      next unless (@en_decl); # May be an argument

      my @as = map { &F ('./array-spec', $_, 1) || '' } @en_decl;
      my $eq = ! grep { $_ ne $as[0] } @as;
      if ($eq)
        {
          # Merge all declarations into a single one
          my $en_decl = shift (@en_decl);
          for (@en_decl)
            {
              my $decl = &Fxtran::stmt ($_);
              &removeStmt ($decl);
            }

          my @expr = &F ('.//named-E/N/n/text()[' . join (' or ', map { "string(.)=\"$_\"" } @N) . ']', $d1);
          for my $expr (@expr)
            {
              $expr->setData ($N);
            }
           
          my ($ENN) = &F ('./EN-N/N/n/text()', $en_decl);
          $ENN->setData ($N);
        }
    }
}


1;

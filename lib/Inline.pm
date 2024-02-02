package Inline;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

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
    
  

  $re->replaceNode ($ra);
}

sub replaceDummyArgumentByActualNamedE
{
  my ($e, $a) = @_;

  my ($ne) = &F ('./N/n/text()', $e);
  my ($na) = &F ('./N/n/text()', $a);

  my @re = &F ('./R-LT/ANY-R', $e);
  my @ra = &F ('./R-LT/ANY-R', $a);

  if (! @re)
    {
      $e->replaceNode ($a->cloneNode (1));
      goto END;
    }

  my $se = $e->toString; my $te = $e->textContent;
  my $sa = $a->toString; my $ta = $a->textContent;

  # Use actual argument name

  $ne->replaceNode (&t ($na->textContent));

  goto END unless (@ra);

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
      elsif ($re)
        {
          die &Dumper ([$e->textContent, $a->textContent]);
        }
    }

END: 

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

  &Decl::forceSingleDecl ($d2);

  my @da = &F ('./dummy-arg-LT/arg-N/N/n/text()', $s2, 1);

  # Dummy arguments to actual arguments
  my %da2aa;
  
  {
    my @arg = &F ('.//arg-spec/arg', $call);

    my $i = 0;
    for my $arg (@arg)
      {
        my $aa;
        if (my ($argn) = &F ('./arg-N/k/text()', $arg, 1))
          {            
            ($aa) = &F ('./ANY-E', $arg);
            $da2aa{$argn} = $aa;
          }
        else
          {
            ($aa) = &F ('./ANY-E', $arg);
            $da2aa{$da[$i++]} = $aa;
          }
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

  # Remove dummy arguments declaration & check dimension consistency
  
  for my $da (@da)
    {
      my ($en_decl_da) = &F ('./T-decl-stmt/EN-decl-LT/EN-decl[string(EN-N)="?"]', $da, $d2);

      if (my $aa = $da2aa{$da})
        {
          goto SKIP unless ($aa->nodeName eq 'named-E');
          my @r = &F ('./R-LT/ANY-R', $aa);

          goto SKIP if (scalar (@r) > 1); # Cannot check dimensions of structure members

          my ($n) = &F ('./N', $aa, 1);
          my ($en_decl_aa) = &F ('./T-decl-stmt/EN-decl-LT/EN-decl[string(EN-N)="?"]', $n, $d1);

          die "Declaration of $n was not found" unless ($en_decl_aa);

          my ($as_aa) = &F ('./array-spec', $en_decl_aa);
          my ($as_da) = &F ('./array-spec', $en_decl_da);

          goto SKIP unless ($as_aa);

          die $da unless ($as_da);

          my @ss_aa = &F ('./shape-spec-LT/shape-spec', $as_aa, 1);
          my @ss_da = &F ('./shape-spec-LT/shape-spec', $as_da, 1);

          die if (&F ('./R-LT/parens-R', $aa));

          for (@ss_aa, @ss_da)
            {
              s/\s+//go;
            }

    
          if (my @ss = &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $aa))
            {
              for my $i (reverse (0 .. $#ss_aa))
                {
                  next if ($ss[$i]->textContent eq ':'); # Keep
                  my @b = &F ('./ANY-bound', $ss[$i]);
                  if (scalar (@b) == 2)
                    {
                      die "Inlining $n2 is unsafe";
                    }
                  elsif (scalar (@b) == 1)
                    {
                      splice (@ss_aa, $i, 1); # Do not check
                      splice (@ss_da, $i, 1); # Do not check
                    }
                  else
                    {
                      die; # Unexpected
                    }
                }
            }
 
          unless ($opts{skipDimensionCheck})
            {
               my $mess = "Dimensions mismatch while inlining $n2: " . $en_decl_da->textContent . " vs " . $en_decl_aa->textContent;

               die $mess unless (scalar (@ss_aa) == scalar (@ss_da));
 
               for my $i (0 .. $#ss_da)
                 {
                   $ss_aa[$i] =~ s/^1://o;
                   $ss_da[$i] =~ s/^1://o;
                   die $mess if ($ss_aa[$i] ne $ss_da[$i]);
                 }
            }

SKIP:
        }

      my ($decl) = &Fxtran::stmt ($en_decl_da);
      &removeStmt ($decl);
    }
  
  # Simplify subroutine (with PRESENT (...) expressions replaced by their values
  &Construct::apply ($d2);

  # See whether some optional missing arguments remain
  for my $da (@da)
    {
      next if (my $aa = $da2aa{$da});
      my @stmt = &F ('.//ANY-stmt[.//named-E[./N/n/text()="?"]]', $da, $d2);
      for my $stmt (@stmt)
        {
          if ($stmt->nodeName eq 'if-then-stmt')
            {
              my $construct = $stmt->parentNode->parentNode;
              $construct->replaceNode (&s ('STOP 1'));
            }
          elsif ($stmt->nodeName eq 'else-if-stmt')
            {
              die;
            }
          else
            {
              $stmt->replaceNode (&s ('STOP 1'));
            }
        }
    }

  my @use = &F ('.//use-stmt', $d2);
  my @decl = &F ('.//T-decl-stmt', $d2);
  my @include = &F ('.//include', $d2);
  
  if (@use || @decl || @include)
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
          for my $include (@include)
            {
              &Decl::include ($d1, $include->cloneNode (1));
              &removeStmt ($include);
            }
        }
      else
        {
          die;
        }
    }

  # Remove possible IMPLICIT NONE statement
  
  for (&F ('.//implicit-none-stmt', $d2))
    {
      $_->unbindNode (); 
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

sub loopElementalSingleCall
{
  my ($d1, $call) = @_;

  my %dim2ind = 
  (
    'D%NIT'  => 'JI',
    'D%NIJT' => 'JIJ',
    'D%NKT'  => 'JK',
    'KLON'   => 'JLON',
    'KLEV'   => 'JLEV',
  );
  my %dim2bnd =
  (
    'D%NIT'  => [qw (D%NIB D%NIE)],
    'D%NIJT' => [qw (D%NIJB D%NIJE)],
    'KLON'   => [qw (KIDIA KFDIA)],
  );

  my @expr = &F ('./arg-spec/arg/named-E[not(R-LT/component-R)]', $call);

  my @X;

  for my $expr (@expr)
    {
      my ($N) = &F ('./N', $expr, 1);
      my ($ar) = &F ('./R-LT/array-R', $expr);

      my $dd = $ar && &F ('./section-subscript-LT/section-subscript/text()[string(.)=":"]', $ar);

      my ($en_decl) = &F ('.//T-decl-stmt//EN-decl[string (EN-N)="?"]', $N, $d1);
      my ($as) = &F ('./array-spec', $en_decl);
      unless ($as)
        {
          my ($decl) = &Fxtran::stmt ($en_decl);
          ($as) = &F ('./attribute/array-spec', $decl);
        }

      push @X, [$N, $expr, $ar, $as] if ((! $ar && $as) || ($dd));
    }

  return unless (@X);

  my (@DIM, @IND, @LBND, @UBND);

  for my $X (@X)
    {
      my ($N, $expr, $ar, $as) = @$X;
      die "$N" unless ($ar);
      die "$N" unless ($as);

      my @ssu = &F ('./section-subscript-LT/section-subscript', $ar);
      my @ssp = &F ('./shape-spec-LT/shape-spec', $as);

      my (@dim, @ind, @lbnd, @ubnd);

      for my $i (0 .. $#ssu)
        {
          my $ssu = $ssu[$i];

          next unless (&F ('./text()[string(.)=":"]', $ssu));

          my $dim = $ssp[$i];

          $dim = $dim->textContent;

          (my $ind = $dim2ind{$dim}) or die;
          $ssu->replaceNode (&n ("<section-subscript><lower-bound><named-E><N><n>$ind</n></N></named-E></lower-bound></section-subscript>"));

          push @ind, $ind;
          push @dim, $dim;

          if (exists $dim2bnd{$dim})
            {
              my ($lbnd, $ubnd) = @{ $dim2bnd{$dim} };
              if ($ssu->textContent eq ':')
                {
                  push @lbnd, $lbnd;
                  push @ubnd, $ubnd;
                }
              else
                {
                  my ($lb) = &F ('./lower-bound/ANY-E', $ssu); $lb or print $ssu;
                  my ($ub) = &F ('./upper-bound/ANY-E', $ssu); $ub or print $ssu;
                  push @lbnd, $lb->textContent;
                  push @ubnd, $ub->textContent;
                }
            }
          else
            {
              if ($ssu->textContent eq ':')
                {
                  push @lbnd, '1';
                  push @ubnd, $dim;
                }
              else
                {
                  my ($lb) = &F ('./lower-bound/ANY-E', $ssu); $lb or print $ssu;
                  my ($ub) = &F ('./upper-bound/ANY-E', $ssu); $ub or print $ssu;
                  push @lbnd, $lb->textContent;
                  push @ubnd, $ub->textContent;
                }
            }

        }

      # Check same indices for all arrays
      if (@IND)
        {
          die unless (scalar (@IND) == scalar (@ind));
          for my $i (0 .. $#IND)
            {
              die "$N: $IND[$i], $ind[$i]" unless ($IND[$i] eq $ind[$i]);
            }
        }
      else
        {
          @IND = @ind;
          @DIM = @dim;
          @LBND = @lbnd;
          @UBND = @ubnd;
        }
    }

  my $do_construct;
  
  $do_construct = join ("\n", 
                        map ({ "DO $IND[$_] = $LBND[$_], $UBND[$_]" } reverse (0 .. $#DIM)),
                        "!",
                        map ({ "ENDDO" } reverse (0 .. $#DIM)));

  ($do_construct) = &Fxtran::parse (fragment => $do_construct, fopts => [qw (-line-length 800 -construct-tag)]);

  my ($C) = &F ('.//C', $do_construct);
  
  $call->replaceNode ($do_construct);

  $C->replaceNode ($call);

}

sub loopElemental
{
  my ($d1, $n2) = @_;
  my @call = &F ('.//call-stmt[string(procedure-designator)="?"]', $n2, $d1);
  for my $call (@call)
    {
      &loopElementalSingleCall ($d1, $call);
    }
}

sub inlineContainedSubroutine
{
  my ($d1, $n2, %opts) = @_;

  # Subroutine to be inlined
  my ($D2) = &F ('.//program-unit[./subroutine-stmt[./subroutine-N/N/n/text()="?"]]', $n2, $d1);
  my ($S2) = &F ('.//subroutine-stmt', $D2);

  if (&F ('./prefix[string(.)="ELEMENTAL"]', $S2))
    {
      &loopElemental ($d1, $n2);
    }

  # Subroutine calls to be replaced by subroutine contents
  my @call = &F ('.//call-stmt[./procedure-designator/named-E/N/n/text()="?"]', $n2, $d1);

  for my $call (@call)
    {
      &inlineSingleCall ($d1, $D2->cloneNode (1), $S2->cloneNode (1), $n2, $call, %opts);
    }
}

sub loadContainedIncludes
{
  my $d = shift;
  my %opts = @_;

  my $find = $opts{find};

  my @include = &F ('.//include-stmt[preceding-sibling::contains-stmt', $d);
  for my $include (@include)
    {   
      my ($filename) = &F ('./filename', $include, 2); 
      for ($filename)
        {
          s/^"//o;
          s/"$//o;
        }

      $filename = $find->resolve (file => $filename);

      my $text = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$filename"); <$fh> };
      my $di = &Fxtran::parse (string => $text, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);
      my @pu = &F ('./object/file/program-unit', $di);
      for my $pu (@pu)
        {
          $include->parentNode->insertBefore ($pu, $include);
          $include->parentNode->insertBefore (&t ("\n"), $include);
        }
      $include->unbindNode (); 
    }   
}

sub sortContainedSubroutines
{
  my @n2 = @_;

  my %ref;

  for my $n2 (@n2)
    {
      my @pu = &F ('ancestor::program-unit', $n2);
      my $pu2 = pop (@pu);
      for my $n (@n2)
        {
          $ref{$n2->textContent}{$n->textContent} = 
          &F ('.//call-stmt[string(procedure-designator)="?"]', $n->textContent, $pu2) && 1;
        }
    }

  my @s2;

  while (%ref)
    {

      for my $n2 (@n2)
        {
          next unless (my $ref = $ref{$n2->textContent});
 
          my $ok = 1;
          for (values (%$ref))
            {
              $ok &&= ($_ == 0);
            }
 
          if ($ok)
            {
              unshift @s2, $n2;
              delete $ref{$n2->textContent};
            }

        }

      for my $s2 (@s2)
        {
          for my $ref (values (%ref))
            {
              $ref->{$s2->textContent} = 0;
            }
        }

    }

  return @s2;
}

sub inlineContainedSubroutines
{
  my ($d1, %opts) = @_;

  &loadContainedIncludes ($d1, %opts);

  my @n2 = &F ('./program-unit/subroutine-stmt/subroutine-N/N/n/text()', $d1);

  @n2 = &sortContainedSubroutines (@n2);

  for my $n2 (@n2)
    {

      &inlineContainedSubroutine ($d1, $n2, %opts);

      my @pu = &F ('ancestor::program-unit', $n2);
      my $pu2 = pop (@pu);
      $pu2->unbindNode ();

    }
  
  if ($d1->nodeName eq 'program-unit')
    {
      for (&F ('./contains-stmt', $d1))
        {
          $_->unbindNode ();
        }
    }
  else
    {
      for (&F ('.//program-unit//contains-stmt', $d1))
        {
          $_->unbindNode ();
        }
    }

}

sub suffixVariables
{
  my ($pu, %opts) = @_;
  my $suffix = $opts{suffix};

  # Do not suffix arguments, as they will be replaced anyway

  my @args = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
  my @en_decl = &F ('.//T-decl-stmt/EN-decl-LT/EN-decl/EN-N/N/n/text()', $pu);

  my @skip = qw (JL JLON JLEV JI);
  my %skip = map { ($_, 1) } (@skip, @args);
  
  my %N;
  for my $en_decl (@en_decl)
    {
      my $N = $en_decl->data;
      next if ($skip{$N});
      $N{$N} = $N . $suffix;
      $en_decl->setData ($N . $suffix);
    }

  for my $expr 
    (&F ('./subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $pu), 
     &F ('.//named-E/N/n/text()', $pu))
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

  # Record renamed local variables from inlined subroutine
  my @rename;

  for my $i (0 .. $#call)
    {
      my $call = $call[$i];
      my $suffix = "_$n2";
      $suffix .= sprintf ('%3.3d', $i) if (scalar (@call) > 1);
      my $DD2 = $D2->cloneNode (1);
      &DrHook::remove ($DD2);
      my ($SS2) = &F ('./subroutine-stmt', $DD2);
  
      my $rename = &suffixVariables ($DD2, suffix => $suffix);
      push @rename, $rename;

      for my $N (sort keys (%$rename))
        {
          my @k = &F ('./arg-spec/arg/arg-N/k/text()[string(.)="?"]', $N, $call);
          for my $k (@k)
            {
              $k->setData ($rename->{$N});
            }
        }

      &inlineSingleCall ($d1, $DD2, $SS2, $n2, $call, %opts, inlineDeclarations => 1, skipDimensionCheck => $opts{skipDimensionCheck});
    }

  my %N;

  for my $rename (@rename)
    {
      for my $N (sort keys (%$rename)) 
        {
          push @{ $N{$N} }, $rename->{$N};
        }
    }

  # Merge consistent declarations (and remove variable suffix)

  my @ENN = &F ('./T-decl-stmt//EN-N', $d1, 1);

  my %ENN = map { ($_, 1) } @ENN;

  for my $N (sort keys (%N))
    {
      next if ($ENN{$N}); # Variable already exists in caller

      my @N = @{ $N{$N} };

      my @en_decl = map { &F ('./T-decl-stmt//EN-decl[string(EN-N)="?"]', $_, $d1) } @N;

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
          $ENN{$N}++;
        }
    }

  # Remove include of inlined subroutine

  my ($include) = &F ('./include[string(filename)="?"]', lc ($n2) . '.intfb.h', $d1);

  $include && &removeStmt ($include);

}


1;

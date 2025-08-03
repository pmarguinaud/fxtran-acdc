package Fxtran::ManyBlocks;

use Data::Dumper;

use strict;

use Fxtran::Common;
use Fxtran::Loop;
use Fxtran::Stack;
use Fxtran::Call;
use Fxtran::Subroutine;
use Fxtran::Pragma;
use Fxtran::Finder;
use Fxtran::Style;
use Fxtran::Decl;
use Fxtran::Dimension;
use Fxtran;

sub processSingleSection
{
  my ($pu, $par, $var2dim, $typearg, $dims, $LDACC, %opts) = @_;

  my ($style, $pragma) = @opts{qw (style pragma)};

  my $KGPBLKS    = $dims->{kgpblks};
  my $nproma     = $dims->{nproma};
  my $jlon       = $dims->{jlon};
  my $kidia      = $dims->{kidia};          # KIDIA to be used in manyblocks kernels
  my $kfdia      = $dims->{kfdia};          # KFDIA to be used in manyblocks kernels
  my $kidia_call = $dims->{kidia_call};     # KIDIA to be used in OpenACC kernels
  my $kfdia_call = $dims->{kfdia_call};     # KFDIA to be used in OpenACC kernels
  my $pointer    = $dims->{pointer}  || {}; # Variable is NPROMA with pointer attribute
  my $optional   = $dims->{optional} || {}; # Variable is NPROMA with optional attribute
  my $argument   = $dims->{argument} || {}; # Variable is argument

  # Make the section single column
  
  &Fxtran::Loop::removeNpromaLoopsInSection
  (
    $par, 
    style => $style, 
    var2dim => $var2dim,
  );
  
  # Get JLON indexed expressions and add JBLK as last index
  
  for my $sslt (&F ('.//array-R/section-subscript-LT[./section-subscript[string(.)="?"]]', $jlon, $par))
    {
      $sslt->appendChild ($_) for (&t (", "), &n ('<section-subscript><lower-bound>' . &e ('JBLK') . '</lower-bound></section-subscript>'));
    }
  
  # JBLK slice for array arguments
  
  for my $call (&F ('.//call-stmt', $par))
    {
      my %seenOptional; # How many times we have seen each optional argument
      my $CALL = $call->textContent;

      for my $expr (&F ('./arg-spec/arg/named-E', $call))
        {
          my ($N) = &F ('./N', $expr, 1);
          next unless (my $nd = $var2dim->{$N});
      
          my ($rlt) = &F ('./R-LT', $expr);
          my ($sslt) = &F ('./R-LT/array-R/section-subscript-LT', $expr);
     
          unless ($sslt)
            {
              if (($rlt) = &F ('./R-LT', $expr))
                {
                  $rlt->unbindNode ();
                }
      
              $rlt = &n ('<R-LT><array-R>(<section-subscript-LT>' . join (', ', ('<section-subscript>:</section-subscript>') x $nd) . '</section-subscript-LT>)</array-R></R-LT>');
              $expr->appendChild ($rlt);
     
              ($sslt) = &F ('./R-LT/array-R/section-subscript-LT', $expr);
            }
      
          $sslt->appendChild ($_) for (&t (', '), &n ('<section-subscript><lower-bound>' . &e ('JBLK') . '</lower-bound></section-subscript>'));
      
          if ($opts{'array-slice-to-address'})  # Transform array slice to the address of the first element of the slice
                                                # We assume that the slice is a contiguous chunk of memory
            {
              my @ss = &F ('./array-R/section-subscript-LT/section-subscript', $rlt); 
     
              for my $i (0 .. $#ss-1)
                {
                  my $ss = $ss[$i];
      
                  my ($lb) = &F ('./lower-bound', $ss);
                  my ($ub) = &F ('./upper-bound', $ss);
                  my ($dd) = &F ('./text()[string(.)=":"]', $ss);
      
                  $_->unbindNode () 
                    for ($ss->childNodes ());
      
                  if ($lb)
                    {
                      $ss->appendChild ($lb);
                    }
                  else
                    {
                      $ss->appendChild (&n ('<lower-bound>' . &e ("LBOUND ($N, " . ($i+1) . ")") . '</lower-bound>'));
                    }
                }
     
              if ($optional->{$N})
                {
                  die ("Optional argument `$N' appears more than once in call statement `$CALL'")
                    if ($seenOptional{$N}++ > 1);

                  my $EXPR = $expr->textContent;
                  my ($if) = &Fxtran::parse (fragment => << "EOF", fopts => [qw (-construct-tag)]);
IF (PRESENT ($N)) THEN
${N}_PTR => $EXPR
ELSE
${N}_PTR => NULL ()
ENDIF
EOF
                  $expr->replaceNode (&e ("${N}_PTR"));

                  $call->parentNode->insertBefore ($_, $call) for ($if, &t ("\n"));
                }
            }
        }
    }
  
  # Move section contents into a DO loop over KLON
  
  my ($do_jlon) = &fxtran::parse (fragment => << "EOF");
DO $jlon = $kidia, MERGE ($nproma, $kfdia, JBLK < $KGPBLKS)
ENDDO
EOF
  
  for my $x ($par->childNodes ())
    {
      $do_jlon->insertBefore ($x, $do_jlon->lastChild);
    }
  
  $par->replaceNode ($do_jlon);    
  
  # Use a stack
  
  if (&Fxtran::Stack::addStackInSection ($do_jlon))
    {
      &Fxtran::Stack::iniStackManyBlocks 
        ( 
          $do_jlon, stack84 => 1, JBLKMIN => 1, KGPBLKS => $KGPBLKS, 
          $opts{'use-stack-manyblocks'} ? (YDOFFSET => 'YLOFFSET') : (),
          'stack-method' => $opts{'stack-method'},
        );
    }
  
  # Replace KIDIA/KFDIA by JLON in call statements
  
  for my $call (&F ('.//call-stmt', $do_jlon))
    {
      for my $var ($kidia_call, $kfdia_call)
        {
          for my $expr (&F ('.//named-E[string(.)="?"]', $var, $call))
            {
              $expr->replaceNode (&e ($jlon));
            }
        }
    }
  
  # Add single column suffix to routines called in this section
  
  &Fxtran::Call::addSuffix 
  (
    $pu,
    section => $do_jlon,
    suffix => $opts{'suffix-singlecolumn'},
    'merge-interfaces' => $opts{'merge-interfaces'},
    match => sub { my $proc = shift; $proc->textContent ne 'ABOR1' },
  );

  # Move loop over NPROMA into a loop over the blocks
  
  my ($do_jblk) = &fxtran::parse (fragment => << "EOF");
DO JBLK = 1, $KGPBLKS
ENDDO
EOF

  my $do_jlon1 = $do_jlon->cloneNode (); $do_jlon1->appendChild ($_) for ($do_jlon->childNodes ());
  
  $do_jblk->insertBefore ($_, $do_jblk->lastChild) 
    for ($do_jlon1, &t ("\n"));
  
  $do_jlon->replaceNode ($do_jblk); $do_jlon = $do_jlon1;
  
  # Find private variables, derived typed arguments, NPROMA blocked arrays
  
  my (%priv, %nproma, %type);
  
  for my $expr (&F ('.//named-E', $do_jlon))
    {
      my ($n) = &F ('./N', $expr, 1);
      if ($var2dim->{$n})
        {
          $nproma{$n}++;
        }
      elsif ($typearg->{$n})
        {
          $type{$n}++;
        }
      elsif (! $argument->{$n}) # Arguments cannot be PRIVATE
        {
          my $p = $expr->parentNode;
          $priv{$n}++ if (($p->nodeName eq 'E-1') || ($p->nodeName eq 'do-V'));
        }
    }

  # Add OpenACC directive  
  
  if ($LDACC ne '.FALSE.')
    {
      my @pointer;

      for my $n (grep { $nproma{$_} && $pointer->{$_} } sort (keys (%$var2dim)))
        {
          push @pointer, $n;
        }

      $pragma->insertLoopVector ($do_jlon, PRIVATE => [sort (keys (%priv))]);
      $pragma->insertParallelLoopGang 
        ( 
          $do_jblk, PRIVATE => ['JBLK'], VECTOR_LENGTH => [$nproma], ($LDACC ne '.TRUE.' ? (IF => [$LDACC]) : ()),
          $opts{'use-stack-manyblocks'} ? (PRESENT => \@pointer) : ()
        );
    }


  for my $abor1 (&F ('.//call-stmt/procedure-designator/named-E/N/n/text()[string(.)="ABOR1"]', $do_jlon))
    {
      $abor1->setData ('FXTRAN_ACDC_ABORT');
    }

  return $do_jlon;
}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  if (%{ $opts{'fuse-outer-dimension-names'} })
    {
      &Fxtran::Dimension::fuseOuterDimensions ($pu, %opts);
    }

  # Process ABORT sections

  for my $abort (&F ('.//abort-section', $pu))
    {    
      $_->unbindNode () for ($abort->childNodes ());
      $abort->appendChild ($_) 
        for (&s ('CALL ABOR1 ("ERROR: WRONG SETTINGS")'), &t ("\n"));
    }    

  my $find = $opts{find};

  my $KGPBLKS = 'KGPBLKS';

  my $style = $opts{style};
  my $pragma = $opts{pragma};

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my @nproma = $style->nproma ();

  my $jlon = $style->jlon ();
  my $kidia = $style->kidia ();
  my $kfdia = $style->kfdia ();
  
  # Arrays dimensioned with KLON and their dimensions

  my $var2dim = &Fxtran::Loop::getVarToDim ($pu, style => $style);
  my $typearg = {};
  
  my %notpresentttype = map { ($_, 1) } @{ $opts{'not-present-types'} }; # Types not present on the device

  my %argument;

  {
    my @argument = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
    %argument = map { ($_, 1) } @argument;

    # Derived types, assume they are present on the device, except for those in %notpresentttype
    my @typearg = &F ('./T-decl-stmt[./_T-spec_/derived-T-spec]/EN-decl-LT/EN-decl/EN-N', $dp, 1);
    for my $decl (&F ('./T-decl-stmt[./_T-spec_/derived-T-spec]', $dp))
      {
        my ($N) = &F ('./EN-decl-LT/EN-decl/EN-N', $decl, 1);
        my ($ts) = &F ('./_T-spec_/derived-T-spec/T-N', $decl, 1);
        $typearg->{$N} = $ts if ($argument{$N});
      }

    my %optional;

    if ($opts{'use-stack-manyblocks'})
      {
        my @present = grep { $var2dim->{$_} || ($typearg->{$_} && (! $notpresentttype{$typearg->{$_}})) } @argument;

        # Allocate target variables with an ACC CREATE

        my (%target, %optional);
   
        for my $n (sort (keys (%$var2dim)))
          {
            my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(./EN-N)="?"]]', $n, $dp);
            $target{$n}++ if ((! $argument{$n}) && &F ('./attribute[string(./attribute-N)="TARGET"]', $decl));
            $optional{$n}++ if (&F ('./attribute[string(./attribute-N)="OPTIONAL"]', $decl));
          }

        @present = grep { ! $optional{$_} } @present;

        $pragma->insertData ($ep, PRESENT => \@present, CREATE => [grep { ! $argument{$_} } sort keys (%target)], IF => ['LDACC']);

        for my $n (sort keys (%optional))
          {
            $pragma->insertData ($ep, PRESENT => [$n], IF => ["LDACC .AND. PRESENT ($n)"]);
          }

      }  
    else
      {
        my @present = grep { $var2dim->{$_} || $typearg->{$_} } @argument;
        my @create = grep { ! $argument{$_} } sort (keys (%$var2dim));
       
        # Create local arrays, assume argument arrays are on the device
        $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@create, IF => ['LDACC']);
      }

  }
  
  # Find pointers 
  # Find optional arguments 
  # - add target attribute 
  # - add a pointer scalar variable V_PTR; we will use that to access slices of arrays
  
  my (%pointer, %optional);

  for my $n (sort keys (%$var2dim))
    {
      my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(./EN-N)="?"]]', $n, $dp);
      $pointer{$n} = 1 if (&F ('./attribute[string(./attribute-N)="POINTER"]', $decl));
      if (my ($optional) = &F ('./attribute[string(./attribute-N)="OPTIONAL"]', $decl))
        {
          $optional{$n} = 1;
          unless (&F ('./attribute[string(./attribute-N)="TARGET"]', $decl))
            {
              $decl->insertAfter ($_, $optional) for (&n ('<attribute><attribute-N>TARGET</attribute-N></attribute>'), &t (', '));
            }
          my ($ts) = &F ('./_T-spec_/ANY-T-spec', $decl, 1);
          &Fxtran::Decl::declare ($pu, "$ts, POINTER :: ${n}_PTR");
        }
    }

  my %dims = (kgpblks => 'KGPBLKS', jlon => $style->jlon (), kidia => $style->kidia (), 
              kfdia => $style->kfdia (), nproma => $style->getActualNproma ($pu),
              kidia_call => $style->kidia (), kfdia_call => $style->kfdia (),
              pointer => \%pointer, optional => \%optional, argument => \%argument);

  # Parallel sections

  my @par = &F ('.//parallel-section', $pu);

  for my $par (@par)
    {
      &processSingleSection ($pu, $par, $var2dim, $typearg, \%dims, 'LDACC', %opts);
    }
  
  # Add many block suffix to routines not called from within parallel sections
  # TODO : Improve this mechanism

  &Fxtran::Call::addSuffix 
  (
    $pu,
    suffix => $opts{'suffix-manyblocks'},
    'merge-interfaces' => $opts{'merge-interfaces'},
    match => sub { my $proc = shift; ! (($proc =~ m/$opts{'suffix-singlecolumn'}$/i) or ($proc eq 'ABOR1') or ($proc eq 'FXTRAN_ACDC_ABORT') or ($proc eq 'GETENV')) },
  );

  # Add KGPLKS argument to manyblock routines + add LDACC argument
  
  for my $call (&F ('.//call-stmt[contains(string(procedure-designator),"?")]', $opts{'suffix-manyblocks'}, $ep))
    {
      my ($argspec) = &F ('./arg-spec', $call);

      $argspec->appendChild ($_) for (&t (", "), &n ("<arg><arg-N><k>LDACC</k></arg-N> = " . &e ('LDACC') . '</arg>'));
      $argspec->appendChild ($_) for (&t (", "), &n ("<arg><arg-N><k>KGPBLKS</k></arg-N> = " . &e ('KGPBLKS') . '</arg>'));

      if ($opts{'use-stack-manyblocks'})
        {
          $argspec->appendChild ($_) for (&t (", "), &n ("<arg><arg-N><k>YDOFFSET</k></arg-N> = " . &e ('YLOFFSET') . '</arg>'));
        }

      for my $expr (&F ('./arg/named-E', $argspec))
        {
          my ($N) = &F ('./N', $expr, 1);
          next unless ($var2dim->{$N});
          if (my ($sslt) = &F ('./R-LT/array-R/section-subscript-LT', $expr))
            {
              $sslt->appendChild ($_) for (&t (','), &n ('<section-subscript>:</section-subscript>'));
            }
        }
    }
  
  # Add extra arguments : LDACC, KGPBLKS, YDOFFSET
  
  {
    my ($dal) = &F ('./subroutine-stmt/dummy-arg-LT', $pu);
    my @arg = &F ('./arg-N', $dal, 1);

    my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $arg[-1], $dp);

    $dp->insertAfter ($_, $decl) for (&s ("LOGICAL, INTENT (IN) :: LDACC"), &t ("\n"));
    $dp->insertAfter ($_, $decl) for (&s ("INTEGER, INTENT (IN) :: $KGPBLKS"), &t ("\n"));

    $dal->appendChild ($_) for (&t (", "), &n ("<arg-N>LDACC</arg-N>"));
    $dal->appendChild ($_) for (&t (", "), &n ("<arg-N>$KGPBLKS</arg-N>"));

    if ($opts{'use-stack-manyblocks'})
      {
        $dp->insertAfter ($_, $decl) for (&s ("TYPE (FXTRAN_ACDC_STACK), INTENT (IN) :: YDOFFSET"), &t ("\n"));
        $dal->appendChild ($_) for (&t (", "), &n ("<arg-N>YDOFFSET</arg-N>"));
      }
  }

  # Stack definition & declaration
  
  my ($implicit) = &F ('.//implicit-none-stmt', $pu);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>fxtran_acdc_stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);
 

  &Fxtran::Decl::declare ($pu, 'TYPE (FXTRAN_ACDC_STACK) :: YLSTACK0') if ($opts{'stack-method'});
  &Fxtran::Decl::declare ($pu, 'TYPE (FXTRAN_ACDC_STACK) :: YLSTACK');
  &Fxtran::Decl::declare ($pu, 'TYPE (FXTRAN_ACDC_STACK) :: YLOFFSET') if ($opts{'use-stack-manyblocks'});
  &Fxtran::Decl::use ($pu, 'USE FXTRAN_ACDC_STACK_MOD');

  # Add extra dimensions to all nproma arrays + make all array spec implicit

  for my $stmt (&F ('./T-decl-stmt', $dp))
    {
      next unless (my ($as) = &F ('./EN-decl-LT/EN-decl/array-spec', $stmt));
      my ($n) = &F ('./EN-decl-LT/EN-decl/EN-N', $stmt, 1);

      my ($sslt) = &F ('./shape-spec-LT', $as);

      my @ss = &F ('./shape-spec', $sslt);

      goto NPROMA if (($ss[0]->textContent eq ':') && $var2dim->{$n});

      for my $nproma (@nproma)
        {
          goto NPROMA if ($ss[0]->textContent eq $nproma);
        }

      next;

NPROMA:

     if (&F ('./attribute[string(attribute-N)="INTENT"]', $stmt))
       {
         # Dummy argument : use implicit shape

         my $comment = $as->textContent;

         my $iss = &n ('<shape-spec>:</shape-spec>');

         for my $ss (@ss)
           {
             next unless (my ($ub) = &F ('./upper-bound', $ss));  # Only for dimensions with upper-bound : (N1:N2) or (N)
             if (my ($lb) = &F ('./lower-bound', $ss))
               {
                 $ub->unbindNode ();                              # (N1:N2) -> (N1:)
               }
             else
               {
                 $ub->replaceNode ($iss->cloneNode (1));          # (N) -> (:)
               }
           }

         $sslt->appendChild ($_) for (&t (", "), $iss);

         $dp->insertAfter ($_, $stmt) for (&n ("<C>! $comment</C>"), &t (' '));
       }   
     elsif (&F ('./attribute[string(attribute-N)="POINTER"]', $stmt))
       {
         if (my ($attr) = &F ('./attribute[string(attribute-N)="CONTIGUOUS"]', $stmt))
           {
             $_->unbindNode () for ($attr->previousSibling, $attr);
           }
         $sslt->appendChild ($_) for (&t (","), &n ('<shape-spec>:</shape-spec>'));
       }
     else
       {
         # Local variable : add KGPBLKS dimension
         $sslt->appendChild ($_) for (&t (","), &n ('<shape-spec>' . &e ($KGPBLKS) . '</shape-spec>'));
       }
    

    }

  &Fxtran::Decl::declare ($pu, 'INTEGER :: JBLK');

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-manyblocks'});

  &stackAllocateTemporaries ($pu, $var2dim, %opts)
    if ($opts{'use-stack-manyblocks'});

  &Fxtran::Decl::use ($pu, 'USE FXTRAN_ACDC_ABORT_MOD');
}

sub stackAllocateTemporaries
{
  my ($pu, $var2dim, %opts) = @_;

  my $pragma = $opts{pragma};

  my ($ep) = &F ('./execution-part', $pu);

  $ep->setNodeName ('comp');
  $pu->replaceChild (my ($ep1) = &n ('<execution-part/>'), $ep);
  $ep1->appendChild ($ep);

  my $comp = $ep;

  $ep = $ep1;

  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  $ep->insertBefore (my $C = &n ("<C>!</C>"), $ep->firstChild);

  my @alloc;

  for my $decl (&F ('./T-decl-stmt', $dp))
    {
      next if (&F ('./attribute[string(./attribute-N)="INTENT"]', $decl));
      next if (&F ('./attribute[string(./attribute-N)="TARGET"]', $decl));
      next if (&F ('./attribute[string(./attribute-N)="POINTER"]', $decl));
      

      my ($en_decl) = &F ('./EN-decl-LT/EN-decl', $decl);
      my ($n) = &F ('./EN-N', $en_decl, 1);
      next unless ($var2dim->{$n});
      my ($ts) = &F ('./_T-spec_', $decl, 1);
      my ($as) = &F ('./array-spec', $en_decl, 1);
      $decl->replaceNode (&t ("fxtran_acdc_temp ($ts, $n, $as)"));


      if ($opts{'stack-method'})
        {
          $ep->insertBefore ($_, $ep->firstChild) for (&t ("\n"), &s ("fxtran_acdc_stack_alloc ($n)"));
        }
      else
        {
          my ($if) = &fxtran::parse (fragment => << "EOF");
IF (KIND ($n) == 8) THEN
  fxtran_acdc_alloc8 ($n)
ELSEIF (KIND ($n) == 4) THEN
  fxtran_acdc_alloc4 ($n)
ELSE
  STOP 1
ENDIF
EOF
          $ep->insertBefore ($_, $ep->firstChild) for (&t ("\n"), $if);
        }

      push @alloc, $n;
    }

  if (@alloc)
    {
      $pragma->insertData ($comp, PRESENT => \@alloc, IF => ['LDACC']);
    }

  if ($opts{'stack-method'})
    {
      # Before allocations : initialize YLSTACK, using YSTACK and YDOFFSET
      for my $x (&t ("\n"), &s ("YLSTACK0 = YLSTACK"), &t ("\n"), &s ("YLSTACK = fxtran_acdc_stack_init (YLSTACK, 1, 1, YDOFFSET)"))
        {
          $ep->insertBefore ($x, $ep->firstChild);
        }
     
      $ep->insertAfter (&t ("\n"), $C);
     
      # After allocations, initialize YLOFFSET
     
      $ep->insertAfter ($_, $C) 
         for (&t ("\n"), &s ("YLOFFSET = YLSTACK - YLSTACK0 + YDOFFSET"));
     
      $C->unbindNode ();
    }
  else
    {
      # Before allocations : initialize YLSTACK, using YSTACK and YDOFFSET
      for my $size (4, 8)
        {
     
          for my $x (&t ("\n"), &s ("YLSTACK%L${size} = fxtran_acdc_stack_l${size}_base (YFXTRAN_ACDC_STACK, 1, 1, YDOFFSET)"),
                     &t ("\n"), &s ("YLSTACK%U${size} = fxtran_acdc_stack_u${size}_base (YFXTRAN_ACDC_STACK, 1, 1, YDOFFSET)"))
            {
              $ep->insertBefore ($x, $ep->firstChild);
            }
     
        }
     
      $ep->insertAfter (&t ("\n"), $C);
     
      # After allocations, initialize YLOFFSET
     
      $ep->insertAfter ($_, $C) 
         for (&t ("\n"), &s ("YLOFFSET%L8 = YLSTACK%L8 - fxtran_acdc_stack_l8_base (YFXTRAN_ACDC_STACK, 1, 1, YDOFFSET) + YDOFFSET%L8"),
              &t ("\n"), &s ("YLOFFSET%L4 = YLSTACK%L4 - fxtran_acdc_stack_l4_base (YFXTRAN_ACDC_STACK, 1, 1, YDOFFSET) + YDOFFSET%L4"));
     
      $C->unbindNode ();
    }


}

1;

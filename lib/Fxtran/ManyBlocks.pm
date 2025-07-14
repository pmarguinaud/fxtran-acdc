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
use Fxtran;

sub processSingleSection
{
  my ($pu, $par, $var2dim, $typearg, $dims, $LDACC, %opts) = @_;

  my ($style, $pragma) = @opts{qw (style pragma)};

  my $KGPBLKS    = $dims->{kgpblks};
  my $nproma     = $dims->{nproma};
  my $jlon       = $dims->{jlon};
  my $kidia      = $dims->{kidia};
  my $kfdia      = $dims->{kfdia};
  my $kidia_call = $dims->{kidia_call};
  my $kfdia_call = $dims->{kfdia_call};
  my $pointer    = $dims->{pointer} || {};
  
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
  
  for my $expr (&F ('.//call-stmt/arg-spec/arg/named-E', $par))
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
      else
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

  return $do_jlon;
}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

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
  
  {
    # Derived types, assume they are present on the device
    my @typearg = &F ('./T-decl-stmt[./_T-spec_/derived-T-spec]/EN-decl-LT/EN-decl/EN-N', $dp, 1);

    my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
    my %arg = map { ($_, 1) } @arg;

    $typearg = {map { ($_, 1) } grep { $arg{$_} } @typearg};

    if ($opts{'use-stack-manyblocks'})
      {
        my @present = grep { $var2dim->{$_} || $typearg->{$_} } @arg;
        # Allocate target variables with an ACC CREATE
        my @target;
        for my $n (grep { ! $arg{$_} } sort (keys (%$var2dim)))
          {
            push @target, $n
             if (&F ('./T-decl-stmt[./attribute[string(./attribute-N)="TARGET"]][./EN-decl-LT/EN-decl[string(./EN-N)="?"]]', $n, $dp));
          }
        $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@target, IF => ['LDACC']);
      }  
    else
      {
        my @present = grep { $var2dim->{$_} || $typearg->{$_} } @arg;
        my @create = grep { ! $arg{$_} } sort (keys (%$var2dim));
       
        # Create local arrays, assume argument arrays are on the device
        $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@create, IF => ['LDACC']);
      }

  }
  

  # Find pointers
  
  my %pointer;

  for my $n (keys (%$var2dim))
    {
      $pointer{$n} = 1
        if (&F ('./T-decl-stmt[./attribute[string(./attribute-N)="POINTER"]][./EN-decl-LT/EN-decl[string(./EN-N)="?"]]', $n, $dp));
    }

  my %dims = (kgpblks => 'KGPBLKS', jlon => $style->jlon (), kidia => $style->kidia (), 
              kfdia => $style->kfdia (), nproma => $style->getActualNproma ($pu),
              kidia_call => $style->kidia (), kfdia_call => $style->kfdia (),
              pointer => \%pointer);

  # Parallel sections

  my @par = &F ('.//parallel-section', $pu);

  for my $par (@par)
    {
      &processSingleSection ($pu, $par, $var2dim, $typearg, \%dims, 'LDACC', %opts);
    }
  
  # Add single block suffix to routines not called from within parallel sections

  &Fxtran::Call::addSuffix 
  (
    $pu,
    suffix => $opts{'suffix-manyblocks'},
    'merge-interfaces' => $opts{'merge-interfaces'},
    match => sub { my $proc = shift; ! (($proc =~ m/$opts{'suffix-singlecolumn'}$/i) or ($proc eq 'ABOR1')) },
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
          my ($N) = &F ('./N', $expr);
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
        $dp->insertAfter ($_, $decl) for (&s ("TYPE (STACK), INTENT (IN) :: YDOFFSET"), &t ("\n"));
        $dal->appendChild ($_) for (&t (", "), &n ("<arg-N>YDOFFSET</arg-N>"));
      }
  }

  # Stack definition & declaration
  
  my ($implicit) = &F ('.//implicit-none-stmt', $pu);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);
 

  &Fxtran::Decl::declare ($pu, 'TYPE (STACK) :: YLSTACK0') if ($opts{'stack-method'});
  &Fxtran::Decl::declare ($pu, 'TYPE (STACK) :: YLSTACK');
  &Fxtran::Decl::declare ($pu, 'TYPE (STACK) :: YLOFFSET') if ($opts{'use-stack-manyblocks'});
  &Fxtran::Decl::use ($pu, 'USE STACK_MOD');

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
      $decl->replaceNode (&t ("temp ($ts, $n, $as)"));


      if ($opts{'stack-method'})
        {
          $ep->insertBefore ($_, $ep->firstChild) for (&t ("\n"), &s ("stack_alloc ($n)"));
        }
      else
        {
          my ($if) = &fxtran::parse (fragment => << "EOF");
IF (KIND ($n) == 8) THEN
  alloc8 ($n)
ELSEIF (KIND ($n) == 4) THEN
  alloc4 ($n)
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
      for my $x (&t ("\n"), &s ("YLSTACK0 = YLSTACK"), &t ("\n"), &s ("YLSTACK = stack_init (YLSTACK, 1, 1, YDOFFSET)"))
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
     
          for my $x (&t ("\n"), &s ("YLSTACK%L${size} = stack_l${size}_base (YSTACK, 1, 1, YDOFFSET)"),
                     &t ("\n"), &s ("YLSTACK%U${size} = stack_u${size}_base (YSTACK, 1, 1, YDOFFSET)"))
            {
              $ep->insertBefore ($x, $ep->firstChild);
            }
     
        }
     
      $ep->insertAfter (&t ("\n"), $C);
     
      # After allocations, initialize YLOFFSET
     
      $ep->insertAfter ($_, $C) 
         for (&t ("\n"), &s ("YLOFFSET%L8 = YLSTACK%L8 - stack_l8_base (YSTACK, 1, 1, YDOFFSET) + YDOFFSET%L8"),
              &t ("\n"), &s ("YLOFFSET%L4 = YLSTACK%L4 - stack_l4_base (YSTACK, 1, 1, YDOFFSET) + YDOFFSET%L4"));
     
      $C->unbindNode ();
    }


}

1;

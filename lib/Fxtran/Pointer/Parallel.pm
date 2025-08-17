package Fxtran::Pointer::Parallel;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;
use File::Basename;
use List::MoreUtils qw (uniq);

use strict;

use Fxtran;
use Fxtran::Decl;
use Fxtran::Call;
use Fxtran::Pointer::Object;
use Fxtran::Pointer::SymbolTable;
use Fxtran::Loop;
use Fxtran::Associate;
use Fxtran::Subroutine;
use Fxtran::Finder;
use Fxtran::Include;
use Fxtran::Bt;
use Fxtran::Canonic;
use Fxtran::Style;
use Fxtran::Style::IAL;
use Fxtran::Style::MESONH;
use Fxtran::Pragma;
use Fxtran::Util;

sub processSingleParallel
{
  my ($pu, $parallel, $ipar, $NAME, $t, $find, $types, $puseUtilMod, %opts) = @_;

  my $target = $parallel->getAttribute ('target');

  my @target = split (m/\//o, $target || 'OpenMP');

  my ($if_construct) = &Fxtran::parse (fragment => << "EOF");
IF (LLCOND) THEN
!
ELSEIF (LLCOND) THEN
!
ELSE
  CALL ABOR1 ('$NAME: NOT METHOD WAS FOUND')
ENDIF
EOF

  my ($if_block, $elseif_block) = &F ('./if-block', $if_construct);
  $elseif_block->unbindNode ();

  $parallel->replaceNode ($if_construct);

  my @block;
  
  my $name = $parallel->getAttribute ('name') || $ipar;
  
  # Do it once for all sections

  my %parallel;
  for my $itarget (0 .. $#target)
    {
      'Fxtran::Pointer::Parallel'->getWhereTargetFromTarget (my $target = $target[$itarget], my $where);
 
      my $class = 'Fxtran::Pointer::Parallel'->class ($target);

      my $onlySimpleFields = $class->onlySimpleFields ();

      my $addBlockIndex = $class->getAddBlockIndex ();

      my $parallel1 = $parallel->cloneNode (1);

      $parallel{$onlySimpleFields}{$addBlockIndex} ||= 
        &Fxtran::Pointer::Parallel::makeParallel ($pu, $parallel1, $t, $find, $types, "$NAME:$name", $opts{'post-parallel'}, $onlySimpleFields, $addBlockIndex);

      $$puseUtilMod ||= $class->requireUtilMod ();
    }

  for my $itarget (0 .. $#target)
    {
      'Fxtran::Pointer::Parallel'->getWhereTargetFromTarget (my $target = $target[$itarget], my $where);
      my $class = 'Fxtran::Pointer::Parallel'->class ($target);
      $where ||= $class->getDefaultWhere ();
      $where = uc ($where);

      my $onlySimpleFields = $class->onlySimpleFields ();

      my $addBlockIndex = $class->getAddBlockIndex ();

      my $parallel1 = $parallel{$onlySimpleFields}{$addBlockIndex};

      unless ($parallel1)
        {
          $if_construct->unbindNode ();
          next;
        }

      $parallel1 = $parallel1->cloneNode (1);

      $parallel1 = $class->makeParallel ($pu, $parallel1, $t, %opts);
      
      my $block;
      if ($itarget == 0)
        {
          $block = $if_block;
        }
      else
        {
          $block = $elseif_block->cloneNode (1);
          $if_construct->insertAfter ($block, $block[-1]);
        }
      
      my ($cond) = &F ('./ANY-stmt/condition-E/ANY-E', $block);
      $cond->replaceNode (&e ("FXTRAN_ACDC_LPARALLELMETHOD ('$target','$NAME:$name')"));

      my ($C) = &F ('./C', $block);

      $C->replaceNode ($parallel1);

      if ($where ne 'HOST')
        {
          my @get = &F ('./prep//named-E[string(N)="GET_HOST_DATA_RDONLY" '
                              .     ' or string(N)="GET_HOST_DATA_RDWR" '
                              .     ' or string(N)="GATHER_HOST_DATA_RDONLY" '
                              .     ' or string(N)="GATHER_HOST_DATA_RDWR" '
                              .     ' ]/N/n/text()', $parallel1);
          for my $get (@get)
            {
              (my $t = $get->data) =~ s/_HOST_/_${where}_/go;
              $get->setData ($t);
            }
        }

      push @block, $block;

    }

  $if_construct->parentNode->insertBefore (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:$name',0,ZHOOK_HANDLE_PARALLEL)"), $if_construct);
  $if_construct->parentNode->insertBefore (&t ("\n"), $if_construct);


  $if_construct->parentNode->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:$name',1,ZHOOK_HANDLE_PARALLEL)"), $if_construct);
  $if_construct->parentNode->insertAfter (&t ("\n"), $if_construct);

}

sub processSingleRoutine
{
  my ($pu, $NAME, $types, %opts) = @_;

  my $find = $opts{find};

  for my $unseen (&F ('.//unseen', $pu))
    {
      $pu->unbindNode ();
    }
  
  &Fxtran::Subroutine::rename ($pu, sub { return $_[0] . uc ($opts{'suffix-pointerparallel'}) });

  # Add modules
  
  my @use = qw (FIELD_MODULE FIELD_FACTORY_MODULE FIELD_ACCESS_MODULE FXTRAN_ACDC_PARALLELMETHOD_MOD FXTRAN_ACDC_STACK_MOD YOMHOOK PARKIND1);
 
  my ($up) = &F ('./specification-part/use-part', $pu);

  for (&F ('./use-stmt[string(module-N)="PARKIND1"]', $up))
    {
      $_->unbindNode ();
    }

  if ($opts{'use-acpy'} || $opts{'use-bcpy'})
    {
      push @use, 'FXTRAN_ACDC_ARRAY_COPY_MOD';
    }
  
  &Fxtran::Decl::use ($pu, map { "USE $_" } @use);
  
  # Add local variables
  
  &Fxtran::Decl::declare 
  ($pu,  
    $opts{style}->declareJlon (),
    'INTEGER(KIND=JPIM) :: JBLK',
    'TYPE(CPG_BNDS_TYPE) :: YLCPG_BNDS', 
    'REAL(KIND=JPHOOK) :: ZHOOK_HANDLE_FIELD_API',
    'REAL(KIND=JPHOOK) :: ZHOOK_HANDLE_PARALLEL',
    'REAL(KIND=JPHOOK) :: ZHOOK_HANDLE_COMPUTE',
    'TYPE(FXTRAN_ACDC_STACK) :: YLSTACK',
    'TYPE(FXTRAN_ACDC_STACK) :: YLOFFSET',
  );
  
  my $t = &Fxtran::Pointer::SymbolTable::getSymbolTable 
    ($pu, skip => $opts{'skip-arrays'}, nproma => $opts{nproma}, 
     'types-fieldapi-dir' => $opts{'types-fieldapi-dir'},
     'types-constant-dir' => $opts{'types-constant-dir'},
     'types-fieldapi-non-blocked' => $opts{'types-fieldapi-non-blocked'},
     'style' => $opts{style});
    
  
  # Remove SKIP sections
  
  for (&F ('.//skip-section', $pu))
    {
      $_->unbindNode ();
    }
  
  # Transform NPROMA fields into a pair of (FIELD API object, Fortran pointer)
  
  &Fxtran::Pointer::Parallel::fieldifyDecl 
     (
       'program-unit' => $pu, 
       'symbol-table' => $t,
       'contiguous-pointers' => $opts{'contiguous-pointers'},
     );

  # Process ABORT sections

  my @abort = &F ('.//abort-section', $pu);

  for my $abort (@abort)
    {
      $_->unbindNode ()
        for (&F ('./*', $abort));
      $abort->appendChild ($_)
        for (&s ('CALL ABOR1 ("ERROR: WRONG SETTINGS")'), &t ("\n"));
    }
  
  # Process PARALLEL sections
  
  my @parallel = &F ('.//parallel-section', $pu);
  
  my ($FILTER);

  my %customIterator;
  
  for my $parallel (@parallel)
    {
      if (my $style = $parallel->getAttribute ('style'))
        {
          $style = 'Fxtran::Style'->new (style => $style);
          if (my $it = $style->customIterator ())
            {
              $customIterator{$it} = $style->customIteratorDecl ();
            }
          if (my $it = $style->customIteratorCopy ())
            {
              $customIterator{$it} = $style->customIteratorCopyDecl ();
            }
        }
      if (my $filter = $parallel->getAttribute ('filter'))
        {
          $FILTER ||= 1;
        }
    }
  
  for (values (%customIterator))
    {
      &Fxtran::Decl::declare ($pu, $_);
    }
  
  if ($FILTER)
    {
      &Fxtran::Decl::use ($pu, "USE FIELD_GATHSCAT_MODULE");
      &Fxtran::Decl::declare ($pu, 'TYPE(FIELD_GATHSCAT) :: YL_FGS');
    }
  
  # Process call to parallel routines
  
  my @call = &F ('.//call-stmt[not(ancestor::parallel-section)]' # Skip calls in parallel sections
              . '[not(string(procedure-designator)="DR_HOOK")]'  # Skip DR_HOOK calls
              . '[not(string(procedure-designator)="ABOR1")]'    # Skip ABOR1 calls
              . '[not(string(procedure-designator)="ABORT")]'    # Skip ABORT calls
              . '[not(string(procedure-designator)="PCRC")]'     # Skip PCRC calls
              . '[not(string(procedure-designator)="GETENV")]'   # Skip GETENV calls
              . '[not(procedure-designator/named-E/R-LT)]'       # Skip objects calling methods
              . '[not(ancestor::serial-section)]', $pu);          # Skip calls in serial sections
  
  my %seen;
  
  for my $call (@call)
    {
      if (&Fxtran::Pointer::Parallel::callParallelRoutine ($call, $t, $types))
        {
          # Add include for the parallel CALL
          my ($name) = &F ('./procedure-designator/named-E/N/n/text()', $call);
          unless ($seen{$name->textContent}++)
            {
              my ($include) = &F ('.//include[./filename[string(.)="?" or string(.)="?"]', lc ($name) . '.intfb.h', lc ($name) . '.h', $pu);
              $include or die $call->textContent;
              unless ($opts{'merge-interfaces'})
                {
                  $include->parentNode->insertAfter (&n ('<include>#include "<filename>' . lc ($name) . '_parallel.intfb.h</filename>"</include>'), $include);
                  $include->parentNode->insertAfter (&t ("\n"), $include);
                }
            }
          $name->setData ($name->data . uc ($opts{'suffix-pointerparallel'}));
        }
    }
  
  # Create/delete fields for local arrays
  
  &Fxtran::Pointer::Parallel::setupLocalFields ($pu, $t, '', $opts{gpumemstat});
  
  unless ($opts{'merge-interfaces'})
    {
      &Fxtran::Include::removeUnusedIncludes ($pu);
    }
  
  my $useUtilMod = 0;
  
  for my $ipar (0 .. $#parallel)
    {
      &processSingleParallel ($pu, $parallel[$ipar], $ipar, $NAME, $t, $find, 
                              $types, \$useUtilMod, %opts);
    }
  
  # Declare pointers required for parallel sections
  
  my (@decl, @use_util);
  
  for my $n (sort keys (%$t))
    {
      my $s = $t->{$n};
      if ($s->{object_based})
        {
          my $nd = $s->{nd};
          $nd++ if ($s->{blocked});
          my $decl = &s ($s->{ts}->textContent . ", POINTER :: " . $n . "(" . join (',', (':') x $nd) . ")");
          push @decl, $decl;
        }
      if ($s->{object})
        {
          my ($tn) = &F ('./T-N', $s->{ts}, 1);
          if ($tn =~ m/^FIELD_\w+_ARRAY$/o)
            {
              push @use_util, &s ("USE FIELD_ARRAY_UTIL_MODULE");
            }
          elsif (! $opts{'type-bound-methods'})
            {
              push @use_util, &s ("USE UTIL_${tn}_MOD");
            }
        }
    }
  
  @use_util = &uniq (@use_util);
  
  
  &Fxtran::Decl::declare ($pu, @decl);
  &Fxtran::Decl::use ($pu, @use_util) if ($useUtilMod);
  
  for my $style (qw (Fxtran::Style::IAL Fxtran::Style::MESONH))
    {
      for my $suffix (@opts{qw (suffix-singlecolumn suffix-manyblocks)})
        {
          $style->setOpenACCInterfaces ($pu, %opts, suffix => $suffix);
        }
    }
  
  if (@parallel)
    {
      # Add abor1.intfb.h
  
      unless (&F ('.//include[string(filename)="abor1.intfb.h"]', $pu))
        {
          &Fxtran::Include::addInclude ($pu, 'abor1.intfb.h');
        }
    }
  
  # include fxtran_acdc_stack.h
  
  my ($implicit) = &F ('.//implicit-none-stmt', $pu);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>fxtran_acdc_stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);

}

sub getWhereTargetFromTarget
{
  my $class = shift;
  $_[0] =~ s/\%(\w+)$//o;
  $_[1] = $1 if (scalar (@_) > 1);
}

my %class;

sub class
{
  my $class = shift;
  my $target = shift;

  $target ||= 'OpenMP';

  unless (%class)
    {
      my $pm = join ('/', split (m/::/o, $class)) . '.pm';
      ($pm = $INC{$pm}) =~ s/\.pm//o;
      for my $pm (map { &basename ($_, qw (.pm)) } <$pm/*.pm>)
        {
          $class{uc ($pm)} = "$class\::$pm";
        }
    }
  
  $class = $class{uc ($target)};

  die "$target was not found" unless ($class);

  eval " use $class";
  if (my $c = $@)
    {
      die $c;
    }

  return $class;
}

sub fieldifySync
{
  my %args = @_;

  my $doc        = $args{'program-unit'};
  my $t          = $args{'symbol-table'};
  my $find       = $args{'find'};
  my $types      = $args{'types'};

  my ($dal) = &F ('./dummy-arg-LT', $doc->firstChild);

  for my $N (sort keys (%$t))
    {
      my $s = $t->{$N};

      next unless ($s->{isFieldAPI});

      my $en_decl = delete $s->{en_decl};
      my $stmt = &Fxtran::stmt ($en_decl);
      $stmt->unbindNode ();

      if ($s->{arg})
        {
          my ($arg) = &F ('./arg-N[string(.)="?"]', $N, $dal);
          &Fxtran::removeListElement ($arg); 
        }
    }

  my $removeLastArrayRef = sub
  {
    my $expr = shift;
    $expr = $expr->cloneNode (1);
    if (my @r = &F ('./R-LT/ANY-R', $expr))
      {
        if (($r[-1]->nodeName eq 'array-R') or ($r[-1]->nodeName eq 'parens-R'))
          {
            my $prev = $r[-1]->previousSibling;
            $prev->unbindNode () if ($prev && $prev->nodeName eq '#text');
            $r[-1]->unbindNode ();
          }
      }
    $expr = $expr->textContent;
    $expr =~ s/\s+//o;
    return $expr;
  };

  my $comment = sub
  {
    my $string = shift;
    my $n = &t (' ');
    $n->setData ("! $string");
    my $C = &n ('<C/>');
    $C->appendChild ($n);
  };

  for my $stmt (&F ('.//ANY-stmt', $doc))
    {
      print $stmt->textContent, "\n\n";
      my $stms = $stmt->textContent;

      my %intent;
      my %sync;

      for my $expr (reverse (&F ('.//named-E', $stmt))) # Reverse because we remove arguments in call statements
        {
          my ($N) = &F ('./N', $expr, 1);
          my $s = $t->{$N};
 
          if ($s->{isFieldAPI})
            {
              my $exps = $removeLastArrayRef->($expr);
              &Fxtran::Call::grokIntent ($expr, \$intent{$exps}, $find);
              if ($expr->parentNode->nodeName eq 'arg')
                {
                  &Fxtran::removeListElement ($expr->parentNode);
                }
              $sync{$exps} = 0;
            }
          elsif ($s->{object})
            {
              my @r = &F ('./R-LT/component-R', $expr);
              my @ctl = &F ('./R-LT/component-R/ct', $expr, 1);
              my $ptr = join ('_', 'Z', $N, @ctl);

              my $key = join ('%', &Fxtran::Pointer::Object::getObjectType ($s, $N), @ctl);
              my $decl;
              eval
                {
                  $decl = &Fxtran::Pointer::Object::getObjectDecl ($key, $types, allowConstant => 1);
                };
              if (my $c = $@)
                {
                  my $stmt = &Fxtran::stmt ($expr); 
                  die $c . $stmt->textContent;
                }

              if ($decl)
                {
                  my $exps = $removeLastArrayRef->($expr);
                  &Fxtran::Call::grokIntent ($expr, \$intent{$exps}, $find);
                  if ($expr->parentNode->nodeName eq 'arg')
                    {
                      &Fxtran::removeListElement ($expr->parentNode);
                    }
                  $sync{$exps} = 1;
                }

            }
        }

     if (%intent)
       {
         for my $exps (sort keys (%intent))
           {
             next unless ($sync{$exps});
             $stmt->parentNode->insertBefore ($_, $stmt)
                for (&s ("CALL $intent{$exps} ($exps)"), &t ("\n"));
           }
         if ($stmt->nodeName eq 'a-stmt')
           {
#            $stmt->unbindNode ();
             $stmt->replaceNode ($comment->($stms));
           } 
         elsif ($stmt->nodeName eq 'call-stmt')
           {
             my @arg = &F ('./arg-spec/arg/named-E', $stmt);
             for my $arg (@arg)
               {
                 my ($N) = &F ('./N', $arg, 1);
                 my $s = $t->{$N};
                 goto KEEP if ($s->{object});
               }
#            $stmt->unbindNode ();
             $stmt->replaceNode ($comment->($stms));
KEEP:
           }
         else
           {
             die $stmt;
           }
       }

     print '-' x 80, "\n";
     print "\n" x 2;
    }

}

sub fieldifyDecl
{
  my %args = @_;

  my $doc        = $args{'program-unit'};
  my $t          = $args{'symbol-table'};

# First step : process all NPROMA arrays declarations
# - local arrays are added an extra dimension for blocks
# - NPROMA arrays are complemented by a field object
# - NPROMA arguments arrays are replaced by field objects

  for my $N (sort keys (%$t))
    {
      my $s = $t->{$N};
      next if ($s->{skip});
  
      next unless ($s->{isFieldAPI});

      my $en_decl = delete $s->{en_decl};
  
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($sslt) = &F ('./array-spec/shape-spec-LT', $en_decl);
      
      # Use implicit shape, with an extra dimension for blocks
  
      for ($sslt->childNodes ())
        {
          $_->unbindNode ();
        }

      my $nd = $s->{nd};

      $nd++ if ($s->{blocked});

      for my $i (1 .. $nd)
        {
          $sslt->appendChild (&n ('<shape-spec>:</shape-spec>'));
          $sslt->appendChild (&t (',')) if ($i < $nd);
        }
  
      &Fxtran::Decl::removeAttributes ($stmt, qw (TARGET CONTIGUOUS));

      my @attr = qw (POINTER);
      push @attr, 'CONTIGUOUS' if ($args{'contiguous-pointers'});

      &Fxtran::Decl::addAttributes ($stmt, @attr);

      my $optional = &Fxtran::Decl::removeAttributes ($stmt, 'OPTIONAL') ? ", OPTIONAL " : "";
  
      my $type_fld = &Fxtran::Pointer::SymbolTable::getFieldType ($nd, $s->{ts});
      $type_fld or die "Unknown type : " . $s->{ts}->textContent;
  
      my $decl_fld;
  
      if ($s->{arg})
        {
          &Fxtran::Decl::removeAttributes ($stmt, 'INTENT');
          $s->{arg}->setData ("YD_$N");
          ($decl_fld) = &s ("CLASS ($type_fld), POINTER$optional :: YD_$N");
          $s->{field} = &n ("<named-E><N><n>YD_$N</n></N></named-E>");
        }
      else
        {
          ($decl_fld) = &s ("CLASS ($type_fld), POINTER :: YL_$N");
          $s->{field} = &n ("<named-E><N><n>YL_$N</n></N></named-E>");
        }
  
      $stmt->parentNode->insertBefore ($decl_fld, $stmt);
      $stmt->parentNode->insertBefore (&t ("\n"), $stmt);
    }

  # Look for pointer assignments and optional arguments

  for my $N (keys (%$t))
    {
      my $s = $t->{$N};
      next unless ($s->{isFieldAPI});
      if ($s->{arg})
        {
          my @present = &F ('.//named-E[string(.)="?"]', "PRESENT ($N)", $doc);
 
          # Presentness for the field replaces presentness of array
   
          for my $present (@present)
            {
              $present->replaceNode (&e ('PRESENT(' . $s->{field}->textContent . ')'));
            }
        }
      elsif ($s->{pointer})
        {
          my @pa = &F ('.//pointer-a-stmt[string(E-1)="?"]', $N, $doc);
          for my $pa (@pa)
            {

              # Replace array pointer assignments by field pointer assignments

              my ($E2) = &F ('./E-2/ANY-E', $pa, 1);
              if ($E2 eq 'NULL()')
                {
                  $pa->replaceNode (&s ($s->{field}->textContent . " => NULL ()"));
                }
              else
                {
                  die $pa->textContent unless (my $s2 = $t->{$E2});
                  die &Dumper ($s2) unless ($s2->{field});
                  $pa->replaceNode (&s ($s->{field}->textContent . " => " . $s2->{field}->textContent));
                }
            }
        }
    }

}

sub replaceObjectExprByPointerExpr
{
  my ($par, $t, $onlysimplefields, $intent, $types, $blockLoop, $find) = @_;

  # Process each expression (only NPROMA local arrays and FIELD API backed data) in the parallel section

  for my $expr (&F ('.//named-E', $par))
    {
      my ($N) = &F ('./N', $expr, 1);
      my $s = $t->{$N};
      next if ($s->{skip});

      # Object wrapping fields : replace by a pointer to data with all blocks
      if ($s->{object} && (! $onlysimplefields))
        {
          my @ctl = &F ('./R-LT/component-R/ct', $expr, 1);
          my $ptr = join ('_', 'Z', $N, @ctl);

          # Create new entry in symbol table 
          # we record the pointer wich will be used to access the object component
          unless ($t->{$ptr})
            {
              my $key = join ('%', &Fxtran::Pointer::Object::getObjectType ($s, $N), @ctl);
              my $decl;
              eval
                {
                  $decl = &Fxtran::Pointer::Object::getObjectDecl ($key, $types, allowConstant => 1);
                };
              if (my $c = $@)
                {
                  my $stmt = &Fxtran::stmt ($expr); 
                  die $c . $stmt->textContent;
                }

              if ($decl)
                {
                  my ($as) = &F ('.//array-spec', $decl);
                  my ($ts) = &F ('./_T-spec_/*', $decl);
                  my @ss = &F ('./shape-spec-LT/shape-spec', $as);
                  my $nd = scalar (@ss);


                  $t->{$ptr} = {
                                 object => 0,
                                 skip => 0,
                                 isFieldAPI => 1,
                                 arg => 0,
                                 ts => $ts,
                                 as => $as,
                                 nd => $nd,
                                 field => &Fxtran::Pointer::Object::getFieldFromExpr ($expr),
                                 object_based => 1, # postpone pointer declaration
                                 blocked => $s->{blocked},
                               };
                }
              else
                {
                  # This member is not backed by field api, it must be a constant
                  $ptr = undef;
                }
            }

          if ($ptr)
            {
              # Backed by field api : remove all references other than last array references

              my @r = &F ('./R-LT/ANY-R', $expr);

              if (($r[-1]->nodeName eq 'array-R') or ($r[-1]->nodeName eq 'parens-R'))
                {
                  pop (@r);
                }

              $_->unbindNode for (@r);

              my ($name) = &F ('./N/n/text()', $expr); 
              $name->setData ($ptr);

              $N = $ptr;
              $s = $t->{$ptr};
            }
          else
            {
              $N = undef;
              $s = undef;
            }
        }
      # Local NPROMA array
      elsif ($s->{isFieldAPI})
        {
        }
      # Other: skip
      else
        {
          next;
        }

      if ($s)
        {
          if ($s->{blocked})
            {
              if ($blockLoop)
                {
                  &addExtraIndex ($expr, &e ('JBLK'), $s) 
                }
            }
          else # Workaround for PGI bug : add (:,:,:) to avoid PGI error (non contiguous array)
            {
              my ($rlt) = &F ('./R-LT', $expr);
              unless ($rlt)
                {
                  $rlt = &n ('<R-LT/>');
                  $expr->appendChild ($rlt);
                }
              my $e = &e ('X(' . join (',', (':') x $s->{nd}) . ')');
              my ($r) = &F ('./R-LT/ANY-R', $e);
              $rlt->appendChild ($r);
            }
          &Fxtran::Call::grokIntent ($expr, \$intent->{$N}, $find);
        }
    }


}

sub makeParallel
{
  my ($pu, $par, $t, $find, $types, $NAME, $POST, $onlysimplefields, $blockLoop) = @_;

  my %POST = map { ($_, 1) } grep { $_ } @$POST;

  my $FILTER = $par->getAttribute ('filter');

  if ($FILTER)
    {

# Remove the IF condition in this case :
#
#  IF (ANY(LLTRIG1(:))) THEN
#  
#     CALL SHALLOW_CONVECTION_PART2_SELECT &
#     & (YDCVP_SHAL, YDCVPEXT, YDCST_MNH, D, YDNSV, YDCONVPAR,                     &
#     & IKICE, LSETTADJ, OTADJS, ZPABS, ZZZ, ZT, ZRV, ZRC, ZRI,                    &
#     & LLOCHTRANS, I_KCH1, ZSHAL_ZCH1, ZRDOCP, ZTHT, ZSTHV, ZSTHES, ISDPL, ISPBL, &
#     & ISLCL, ZSTHLCL, ZSTLCL, ZSRVLCL, ZSWLCL, ZSZLCL, ZSTHVELCL, LLTRIG1,       &
#     & ZZUMF, ZTTEN, ZRVTEN, ZRCTEN, ZRITEN, ICLTOPS, ICLBASS, ZSHAL_ZCH1TENS,    &   
#     & COUNT(LLTRIG1(D%NIB:D%NIE)))
#  
#  ENDIF

      for my $if_construct (&F ('.//if-construct[./if-block/if-then-stmt[./condition-E/named-E[string(N)="?"]]]', $FILTER, $par))
        {
          my @block = &F ('./if-block', $if_construct);

          die ("Multiple blocks in filter") if (scalar (@block) > 1);

          my @node = &F ('./node()', $block[0]);

          pop (@node); shift (@node); # Remove IF (...) THEN & ENDIF

          # Drop if construct

          for (@node)
            {
              $if_construct->parent->insertBefore ($_, $if_construct);
            }

          $if_construct->unbindNode ();
        }
    }

  my $SUBROUTINE = $FILTER && $par->getAttribute ('subroutine');

  if ($SUBROUTINE)
    {
      my @proc = &F ('.//procedure-designator/named-E/N/n/text()[string(.)="?"]', $SUBROUTINE, $par);
      for my $proc (@proc)
        {
          my $stmt = &Fxtran::stmt ($proc);
          my @arg = &F ('./arg-spec/arg', $stmt);
          &Fxtran::removeListElement ($arg[-1]);
          (my $tt = $proc->textContent) =~ s/_SELECT$//o;
          $proc->setData ($tt);
        }
    }

  # Add a loop nest on blocks

  my ($stmt) = &F ('.//ANY-stmt', $par);

  $stmt or return;

  my ($JBLKMIN, $JBLKMAX);

  if ($FILTER)
    {
      ($JBLKMIN, $JBLKMAX) = ('1', 'YL_FGS%KGPBLKS');
    }
  else
    {
      ($JBLKMIN, $JBLKMAX) = ('YDCPG_OPTS%JBLKMIN', 'YDCPG_OPTS%JBLKMAX');
    }

  my $loop;

  if ($blockLoop)
    {
      ($loop) = &Fxtran::parse (fragment => << "EOF");
DO JBLK = $JBLKMIN, $JBLKMAX
CALL YLCPG_BNDS%UPDATE (JBLK)
ENDDO
EOF

      my ($enddo) = &F ('.//end-do-stmt', $loop);
      my $p = $enddo->parentNode;
     
      for my $node ($par->childNodes ())
        {
          $p->insertBefore ($node, $enddo);
        }
      
      $par->appendChild ($loop);
    }
  else
    {
      $loop = &n ('<comp/>');

      for my $node ($par->childNodes ())
        {
          $loop->appendChild ($node);
        }
      
      $par->appendChild ($loop);
    }


  if ($blockLoop)
    {
      for my $expr (&F ('.//named-E/N/n[string(.)="YDCPG_BNDS"]/text()', $par))
        {
          $expr->setData ('YLCPG_BNDS');
        }
    }

  my %intent;

  &replaceObjectExprByPointerExpr ($par, $t, $onlysimplefields, \%intent, $types, $blockLoop, $find);

  my %intent2access = qw (IN RDONLY INOUT RDWR OUT WRONLY);

  $par->insertBefore (&t ("\n"), $loop);
  $par->insertBefore (my $prep = &n ('<prep/>'), $loop);

  $par->insertAfter (my $nullify = &n ('<nullify/>'), $loop);
  $par->insertAfter (&t ("\n"), $loop);

  my $synchost;

  if ($POST{synchost}) 
    {
      my ($if_construct) = &fxtran::parse (fragment => << "EOF");
IF (FXTRAN_ACDC_LSYNCHOST ('$NAME')) THEN
ENDIF
EOF
      $par->insertAfter ($if_construct, $loop);
      my $if_block = $if_construct->firstChild;
      my $if_then = $if_block->firstChild;
      $if_block->insertAfter ($synchost = &n ('<synchost/>'), $if_then);
      $if_block->insertAfter (&t ("\n"), $if_then);
    }

  $par->insertAfter (&t ("\n"), $loop);

  if ($FILTER)
    {
      $par->insertAfter (&s ("CALL YL_FGS%SCATTER ()"), $loop);
      $par->insertAfter (&t ("\n"), $loop);
      $prep->appendChild (&s ("CALL YL_FGS%INIT (YL_$FILTER, YDCPG_OPTS%KGPTOTB)"));
      $prep->appendChild (&t ("\n"));
    }

  $par->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:COMPUTE',1,ZHOOK_HANDLE_COMPUTE)"), $loop);
  $par->insertAfter (&t ("\n"), $loop);


  $prep->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:GET_DATA',0,ZHOOK_HANDLE_FIELD_API)"));
  $prep->appendChild (&t ("\n"));

  if ($POST{synchost})
    {
      $synchost->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:SYNCHOST',0,ZHOOK_HANDLE_FIELD_API)"));
      $synchost->appendChild (&t ("\n"));
    }
  if ($POST{nullify})
    {
      $nullify->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:NULLIFY',0,ZHOOK_HANDLE_FIELD_API)"));
      $nullify->appendChild (&t ("\n"));
    }
    

  for my $ptr (sort keys (%intent))
    {
      my $s = $t->{$ptr};
      my $access = $intent2access{$intent{$ptr}};
      my $var = $s->{field}->textContent;

      my $stmt;
      if ($FILTER)
        {
          $stmt = &s ("$ptr => GATHER_HOST_DATA_$access (YL_FGS, $var)");
        }
      else
        {
          $stmt = &s ("$ptr => GET_HOST_DATA_$access ($var)");
        }
      $prep->appendChild ($stmt);
      $prep->appendChild (&t ("\n"));

      if ($POST{nullify})
        {
          $nullify->appendChild (&s ("$ptr => NULL ()"));
          $nullify->appendChild (&t ("\n"));
        }
      if ($POST{synchost})
        {
          $synchost->appendChild (&s ("$ptr => GET_HOST_DATA_RDWR ($var)"));
          $synchost->appendChild (&t ("\n"));
        }
    }
  $prep->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:GET_DATA',1,ZHOOK_HANDLE_FIELD_API)"));
  $prep->appendChild (&t ("\n"));

  $prep->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:COMPUTE',0,ZHOOK_HANDLE_COMPUTE)"));
  $prep->appendChild (&t ("\n"));

  if ($POST{nullify})
    {
      $nullify->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:NULLIFY',1,ZHOOK_HANDLE_FIELD_API)"));
      $nullify->appendChild (&t ("\n"));
      $nullify->appendChild (&t ("\n"));
    }
  if ($POST{synchost})
    {
      $synchost->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:SYNCHOST',1,ZHOOK_HANDLE_FIELD_API)"));
      $synchost->appendChild (&t ("\n"));
      $synchost->appendChild (&t ("\n"));
    }

  $prep->appendChild (&t ("\n"));

  return $par;
}

sub addExtraIndex
{
  my ($expr, $ind, $s) = @_;

  # Add reference list if needed
  
  my ($rlt) = &F ('./R-LT', $expr);

  if ((! $rlt) && ($ind->textContent eq ':'))
    {
      return;
    }

  if ($rlt && (! &F ('./ANY-R', $rlt)))
    {
      return if ($ind->textContent eq ':');
      $rlt->unbindNode ();
      $rlt = undef;
    }

  unless ($rlt)
    {
      $expr->appendChild ($rlt = &n ('<R-LT/>'));
    }

  # Add array reference if needed

  my $r = $rlt->lastChild;

  unless ($r)
    {
      $rlt->appendChild ($r = &n ('<array-R>(<section-subscript-LT>' . join (',', ('<section-subscript>:</section-subscript>') x $s->{nd}) 
                                . '</section-subscript-LT>)</array-R>'));
    }

  # Add extra block dimension

  if ($r->nodeName eq 'array-R')
    {
      my ($sslt) = &F ('./section-subscript-LT', $r);
      $sslt->appendChild (&t (', '));
      $sslt->appendChild (&n ('<section-subscript><lower-bound>' . $ind . '</lower-bound></section-subscript>'));
    }
  elsif ($r->nodeName eq 'parens-R')
    {
      my ($elt) = &F ('./element-LT', $r);
      $elt->appendChild (&t (', '));
      $elt->appendChild (&n ($ind));
    }
  else
    {
      my $stmt = &Fxtran::stmt ($expr);
      print $expr, "\n";
      print $expr->textContent, "\n";
      print $stmt->textContent, "\n";
      die $r;
    }


}

sub callParallelRoutine
{
# Process CALL statement outside PARALLEL sections
# Replace NPROMA array arguments by field descriptor arguments; no array section allowed

  my ($call, $t, $types) = @_;

  my $text = $call->textContent;

  my @arg = &F ('./arg-spec/arg/named-E/N/n/text()', $call);

  # Append YDCPG_OPTS to parallel routines argument list if the argument does not exist

  my $ydcpg_opts = 0;
  for my $arg (@arg)
    {
      if ($arg->textContent eq 'YDCPG_OPTS')
        {
          my $expr = &Fxtran::expr ($arg);
          $ydcpg_opts++ if ($expr->textContent eq 'YDCPG_OPTS');
        }
    }

  unless ($ydcpg_opts)
    {
      my ($arg_spec) = &F ('./arg-spec', $call);
      $arg_spec->appendChild (&t (','));
      $arg_spec->appendChild (&n ('<arg><arg-N><k>YDCPG_OPTS</k></arg-N>=<named-E><N><n>YDCPG_OPTS</n></N></named-E></arg>'));
    }

  my $found = 0;
  for my $arg (@arg)
    {
      my $s = $t->{$arg};
      next if ($s->{skip});
      $found++ if ($s->{object});

      my ($Arg) = &F ('ancestor::arg', $arg);

      my ($k) = &F ('./arg-N/k/text()', $Arg);

      # Is the actual argument a dummy argument of the current routine ?
      my $isArg = $s->{arg};

      if ($s->{isFieldAPI})
        {
          my ($expr) = &Fxtran::expr ($arg);
          die ("No array reference allowed in CALL statement:\n$text\n") if (&F ('./R-LT', $expr));
          $s->{field} or die &Dumper ([$arg->textContent, $s, $text]);
          $expr->replaceNode ($s->{field}->cloneNode (1));

          if ($k)
            {
              $k->setData ('YD_' . $k->textContent);
            }

          $found++;
        }
      elsif ($s->{object})
        {
          my ($expr) = &Fxtran::expr ($arg);

          my @ctl = &F ('./R-LT/component-R/ct', $expr, 1);

          if (@ctl && &Fxtran::Pointer::Object::isField ($types, $s, @ctl))
            {
              my $e = &Fxtran::Pointer::Object::getFieldFromObjectComponents ($arg->textContent, @ctl);
              $expr->replaceNode ($e);
              if ($k)
                {
                  $k->setData ('YD_' . $k->textContent);
                }
            }
        }



    }

  return $found;
}

sub setupLocalFields
{
# Use FIELD_NEW at the beginning of the routine (after the call to DR_HOOK) to create
# FIELD API objects backing local NPROMA arrays
# Use FIELD_DELETE to delete these FIELD API objects

  my ($doc, $t, $hook_suffix, $gpumemstat) = @_;

  &Fxtran::Decl::use ($doc, 'USE FXTRAN_ACDC_GPUMEM_MOD');

  my @drhook = &F ('.//if-stmt[.//call-stmt[string(.//procedure-designator)="DR_HOOK"]]', $doc);
  @drhook = @drhook[0,-1];

  my ($drhook1, $drhook2) = @drhook;

  return unless ($drhook1 && $drhook2);

  my ($p1  , $p2  ) = map { $_->parentNode          } ($drhook1, $drhook2);

  $p1->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('CREATE_TEMPORARIES$hook_suffix',1,ZHOOK_HANDLE_FIELD_API)"), $drhook1);
  $p1->insertAfter (&t ("\n"), $drhook1);


  $p2->insertBefore (&t ("\nCALL FXTRAN_ACDC_GPUMEMSTAT (__FILE__, __LINE__, \"END\")\n\n"), $drhook2) if ($gpumemstat);
  $p2->insertBefore (&s ("IF (LHOOK) CALL DR_HOOK ('DELETE_TEMPORARIES$hook_suffix',0,ZHOOK_HANDLE_FIELD_API)"), $drhook2);
  $p2->insertBefore (&t ("\n"), $drhook2);

  for my $n (sort keys (%$t))
    {
      my $s = $t->{$n};

      next unless ($s->{isFieldAPI});
      next if ($s->{pointer});

      next if ($s->{object_based} || $s->{arg});
      my @ss = &F ('./shape-spec-LT/shape-spec', $s->{as});

      my (@lb, @ub);
    
      for my $i (0 .. $#ss)
        {
          my $ss = $ss[$i];
          my @b = map { $_->textContent } &F ('./ANY-bound', $ss);
          unshift (@b, '1') if (@b == 1);
          push @lb, $b[0];
          push @ub, $b[1];
        }

      push @lb, 'YDCPG_OPTS%JBLKMIN';
      push @ub, 'YDCPG_OPTS%JBLKMAX';
      
      my $f = $s->{field}->textContent;

      my $ubounds = 'UBOUNDS=[' . join (', ', @ub) . '], ';
      my $lbounds = grep ({ $_ ne '1' } @lb) 
                  ? 'LBOUNDS=[' . join (', ', @lb) . '], '
                  : '';

      $p1->insertAfter (&s ("CALL FIELD_NEW ($f, ${ubounds}${lbounds}PERSISTENT=.TRUE.)"), $drhook1);
      $p1->insertAfter (&t ("\n"), $drhook1);

      $p2->insertBefore (&s ("IF (ASSOCIATED ($f)) CALL FIELD_DELETE ($f)"), $drhook2);
      $p2->insertBefore (&t ("\n"), $drhook2);
      

    }

  $p1->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('CREATE_TEMPORARIES$hook_suffix',0,ZHOOK_HANDLE_FIELD_API)"), $drhook1);
  $p1->insertAfter (&t ("\n"), $drhook1);
  $p1->insertAfter (&t ("\n\nCALL FXTRAN_ACDC_GPUMEMSTAT (__FILE__, __LINE__, \"BEGIN\")\n\n"), $drhook1) if ($gpumemstat);

  $p2->insertBefore (&s ("IF (LHOOK) CALL DR_HOOK ('DELETE_TEMPORARIES$hook_suffix',1,ZHOOK_HANDLE_FIELD_API)"), $drhook2);
  $p2->insertBefore (&t ("\n"), $drhook2);


}

sub getPrivateVariables
{
  my ($par, $t) = @_;

  my @N = &F ('.//named-E/N', $par);

  my %priv;

  for my $N (@N)
    {
      my $n = $N->textContent;
      my $s = $t->{$n};
      next if ($s->{isFieldAPI});
      my $expr = $N->parentNode;
      my $p = $expr->parentNode;
      
      $priv{$n}++ if (($p->nodeName eq 'E-1') || ($p->nodeName eq 'do-V'));
    }

  return sort keys (%priv);
}

sub getConstantObjects
{
  my ($par, $t) = @_;

  my @N = &F ('.//named-E/N', $par);

  my %const;

  for my $N (@N)
    {
      my $n = $N->textContent;
      my $s = $t->{$n};
      next unless ($s->{constant});
      $const{$n}++;
    }

  return sort keys (%const);
}

1;

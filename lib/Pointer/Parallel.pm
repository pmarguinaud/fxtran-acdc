package Pointer::Parallel;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Decl;
use Call;
use Data::Dumper;
use File::Basename;

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

sub fieldifyDecl
{
  my ($doc, $t) = @_;

# First step : process all NPROMA arrays declarations
# - local arrays are added an extra dimension for blocks
# - NPROMA arrays are complemented by a field object
# - NPROMA arguments arrays are replaced by field objects

  for my $N (sort keys (%$t))
    {
      my $s = $t->{$N};
      next if ($s->{skip});
  
      next unless ($s->{nproma});
  
      my $en_decl = delete $s->{en_decl};
  
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($sslt) = &F ('./array-spec/shape-spec-LT', $en_decl);
      
      # Use implicit shape, with an extra dimension for blocks
  
      for ($sslt->childNodes ())
        {
          $_->unbindNode ();
        }
      for my $i (0 .. $s->{nd})
        {
          $sslt->appendChild (&n ('<shape-spec>:</shape-spec>'));
          $sslt->appendChild (&t (',')) if ($i < $s->{nd});
        }
  
      &Decl::addAttributes ($stmt, qw (POINTER));
  
      my $type_fld = &Pointer::SymbolTable::getFieldType ($s->{nd}, $s->{ts});
      $type_fld or die "Unknown type : " . $s->{ts}->textContent;
  
      my $decl_fld;
  
      if ($s->{arg})
        {
          &Decl::removeAttributes ($stmt, 'INTENT');
          $s->{arg}->setData ("YD_$N");
          ($decl_fld) = &s ("TYPE ($type_fld), POINTER :: YD_$N");
          $s->{field} = &n ("<named-E><N><n>YD_$N</n></N></named-E>");
        }
      else
        {
          ($decl_fld) = &s ("TYPE ($type_fld), POINTER :: YL_$N");
          $s->{field} = &n ("<named-E><N><n>YL_$N</n></N></named-E>");
        }
  
      $stmt->parentNode->insertBefore ($decl_fld, $stmt);
      $stmt->parentNode->insertBefore (&t ("\n"), $stmt);
  
  
    }

}

sub makeParallel
{
  my ($par, $t, $find, $types, $NAME, $POST) = @_;

  my %POST = map { ($_, 1) } grep { $_ } split (m/,/o, $POST || '');

  my $FILTER = $par->getAttribute ('filter');

  # Add a loop nest on blocks

  my ($stmt) = &F ('.//ANY-stmt', $par);

  my $indent = &Fxtran::getIndent ($stmt);

  my $str = ' ' x $indent;

  my $KGPBLKS;

  if ($FILTER)
    {
      $KGPBLKS = 'YL_FGS%KGPBLKS';
    }
  else
    {
      $KGPBLKS = 'YDCPG_OPTS%KGPBLKS';
    }

  my ($loop) = &Fxtran::parse (fragment => << "EOF");
DO JBLK = 1, $KGPBLKS
${str}  CALL YLCPG_BNDS%UPDATE (JBLK)
${str}ENDDO
EOF

  my ($enddo) = &F ('.//end-do-stmt', $loop);
  my $p = $enddo->parentNode;

  for my $node ($par->childNodes ())
    {
      $p->insertBefore (&t (' ' x (2)), $enddo);
      &Fxtran::reIndent ($node, 2);
      $p->insertBefore ($node, $enddo);
    }
  $p->insertBefore (&t (' ' x $indent), $enddo);
  
  $par->appendChild ($loop);

  my @expr = &F ('.//named-E/N/n[string(.)="YDCPG_BNDS"]/text()', $par);

  for my $expr (@expr)
    {
      $expr->setData ('YLCPG_BNDS');
    }

  my %intent;

  # Process each expression (only NPROMA local arrays and FIELD API backed data) in the parallel section

  for my $expr (&F ('.//named-E', $par))
    {

      my ($N) = &F ('./N', $expr, 1);
      my $s = $t->{$N};
      next if ($s->{skip});

      # Object wrapping fields : replace by a pointer to data with all blocks
      if ($s->{object})
        {
          my ($name) = &F ('./N/n/text()', $expr); 
          my @ctl = &F ('./R-LT/component-R/ct', $expr, 1);
          my $e = $expr->cloneNode (1);
          my @r = &F ('./R-LT/component-R', $expr);
          $_->unbindNode for (@r);
          my $ptr = join ('_', 'Z', $N, @ctl);
          $name->setData ($ptr);

          # Create new entry in symbol table 
          # we record the pointer wich will be used to access the object component
          unless ($t->{$ptr})
            {
              my $key = join ('%', &Pointer::Object::getObjectType ($s, $N), @ctl);
              my $decl;
              eval
                {
                  $decl = &Pointer::Object::getObjectDecl ($key, $types);
                };
              if (my $c = $@)
                {
                  my $stmt = &Fxtran::stmt ($expr); 
                  die $c . $stmt->textContent;
                }
              my ($as) = &F ('.//array-spec', $decl);
              my ($ts) = &F ('./_T-spec_/*', $decl);
              my @ss = &F ('./shape-spec-LT/shape-spec', $as);
              my $nd = scalar (@ss);
              $t->{$ptr} = {
                             object => 0,
                             skip => 0,
                             nproma => 1,
                             arg => 0,
                             ts => $ts,
                             as => $as,
                             nd => $nd,
                             field => &Pointer::Object::getFieldFromObjectComponents ($N, @ctl),
                             object_based => 1, # postpone pointer declaration
                           };
            }
          $N = $ptr;
          $s = $t->{$ptr};
        }
      # Local NPROMA array
      elsif ($s->{nproma})
        {
        }
      # Other: skip
      else
        {
          next;
        }

      &addExtraIndex ($expr, &n ("<named-E><N><n>JBLK</n></N></named-E>"), $s);

      &Call::grokIntent ($expr, \$intent{$N}, $find);
    }

  my %intent2access = qw (IN RDONLY INOUT RDWR OUT WRONLY);

  $par->insertBefore (&t ("\n" . (' ' x $indent)), $loop);
  $par->insertBefore (my $prep = &n ('<prep/>'), $loop);

  $par->insertAfter (my $nullify = &n ('<nullify/>'), $loop);
  $par->insertAfter (&t ("\n" . (' ' x $indent)), $loop);

  my $synchost;

  if ($POST{synchost}) 
    {
      my ($if_construct) = &fxtran::parse (fragment => << "EOF");
IF (LSYNCHOST ('$NAME')) THEN
ENDIF
EOF
      $par->insertAfter ($if_construct, $loop);
      my $if_block = $if_construct->firstChild;
      my $if_then = $if_block->firstChild;
      $if_block->insertAfter ($synchost = &n ('<synchost/>'), $if_then);
      $if_block->insertAfter (&t ("\n"), $if_then);
    }

  $par->insertAfter (&t ("\n" . (' ' x $indent)), $loop);

  if ($FILTER)
    {
      $par->insertAfter (&s ("CALL YL_FGS%SCATTER ()"), $loop);
      $par->insertAfter (&t ("\n" . (' ' x $indent)), $loop);
      $prep->appendChild (&s ("CALL YL_FGS%INIT (YL_$FILTER, YDCPG_OPTS%KGPTOT)"));
      $prep->appendChild (&t ("\n" . (' ' x $indent)));
    }

  $par->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:COMPUTE',1,ZHOOK_HANDLE_COMPUTE)"), $loop);
  $par->insertAfter (&t ("\n" . (' ' x $indent)), $loop);


  $prep->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:GET_DATA',0,ZHOOK_HANDLE_FIELD_API)"));
  $prep->appendChild (&t ("\n" . (' ' x $indent)));

  if ($POST{synchost})
    {
      $synchost->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:SYNCHOST',0,ZHOOK_HANDLE_FIELD_API)"));
      $synchost->appendChild (&t ("\n" . (' ' x $indent)));
    }
  if ($POST{nullify})
    {
      $nullify->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:NULLIFY',0,ZHOOK_HANDLE_FIELD_API)"));
      $nullify->appendChild (&t ("\n" . (' ' x $indent)));
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
      $prep->appendChild (&t ("\n" . (' ' x $indent)));

      if ($POST{nullify})
        {
          $nullify->appendChild (&s ("$ptr => NULL ()"));
          $nullify->appendChild (&t ("\n" . (' ' x $indent)));
        }
      if ($POST{synchost})
        {
          $synchost->appendChild (&s ("$ptr => GET_HOST_DATA_RDWR ($var)"));
          $synchost->appendChild (&t ("\n" . (' ' x $indent)));
        }
    }
  $prep->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:GET_DATA',1,ZHOOK_HANDLE_FIELD_API)"));
  $prep->appendChild (&t ("\n" . (' ' x $indent)));

  $prep->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:COMPUTE',0,ZHOOK_HANDLE_COMPUTE)"));
  $prep->appendChild (&t ("\n" . (' ' x $indent)));

  if ($POST{nullify})
    {
      $nullify->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:NULLIFY',1,ZHOOK_HANDLE_FIELD_API)"));
      $nullify->appendChild (&t ("\n" . (' ' x $indent)));
      $nullify->appendChild (&t ("\n" . (' ' x $indent)));
    }
  if ($POST{synchost})
    {
      $synchost->appendChild (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:SYNCHOST',1,ZHOOK_HANDLE_FIELD_API)"));
      $synchost->appendChild (&t ("\n" . (' ' x $indent)));
      $synchost->appendChild (&t ("\n" . (' ' x $indent)));
    }

  $prep->appendChild (&t ("\n" . (' ' x $indent)));


}

sub addExtraIndex
{
  my ($expr, $ind, $s) = @_;

  # Add reference list if needed
  
  my ($rlt) = &F ('./R-LT', $expr);
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
      $sslt->appendChild (&t (','));
      $sslt->appendChild (&n ('<section-subscript><lower-bound><named-E><N><n>JBLK</n></N></named-E></lower-bound></section-subscript>'));
    }
  elsif ($r->nodeName eq 'parens-R')
    {
      my ($elt) = &F ('./element-LT', $r);
      $elt->appendChild (&t (','));
      $elt->appendChild (&n ('<named-E><N><n>JBLK</n></N></named-E>'));
    }
  else
    {
      die $r;
    }


}

sub callParallelRoutine
{
# Process CALL statement outside PARALLEL sections
# Replace NPROMA array arguments by field descriptor arguments; no array section allowed

  my ($call, $t) = @_;

  my $text = $call->textContent;

  my @arg = &F ('./arg-spec/arg/named-E/N/n/text()', $call);
  
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
   

      # Is the actual argument a dummy argument of the current routine ?
      my $isArg = $s->{arg};


      if ($s->{nproma})
        {
          my ($expr) = &Fxtran::expr ($arg);
          die ("No array reference allowed in CALL statement:\n$text\n") if (&F ('./R-LT', $expr));
          $s->{field} or die &Dumper ([$arg->textContent, $s, $text]);
          $expr->replaceNode ($s->{field}->cloneNode (1));
          $found++;
        }
      elsif ($s->{object})
        {
          my ($expr) = &Fxtran::expr ($arg);
          my @ctl = &F ('./R-LT/component-R/ct', $expr, 1);
          if (@ctl)
            {
              my $e = &Pointer::Object::getFieldFromObjectComponents ($arg->textContent, @ctl);
              $expr->replaceNode ($e);
            }
        }
    }

  return $found;
}

sub setupLocalFields
{
# Use CREATE_TEMPORARY at the beginning of the routine (after the call to DR_HOOK) to create
# FIELD API objects backing local NPROMA arrays
# Use DELETE_TEMPORARY to delete these FIELD API objects

  my ($doc, $t, $hook_suffix) = @_;

  my @drhook = &F ('.//if-stmt[.//call-stmt[string(.//procedure-designator)="DR_HOOK"]]', $doc);
  @drhook = @drhook[0,-1];

  my ($drhook1, $drhook2) = @drhook;
  my ($ind1, $ind2) = map { &Fxtran::getIndent ($_) } ($drhook1, $drhook2);
  my ($p1  , $p2  ) = map { $_->parentNode          } ($drhook1, $drhook2);

  $p1->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('CREATE_TEMPORARIES$hook_suffix',1,ZHOOK_HANDLE_FIELD_API)"), $drhook1);
  $p1->insertAfter (&t ("\n" . (' ' x $ind1)), $drhook1);

  $p2->insertBefore (&s ("IF (LHOOK) CALL DR_HOOK ('DELETE_TEMPORARIES$hook_suffix',0,ZHOOK_HANDLE_FIELD_API)"), $drhook2);
  $p2->insertBefore (&t ("\n" . (' ' x $ind2)), $drhook2);

  for my $n (sort keys (%$t))
    {
      my $s = $t->{$n};

      next unless ($s->{nproma});
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

      push @lb, '1';
      push @ub, 'YDCPG_OPTS%KGPBLKS';
      
      my $f = $s->{field}->textContent;

      my $ubounds = 'UBOUNDS=[' . join (', ', @ub) . '], ';
      my $lbounds = grep ({ $_ ne '1' } @lb) 
                  ? 'LBOUNDS=[' . join (', ', @lb) . '], '
                  : '';

      $p1->insertAfter (&s ("CALL CREATE_TEMPORARY_LU ($f, ${ubounds}${lbounds}PERSISTENT=.TRUE.)"), $drhook1);
      $p1->insertAfter (&t ("\n" . (' ' x $ind1)), $drhook1);

      $p2->insertBefore (&s ("IF (ASSOCIATED ($f)) CALL DELETE_TEMPORARY ($f)"), $drhook2);
      $p2->insertBefore (&t ("\n" . (' ' x $ind2)), $drhook2);
      

    }

  $p1->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('CREATE_TEMPORARIES$hook_suffix',0,ZHOOK_HANDLE_FIELD_API)"), $drhook1);
  $p1->insertAfter (&t ("\n" . (' ' x $ind1)), $drhook1);

  $p2->insertBefore (&s ("IF (LHOOK) CALL DR_HOOK ('DELETE_TEMPORARIES$hook_suffix',1,ZHOOK_HANDLE_FIELD_API)"), $drhook2);
  $p2->insertBefore (&t ("\n" . (' ' x $ind2)), $drhook2);


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
      next if ($s->{nproma});
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

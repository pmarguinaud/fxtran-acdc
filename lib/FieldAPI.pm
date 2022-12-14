package FieldAPI;
#
use strict;
use FileHandle;
use Data::Dumper;
use Storable;
use Memoize;
use Subroutine;
use Fxtran;

sub isUpdatable
{
  my $UI = &getUpdatableInfo ();
  return $UI->{$_[0]};
}

sub getFieldAPIMember
{
  my $TI = &getTypeInfo ();

  my $t = $TI;

  for my $x (@_)
    {
      if (exists ($t->{$x}))
        {
          my $ref = ref ($t->{$x});
          if ($ref && ($ref eq 'HASH'))
            {
              $t = $t->{$x};
            }
          elsif ($ref && ($ref eq 'ARRAY'))
           {
             $t = $t->{$x};
           }
        }
      else
        {
          goto NOTFOUND;
        }
    }

  return $t;
NOTFOUND:
  return;
}

sub isFieldAPIMember
{
  my $TI = &getTypeInfo ();

  my $t = $TI;

  my $X = pop (@_);

  for my $x (@_)
    {
      if (exists ($t->{$x}))
        {
          my $ref = ref ($t->{$x});
          if ($ref && ($ref eq 'HASH'))
            {
              $t = $t->{$x};
            }
          else
           {
             goto NOTFOUND;
           }
        }
      else
        {
          goto NOTFOUND;
        }
    }

  my $ref = ref ($t);
  if ($ref && ($ref eq 'HASH'))
    {
      my %t = %$t;
      for my $k (sort keys (%$t))
        {
          next unless (ref (my $v = $t->{$k}) eq 'ARRAY');
          return $v if ($v->[0] eq $X);
        }
    }
  
NOTFOUND:
  return;
}

&memoize ('getFieldAPIMember');
&memoize ('isFieldAPIMember');
&memoize ('isUpdatable');


{

use FindBin qw ($Bin);
my $TI;

sub getTypeInfo
{
  $TI = &Storable::retrieve ("$Bin/types.dat") unless ($TI);
  return $TI;
}

my $UI;

sub getUpdatableInfo
{
  $UI = &Storable::retrieve ("$Bin/update_view.dat") unless ($UI);
  return $UI;
}

}

sub getExprFieldAPIMember
{
  my ($expr, $type, $TI) = @_;
  return unless ($type);
  my @ct = &F ('./R-LT/component-R/ct', $expr, 1);
  return &getFieldAPIMember ($type, @ct);
}
  
sub pointers2FieldAPIPtrHost
{
  &pointers2FieldAPIPtr (@_, what => 'host');
}

sub pointers2FieldAPIPtrDevice
{
  &pointers2FieldAPIPtr (@_, what => 'device');
}

sub pointers2FieldAPIPtr
{
  my $d = shift;
  my %args = @_;

  my $s = $args{section} || $d;

  my $TI = &getTypeInfo ();
  
  my @T = sort keys (%$TI);

  my $PTR;

  if (lc ($args{what}) eq 'host')
    {
      $PTR = 'PTR';
    }
  elsif (lc ($args{what}) eq 'device')
    {
      $PTR = 'DEVPTR';
    }
  die &Dumper (\%args) unless ($PTR);
  
  for my $T (@T)
    {
      my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
      for my $F (@F)
        {
          my @expr = &F ('.//named-E[string(N)="?"][R-LT]', $F, $s);
          for my $expr (@expr)
            {
              my @ct = &F ('./R-LT/component-R/ct', $expr);
              my $fd = &getFieldAPIMember ($T, map ({ $_->textContent } @ct));
              next unless ($fd);
  
              my ($f, $d) = @$fd;
  
              $ct[-1]->replaceNode (my $ct = &n ("<ct>$f</ct>"));
              my ($rlt) = &F ('./R-LT', $expr);
              $rlt->insertAfter (my $ptr = &n ("<component-R>%<ct>$PTR</ct></component-R>"), $ct->parentNode);
  
              my ($ar) = &F ('following-sibling::ANY-R', $ptr);
  
              unless ($ar)
                {
                  # Add array reference
                  $ar = &n ('<array-R>(<section-subscript-LT>' . join (',', ('<section-subscript>:</section-subscript>') x $fd->[1]) . '</section-subscript-LT>)</array-R>');
                  $ptr->parentNode->insertAfter ($ar, $ptr);
                }
  
              # Add block number as last index
              if ($ar->nodeName eq 'parens-R')
                {
                  my ($lt) = &F ('./element-LT', $ar);
                  $lt->appendChild (&t (','));
                  $lt->appendChild (&n ('<element><named-E><N><n>YDCPG_BNDS</n></N><R-LT>' 
                                      . '<component-R>%<ct>KBL</ct></component-R></R-LT></named-E>' 
                                      . '</element>'))
                }
              elsif ($ar->nodeName eq 'array-R')
                {
                  my ($lt) = &F ('./section-subscript-LT', $ar);
                  $lt->appendChild (&t (','));
                  $lt->appendChild (&n ('<section-subscript><lower-bound><named-E><N><n>YDCPG_BNDS</n></N><R-LT>' 
                                      . '<component-R>%<ct>KBL</ct></component-R></R-LT></named-E></lower-bound>' 
                                      . '</section-subscript>'))
                }
              else
                {
                  die $expr->textContent;
                }
              
            }
        }
    }
}


sub makeSyncContext
{
  my $d = shift;

  my $TI = &getTypeInfo ();
  my @T = sort keys (%$TI);

  # Find objects containing field API pointers and map them to their types

  my %T;
  for my $T (@T)
    {
      my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
      for my $F (@F)
        {
          $T{$F} = $T;
        }
    }
  my @F = sort keys (%T);

  return {TI => $TI, T => \%T};
}

sub makeSyncHostSection
{
  &makeSyncSection (@_, what => 'host');
}

sub makeSyncSection
{
  my $d = shift;
  my %args = @_;

  my ($s, $ctx, $suffix, $name) = @args{qw (section context suffix name)};

  $s = $s->cloneNode (1);

  my $TI = $ctx->{TI};
  my %T = %{ $ctx->{T} };
  my @F = sort keys (%T);

  # This anonymous sub tests whether and expression contains NPROMA data (backed by field API)
  my $isNproma = sub
  {
    my $expr = shift;
    my ($N) = &F ('./N', $expr, 1);
    return &getExprFieldAPIMember ($expr, $T{$N}, $TI);
  };
  
  my $pu = &n ("<program-unit><subroutine-stmt>SUBROUTINE <subroutine-N><N><n>$name</n></N></subroutine-N></subroutine-stmt>
<end-subroutine-stmt>END SUBROUTINE</end-subroutine-stmt></program-unit>");

  &Decl::use ($pu, 'USE FIELD_MODULE, ONLY : FIELD_BASIC');
  
  $pu->insertBefore ($s, $pu->lastChild);
  $pu->insertBefore (&t ("\n"), $pu->lastChild);

  $s = $pu;

  # Handle call statements

  my @call = &F ('.//call-stmt', $s);
  my %proc;

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
      my @expr = &F ('./arg-spec/arg/named-E', $call);
      for my $expr (@expr)
        {
          next unless ($isNproma->($expr));
          push @{ $proc{$proc->textContent} }, $proc;
          last;
        }
    }

  while (my ($name, $list) = each (%proc))
    {
      for my $proc (@$list)
        {
          $proc->setData ($name . $suffix);
        }
    }

  &removeNPROMAIfConstruct ($s, $isNproma);

  &changeFieldAPIAccessToSimpleStatements ($s, $isNproma);

  &useFieldAPIObjects ($s, \%T, \@F);

  &removeJLONJLEVLoops ($s);

  &simplifyReadWriteAssignemnts ($s);

  &changeSimpleAssignmentsToSync ($s, what => $args{what});

  &Scope::removeWhiteLines ($s);

  return $s;
}

sub removeJLONJLEVLoops 
{
  my $d = shift;

  # Remove loops on JLEV or JLON

  for my $ind (qw (JLON JLEV))
   {
     my @do = &F ('.//do-construct[./do-stmt[string(do-V)="?"]]', $ind, $d);
     
     for my $do (@do)
       {   
         $do->firstChild->unbindNode;
         $do->lastChild->unbindNode;
 
         # Remove calculations involving non-NPROMA data

         for (&F ('.//a-stmt[string(E-1)!="ZDUM" and string(E-2)!="ZDUM"]', $do))
           {
             $_->unbindNode ();
           }

         # Remove construct

         my @nodes = &F ('./node()', $do);
         for (@nodes)
           {
             $do->parentNode->insertBefore ($_, $do);
           }
         $do->unbindNode (); 
       }   
   }

  $d->normalize ();

}

sub simplifyReadWriteAssignemnts
{
  my $d = shift;

  my @stmt = &F ('.//a-stmt[string(E-1)="ZDUM" or string(E-2)="ZDUM"]', $d);

  STMT : for my $stmt (@stmt)
    {
      my ($E1, $E2) = &F ('./E-1|./E-2', $stmt, 1);
      next unless (my @p = &F ('ancestor::*', $stmt));

      my $C = &n ("<C>! " . $stmt->textContent . "</C>");
      $stmt->replaceNode ($C);

      # Remove equivalent statement in same block

      for my $s (&F ('./a-stmt[string(.)="?"]', $stmt->textContent, $p[-1]))
        {
          $s->unbindNode ();
        }

      $C->replaceNode ($stmt);

      if ($E2 eq 'ZDUM')
        {
          # Remove RDONLY statement in same block
          for my $s (&F ('./a-stmt[string(.)="ZDUM = ?"]', $E1, $p[-1]))
            {
              $s->unbindNode ();
            }
        }  
      pop (@p);

      for my $p (@p)
        {
          # Outer block contains the same statement -> remove current statement
          if (my @s = &F ('./a-stmt[string(.)="?"]', $stmt->textContent, $p))
            {
              $stmt->unbindNode ();
              next STMT;
            }
          if ($E1 eq 'ZDUM')
            {
              # Current statement is RDONLY, but outer block contains RDWR -> remove current statement
              if (&F ('./a-stmt[string(.)="? = ZDUM"]', $E2, $p))
                {
                  $stmt->unbindNode ();
                  next STMT;
                }
            }
        }
    }

  &Construct::removeEmptyConstructs ($d);

}


sub removeNPROMAIfConstruct
{
  my ($s, $isNproma) = @_;

  # Remove if construct where conditions involve NPROMA expressions (local arrays of field API pointers)

  for my $ifc (&F ('.//if-construct', $s))
    {
      my $found = 0;
      for my $expr (&F ('./if-block/ANY-stmt/condition-E//named-E', $ifc))
        {
          if ($isNproma->($expr))
            {
              $found = 1;
              last;
            }
        }

      next unless ($found);
      my @block = &F ('./if-block', $ifc);
      for (@block)
        {
          $_->firstChild->unbindNode;
        }
      $block[-1]->lastChild->unbindNode ();
      for my $node (&F ('./if-block/node()', $ifc))
        {
          $ifc->parentNode->insertBefore ($node, $ifc);
        }
      $ifc->unbindNode ();
    }

}

sub changeFieldAPIAccessToSimpleStatements
{
  my ($s, $isNproma) = @_;

  # Find where field data is written to or read from

  for my $stmt (&F ('.//a-stmt', $s))
    {
      my $indent = ' ' x &Fxtran::getIndent ($stmt);
      my $n = 0;
      for my $expr (&F ('./E-1//named-E', $stmt))
        {
          next unless ($isNproma->($expr));

          # Remove last reference if it is an array reference

          my @r = &F ('./R-LT/ANY-R', $expr);
          my $ar = $r[-1];
          if ($ar && (($ar->nodeName eq 'parens-R') || ($ar->nodeName eq 'array-R')))
            {
              $ar->previousSibling->unbindNode ()
                if ($ar->previousSibling->nodeName eq '#text');
              $ar->unbindNode ();
            }
          my $dum = &s ($expr->textContent . " = ZDUM");
          $stmt->parentNode->insertBefore ($dum, $stmt);
          $stmt->parentNode->insertBefore (&t ("\n$indent"), $stmt);

          $n++;
        }
      for my $expr (&F ('./E-2//named-E', $stmt))
        {
          next unless ($isNproma->($expr));

          # Remove last reference if it is an array reference

          my @r = &F ('./R-LT/ANY-R', $expr);
          my $ar = $r[-1];
          if ($ar && (($ar->nodeName eq 'parens-R') || ($ar->nodeName eq 'array-R')))
            {
              $ar->previousSibling->unbindNode ()
                if ($ar->previousSibling->nodeName eq '#text');
              $ar->unbindNode ();
            }

          my $dum = &s ("ZDUM = " . $expr->textContent);
          $stmt->parentNode->insertBefore ($dum, $stmt);
          $stmt->parentNode->insertBefore (&t ("\n$indent"), $stmt);

          $n++;
        }
      $n && $stmt->unbindNode ();
    }
}

sub useFieldAPIObjects
{
  my ($d, $Th, $Fl) = @_;

  # Use the field API object

  my %YLFLDPTR;

  for my $F (@$Fl)
    {
      my $T = $Th->{$F};
      my @expr = &F ('.//named-E[string(N)="?"][R-LT]', $F, $d);
      for my $expr (@expr)
        {

          my @ct = &F ('./R-LT/component-R/ct', $expr);
          my $fd = &getFieldAPIMember ($T, map ({ $_->textContent } @ct));
          next unless ($fd);
  
          my ($f, $d) = @$fd;
          $ct[-1]->replaceNode (my $ct = &n ("<ct>$f</ct>"));

          my $stmt = &Fxtran::stmt ($expr);

          if ($stmt->nodeName eq 'call-stmt')
            {
              my $target = $expr->cloneNode (1);
              my %basic = map { m/^YLFLDPTR/o ? ($_, 1) : () } &F ('./arg-spec/arg/named-E/N', $stmt, 1);
              my $basic;
              for (my $i = 0; ; $i++)
                {
                  $basic = "YLFLDPTR" . $i;
                  last unless ($basic{$basic});
                }
              $YLFLDPTR{$basic}++;
              $expr->replaceNode (&e ($basic));
              my $assoc = &s ("$basic => " . $target->textContent);
              my $indent = "\n" . (' ' x &Fxtran::getIndent ($stmt));
              $stmt->parentNode->insertBefore ($assoc, $stmt);
              $stmt->parentNode->insertBefore (&t ($indent), $stmt);
            }
          
        }
    }
 
  &Decl::declare ($d,
                  map ({ &s ("CLASS (FIELD_BASIC), POINTER :: $_") } sort keys (%YLFLDPTR)));

}

sub changeSimpleAssignmentsToSync
{
  my $d = shift;
  my %args = @_;

  my @stmt = &F ('.//a-stmt[string(E-1)="ZDUM" or string(E-2)="ZDUM"]', $d);

  my $SYNC = 'SYNC_' . uc ($args{what});

  for my $stmt (@stmt)
    {
      my ($E1, $E2) = &F ('./E-1|./E-2', $stmt, 1);
      if ($E2 eq 'ZDUM')
        {
          $stmt->replaceNode (&s ("IF (ASSOCIATED ($E1)) CALL $E1%${SYNC}_RDWR"));
        }
      else
        {
          $stmt->replaceNode (&s ("IF (ASSOCIATED ($E2)) CALL $E2%${SYNC}_RDONLY"));
        }
    }
}



sub makeSyncHost
{
  &makeSync (@_, what => 'host');
}

sub makeSyncDevice
{
  &makeSync (@_, what => 'device');
}

sub makeSync
{
  use Decl;
  use Construct;
  use Inline;
  use FieldAPI;
  use Construct;

  my $d = shift;
  my %args = @_;

  &Decl::forceSingleDecl ($d);
  &Construct::changeIfStatementsInIfConstructs ($d);
  &Inline::inlineContainedSubroutines ($d);

  my $suffix = '_SYNC_' . uc ($args{what});

  my @en_decl = (&F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="YDCPG_OPTS%KLON"]]', $d),
                 &F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="KLON"]]', $d));

  my @NPROMA = map { &F ('./EN-N', $_, 1) } @en_decl;

  my %LOCAL;

  # Convert NPROMA arrays into field API objects

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);
      my ($as) = &F ('./array-spec', $en_decl);
      $as->unbindNode ();
      my ($ts) = &F ('./_T-spec_/node()', $stmt);
      
      if (my ($intent) = &F ('./attribute[string(attribute-N)="INTENT"]', $stmt))
        {
          $ts->replaceNode (&n ('<derived-T-spec>CLASS(<T-N><N><n>FIELD_BASIC</n></N></T-N>)</derived-T-spec>'));
        }
      else
        {
          my @ss = &F ('./shape-spec-LT/shape-spec', $as);
          my $d = scalar (@ss) + 1;
          $ts->replaceNode (&n ("<derived-T-spec>CLASS(<T-N><N><n>FIELD_BASIC</n></N></T-N>)</derived-T-spec>"));
          # Set to NULL
          $en_decl->appendChild (&t (' => '));
          $en_decl->appendChild (&e ('NULL ()'));
          $LOCAL{$N} = 1;
        }

      my ($dd) = &F ('./text()[contains(.,"::")]', $stmt);
      $stmt->insertBefore (&t (', '), $dd);
      $stmt->insertBefore (&n ('<attribute><attribute-N>POINTER</attribute-N></attribute>'), $dd);
    }

  # Remove array reference of NPROMA arrays

  for my $NPROMA (@NPROMA)
    {
      my @expr = &F ('.//named-E[string(N)="?"]', $NPROMA, $d);
      for my $expr (@expr)
        {
          my ($rlt) = &F ('./R-LT', $expr);
          $rlt && $rlt->unbindNode ();
        }
    }


  my $TI = &getTypeInfo ();
  my @T = sort keys (%$TI);

  # Find objects containing field API pointers and map them to their types

  my %T;
  for my $T (@T)
    {
      my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
      for my $F (@F)
        {
          $T{$F} = $T;
        }
    }
  my @F = sort keys (%T);

  # This anonymous sub tests whether and expression contains NPROMA data (field API or local/argument array)
  my $isNproma = sub
  {
    my $expr = shift;
    my ($N) = &F ('./N', $expr, 1);
    return 1 if (grep { $N eq $_ } @NPROMA);
    return &getExprFieldAPIMember ($expr, $T{$N}, $TI);
  };
  

  # Handle call statements

  my @call = &F ('.//call-stmt', $d);
  my %proc;

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
      my @expr = &F ('./arg-spec/arg/named-E', $call);
      for my $expr (@expr)
        {
          next unless ($isNproma->($expr));
          push @{ $proc{$proc->textContent} }, $proc;
          last;
        }
    }

  while (my ($name, $list) = each (%proc))
    {
      for my $proc (@$list)
        {
          $proc->setData ($name . $suffix);
        }

      next unless (my ($filename) = &F ('.//include/filename[string(.)="?"]/text()', lc ($name) . '.intfb.h', $d));
      $filename->setData (lc ($name) . lc ($suffix) . '.intfb.h');
    }

  &removeNPROMAIfConstruct ($d, $isNproma);

  &changeFieldAPIAccessToSimpleStatements ($d, $isNproma);

  &useFieldAPIObjects ($d, \%T, \@F);

  &removeJLONJLEVLoops ($d);

  &simplifyReadWriteAssignemnts ($d);

  &changeSimpleAssignmentsToSync ($d, what => $args{what});

  &Subroutine::addSuffix ($d, $suffix);
  
  &Decl::use ($d, 'USE FIELD_MODULE, ONLY : FIELD_BASIC');

  &Scope::removeWhiteLines ($d);

}



1;

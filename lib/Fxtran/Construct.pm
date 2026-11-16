package Fxtran::Construct;

=head1 NAME

Fxtran::Construct

=head1 DESCRIPTION

This module provides functions whose purpose is construct reorganization.

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;


sub changeIfStatementsInIfConstructs
{

=head2 changeIfStatementsInIfConstructs

Change statements such as:

  IF (LLCOND) CALL SUBR (...)

into:

  IF (LLCOND) THEN
    CALL SUBR (...)
  ENDIF

=cut

  my $d = shift;

  # Change if statements into if constructs

  my @if_stmt = &F ('.//if-stmt', $d);

  for my $if_stmt (@if_stmt)
    {
      my ($action) = &F ('./action-stmt', $if_stmt);
      my ($stmt) = &F ('./ANY-stmt', $action);

      die $if_stmt->textContent unless ($stmt);

      next if ($stmt->nodeName eq 'call-stmt' && $stmt->textContent =~ m/DR_HOOK/o);

      $action->unbindNode ();

      $stmt = $stmt->textContent;
      my $if = $if_stmt->textContent;

      my ($if_construct) = &Fxtran::parse (fragment => << "EOF", fopts => [qw (-line-length 800 -construct-tag)]);
$if THEN
$stmt
ENDIF
EOF

     $if_stmt->replaceNode ($if_construct);
    }

}


# Operator classes: binary operators of a class form chains evaluated from left to right
# (from right to left for **)
my %CLASS =
(
  '+'  => 'add', '-' => 'add',
  '*'  => 'mul', '/' => 'mul',
  '**' => 'pow',
  '//' => 'cat',
  (map { ($_, 'cmp') } qw (== /= < <= > >= .EQ. .NE. .LT. .LE. .GT. .GE.)),
  (map { ($_, 'log') } qw (.AND. .OR. .EQV. .NEQV.)),
);

my %CMP = ('.EQ.' => '==', '.NE.' => '/=', '.LT.' => '<', '.LE.' => '<=', '.GT.' => '>', '.GE.' => '>=');

sub exactSingle
{
  my $v = shift;
  return unpack ('f', pack ('f', $v)) == $v;
}

sub literalValue
{

=head2 literalValue

Return the value of a literal expression (C<literal-E> or C<string-E>), as a hash
with keys C<type> (C<I>, C<R>, C<L> or C<C>), C<value>, C<kind> (the kind suffix, if any) and
C<text>; return nothing if the literal is not supported (C<D> exponents, BOZ, kind prefixes of strings).

=cut

  my $e = shift;
  my $text = &strip ($e->textContent);

  if ($e->nodeName eq 'string-E')
    {
      return unless ($text =~ m/^(["'])(.*)\1$/so);
      my ($q, $s) = ($1, $2);
      $s =~ s/$q$q/$q/g;
      return {type => 'C', value => $s, kind => '', text => $text, quote => $q};
    }

  # Blanks are not significant in literals, but they are in strings
  $text = $e->textContent;
  $text =~ s/\s+//go;

  if ($text =~ m/^\.(TRUE|FALSE)\.(?:_(\w+))?$/io)
    {
      return {type => 'L', value => (uc ($1) eq 'TRUE' ? 1 : 0), kind => $2 || '', text => $text};
    }
  if ($text =~ m/^(\d+)(?:_(\w+))?$/o)
    {
      return {type => 'I', value => $1 + 0, kind => $2 || '', text => $text};
    }
  if ($text =~ m/^((?:\d+\.\d*|\.\d+)(?:[eE][-+]?\d+)?|\d+[eE][-+]?\d+)(?:_(\w+))?$/o)
    {
      return {type => 'R', value => $1 + 0, kind => $2 || '', text => $text};
    }

  return;
}

sub constantNode
{

=head2 constantNode

Return an expression node for the constant C<$v> (see C<literalValue>), to be placed under
C<$parent>: negative numbers are parenthesized inside operator expressions.

=cut

  my ($v, $parent) = @_;

  # A value coming from a literal keeps its text
  if (defined ($v->{text}))
    {
      my $text = $v->{text};
      $text = "($text)" if (($text =~ m/^-/o) && $parent && ($parent->nodeName =~ m/^(?:op|parens)-E$/o));
      return &e ($text);
    }

  my $text;

  if ($v->{type} eq 'L')
    {
      $text = $v->{value} ? '.TRUE.' : '.FALSE.';
    }
  elsif ($v->{type} eq 'I')
    {
      $text = abs ($v->{value});
    }
  elsif ($v->{type} eq 'R')
    {
      $text = sprintf ('%.17g', abs ($v->{value}));
      if ($text =~ m/^([^e]*)e([-+]?)0*(\d+)$/o)
        {
          my ($m, $s, $x) = ($1, $2, $3);
          $m .= '.' unless ($m =~ m/\./o);
          $text = $m . 'E' . ($s eq '-' ? '-' : '') . $x;
        }
      elsif ($text !~ m/\./o)
        {
          $text .= '.';
        }
    }
  elsif ($v->{type} eq 'C')
    {
      my $q = $v->{quote} || "'";
      (my $s = $v->{value}) =~ s/$q/$q$q/g;
      return &e ($q . $s . $q);
    }

  $text .= '_' . $v->{kind} if ($v->{kind});

  if ((($v->{type} eq 'I') || ($v->{type} eq 'R')) && ($v->{value} < 0))
    {
      $text = '-' . $text;
      $text = "($text)" if ($parent && ($parent->nodeName =~ m/^(?:op|parens)-E$/o));
    }

  return &e ($text);
}

sub strip
{
  (my $s = shift) =~ s/\s+//go;
  return $s;
}

sub children
{
  my $e = shift;
  return grep { $_->nodeType == 1 } $e->childNodes ();
}

sub chain
{

=head2 chain

Return the operands and operators of the chain of binary operators of class C<$class>
whose top is C<$e>, from left to right (operands at even positions).

=cut

  my ($e, $class) = @_;

  my @c = &children ($e);

  return ($e) unless (($e->nodeName eq 'op-E') && (@c == 3) && ($c[1]->nodeName eq 'op')
                   && (($CLASS{&strip ($c[1]->textContent)} || '') eq $class));

  return (&chain ($c[0], $class), $c[1], &chain ($c[2], $class));
}

# Number of expressions changed by foldExpression
my $CHANGED = 0;

sub replaceByConstant
{
  my ($e, $v) = @_;
  my $c = &constantNode ($v, $e->parentNode);
  # Nothing to do if the text is the same (a parenthesized negative literal)
  return $v if (&strip ($c->textContent) eq &strip ($e->textContent));
  $e->replaceNode ($c);
  $CHANGED++;
  return $v;
}

sub replaceByExpression
{

=head2 replaceByExpression

Replace the expression C<$e> by the expression C<$x> (one of its operands or arguments),
parenthesised if it is an operator expression and C<$e> is an operand.

=cut

  my ($e, $x) = @_;

  if (($x->nodeName eq 'op-E') && ($e->parentNode->nodeName eq 'op-E'))
    {
      my $p = &n ('<parens-E>(<x/>)</parens-E>');
      my ($y) = &F ('./x', $p);
      $y->replaceNode ($x);
      $x = $p;
    }

  $e->replaceNode ($x);
  $CHANGED++;
}

sub arguments
{

=head2 arguments

Return the expressions of the positional arguments of the function reference C<$e>,
or nothing if it has keyword arguments.

=cut

  my $e = shift;
  my @r = &F ('./R-LT/ANY-R', $e);
  return unless (@r == 1);
  my @a = &F ('./element-LT/element|./arg-spec/arg', $r[0]);
  return if (grep { &F ('./arg-N', $_) } @a);
  my @x = map { grep { $_->nodeName =~ m/-E$/o } &children ($_) } @a;
  return unless (@x == @a);
  return @x;
}

sub arith
{

=head2 arith

Evaluate C<$x OP $y> for numbers (C<I> or C<R>), following Fortran rules; return nothing when
the operation is not folded: reals of different kinds, real results not exactly representable
in single precision, integer overflow or division by zero.

=cut

  my ($x, $op, $y) = @_;

  my $type = (($x->{type} eq 'R') || ($y->{type} eq 'R')) ? 'R' : 'I';

  # Kinds must agree
  return if (($x->{type} eq $y->{type}) && ($x->{kind} ne $y->{kind}));
  my $kind = $x->{type} eq $type ? $x->{kind} : $y->{kind};

  my ($a, $b) = ($x->{value}, $y->{value});

  if ($type eq 'R')
    {
      return unless (&exactSingle ($a) && &exactSingle ($b));
    }

  my $r;

  if ($op eq '+')
    {
      $r = $a + $b;
    }
  elsif ($op eq '-')
    {
      $r = $a - $b;
    }
  elsif ($op eq '*')
    {
      $r = $a * $b;
    }
  elsif ($op eq '/')
    {
      return if ($b == 0);
      $r = $type eq 'I' ? int ($a / $b) : $a / $b;
    }
  elsif ($op eq '**')
    {
      return unless (($type eq 'I') && ($b >= 0));
      $r = $a ** $b;
    }
  else
    {
      return;
    }

  if ($type eq 'R')
    {
      return unless (&exactSingle ($r));
    }
  else
    {
      return unless (abs ($r) < 2 ** 31);
    }

  return {type => $type, value => $r, kind => $kind};
}

sub compare
{

=head2 compare

Evaluate the comparison C<$x OP $y> (numbers, or strings compared after padding the shortest
one with blanks); return nothing if it is not folded.

=cut

  my ($x, $op, $y) = @_;

  $op = $CMP{$op} || $op;

  my $c;

  if (($x->{type} eq 'C') && ($y->{type} eq 'C'))
    {
      my ($s, $t) = ($x->{value}, $y->{value});
      my $n = length ($s) > length ($t) ? length ($s) : length ($t);
      $s .= ' ' x ($n - length ($s));
      $t .= ' ' x ($n - length ($t));
      $c = $s cmp $t;
    }
  elsif (($x->{type} =~ m/^[IR]$/o) && ($y->{type} =~ m/^[IR]$/o))
    {
      if ($x->{type} eq 'R' || $y->{type} eq 'R')
        {
          # Reals: identical texts, or exact values
          if (defined ($x->{text}) && defined ($y->{text}) && ($x->{text} eq $y->{text}))
            {
              $c = 0;
            }
          else
            {
              return unless (&exactSingle ($x->{value}) && &exactSingle ($y->{value}));
              return if (($x->{type} eq $y->{type}) && ($x->{kind} ne $y->{kind}));
            }
        }
      $c = $x->{value} <=> $y->{value} unless (defined ($c));
    }
  else
    {
      return;
    }

  my %r = ('==' => $c == 0, '/=' => $c != 0, '<' => $c < 0, '<=' => $c <= 0, '>' => $c > 0, '>=' => $c >= 0);

  return {type => 'L', value => $r{$op} ? 1 : 0, kind => ''};
}

sub singleArgument
{

=head2 singleArgument

Return the expression of the single positional argument of the function reference C<$e>,
if any.

=cut

  my $e = shift;
  my @r = &F ('./R-LT/ANY-R', $e);
  return unless (@r == 1);
  my @a = &F ('./element-LT/element|./arg-spec/arg|./section-subscript-LT/section-subscript', $r[0]);
  return unless (@a == 1);
  return if (&F ('./arg-N', $a[0]));
  my @x = grep { $_->nodeName =~ m/-E$/o } &children ($a[0]);
  @x = &F ('./lower-bound/ANY-E', $a[0]) if ($a[0]->nodeName eq 'section-subscript' && ! &F ('./upper-bound|./text()[contains(.,":")]', $a[0]));
  return unless (@x == 1);
  return $x[0];
}

sub foldExpression
{

=head2 foldExpression

Fold the expression C<$e>, replacing its constant sub-expressions by literals, and return its
value if it is constant (see C<literalValue>):

=over 4

=item

integer and real arithmetic (C<+ - * / **>); chains of operators of the same precedence are
evaluated from left to right, as a whole (never a part of a chain); real operations are folded
only when their operands and results are exactly representable in single precision, so that the
value does not depend on the precision;

=item

comparisons (numbers, and strings padded with blanks), concatenation, C<TRIM> and C<ADJUSTL> of
strings;

=item

logical operators, including partially (C<X .AND. .TRUE.> is C<X>, C<X .OR. .TRUE.> is C<.TRUE.>);

=item

C<MERGE> with a constant mask, replaced by one of its arguments;

=item

parentheses around a constant.

=back

Sub-expressions of variables and function references (subscripts, arguments) are not visited:
they are folded separately (see C<foldExpressions>).

=cut

  my $e = shift;

  my $name = $e->nodeName;

  if (($name eq 'literal-E') || ($name eq 'string-E'))
    {
      return &literalValue ($e);
    }

  if ($name eq 'parens-E')
    {
      my ($x) = grep { $_->nodeName =~ m/-E$/o } &children ($e);
      return unless ($x);
      my $v = &foldExpression ($x);
      return $v ? &replaceByConstant ($e, $v) : undef;
    }

  if ($name eq 'named-E')
    {
      my ($N) = &F ('./N', $e, 1);
      if ($N eq 'MERGE')
        {
          my @x = &arguments ($e);
          return unless (@x == 3);
          my $m = &foldExpression ($x[2]);
          return unless ($m && ($m->{type} eq 'L'));
          my $x = $m->{value} ? $x[0] : $x[1];
          &replaceByExpression ($e, $x);
          return &foldExpression ($x->parentNode->nodeName eq 'parens-E' ? $x->parentNode : $x);
        }
      return unless (($N eq 'TRIM') || ($N eq 'ADJUSTL'));
      my $x = &singleArgument ($e);
      return unless ($x);
      my $v = &foldExpression ($x);
      return unless ($v && ($v->{type} eq 'C'));
      my $s = $v->{value};
      if ($N eq 'TRIM')
        {
          $s =~ s/ +$//o;
        }
      else
        {
          my ($b) = ($s =~ m/^( *)/o);
          $s = substr ($s, length ($b)) . $b;
        }
      return &replaceByConstant ($e, {%$v, value => $s, text => undef});
    }

  return unless ($name eq 'op-E');

  my @c = &children ($e);

  # Unary operators

  if ($c[0]->nodeName eq 'op')
    {
      my $op = &strip ($c[0]->textContent);
      my $literal = $c[1]->nodeName eq 'literal-E';
      my $v = &foldExpression ($c[1]);
      return unless ($v);
      if (($op eq '.NOT.') && ($v->{type} eq 'L'))
        {
          return &replaceByConstant ($e, {%$v, value => 1 - $v->{value}, text => undef});
        }
      if ((($op eq '-') || ($op eq '+')) && ($v->{type} =~ m/^[IR]$/o))
        {
          my $w = {%$v, value => $op eq '-' ? - $v->{value} : $v->{value}};
          # A signed literal is left as it is
          return {%$w, text => $op . $v->{text}} if ($literal);
          return &replaceByConstant ($e, {%$w, text => undef});
        }
      return;
    }

  return unless ((@c == 3) && ($c[1]->nodeName eq 'op'));

  my $op = &strip ($c[1]->textContent);
  my $class = $CLASS{$op} || '';

  if ($class eq 'log')
    {
      my @v = map { scalar (&foldExpression ($_)) } @c[0,2];
      @c = &children ($e);
      my @x = ($c[0], $c[2]);
      for my $i (0, 1)
        {
          return unless (! $v[$i] || ($v[$i]{type} eq 'L'));
        }
      if ($v[0] && $v[1])
        {
          my ($a, $b) = ($v[0]{value}, $v[1]{value});
          my %r = ('.AND.' => $a && $b, '.OR.' => $a || $b, '.EQV.' => $a == $b, '.NEQV.' => $a != $b);
          return &replaceByConstant ($e, {type => 'L', value => $r{$op} ? 1 : 0, kind => ''});
        }
      for my $i (0, 1)
        {
          next unless ($v[$i]);
          my $other = $x[1-$i];
          my $t = $v[$i]{value};
          if ((($op eq '.AND.') && $t) || (($op eq '.OR.') && ! $t) || (($op eq '.EQV.') && $t) || (($op eq '.NEQV.') && ! $t))
            {
              $e->replaceNode ($other);
              $CHANGED++;
              return;
            }
          if ((($op eq '.AND.') && ! $t) || (($op eq '.OR.') && $t))
            {
              return &replaceByConstant ($e, $v[$i]);
            }
        }
      return;
    }

  if ($class eq 'cmp')
    {
      my @v = map { scalar (&foldExpression ($_)) } @c[0,2];
      return unless ($v[0] && $v[1]);
      my $v = &compare ($v[0], $op, $v[1]);
      return $v ? &replaceByConstant ($e, $v) : undef;
    }

  return unless ($class);

  # Arithmetic and concatenation: fold a whole chain

  my @chain = &chain ($e, $class);
  my @v;
  for (my $i = 0; $i < @chain; $i += 2)
    {
      push @v, scalar (&foldExpression ($chain[$i]));
    }

  return if (grep { ! $_ } @v);

  my @op = map { &strip ($chain[$_]->textContent) } grep { $_ % 2 } (0 .. $#chain);

  my $v;

  if ($class eq 'cat')
    {
      return if (grep { $_->{type} ne 'C' } @v);
      $v = {%{ $v[0] }, value => join ('', map { $_->{value} } @v), text => undef};
    }
  else
    {
      return if (grep { $_->{type} !~ m/^[IR]$/o } @v);
      if ($class eq 'pow')
        {
          $v = $v[-1];
          for (my $i = $#op; $i >= 0; $i--)
            {
              return unless ($v = &arith ($v[$i], $op[$i], $v));
            }
        }
      else
        {
          $v = $v[0];
          for my $i (0 .. $#op)
            {
              return unless ($v = &arith ($v, $op[$i], $v[$i+1]));
            }
        }
    }

  return &replaceByConstant ($e, $v);
}

sub foldExpressions
{

=head2 foldExpressions

Fold all expressions of C<$d> (see C<foldExpression>); inner expressions (subscripts, arguments)
are folded first. Return the number of expressions changed.

=cut

  my $d = shift;

  my @e = grep { ($_->parentNode->nodeName !~ m/^(?:op|parens)-E$/o) && &Fxtran::stmt ($_) }
          &F ('.//op-E|.//parens-E|.//named-E[./N/n[string(.)="TRIM" or string(.)="ADJUSTL" or string(.)="MERGE"]]', $d);

  $CHANGED = 0;

  for my $e (reverse (@e))
    {
      next unless (&Fxtran::stmt ($e));
      &foldExpression ($e);
    }

  return $CHANGED;
}

sub apply
{
  my ($d, %c) = @_;

=head2 apply

Replace expressions with constants values, and simplify the code (ie remove
some C<IF> blocks) when possible.

For instance, a block such as:

  IF (LLCOND1) THEN
    [ list of statements 1 ]
  ELSEIF (LLCOND2) THEN
    [ list of statements 2 ]
  ELSE
    [ list of statements 3 ]
  ENDIF

would become, after forcing C<LLCOND2> to C<.FALSE.>.

  IF (LLCOND1) THEN
    [ list of statements 1 ]
  ELSE
    [ list of statements 3 ]
  ENDIF

This is achieved by calling C<apply> like this:

  &Fxtran::Construct::apply 
  (
    $d,                                 # section to be reorganized
    './/named-E[string(N)="LLCOND2"]',  # XPath expression for expression matching
    &e ('.FALSE.'),                     # expression to replace matching expressions with
  );

Matching expressions are not replaced where they are written: assignment targets,
DO variables and READ input items. They are replaced in CALL arguments.

Constant expressions are then folded (see C<foldExpression>), and statements and
constructs with constant conditions simplified, until nothing changes: C<IF> and
C<WHERE> statements and constructs (see C<simplifyIfConstructs>, C<simplifyWhereConstructs>),
C<SELECT CASE> constructs (see C<simplifySelectCaseConstructs>) and C<DO WHILE (.FALSE.)>
loops (see C<removeDoWhileFalse>).

=cut

  while (my ($k, $v) = each (%c))
    {
      my @expr = &F ($k, $d);
      for my $expr (@expr)
        {
          my $p = $expr->parentNode ();
          next if ($p->nodeName =~ m/^(?:E-1|do-V|input-item)$/o);
          $expr->replaceNode ($v->cloneNode (1));
        }
    }

  for my $iter (1 .. 100)
    {
      die ("Fxtran::Construct::apply does not converge") if ($iter == 100);

      my $count = &foldExpressions ($d);

      # IF and WHERE statements

      for my $ce (&F ('.//if-stmt/condition-E[string(.)=".TRUE." or string(.)=".FALSE."]'
                    . '|.//where-stmt/mask-E[string(.)=".TRUE." or string(.)=".FALSE."]', $d))
        {
          my $stmt = $ce->parentNode;
          if ($ce->textContent eq '.TRUE.')
            {
              my ($stmt1) = &F ('./action-stmt/ANY-stmt', $stmt);
              $stmt->replaceNode ($stmt1);
            }
          else
            {
              $stmt->unbindNode ();
            }
          $count++;
        }

      $count += &simplifyIfConstructs ($d);
      $count += &simplifyWhereConstructs ($d);
      $count += &simplifySelectCaseConstructs ($d);
      $count += &removeDoWhileFalse ($d);

      last unless ($count);
    }
}

sub detachEnd
{

=head2 detachEnd

Detach the end statement C<$name> of the last block C<$block> of a construct, with its label;
return the label (if any) and the detached nodes.

=cut

  my ($block, $name) = @_;

  my ($end) = &F ("./$name", $block);
  my @end = ($end);

  my $prev = $end->previousSibling;
  $prev = $prev->previousSibling if ($prev && ($prev->nodeName eq '#text') && ($prev->data !~ m/\n/o));
  my $label = ($prev && ($prev->nodeName eq 'label')) ? $prev : undef;

  if ($label)
    {
      my $n = $label;
      while (($n = $n->nextSibling) && (! $n->isSameNode ($end)))
        {
          push @end, $n;
        }
      unshift @end, $label;
    }

  $_->unbindNode () for (@end);

  return ($label, @end);
}

sub replaceConstruct
{

=head2 replaceConstruct

Replace the construct C<$construct> by the statements of its block C<$block> (if any), after
its first statement; the label C<$label> of its end statement is kept on a C<CONTINUE> statement.

=cut

  my ($construct, $block, $label) = @_;

  my @node;

  if ($block)
    {
      my ($stmt) = &F ('./ANY-stmt[1]', $block);
      $stmt->unbindNode ();
      @node = $block->childNodes ();
      shift (@node) while (@node && ($node[0]->nodeName eq '#text') && ($node[0]->data !~ m/\S/o)
                          && ($node[0]->data !~ m/\n/o));
    }

  push @node, $label, &t (' '), &s ('CONTINUE') if ($label);

  my $p = $construct->parentNode;
  $p->insertBefore ($_, $construct) for (@node);

  $construct->unbindNode ();
}

# Element names of IF and WHERE constructs
my %IF    = (construct => 'if-construct', block => 'if-block', first => 'if-then-stmt', else => 'else-stmt',
             cond => 'condition-E', end => 'end-if-stmt', keyword => 'IF', then => ' THEN', elsekw => 'ELSE');
my %WHERE = (construct => 'where-construct', block => 'where-block', first => 'where-construct-stmt', else => 'else-where-stmt',
             cond => 'mask-E', end => 'end-where-stmt', keyword => 'WHERE', then => '', elsekw => 'ELSEWHERE');

sub simplifyBlockConstructs
{

=head2 simplifyBlockConstructs

Simplify the C<IF> or C<WHERE> constructs (C<%$spec> gives the element names, see C<%IF> and
C<%WHERE>) whose conditions are literal C<.TRUE.> or C<.FALSE.>, as described in
C<simplifyIfConstructs>. Return the number of constructs simplified.

=cut

  my ($d, $spec) = @_;

  my ($construct, $block, $cond) = @{$spec}{qw (construct block cond)};

  my @construct = &F (".//$construct\[./$block/ANY-stmt/$cond\[string(.)=\".TRUE.\" or string(.)=\".FALSE.\"]]", $d);

  # Inner constructs first
  for my $c (reverse (@construct))
    {
      my @block = &F ("./$block", $c);

      my ($name) = &F ("./$spec->{first}/N", $block[0], 1);

      my ($label, @end) = &detachEnd ($block[-1], $spec->{end});

      # Blocks which are kept

      my @keep;

      for my $b (@block)
        {
          my ($stmt) = &F ('./ANY-stmt[1]', $b);
          my ($ce) = &F ("./$cond", $stmt);
          my $value = $ce ? $ce->textContent : 'ELSE';
          next if ($value eq '.FALSE.');
          push @keep, [$b, $stmt, $value];
          last if (($value eq '.TRUE.') || ($value eq 'ELSE'));
        }

      my %keep = map { ($_->[0]->unique_key, 1) } @keep;

      $_->unbindNode () for (grep { ! $keep{$_->unique_key} } @block);

      if ((! @keep) || ($keep[0][2] eq '.TRUE.') || ($keep[0][2] eq 'ELSE'))
        {
          &replaceConstruct ($c, @keep ? $keep[0][0] : undef, $label);
          next;
        }

      # First block left: IF (...) THEN, WHERE (...)

      if ($keep[0][1]->nodeName ne $spec->{first})
        {
          my ($ce) = &F ("./$cond", $keep[0][1]);
          my $first = &n ("<$spec->{first}>" . ($name ? "<N><n>$name</n></N>:" : '') . "$spec->{keyword} (</$spec->{first}>");
          $first->appendChild ($ce->cloneNode (1));
          $first->appendChild (&t (")$spec->{then}"));
          $keep[0][1]->replaceNode ($first);
        }

      # Last block left, with a .TRUE. condition: ELSE, ELSEWHERE

      if ((@keep > 1) && ($keep[-1][2] eq '.TRUE.'))
        {
          $keep[-1][1]->replaceNode (&n ("<$spec->{else}>$spec->{elsekw}" . ($name ? " <N><n>$name</n></N>" : '') . "</$spec->{else}>"));
        }

      $keep[-1][0]->appendChild ($_) for (@end);
    }

  return scalar (@construct);
}

sub simplifyIfConstructs
{

=head2 simplifyIfConstructs

Remove dead branches from C<IF> constructs whose conditions have been reduced to
literal C<.TRUE.> or C<.FALSE.>:

=over 4

=item

blocks with a C<.FALSE.> condition are removed;

=item

blocks following a block with a C<.TRUE.> condition are removed, and this block
becomes the C<ELSE> block (or the only block);

=item

a construct left with no block is removed; a construct left with a single unconditional
block (C<.TRUE.> condition, or C<ELSE>) is replaced by the statements of this block.

=back

The C<END IF> statement is kept, with the name of the construct and its label; when
it is removed, its label is kept on a C<CONTINUE> statement.

Return the number of constructs simplified.

=cut

  my $d = shift;
  return &simplifyBlockConstructs ($d, \%IF);
}

sub simplifyWhereConstructs
{

=head2 simplifyWhereConstructs

Simplify C<WHERE> constructs whose masks (scalars) have been reduced to literal C<.TRUE.> or
C<.FALSE.>, as C<IF> constructs (see C<simplifyIfConstructs>): the statements of a block
whose mask is C<.TRUE.> become plain array assignments. Return the number of constructs
simplified.

=cut

  my $d = shift;
  return &simplifyBlockConstructs ($d, \%WHERE);
}

sub simplifySelectCaseConstructs
{

=head2 simplifySelectCaseConstructs

Replace C<SELECT CASE> constructs whose selector is a literal by the statements of the
matching C<CASE> block (values and ranges of literals), or of the C<CASE DEFAULT> block;
remove the construct if no block matches. Constructs with a case value which is not a
literal, or left by an C<EXIT> statement, are not changed. Return the number of constructs
simplified.

=cut

  my $d = shift;

  my $count = 0;

  for my $c (reverse (&F ('.//selectcase-construct', $d)))
    {
      my @block = &F ('./selectcase-block', $c);

      my ($stmt) = &F ('./select-case-stmt', $block[0]);
      my ($x) = grep { $_->nodeName =~ m/-E$/o } &children (&F ('./case-E', $stmt));
      next unless ($x && ($x->nodeName =~ m/^(?:literal|string|op)-E$/o));
      my $v = &foldExpression ($x);
      next unless ($v && ($v->{type} =~ m/^[ILC]$/o));

      my ($name) = &F ('./N', $stmt, 1);
      next if ($name && &F ('.//exit-stmt[string(N)="?"]', $name, $c));

      my ($match, $default, $undecided);

      BLOCK: for my $b (@block[1 .. $#block])
        {
          my ($cs) = &F ('./case-selector', &F ('./case-stmt', $b));

          if (&strip ($cs->textContent) eq 'DEFAULT')
            {
              $default = $b;
              next;
            }

          for my $r (&F ('./case-value-range-LT/case-value-range', $cs))
            {
              # Case values; an empty case value is the missing bound of a range (:B or A:)
              my @x = map { my ($e) = grep { $_->nodeName =~ m/-E$/o } &children ($_); $e } &F ('./case-value', $r);
              my @w = map { $_ ? scalar (&foldExpression ($_)) : undef } @x;

              if (grep { $x[$_] && ! $w[$_] } (0 .. $#x))
                {
                  $undecided = 1;
                  next;
                }

              my $m;

              if (@x == 1)
                {
                  $m = $v->{type} eq 'L' ? (($w[0]{type} eq 'L') ? ($v->{value} == $w[0]{value}) : undef)
                     : (&compare ($v, '==', $w[0]) || {})->{value};
                }
              else
                {
                  my ($lo, $hi) = @w;
                  my $l = $lo ? &compare ($lo, '<=', $v) : {value => 1};
                  my $h = $hi ? &compare ($v, '<=', $hi) : {value => 1};
                  $m = ($l && $h) ? ($l->{value} && $h->{value}) : undef;
                }

              if (! defined ($m))
                {
                  $undecided = 1;
                }
              elsif ($m)
                {
                  $match = $b;
                  last BLOCK;
                }
            }
        }

      next if ((! $match) && $undecided);

      my $selected = $match || $default;

      my ($label) = &detachEnd ($block[-1], 'end-select-case-stmt');

      &replaceConstruct ($c, $selected, $label);

      $count++;
    }

  return $count;
}

sub removeDoWhileFalse
{

=head2 removeDoWhileFalse

Remove C<DO WHILE (.FALSE.)> loops; the label of the C<END DO> statement is kept on a
C<CONTINUE> statement. Return the number of loops removed.

=cut

  my $d = shift;

  my @do = &F ('.//do-construct[./do-stmt/test-E[string(.)=".FALSE."]]', $d);

  for my $do (reverse (@do))
    {
      my ($label) = &detachEnd ($do, 'end-do-stmt');
      &replaceConstruct ($do, undef, $label);
    }

  return scalar (@do);
}

sub removeEmptyDoConstructs
{

=head2 removeEmptyDoConstructs

Remove C<DO> constructs that contain no statements other than the C<DO> and
C<ENDDO> delimiters. Empty C<IF> constructs nested inside each C<DO> are
removed first.

=cut

  my $d = shift;

  for my $do_construct (reverse (&F ('.//do-construct', $d)))
    {
      &removeEmptyIfConstructs ($do_construct);
      my @stmt = &F ('.//ANY-stmt', $do_construct);
      next unless (scalar (@stmt) == 2);
      $do_construct->unbindNode ();
    }
}

sub removeEmptyIfConstructs
{

=head2 removeEmptyIfConstructs

Remove C<IF> constructs where every block contains only its delimiter
statement (i.e. there are no executable statements inside any branch).

=cut

  my $d = shift;

  for my $if_construct (reverse (&F ('.//if-construct', $d)))
    {
#     &removeEmptyDoConstructs ($if_construct);
      my @if_block = &F ('./if-block', $if_construct);
      my @stmt = &F ('.//ANY-stmt', $if_construct);

      # Non final blocks have a single statement, final block has two statements
      next unless (scalar (@stmt) == (scalar (@if_block) + 1));
      $if_construct->unbindNode ();
    }
}

sub removeEmptyConstructs
{

=head2 removeEmptyConstructs

Convenience wrapper that removes both empty C<IF> constructs and empty C<DO>
constructs from the given document or program unit.

=cut

  my $d = shift;
  &removeEmptyIfConstructs ($d);
  &removeEmptyDoConstructs ($d);
}

=head1 SEE ALSO

L<Fxtran::Decl>, L<Fxtran::Loop>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

1;

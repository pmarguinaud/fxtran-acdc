package Fxtran::Formatter;

=head1 NAME

Fxtran::Formatter

=head1 DESCRIPTION

This module provides a framework for reformatting particular statements.

Adding a capability for a new statement type involves adding a new class, inheriting
from C<Fxtran::Formatter::regular> or C<Fxtran::Formatter::block>.

The new class must implement the following methods:

=over 4

=item expand

Transform the statement into a single line, plain statement, making simplifications
when possible.

=item repack

Transform a packed statement into a multiline statement.

=back

The module also provides unit wide reformatting:

=over 4

=item simplifyAssociateBlocks

Remove unused C<ASSOCIATE> selectors.

=item alignUseStatements

Align use statements, remove unused imported entities.

=item alignArgumentDeclarations

Align argument declarations statements.

=back

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=head1 SEE ALSO

L<fxtran-formatter>, L<Fxtran::Formatter::regular>, L<Fxtran::Formatter::block>

=cut

use Data::Dumper;
use FileHandle;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

use strict;

sub getIndent
{
  my $stmt = shift;

  $stmt or die;

  my $n = $stmt->previousSibling;

  unless ($n) 
    {    
      if ($stmt->parentNode)
        {
          return &getIndent ($stmt->parentNode);
        }
      return 0;
    }    

  if (($n->nodeName eq '#text') && ($n->data =~ m/\n/o))
    {    
      (my $t = $n->data) =~ s/^.*\n//gsmo;
      return length ($t);
    }    

  if (my $m = $n->lastChild)
    {
      if (($m->nodeName eq '#text') && ($m->data =~ m/\n/o))
        {    
          (my $t = $m->data) =~ s/^.*\n//gsmo;
          return length ($t);
        }    
      return &getIndent ($m);
    }
  elsif (($n->nodeName eq '#text') && ($n->data =~ m/^\s*$/o) && $n->parentNode)
    {
      return length ($n->data) + &getIndent ($n->parentNode);
    }

  return 0;
}

sub runcommand
{
  my %args = @_;
  my @cmd = @{ $args{cmd} };
  system (@cmd) && die ("Command `@cmd' failed");
}

sub getDocument
{
  my ($f, %opts) = @_;

  my @fopts = qw (-construct-tag -line-length 1000 -no-cpp -no-include);

  ($opts{runcommand} || \&runcommand)->(cmd => ['fxtran', @fopts, $f], debug => 1, file => $f);

  return 'XML::LibXML'->load_xml (location => "$f.xml");
}

sub simplifyAssociateBlocks
{
  my $d = shift;

  for my $block (reverse (&F ('.//associate-construct', $d)))
    {
      my @N = &F ('.//named-E/N', $block, 1);  # Identifiers in block
      my %N = map { ($_, 1) } @N; 
  
      my $stmt = $block->firstChild;
      my ($stmt1) = &parse (fragment => $stmt->textContent . "\nEND ASSOCIATE\n", fopts => [qw (-line-length 1000 -canonic)]);

      my $count = 0; # Number of removed associates
  
      for my $assoc (&F ('./associate-LT/associate', $stmt1))
        {   
          my ($N) = &F ('./associate-N', $assoc, 1); 
          next if ($N{$N});
  
          $count++;
  
          my ($p, $n) =  ($assoc->previousSibling, $assoc->nextSibling);
  
          if ($p)
            {
              $p->unbindNode;
              $assoc->unbindNode;
            }
          elsif ($n)
            {
              $n->unbindNode;
              $assoc->unbindNode;
            }
          else
            {
              # Remove ASSOCIATE block
              $block->firstChild->unbindNode;
              $block->lastChild->unbindNode;
              $count = 0;
              my @n = $block->childNodes;
              my $p = $block->parentNode;
              for my $n (@n)
                {
                  $p->insertBefore ($n, $block);
                }
              $block->unbindNode;
              last;
            }
        }   
  
      if ($count)
        {   
          if (&F ('./associate-LT/associate', $stmt1))
            {
              $stmt->replaceNode ($stmt1);
            }
          else
            {
              my @n = &F ('./node()', $block); shift (@n); pop (@n);
              for my $n (@n)
                {
                  $block->parentNode->insertBefore ($n, $block);
                }
              $block->unbindNode ();
            }
        }   
  
    }

}


my %class;

sub loadClass
{
  my $class = shift;


  unless (exists $class{$class})
    {
      (my $pm = $class) =~ s,::,/,go; $pm .= '.pm';
      eval "use $class;";

      if (my $c = $@)
        {
          if (index ($c, "Can't locate $pm ") == 0)
            {
              $class{$class} = undef;
            }
          else
            {
              die $c;
            }
        }
      else
        {
          $class{$class} = $class;
        }
    }

  return $class{$class};
}

sub class
{
  my $stmt = shift;

  (my $class = $stmt->nodeName) =~ s/-stmt$//o; 

  $class = join ('::', 'Fxtran', 'Formatter', map { ucfirst ($_) } split (m/-/o, $class));

  return &loadClass ($class);
}

sub prepareFileForMerging
{
  shift;
  my ($f, %opts) = @_;

  return unless ($f =~ m/\.(?:F90|F)$/io);

  my $d = &getDocument ($f, %opts);

  &simplifyAssociateBlocks ($d)
    if ($opts{'simplify-associate-blocks'});

  for my $stmt (&F ('.//ANY-stmt', $d))
    {
      next unless (my $class = &class ($stmt));

      my $indent = ' ' x &getIndent ($stmt);

      my $expandStmt = $class->expand ($stmt, $indent);

      $stmt->replaceNode ($expandStmt);
    }

  'FileHandle'->new (">$f")->print ($d->textContent);
}

sub repackStatementsAfterMerge
{
  my $class = shift;
  my ($f, %opts) = @_;

  return unless ($f =~ m/\.(?:F90|F)$/io);

  my $d = &getDocument ($f, %opts);

  $class->repackStatements ($d);

  'FileHandle'->new (">$f")->print ($d->textContent);
}

sub formatStatements
{
  my $class = shift;
  my ($f, %opts) = @_;

  return unless ($f =~ m/\.(?:F90|F)$/io);

  my $d = &getDocument ($f, %opts);


  for my $pu (&F ('.//program-unit', $d))
    {
      &simplifyAssociateBlocks ($pu) if ($opts{'simplify-associate-blocks'});
      &alignUseStatements ($pu) if ($opts{'align-use-statements'});
      &alignArgumentDeclarations ($pu) if ($opts{'align-argument-declarations'});
    }

  $class->repackStatements ($d, %opts);

# 'FileHandle'->new (">$f")->print ($d->textContent);
  (my $g = $f) =~ s/\.F90$/.new.F90/o;
  'FileHandle'->new (">$g")->print ($d->textContent);
}

sub alignUseStatements
{
  my $pu = shift;

  for my $use (&F ('./use-stmt[./rename-LT]', $pu))
    {   
      my $count = 0;
      my @n = &F ('./rename-LT/rename/use-N', $use);
      for my $n (@n)
        {
          my @expr = &F ('.//named-E[string(N)="?"]', $n->textContent, $pu);
          my @type = &F ('.//T-N[string(.)="?"]', $n->textContent, $pu);
          my @gen = &F ('.//op|.//a', $n);
          if (@expr || @type || @gen)
            {
              $count++;
              next;
            }
          my ($rename) = &F ('ancestor::rename', $n);
          $rename->unbindNode (); 
        }
      $use->unbindNode unless ($count);
    }   
  
  my @use = &F ('./use-stmt', $pu);

  my %use;
  my %op;
  
  for my $use (@use)
    {   
      my ($N) = &F ('./module-N', $use, 1); 
      my @U = &F ('.//use-N', $use, 1); 

      for my $U (@U)
        {
          $use{$N}{$U}++;
        }
    }   

  my ($len) = sort { $b <=> $a } map { length ($_) } keys (%use);
  
  for my $use (@use)
    {   
      my ($N) = &F ('./module-N', $use, 1); 
      if ($use{$N}) 
        { 
          $use->replaceNode (&s (sprintf ("USE %-${len}s, ONLY : ", $N) . join (', ', sort keys (%{ $use{$N} }))));
        }
      else
        {
          $use->replaceNode (&s (sprintf ("USE %-${len}s", $N)));
        }
    }   
}

sub alignArgumentDeclarations
{
  use List::Util qw (max);

  my $pu = shift;

  my @decl = &F ('./T-decl-stmt[.//attribute-N[string(.)="INTENT"]', $pu);
  
  my %len;  # Max length of each of attribute
  my %att;  # Attributes used in type decl statements
  
  for my $decl (@decl)
    {   
      my ($tspec) = &F ('./_T-spec_', $decl, 1); $tspec =~ s/\s+//go;
  
      $len{type} = &max ($len{type} || 0, length ($tspec));
  
      my @attr = &F ('.//attribute', $decl);
      for my $attr (@attr)
        {
          my ($N) = &F ('./attribute-N', $attr, 1); 
          $attr = $attr->textContent; $attr =~ s/\s+//go;
          $att{$N} = 1;
          $len{$N} = &max (($len{$N} || 0), length ($attr));
        }
    }   
  
  
  my @att = sort keys (%att);
  
  for (values (%len))
    {   
      $_++;
    }   
  
  for my $decl (@decl)
    {   
      my ($tspec) = &F ('./_T-spec_', $decl, 1); $tspec =~ s/\s+//go;
      my ($endlt) = &F ('./EN-decl-LT', $decl, 1); 
  
      my @attr = &F ('.//attribute', $decl);
      my %attr = map { my ($N) = &F ('./attribute-N', $_, 1); ($N, $_->textContent) } @attr;
  
      for (values (%attr))
        {
          s/\s+//go;
        }
  
      my $code = sprintf ("%-$len{type}s", $tspec);
  
      for my $att (@att)
        {
          if ($attr{$att})
            {
              $code .= sprintf (",%-$len{$att}s", $attr{$att});
            }
          else
            {
              $code .= ' ' x ($len{$att} + 1); 
            }
        }
  
      $code .= ' :: ' . $endlt;
  
      my $stmt = &s ($code);
  
      $decl->replaceNode ($stmt);
    }   

}

sub repackStatements
{
  shift;
  my ($d, %opts) = @_;

  for my $stmt (&F ('.//ANY-stmt', $d))
    {
      next unless (my $class = &class ($stmt));
    
      my $indent = ' ' x &getIndent ($stmt);

      my $canonicStmt = $class->canonic ($stmt);
      my $repackStmt = $class->repack ($canonicStmt, $indent);

      $stmt->replaceNode ($repackStmt);
    }

}

sub repackCallLikeStatement
{
  my $class = shift;

  my $prefix = shift (@_);
  my $indent = pop (@_);
  my $suffix = pop (@_);
  my @arg = @_;

  my $len = 0; 
  my $max = 120;
  my $ind = $indent;
  my $str = '';

  my $pp = sub 
  {
    if ($len + length ($_[0]) > $max)
      {   
        $str .= "&\n$ind  & ";
        $len = 0;
      }   

    $str .= $_[0];
    $len += length ($_[0]);
  };  

  $pp->($prefix);
 
  for my $i (0 .. $#arg)
    {   
      my $arg = $arg[$i];
      $arg = "$arg, " unless ($i == $#arg);
      $pp->($arg);
    }   

  $pp->($suffix);

  return $class->reparse ($str);
}

1;

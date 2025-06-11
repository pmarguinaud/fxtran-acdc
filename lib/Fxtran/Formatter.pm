package Fxtran::Formatter;

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

  $class->formatStatements ($f);
}

sub formatStatements
{
  shift;
  my ($f, %opts) = @_;

  return unless ($f =~ m/\.(?:F90|F)$/io);

  my $d = &getDocument ($f, %opts);

  for my $stmt (&F ('.//ANY-stmt', $d))
    {
      next unless (my $class = &class ($stmt));
    
      my $indent = ' ' x &getIndent ($stmt);

      my $canonicStmt = $class->canonic ($stmt);
      my $repackStmt = $class->repack ($canonicStmt, $indent);

      $stmt->replaceNode ($repackStmt);
    }

  'FileHandle'->new (">$f")->print ($d->textContent);
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

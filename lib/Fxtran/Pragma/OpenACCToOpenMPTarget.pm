package Fxtran::Pragma::OpenACCToOpenMPTarget;

use strict;

use Fxtran;

sub convert
{
  my ($d, $opts) = @_;

  my @acc = &F ('.//acc[not(ancestor::ANY-openacc)', $d);

  for my $acc (@acc)
    {
      my ($openacc) = &F ('following-sibling::ANY-openacc', $acc);
  
      for (&F ('.//acc', $openacc))
        {
          $_->replaceNode (&n ('<C>!$OMP</C>'));
        }
  
      $acc->replaceNode (&n ('<C>!$OMP</C>'));
  
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::directive'->convert ($openacc);
    }
}


package Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause;

use strict;

use Data::Dumper;

use Fxtran;

my %seen;

sub convert
{ 
  my $class = shift;

  my $n = shift;

  my ($N) = &F ('./N/text()', $n, 1);

  my $method = lc ($N);

  if ($class->can ($method))
    {
      $class->$method ($n);
    }
  else
    {
      for my $c (grep { my $nn = $_->nodeName; ($nn ne 'C') && ($nn ne 'cnt') } $n->childNodes ())
        {
          (my $t = $c->textContent) =~ s/\S/X/go;
          $c->replaceNode (&t ($t));
        }

      print STDERR "$class : method `$method' not implemented\n"
        if (! ($seen{$method}++));
    }
}

sub host
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('FROM');
}

sub device
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('TO');
}

sub vector_length
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('THREAD_LIMIT');
}

sub create
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('MAP');
  $t = shift (@n); $t->setData ('(CREATE:');
}

sub present
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $PRESENT = 'TOFROM';
# my $PRESENT = 'PRESENT';

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('MAP');
  $t = shift (@n); $t->setData ("($PRESENT:");
}

sub copyin
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('MAP');
  $t = shift (@n); $t->setData ('(TO:');
}

sub use_device
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('USE_DEVICE_ADDR');
}

sub gang
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('TEAMS DISTRIBUTE');
}

sub seq
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('FOR');
}

sub vector
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('PARALLEL DO SIMD');
}

sub if { }
sub private { }
sub collapse { }
sub default { }
sub tile { }
sub firstprivate { }
sub async { }

package Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::directive;

use strict;

use Data::Dumper;

use Fxtran;

my %seen;

sub convert
{ 
  my $class = shift;

  my $n = shift;

  my $method = $n->nodeName;

  for ($method)
    {
      s/-openacc$//o;
      s/-/_/go;
    }

  if ($class->can ($method))
    {
      $class->$method ($n);
    }
  else
    {

      for my $c (grep { my $nn = $_->nodeName; ($nn ne 'C') && ($nn ne 'cnt') } $n->childNodes ())
        {
          (my $t = $c->textContent) =~ s/\S/X/go;
          $c->replaceNode (&t ($t));
        }

      print STDERR "$class : method `$method' not implemented\n"
        if (! ($seen{$method}++));
    }
}

sub data
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('TARGET DATA ');

  my @c = grep { $_->nodeName ne '#text' } @n;

  for (grep { $_->nodeName eq 'clause' } @c)
    {
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($_);
    }
}

sub cache { }

sub end_data
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('END TARGET DATA ');
}

sub parallel_loop
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('TARGET ');

  my @c = grep { $_->nodeName ne '#text' } @n;

  for (grep { $_->nodeName eq 'clause' } @c)
    {
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($_);
    }

}

sub parallel
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('TARGET TEAMS ');

  my @c = grep { $_->nodeName ne '#text' } @n;

  for (grep { $_->nodeName eq 'clause' } @c)
    {
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($_);
    }
}

sub end_parallel
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('END TARGET TEAMS DISTRIBUTE PARALLEL ');
}

sub end_parallel_loop
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('END TARGET TEAMS DISTRIBUTE PARALLEL ');
}

sub host_data
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('TARGET DATA ');

  my @c = grep { $_->nodeName ne '#text' } @n;

  for (grep { $_->nodeName eq 'clause' } @c)
    {
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($_);
    }
}

sub end_host_data
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('END TARGET DATA ');
}

sub loop
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my @c = grep { $_->nodeName eq 'clause' } @n;

  my $t = shift (@n); 

  for (@c)
    {
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($_);
    }
}

sub update
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('TARGET UPDATE ');

  my @c = grep { $_->nodeName ne '#text' } @n;

  for (grep { $_->nodeName eq 'clause' } @c)
    {
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($_);
    }
}

1;

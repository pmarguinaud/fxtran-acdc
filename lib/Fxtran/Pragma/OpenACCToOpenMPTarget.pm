package Fxtran::Pragma::OpenACCToOpenMPTarget;

use strict;

use Fxtran;

sub apply
{
  my ($d, $opts) = @_;

  for my $pl (&F ('.//parallel-loop-openacc', $d))
    {
      &Fxtran::Pragma::OpenACC::cleanOpenACCParallelLoop ($pl);
    }
  
  if ($opts->{'expand-openacc'})
    {
      for my $pl (&F ('.//parallel-loop-openacc', $d))
        {
          &Fxtran::Pragma::OpenACC::expandParallelLoop ($pl);
        }
      
      for my $pa (&F ('.//parallel-openacc|.//serial-openacc', $d))
        {
          &Fxtran::Pragma::OpenACC::expandParallelData ($pa);
        }
    }
  
  if ($opts->{openmptarget})
    {
      &convertToOpenMPTarget ($d, $opts);
    }
  
  if ($opts->{'ifdef-fxtran'})
    {
      &addIfdefFxtran ($d, $opts);
    }

}

sub convertToOpenMPTarget
{
  my ($d, $opts) = @_;

  my @acc = &F ('.//acc[not(ancestor::ANY-openacc)', $d);

  for my $acc (@acc)
    {
      my ($openacc) = &F ('following-sibling::ANY-openacc', $acc);
  
      for (&F ('.//acc', $openacc))
        {
          $_->replaceNode (&n ('<omp-target>!$OMP</omp-target>'));
        }
  
      $acc->replaceNode (&n ('<omp-target>!$OMP</omp-target>'));
  
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::directive'->convert ($openacc);
    }

  for my $openacc (&F ('.//ANY-openacc', $d))
    {
      &normalizeOpenACC ($openacc);
    }
}

sub normalizeOpenACC
{
  my $openacc = shift;


  # normalize space

  for my $tt (&F ('./text()', $openacc))
    {
      my $text = $tt->textContent;

      next unless (my $n = $tt->nextSibling);

      next unless ($n->nodeName eq 'clause');

      for ($text)
        {
          s/^[ ]*//o;
          s/[ ]*$//o;
          s/[ ]+/ /go;
        }
      if (length ($text) > 0)
        {
          $tt->setData ($text); 
        }
      else
        {
          $tt->unbindNode ();
        }
    }


  for my $clause (&F ('./clause', $openacc))
    {
      $openacc->insertBefore (&t (' '), $clause);
    }

  # detect consecutive &&

  for my $cnt (&F ('.//cnt', $openacc))
    {  
      if ($cnt->textContent =~ m/&&/goms)
        {
          my @x;
          for (my $x = $cnt; ; $x = $x->previousSibling)
            {
              if ($x->nodeName eq '#text') 
                {
                  if ((my $tt = $x->textContent) =~ s/^.*?\n//o)
                    {
                      $x->setData ($tt);
                      last;
                    }
                }
              push @x, $x;
            }
          $_->unbindNode () for (@x);
        }
    }
}

sub addIfdefFxtran
{
  my ($d, %opts) = @_;
  
  my %seen;
  
  my ($TAG, $MACRO) = $opts{openmptarget}
      ? ('omp-target', '_FXTRAN_ACDC_USE_OPENMPTARGET')
      : ('acc',        '_FXTRAN_ACDC_USE_OPENACC'     );
  
  for my $acc (&F ('.//' . $TAG . '[not(ancestor::ANY-openacc)', $d))
    {
      next if ($seen{$acc->unique_key ()});
  
      my @n;
  
      for (my $n = $acc->previousSibling; $n; $n = $n->nextSibling)
        {
          my $nn = $n->nodeName;
          last unless (($nn =~ m{^(?:#text|C|$TAG)$}) || ($nn =~ m{-openacc$}o));
          push @n, $n;
        }
  
      for my $n (@n)
        {
          $seen{$n->unique_key ()} = 1;
        }
  
      my $sp1 = $n[+0]; my @tt1 = ($sp1->textContent =~ m{^(.*\n)(.*)$}goms);
      my $sp2 = $n[-1]; my @tt2 = ($sp2->textContent =~ m{^(.*\n)(.*)$}goms);
  
      my $p = $sp1->parentNode;
  
      $sp1->setData ($tt1[1]); $p->insertBefore (&t ($tt1[0]), $sp1);
      $sp2->setData ($tt2[0]); $p->insertAfter  (&t ($tt2[1]), $sp2);
  
      $p->insertBefore ($_, $sp1) for (&n ("<cpp>#ifdef $MACRO</cpp>"), &t ("\n"));
      $p->insertAfter  ($_, $sp2) for (&t ("\n"), &n ('<cpp>#endif</cpp>'));
  
    }
  
  my ($implicitnone) = &F ('.//implicit-none-stmt', $d);
  
  my $include = &n ("<include>#include &quot;<filename>fxtran_acdc_config.h</filename>&quot;</include>");
  
  $implicitnone->parentNode->insertBefore ($_, $implicitnone) for ($include, &t ("\n\n"));

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
  $t = shift (@n); $t->setData ('(ALLOC:');
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

sub copyout
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('MAP');
  $t = shift (@n); $t->setData ('(FROM:');
}

sub copy
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t = $t->firstChild; $t->setData ('MAP');
  $t = shift (@n); $t->setData ('(TOFROM:');
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
  die;
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

sub default 
{ 
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my ($t, $x) = @n;

  my $tt = lc ($x->textContent);

  $tt =~ s/(?:^\s*|\s*$)//o;

  if ($tt eq '(none)')
    {
      $x->setData ('(SHARED)');
    }
}

sub tile 
{ 
  shift;
  my $n = shift;
  $n->unbindNode ();
}

sub firstprivate { }

sub async 
{ 
  shift;
  my $n = shift;
  $n->unbindNode ();
}

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

sub prune
{ 
  shift;
  my $n = shift;

  my @x;
  for (my $x = $n; $x; $x = $x->previousSibling)
    {
      unshift (@x, $x);
      last if (uc ($x->textContent) eq '!$OMP');
    }

  $_->unbindNode () for (@x);
}

sub cache
{
  my $class = shift;
  $class->prune (@_);
}

sub end_data
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('END TARGET DATA ');
}

sub serial
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('TARGET');

  my @c = grep { $_->nodeName ne '#text' } @n;

  for (grep { $_->nodeName eq 'clause' } @c)
    {
      'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($_);
    }
}

sub end_serial
{
  shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my $t = shift (@n); $t->setData ('END TARGET');
}

sub parallel_loop 
{ 
  my $class = shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my @c = grep { $_->nodeName eq 'clause' } @n;

  my $t = shift (@n); $t->setData ('TARGET TEAMS ');

  $class->handleLoopClauses ($n, $t, @c);
}

sub end_parallel_loop 
{ 
  die; 
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

  my $t = shift (@n); $t->setData ('END TARGET TEAMS');
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
  my $class = shift;
  my $n = shift;

  my @n = $n->childNodes ();

  my @c = grep { $_->nodeName eq 'clause' } @n;

  my $t = shift (@n); $t->setData ('');

  $class->handleLoopClauses ($n, $t, @c);
}

sub handleLoopClauses
{
  my $class = shift;
  my ($n, $t, @c) = @_;

  for my $c (@c)
    {
      my ($N) = &F ('./N', $c, 1); $N = lc ($N);
      if ($N eq 'seq')
        {
          $class->prune ($n);
        }
      elsif ($N eq 'gang')
        {
          my $tt = $t->textContent;
          $t->setData ($tt ? "$tt DISTRIBUTE" : 'DISTRIBUTE');
          $c->unbindNode ();
        }
      elsif ($N eq 'vector')
        {
          my $tt = $t->textContent;
          $t->setData ($tt ? "$tt PARALLEL DO" : "PARALLEL DO");
          $c->unbindNode ();
        }
      else
        {
          'Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause'->convert ($c);
        }
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

sub wait 
{ 
  my $class = shift;
  $class->prune (@_);
}


1;

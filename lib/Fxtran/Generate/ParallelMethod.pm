package Fxtran::Generate::ParallelMethod;

use strict;

sub generateCCode
{
  my ($d, $opts) = @_;

  my %section2method;
  
  my @method = ($d->textContent =~ m/LPARALLELMETHOD\s*\('([^']+)'\s*,\s*'([^']+)'\s*\)/goms);
  
  while (my ($method, $section) = splice (@method, 0, 2)) 
    {   
      for ($method, $section)
        {
          s/\&\s*\&//goms;
          $_ = uc ($_);
        }
      $section2method{$section}{$method} = 1;
    }   
  
  my $fh = 'FileHandle'->new (">$opts->{dir}/parallelmethod.c");
  
  for my $METHOD (qw (OPENMP OPENMPSINGLECOLUMN OPENACCSINGLECOLUMN))
    {
      $fh->printf ('static const char %s [] __attribute__ ((section (".parallelmethod.%s"))) = ""' . "\n", $METHOD, $METHOD);
      for my $section (sort keys (%section2method))
        {
          my @method = ($METHOD);
  
          push @method, ('OPENMPMETHOD')             if ($METHOD =~ m/^OPENMP/o);
          push @method, ('OPENACCMETHOD', 'OPENACC') if ($METHOD =~ m/^OPENACC/o);
  
          push @method, ('OPENMP', 'UPDATEVIEW');
  
          for my $method (@method)
            {
              if ($section2method{$section}{$method})
                {
                  $fh->printf ('"%-40s %s\\n"', $method, $section);
                  $fh->print ("\n");
                  last;
                }
            }
        }
      $fh->print (";\n");
    }
  
  $fh->close ();
}

1;

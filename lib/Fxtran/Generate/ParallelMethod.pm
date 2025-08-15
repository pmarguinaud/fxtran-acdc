package Fxtran::Generate::ParallelMethod;

use Data::Dumper;

use strict;

sub generateCCode
{
  my ($d, $opts) = @_;

  my %section2method;
  
  my @method = ($d->textContent =~ m/\bFXTRAN_ACDC_LPARALLELMETHOD\s*\('([^']+)'\s*,\s*'([^']+)'\s*\)/goms);

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
      $fh->printf ('static const char %s [] __attribute__ ((section (".fxtran.acdc.parallelmethod.%s"))) = ""' . "\n", $METHOD, $METHOD);

      for my $section (sort keys (%section2method))
        {
          my @method = ($METHOD, 'PARALLEL');
  
          push @method, ('OPENMPMETHOD')             if ($METHOD =~ m/^OPENMP/o);
          push @method, ('OPENACCMETHOD', 'OPENACC') if ($METHOD =~ m/^OPENACC/o);
  
          push @method, ('OPENMP', 'UPDATEVIEW');
  
          unshift (@method, 'HOSTMANYBLOCKS')    if ($METHOD =~ m/^OPENMPSINGLECOLUMN/o);
          unshift (@method, 'OPENACCMANYBLOCKS') if ($METHOD =~ m/^OPENACC/o);

          for my $method (@method)
            {
              if ($section2method{$section}{$method})
                {
                  $fh->printf ('"%-40s %86s\\n"', $method, $section);
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

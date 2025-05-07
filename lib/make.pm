package make;

use strict;
use FileHandle;
use Data::Dumper;
use File::Basename;

use task;

sub slurp
{
  do { my $fh = 'FileHandle'->new ("<$_[0]"); <$fh> };
}

sub study
{
  my $f = shift;
  my @line = &slurp ($f);

  my (%mod, %use);

  for my $line (@line)
    {
      if ($line =~ m/^\s*MODULE\s+(\w+)(?:\s*$|\s*\!)/gomsi)
        {
          $mod{uc ($1)}++;
        }
      elsif ($line =~ m/^\s*USE\s+(\w+)/gomsi)
        {
          $use{uc ($1)}++;
        }
      elsif ($line =~ m/^\s*SUBMODULE\s*\(\s*(\w+)\s*\)/gomsi)
        {
          $use{uc ($1)}++;
        }
    }
  
  return 
    {
      mod => [sort keys (%mod)],
      use => [sort keys (%use)],
    };
}

sub obj
{
  my $F90 = shift;
  (my $obj = &basename ($F90)) =~ s/\.F90$/.o/o;
  return "O_$obj";
}

sub make
{
  my %args = @_;

  my $obj = $args{obj};
  my @F90 = @{ $args{F90} };
  my $f90compiler = $args{f90compiler};
  my @f90flags = grep { $_ ne '-c' } @{ $args{f90flags} };

  my %mod2obj;
  my %obj2use;
  
  my @obj;
  
  for my $F90 (@F90)
    {
      my $dep = &study ($F90);
      for my $mod (@{ $dep->{mod} })
        {
          $mod2obj{$mod} = &obj ($F90);
        }
      my $obj = &obj ($F90);
      @{ $obj2use{$obj} } = @{ $dep->{use} };
  
      push @obj, $obj;
    }
  
  my $fh = 'FileHandle'->new ('>Makefile');
  
  $fh->print (<< "EOF");

FC=$f90compiler @f90flags
LD=ld

$obj: @obj
	\$(LD) -r -o $obj @obj

clean:
	\\rm -f $obj @obj *.mod *.smod *.lst
  
EOF
  
  for my $F90 (@F90)
    {
      my $obj = &obj ($F90);
      my @dep = grep { $_ && ($_ ne $obj) } 
                map { $mod2obj{$_} } 
                @{ $obj2use{$obj} };
      $fh->print (<<"EOF");
$obj: $F90 @dep
	\$(FC) -o $obj -c $F90
  
EOF
    }
  
  $fh->close ();

  &task::runCommand (cmd => ['make', -j => 4], %args);
}

1;

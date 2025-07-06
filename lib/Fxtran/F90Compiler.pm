package Fxtran::F90Compiler;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use FileHandle;
use Data::Dumper;
use File::Basename;
use File::Path;
use File::Copy;

use strict;

use Fxtran::Util;

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
  my $src = shift;
  (my $obj = &basename ($src)) =~ s/\.(?:F90|f90|c|cc)$/.o/o;
  return "O_$obj";
}

sub make
{
  my %args = @_;

  my $obj = $args{obj};
  my $lib = $args{lib};
  my @F90 = @{ $args{F90} || [] };
  my @C   = @{ $args{C}   || [] };
  my @CXX = @{ $args{CXX} || [] };

  my $f90compiler = $args{f90compiler};
  my $ccompiler   = $args{ccompiler}   || 'gcc';
  my $cxxcompiler = $args{cxxcompiler} || '';

  my @f90flags = grep { $_ ne '-c' } @{ $args{f90flags} || [] };
  my @cflags   = grep { $_ ne '-c' } @{ $args{cflags}   || ['-fPIC'] };
  my @cxxflags = grep { $_ ne '-c' } @{ $args{cxxflags} || [] };

  my %mod2obj;
  my %obj2use;

  if (@CXX)
    {
      die ("CXX compiler is required\n") unless ($cxxcompiler);
    }
  
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

  @obj = reverse (sort (@obj));
  
  for my $C (@C)
    {
      push @obj, &obj ($C);
    }

  for my $CXX (@CXX)
    {
      push @obj, &obj ($CXX);
    }

  my $fh = 'FileHandle'->new ('>Makefile');
  
  $fh->print (<< "EOF");
FC=$f90compiler 
CC=$ccompiler
CXX=$cxxcompiler
FCFLAGS=@f90flags
CFLAGS=@cflags
CXXFLAGS=@cxxflags
LD=ld
AR=ar

EOF

  if ($obj)
    {
      $fh->print (<< "EOF");
$obj: @obj
	@\$(LD) -r -o $obj @obj

EOF

  $fh->print (<< "EOF");
clean:
	\\rm -f $obj @obj *.mod *.smod *.lst
  
EOF
    }
  elsif ($lib)
    {
      $fh->print (<< "EOF");
$lib: @obj
	@\$(AR) crv $lib @obj

EOF

  $fh->print (<< "EOF");
clean:
	\\rm -f $lib @obj *.mod *.smod *.lst
  
EOF
    }
  else
    {
      die ("Either obj or lib must be specified");
    } 
  
  for my $F90 (@F90)
    {
      my $obj = &obj ($F90);
      my @dep = grep { $_ && ($_ ne $obj) } 
                map { $mod2obj{$_} } 
                @{ $obj2use{$obj} };
      $fh->print (<<"EOF");
$obj: $F90 @dep
	\$(FC) \$(FCFLAGS) -o $obj -c $F90
  
EOF
    }

  for my $C (@C)
    {
      my $obj = &obj ($C);
      $fh->print (<<"EOF");
$obj: $C 
	\@echo "\$(CC) -c $C"
	@\$(CC) \$(CFLAGS) -o $obj -c $C
  
EOF
    }
  
  for my $CXX (@CXX)
    {
      my $obj = &obj ($CXX);
      $fh->print (<<"EOF");
$obj: $CXX 
	\@echo "\$(CXX) -c $CXX"
	@\$(CXX) \$(CXXFLAGS) -o $obj -c $CXX
  
EOF
    }
  
  $fh->close ();

  &Fxtran::Util::runCommand (cmd => ['make', -j => 4], %args);
}

sub run
{
  my %args = @_;

  if (my $dir = $args{'user-directory-in'})
    {
      for my $f (<*.F90>, <*.h>)
        {
          if (-f "$dir/$f")
            {
              unlink ($f);
              print "Take $dir/$f\n";
              &copy ("$dir/$f", $f);
            }
        }
    }

  if (my $dir = $args{'user-directory-out'})
    { 
      (-d $dir) or &mkpath ($dir);
      for my $f (<*.F90>, <*.h>)
        {
          &copy ($f, "$dir/$f");
        }
    }

  return if ($args{dryrun});

  if (my $dir = $args{'user-directory-out'})
    { 
      for my $F90 (@{ $args{F90} || [] })
        {
          if (-f "$dir/$F90")
            {
              $F90 = "$dir/$F90";
            }
        }
    }

  my @F90 = @{ $args{F90} || [] };
  my @C   = @{ $args{C}   || [] };
  my @CXX = @{ $args{CXX} || [] };

  if ((scalar (@F90) == 1) && (scalar (@C) == 0) && (scalar (@CXX) == 0))
    {
      my $f90compiler = $args{f90compiler};
      my @f90flags = @{ $args{f90flags} };
      my $obj = $args{obj};
      &Fxtran::Util::runCommand (cmd => [$f90compiler, @f90flags, ($obj ? (-o => $obj) : ()), @F90], %args);
    }
  else
    {
      &make (%args);
    }
}

sub concatenateSource
{
  my %args = @_;

  my @f90flags = @{ $args{f90flags} };
  my ($obj, $f90compiler, $F90) = @args{qw (obj f90compiler F90)};
  my $opts = $args{opts};

  my $fho;

  for my $f ($F90, sort <*.F90>)
    {
      $fho ||= 'FileHandle'->new ('>' . &basename ($F90));
      my $code = do { my $fh = 'FileHandle'->new ("<$f"); local $/ = undef; <$fh> };
      $fho->print ($code);
      $fho->print ("\n" x 3);
    }

  $fho->close ();

  &Fxtran::F90Compiler::run 
  (
    f90compiler => $f90compiler, 
    f90flags    => \@f90flags, 
    obj         => $obj, 
    F90         => [&basename ($F90)], 
    %$opts 
  );
}

sub compile
{
  my %args = @_;

  my @f90flags = @{ $args{f90flags} };
  my ($obj, $lib, $f90compiler) = @args{qw (obj lib f90compiler)};
  my $opts = $args{opts};

  return if ($opts->{dryrun});

  my @F90 = <*.F90>;
  my @C = <*.c>;

  &Fxtran::F90Compiler::run 
  (
    f90compiler => $f90compiler, 
    f90flags    => \@f90flags, 
    lib         => $lib, 
    obj         => $obj, 
    F90         => \@F90, 
    C           => \@C,
    %$opts 
  );
}

1;

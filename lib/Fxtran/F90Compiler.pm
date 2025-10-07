package Fxtran::F90Compiler;

=head1 NAME

Fxtran::F90Compiler

=head1 DESCRIPTION

The purpose of this module is to provide utilities to wrap the 
compiler and compile sets of files.

=head1 PUBLIC FUNCTIONS

=cut

use FileHandle;
use Data::Dumper;
use File::Basename;
use File::Path;
use File::Copy;
use File::stat;
use Cwd;

use strict;

use Fxtran::Util;

sub compile
{
  my %args = @_;

=head2 compile

This function compiles FORTRAN and C files which are present in
the current directory. Dependencies between FORTRAN files are detected.
A F<Makefile> is generated and the compilation run with make if
several files have to be compiled.

=cut

  my @f90flags = @{ $args{f90flags} };
  my ($obj, $lib, $f90compiler) = @args{qw (obj lib f90compiler)};
  my $opts = $args{opts};

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

sub touch
{
  my $time = shift;
  $time = time () if ($time < 0);
  for my $f (@_)
    {
      utime ($time, $time, $f);
    }
}

sub run
{
  my %args = @_;

=head2 run

This function compiles files passed as arguments, using the specified compilers. Named options are:

=over 4

=item F90

list of FORTRAN files

=item C

list of C files.

=item CXX

list of C++ files.

=item dryrun

Stop the process just before compilation.

=item user-directory-in

Substitute files to be compiled with files from this directory. Used for debugging.

=item user-directory-out

Save files from current directory (mostly generated code) into this directory.

=back

=cut

  return if ($args{dryrun});

  if (my $dir = $args{'user-directory-in'})
    {
      for my $f (<*.F90>, <*.h>)
        {
          if (-f "$dir/$f")
            {
              unlink ($f);
              print "Take $dir/$f\n";
              &copy ("$dir/$f", $f);
              &touch (stat ("$dir/$f")->mtime (), $f);
            }
        }
    }

  if (my $dir = $args{'user-directory-out'})
    { 
      my $cwd = &cwd ();
      (-d $dir) or &mkpath ($dir);
      for my $f (<*.F90>, <*.h>)
        {
          unlink ("$dir/$f");
          &copy ($f, "$dir/$f") or die ("Cannot copy `$f' to `$dir/$f', cwd=$cwd");
          (my $st = stat ($f)) or die ("Cannot stat `$f', cwd=$cwd");
          &touch ($st->mtime, "$dir/$f");
        }
    }

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
  elsif ($args{'object-merge-method'} eq 'concatenate')
    {
      die ("Concatenating source code works only with FORTRAN source code")
        if (scalar (@C) or scalar (@CXX));
      &concatenateSource (%args);
    }
  elsif ($args{lib})
    {
      &make (%args);
    }
  elsif ($args{'object-merge-method'} eq 'link')
    {
      &make (%args);
    }
  elsif ($args{'object-merge-method'} eq 'archive')
    {
      &make (%args);
    }
  else
    {
      die ("Unexpected value $args{'object-merge-method'} for option 'object-merge-method'");
    }
}

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

  if ($obj && ($args{'object-merge-method'} eq 'link'))
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
  elsif ($obj && ($args{'object-merge-method'} eq 'archive'))
    {
      $fh->print (<< "EOF");
$obj: @obj
	@\$(AR) crv $obj @obj

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

sub concatenateSource
{
  my %args = @_;

  my @f90flags = @{ $args{f90flags} };
  my ($obj, $f90compiler) = @args{qw (obj f90compiler)};
  my $opts = $args{opts};
  my @F90 = @{ $args{F90} };

  my $F90_c = 'C_' . &basename ($F90[0]);
  my $fho = 'FileHandle'->new ('>' . &basename ($F90_c));

  for my $F90 (@F90) # Should sort the files, no dependency first; it works for now
    {
      my $code = do { my $fh = 'FileHandle'->new ("<$F90"); local $/ = undef; <$fh> };
      $fho->print ($code);
      $fho->print ("\n" x 3);
    }

  $fho->close ();

  &Fxtran::Util::runCommand (cmd => [$f90compiler, @f90flags, ($obj ? (-o => $obj) : ()), $F90_c], %args);

}

1;

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

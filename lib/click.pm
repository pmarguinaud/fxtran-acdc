package click;

=head1 NAME

click

=head1 SYNOPSIS

Implement a C<clickable> package:

  package clickable;

  use click;

  &click (<< "EOF");
    flag1                     -- This is a boolean option
    value1=s                  -- This is a string option        -- default
    list1=s@                  -- This is a list option          -- value1,value2
    hash1=s%                  -- This is a hash option          -- key1=vaule1,key2=value2
  EOF
  sub method1
  {
    my ($opts, @args) = @_;
    
    # $opts = {...} contains parsed options
    # @opts contains options after the double hyphen --

  }

then a F<clickme.pl> script:

  #!/usr/bin/perl -w

  use clickable;

  clickable->method1 (@ARGV);

and run the script:

  $ ./clickme.pl --flag1 --value1=... ...

=head1 DESCRIPTION

This module is similar to the python click module. It maps command line methods and options to module 
methods and arguments.

=head1 SEE ALSO

Getopt::Long

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;
use FileHandle;
use Storable;

use strict;

use base qw (Exporter);
our @EXPORT = qw (click run);

my %METHOD;

my %FILE2CODE;

sub slurpl
{
  my $file = shift;

  unless ($FILE2CODE{$file})
    {
      my @code = do { my $fh = 'FileHandle'->new ("<$file"); <$fh> };
      $FILE2CODE{$file} = \@code;
    }

  return @{ $FILE2CODE{$file} };
}

sub click
{
  my @opt = @_;

  my $oopt = []; # Options for OO methods
  if (@opt && (ref ($opt[-1]) eq 'ARRAY'))
    {
      $oopt = pop (@opt);
      $oopt = &Storable::dclone ($oopt);
    }

  # Split options text, only line per option
  @opt = grep { !/^\s*$/o } map { split (m/\n/o, $_) } @opt;

  for (@opt)
    {
      chomp;
      s/^\s*//o;
    }

  # Guess who called us and get the source code

  my ($package, $file, $line) = caller (0);
  my @code = &slurpl ($file);

  # Seek to subroutine declaration

  my $name; # Subroutine name

  while ($line <= $#code)
    {
      my $text = $code[$line++];
      next if ($text =~ m/^\s*#.*|^\s*$/o);
      if ($text =~ m/^s*sub\s+(\w+)/o)
        {
          $name = "$1";
	  last;
	}
    }

  # Find subroutine scalar arguments

  my @arg;
  
  while ($line <= $#code)
    {
      my $text = $code[$line++];
      next if ($text =~ m/^\s*#.*|^\s*$/o); # Skip comments & white lines
      next if ($text =~ m/^\s*\{\s*$/o);    # Skip {
      next if ($text =~ m/^\s*use \s*/o);   # Skip use statements
      if ($text =~ m/^\s*my\s*\$(self|class)\s*=\s*shift;/o)  # Object/class argument
        {
          @arg = ($1);
	}
      elsif ($text =~ m/^\s*my\s*\((.*)\)\s*=\s*\@_;/o)       # List of named arguments
        {
          @arg = ($text =~ m/\$(\w+)/go);
	}
      else
        {
          last;
	}
    }

  my ($ctor, $dtor);

  if ($oopt)
    {
      for (my $i = $#{$oopt}-1; $i >= 0; $i--)
        {
          if ($oopt->[$i] eq 'ctor')
	    {
              $ctor = $oopt->[$i+1];
	      splice (@$oopt, $i, 2);
	    }
          elsif ($oopt->[$i] eq 'dtor')
	    {
              $dtor = $oopt->[$i+1];
	      splice (@$oopt, $i, 2);
	    }
	}
    }


  my $h = do { no strict 'refs'; \%{"$package\::"} }; # Package symbol table

  my $code = $h->{$name};

  my (%hopts, @aopts, @oopts, %desc, %list, %flag, %hash);

  $METHOD{$package}{$name} =
  {
    package => $package,                     # Package the method/routine belongs to
    name  => $name,                          # Routine/method name
    code  => $code,                          # Routine/method to be called
    hopts => \%hopts,                        # Hash with all options & values
    aopts => \@aopts,                        # Hash will all options names & references to values (list of arguments of GetOptions)
    oopts => \@oopts,                        # Arguments for constructor method
    arg   => \@arg,                          # List of routine scalar arguments, to pass to method/routine
    ctor  => $ctor || 'new',                 # Constructor name
    dtor  => $dtor,                          # Destructor name
    desc  => \%desc,                         # Description (help message)
    list  => \%list,                         # Option is a list
    hash  => \%hash,                         # Option is a hash
    flag  => \%flag,                         # Option is a flag
  };

  for my $opt (@{[@opt, 'help', @$oopt]})
    {
      my $oo = grep { $_ eq $opt } @$oopt;

      ($opt, my $desc, my $default) = split (m/\s+--\s+/o, $opt);

      my $list = $opt =~ s/\@$//o;
      my $hash = $opt =~ s/\%$//o;
      my $val = ((my $name = $opt) =~ s/[=:].*$//o);
      $name =~ s/\+$//o;

      $hopts{$name} = $default;
      $desc{$name}  = $desc;
      $list{$name}  = $list;
      $hash{$name}  = $hash;
      $flag{$name}  = ! $val;

      if ((! $list) && (! $hash) && ($opt !~ m/=s/o)) # Allow for --nooption
        {
          $opt .= '!';
        }

      push @aopts, $opt, \$hopts{$name};

      push @oopts, $name if ($oo);
    }

}

sub getMethodHelpAsString
{
  shift;
  my ($package, $method) = @_;

  my $help = '';

  $help .= "* $method\n";
  my $m = $METHOD{$package}{$method};
  for my $opt (sort keys (%{ $m->{desc} }))
    {
      next if ($opt eq 'help');

      my $kind = '';
      $kind = '(FLAG)' if ($m->{flag}{$opt});
      $kind = '(LIST)' if ($m->{list}{$opt});

      my $default;

      if ($m->{flag}{$opt})
        {
          $default = '';
        }
      elsif ($m->{list}{$opt} && ref ($m->{hopts}{$opt}))
        {
          $default = defined ($m->{hopts}{$opt}) ? join (',', @{$m->{hopts}{$opt}}) : 'NONE';
        }
      elsif ($m->{hash}{$opt} && ref ($m->{hopts}{$opt}))
        {
          $default = defined ($m->{hopts}{$opt}) ? join (',', %{$m->{hopts}{$opt}}) : 'NONE';
        }
      else
        {
          $default = defined ($m->{hopts}{$opt}) ? $m->{hopts}{$opt} : 'NONE';
        }

      $help .= sprintf("  %-30s %6s : %-20s : %s\n", "--$opt", $kind, $default,
    	 (defined ($m->{desc}{$opt}) ? $m->{desc}{$opt} : '?'),
    	);
    }
  $help .= "\n";
}

sub help
{
  use File::Basename;

  my $package = shift;

  print &basename ($0), ":\n\n";

  for my $method (sort keys (%{ $METHOD{$package} }))
    {
      print __PACKAGE__->getMethodHelpAsString ($package, $method);
    }

  exit (0);
}

sub run
{
  my $package = shift;

  use Getopt::Long;

  my $method = shift;

  if ($method && ($method eq '--help'))
    {
      &help ($package);
    }

  my @argv = @_;

  my %opts = __PACKAGE__->commandLineToHash (method => $method, package => $package, argv => \@argv);

  if ($opts{help})
    {
      &help ($package);
    }

  defined ($method)
    or die ("No method was defined for package `$package'");

  exists ($METHOD{$package}{$method}) 
    or die ("Command `$method' was not found in package `$package'");

  $method = $METHOD{$package}{$method};

  my ($ctor, $dtor, $obj) = @{$method}{qw (ctor dtor)};

  my $seen_opts;

  my @args;

  for my $i (0 .. $#{ $method->{arg} })
    {
      my $arg = $method->{arg}[$i];

      if ($i == 0)
        {
          if ($arg eq 'class')                                  # class is a reserved argument name = package name
	    {
              push @args, $method->{package}; 
	      next;
	    }
          elsif ($arg eq 'opts')                                # opts is a reserved argument name = reference to hashed options
            {
              $seen_opts = 1;
              push @args, \%opts;
              next;
            }
          elsif (($arg eq 'self') || ($arg eq 'this'))          # self & this are reserved : the object used to invoke the method
	    {
              my @oopts = @{$method->{oopts}};
	      @oopts = map { ($_, $opts{$_}) } @oopts;
	      delete $opts{$_} for (@{$method->{oopts}});
              $obj = $method->{package}->$ctor (@oopts);
              push @args, $obj;
	      next;
	    }
        }

      push @args, $opts{$arg};
    }

  # Invoke the routine/method with appropriate arguments

  if ($seen_opts)
    {
      $method->{code}->(@args, @argv);
    }
  else
    {
      $method->{code}->(@args, %opts, @argv);
    }

  $obj->$dtor if ($obj && $dtor);
}

sub getPackageList
{
  shift;
  return sort keys (%METHOD);
}

sub getMethodList
{
  shift;
  my $package = shift;
  return sort keys (%{ $METHOD{$package} || {} });
}

sub getOptionList
{
  my $class = shift;

  my %opts = @_;

  my ($package, $method) = @opts{qw (package method)};

  return unless ($package);
    
  if ($method) 
    {
      my $i = 0;
      die ("Unknown method `$method'") unless ($METHOD{$package}{$method});
      my %hopts = @{ $METHOD{$package}{$method}{aopts} };
      return {map { ($_, ${ $hopts{$_} }) } keys (%hopts)};
    }
  else
    {
      my %hopts;
      for my $method ($class->getMethodList ($package))
        {
          my $hopts = $class->getOptionList (%opts, method => $method);
          %hopts = (%hopts, %$hopts);
        }
      return \%hopts;
    }
}

sub commandLineToHash
{
  my $class = shift;
  my %args = @_;

# Transform command lines arguments to hashed options

  my ($method, $package) = @args{qw (method package)};

  local @ARGV = @{ $args{argv} };

  my @I = grep { m/^-I/o } @ARGV;         # I is for includes
  @ARGV = grep { ! m/^-I/o } @ARGV;

  my @D = grep { m/^-D/o } @ARGV;         # D is for macro definition
  @ARGV = grep { ! m/^-D/o } @ARGV;

  defined ($method)
    or die ("No method was defined for package `$package'");

  exists ($METHOD{$package}{$method}) 
    or die ("Command `$method' was not found in package `$package'");

  $method = $METHOD{$package}{$method};

  my @opts = @{ $method->{aopts} };

  &GetOptions (@opts);

  my $hopts = $method->{hopts};

  for my $name (sort keys (%{ $hopts }))
    {
      if ($method->{list}{$name})
        {
          $hopts->{$name} = [split (m/,/o, defined ($hopts->{$name}) ? $hopts->{$name} : '')];
        }
      elsif ($method->{hash}{$name})
        {
          $hopts->{$name} = {split (m/,|=/o, defined ($hopts->{$name}) ? $hopts->{$name} : '')};
        }
    }

  my %opts = (%{ $method->{hopts} }, I => \@I, D => \@D);

  @{ $args{argv} } = @ARGV;

  return %opts;
}

sub hashToCommandLine
{
  my $class = shift;
  my %args = @_;

# Transform hashed options to command line arguments

  my ($method, $package, $opts) = @args{qw (method package opts)};

  my $hopts = $class->getOptionList (package => $package, method => $method);

  my @opts = 
    map 
    { 
      my $key = my $opt = $_;

      my ($type, @v) = ('');

      if ($key =~ s/\!$//o)
        {
          $type = 'f'; # Flag
        }
      elsif ($key =~ s/=s$//o)
        {
          $type = 's'; # Value
        }

      if (defined (my $val = $opts->{$key})) # Option is set
        {
          if (($type eq 'f') && $val)
            {
              push @v, "--$key" 
            }
          elsif (($type eq 'f') && (! $val))
            {
              push @v, "--no$key" 
            }
          elsif ($type eq 's')
            {
              push @v, "--$key";
              if (ref ($val) eq 'ARRAY')
                {
                  push @v, join (',', @$val);
                }
              elsif (ref ($val) eq 'HASH')
                {
                  push @v, join (',', map { "$_=$val->{$_}" } sort keys (%$val));
                }
              elsif (ref ($val) eq '')
                {
                  push @v, $val;
                }
            }
          else 
            {
              die $opt;
            }
        }
      @v
    } keys (%$hopts);

  return @opts;
}

1;

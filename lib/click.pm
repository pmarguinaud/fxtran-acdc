package click;

use strict;
use Data::Dumper;
use FileHandle;
use Storable;

use base qw (Exporter);
our @EXPORT = qw (click run);

my %method;

sub click
{
  my @opt = @_;

  my $oopt = [];
  if (@opt && (ref ($opt[-1]) eq 'ARRAY'))
    {
      $oopt = pop (@opt);
      $oopt = &Storable::dclone ($oopt);
    }

  @opt = map { split (m/\n/o, $_) } @opt;

  for (@opt)
    {
      chomp;
      s/^\s*//o;
    }

  my ($package, $file, $line) = caller (0);
  my @code = do { my $fh = 'FileHandle'->new ("<$file"); <$fh> };

  # Find subroutine name

  my $name;

  while ($line <= $#code)
    {
      my $text = $code[$line++];
      next if ($text =~ m/^\s*#.*|^\s*$/o);
      if ($text =~ m/^s*sub\s+(\w+)/o)
        {
          $name = "$1";
	  last;
	}
      else
        {
          next;
	}
    }

  # Find subroutine scalar arguments

  my @arg;
  
  while ($line <= $#code)
    {
      my $text = $code[$line++];
      next if ($text =~ m/^\s*#.*|^\s*$/o); # Skip comments & white lines
      next if ($text =~ m/^\s*\{\s*$/o);    # Skip {
      next if ($text =~ m/^\s*use \s*/o);  # Use use statements
      if ($text =~ m/^\s*my\s*\$(self|class)\s*=\s*shift;/o)
        {
          @arg = ($1);
	}
      elsif ($text =~ m/^\s*my\s*\((.*)\)\s*=\s*\@_;/o)
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


  my $h = do { no strict 'refs'; \%{"$package\::"} };

  my $code = $h->{$name};

  my (%hopts, @aopts, @copts, @oopts, %desc, %list, %flag);

  $method{$package}{$name} =
  {
    package => $package,
    name  => $name,
    code  => $code,
    hopts => \%hopts,
    aopts => \@aopts,
    copts => \@copts,
    oopts => \@oopts,
    arg   => \@arg,
    ctor  => $ctor || 'new',
    dtor  => $dtor,
    desc  => \%desc,
    list  => \%list,
    flag  => \%flag,
  };

  for my $opt (@{[@opt, @$oopt]})
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
      $flag{$name}  = ! $val;

      if ((! $list) && (! $hash) && ($opt !~ m/=s/o)) # Allow for --nooption
        {
          $opt .= '!';
        }

      push @aopts, $opt, \$hopts{$name};

      if ($list)
        {
          push @copts, sub { $hopts{$name} = [split (m/,/o, $hopts{$name} || '')] };
        }
      elsif ($hash)
        {
          push @copts, sub { $hopts{$name} = {split (m/,|=/o, $hopts{$name} || '')} };
        }
      push @oopts, $name if ($oo);
    }
}

sub help
{
  use File::Basename;

  my $package = shift;


  print &basename ($0), ":\n\n";

  for my $method (sort keys (%{ $method{$package} }))
    {
      print "* $method\n";
      my $m = $method{$package}{$method};
      for my $opt (sort keys (%{ $m->{desc} }))
        {
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

          printf("  %-30s %6s : %-20s : %s\n", "--$opt", $kind, $default,
		 (defined ($m->{desc}{$opt}) ? $m->{desc}{$opt} : '?'),
		);
	}
      print "\n";
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

  local @ARGV = @_;

  my @I = grep { m/^-I/o } @ARGV;
  @ARGV = grep { ! m/^-I/o } @ARGV;

  defined ($method)
    or die ("No method was defined for package `$package'");

  exists ($method{$package}{$method}) 
    or die ("Command `$method' was not found in package `$package'");

  $method = $method{$package}{$method};

  my @opts = @{ $method->{aopts} };
  my $help;
 
  &GetOptions (help => \$help, @opts);

  push @opts, I => \@I;

  for (@{ $method->{copts} })
    {
      $_->();
    }

  if ($help)
    {
      &help ($package);
    }

  my ($ctor, $dtor, $obj) = @{$method}{qw (ctor dtor)};

  my %opts = %{ $method->{hopts} };

  my $seen_opts;

  my @args;

  for my $i (0 .. $#{ $method->{arg} })
    {
      my $arg = $method->{arg}[$i];

      if ($i == 0)
        {
          if ($arg eq 'class') 
	    {
              push @args, $method->{package};
	      next;
	    }
          elsif ($arg eq 'opts')
            {
              $seen_opts = 1;
              push @args, \%opts;
              next;
            }
          elsif ($arg eq 'self')
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

  if ($seen_opts)
    {
      $method->{code}->(@args, @ARGV);
    }
  else
    {
      $method->{code}->(@args, %opts, @ARGV);
    }

  $obj->$dtor if ($obj && $dtor);
}

1;

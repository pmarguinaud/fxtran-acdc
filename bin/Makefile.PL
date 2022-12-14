#!/usr/bin/perl -w

use strict;

use FileHandle;
use List::MoreUtils qw (uniq);
use Data::Dumper;
use FindBin qw ($Bin);
use lib $Bin;

use Common;

use Fxtran;

my @sys = qw (OMP_LIB CUDAFOR IEEE_ARITHMETIC ISO_C_BINDING OPENACC);

unless (-f 'yomhook.F90')
  {
    push @sys, 'YOMHOOK';
  }

my %slurp;

sub slurp
{
  my $f = shift;
  unless ($slurp{$f})
    {
      $slurp{$f} = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
    }

  return $slurp{$f};
}

my (%deps, %mod2obj);

sub deps
{
  my $f = shift;

  my %sys = map { ($_, 1) } @sys;

  unless ($deps{$f})
    {
      my $code = &slurp ($f);
      my @inc = ($code =~ m/\n\s*#include\s+"(.*?)"/goms);
      my @use = map { uc ($_) } ($code =~ m/\n\s*use\s+(\w+)/gomis);
      @use = grep { ! $sys{$_} } @use;
      my @mod = map { lc ($_) . '.mod' } @use;
      $deps{$f} = [@mod, @inc];

      for (@{ $deps{$f} })
        {
          s/\.h$/.ok/o;
          if (m/\.mod$/o)
            {
               $_ = $mod2obj{$_};
            }
        }

      die $f if (grep { ! defined ($_) } @{ $deps{$f} });

      @{ $deps{$f} } = &uniq (@{ $deps{$f} });

   }

  return @{ $deps{$f} };
}

sub mods
{
  my $f = shift;
  my $code = &slurp ($f);
  my @mods = ($code =~ m/^\s*MODULE\s+(\w+)\s*$/igom);
  @mods = map { lc ($_) . '.mod' } @mods;
  return @mods;
}

my %kind;

sub kind
{
  my $f = shift;
  unless ($kind{$f})
    {
      my $code = &slurp ($f);
      my ($kind) = ($code =~ m/^(PROGRAM|SUBROUTINE|MODULE)/igoms);
      $kind{$f} = uc ($kind);
    }

  return $kind{$f};
}

sub newer
{
  my ($f1, $f2) = @_;
  return unless ((-f $f1) && (-f $f2));
  use File::stat;
  my $st1 = stat ($f1);
  my $st2 = stat ($f2);
  return $st1->mtime > $st2->mtime;
}

for my $fypp (<*.fypp>)
  {
    (my $F90 = $fypp) =~ s/\.fypp/.F90/o;
    next if (&newer ($F90, $fypp));
    system (qw (/opt/softs/anaconda3/bin/fypp -m os -M . -m yaml -m field_config), "./$fypp", $F90) and die ($fypp);
  }

my @F90 = <*.F90>;

my $fh = 'FileHandle'->new (">Makefile");

$fh->print (<< "EOF");
include Makefile.inc

EOF

my (@obj, @prg);

for my $F90 (@F90)
  {
    (my $obj = $F90) =~ s/\.F90$/.o/o;
    my $kind = &kind ($F90);
    if ($kind eq 'MODULE')
      {
        my @mod = &mods ($F90);
        for my $mod (@mod)
          {
            $mod2obj{$mod} = $obj;
          }
      }
  }

for my $F90 (@F90)
  {
    my $kind = &kind ($F90);

    if ($kind eq 'SUBROUTINE')
      {
        my $intfb = &Fxtran::intfb ($F90);
        (my $ok = $intfb) =~ s/\.h$/.ok/o;
        my @deps = &deps ($intfb);
        $fh->print (<< "EOF");
$ok: $intfb @deps
	touch $ok

EOF
      }
    elsif ($kind eq 'PROGRAM')
      {
        push @prg, $F90;
      }
  }

for my $h (grep { ! m/\.intfb\.h$/o } <*.h>)
  {
    (my $ok = $h) =~ s/\.h$/.ok/o;
    $fh->print (<< "EOF");
$ok: $h
	touch $ok
EOF
  }

my @subobj;

for my $F90 (@F90)
  {
    my @deps = &deps ($F90);
    my @mods = &mods ($F90);
    my $kind = &kind ($F90);
    (my $obj = $F90) =~ s/\.F90$/.o/o;

    if ($kind eq 'MODULE')
      {
        @deps = grep { ($_ ne $obj) } @deps;
      }

    $fh->print (<< "EOF");
$obj: $F90 @deps
	\$(FC) -c $F90

EOF

    push @obj, $obj if ($kind ne 'PROGRAM');

    push @subobj, $obj if ($kind ne 'MODULE');
  }

for my $prg (@prg)
  {
    (my $exe = $prg) =~ s/\.F90$/.x/o;
    (my $obj = $prg) =~ s/\.F90$/.o/o;
    $fh->print (<< "EOF");
$exe: $obj @obj
	\$(FC) -o $exe $obj @obj \$(LIBS)

EOF
  }


my @F90_fypp = map { (my $F90 = $_) =~ s/\.fypp/.F90/o; $F90 } <*.fypp>;


$fh->print (<< "EOF");

subclean:
	\\rm -f @subobj

fyppclean: 
	\\rm -f @F90_fypp

clean: 
	\\rm -f *.o *.xml *.a *.x *.mod *.optrpt 

tidy:
	\\rm -f *.xml *.optrpt
EOF


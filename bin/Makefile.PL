#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use Getopt::Long;
use FileHandle;
use List::MoreUtils qw (uniq);
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;

my %opts = qw (out Makefile include Makefile.inc);
my @opts_f = qw ();
my @opts_s = qw (out include);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

$opts{include} = [split (m/,/o, $opts{include})];

my @sys = qw (OMP_LIB CUDAFOR IEEE_ARITHMETIC ISO_C_BINDING OPENACC ECCODES CUBLAS ISO_FORTRAN_ENV NVTX);
my @extern_mod = qw (YOMHOOK MPL_MODULE PARKIND1 OML_MOD EC_LUN MPL_MPIF MPL_MODULE MPL_BARRIER_MOD);
my @extern_inc = qw (setup_trans.h setup_trans0.h inv_trans.h gath_grid.h gath_spec.h dist_spec.h);

for my $mod (@extern_mod)
  {
    next if (-f lc ($mod) . '.F90');
    push @sys, $mod;
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

  my %extern_inc = map { ($_, 1) } @extern_inc;

  unless ($deps{$f})
    {
      my $code = &slurp ($f);
      my @inc = ($code =~ m/\n\s*#include\s+"(.*?)"/goms);
      @inc = grep { ! $extern_inc{$_} } @inc;

      my @use = map { uc ($_) } ($code =~ m/\n\s*use\s+(\w+)/gomis);
      @use = grep { ! $sys{uc ($_)} } @use;

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

      die $f . "\n" . &Dumper ([$f, $deps{$f}]) if (grep { ! defined ($_) } @{ $deps{$f} });

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
      my @code = split (m/\n/o, $code);
      for (@code)
        {
          s/^#.*//o;
          s/^\s*!.*//o;
        }
      @code = grep { length ($_) } @code;
      $code = join ("\n", @code, "");
      my ($kind) = ($code =~ m/^\s*(PROGRAM|SUBROUTINE|MODULE|FUNCTION)/igoms);
      $kind or die &Dumper ([$f, $code]);
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
    my $code = &slurp ($fypp);
    my @field_config = ($code =~ m/field_config/goms ? qw (-m -field_config) : ());
    (my $F90 = $fypp) =~ s/\.fypp/.F90/o;
    next if (&newer ($F90, $fypp));
    system (qw (/opt/softs/anaconda3/bin/fypp -m os -M . -m yaml), @field_config, "./$fypp", $F90) and die ($fypp);
  }

my @F90 = <*.F90>;

my $fh = 'FileHandle'->new (">$opts{out}");


$fh->print (join ("\n", map ({ "include $_"  } @{ $opts{include} }), '', ''));

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
$ok: \$(SRC)/$intfb @deps
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
$ok: \$(SRC)/$h
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
$obj: \$(SRC)/$F90 @deps
	\$(FC) -c \$(SRC)/$F90

EOF

    push @obj, $obj if ($kind ne 'PROGRAM');

    push @subobj, $obj if ($kind ne 'MODULE');
  }

for my $c (<*.c>)
  {
    (my $o = $c) =~ s/\.c$/.o/o;
    push @obj, $o;
    $fh->print (<< "EOF");
$o: $c
	\$(CC) -c \$(SRC)/$c
EOF
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


$fh->print (<< "EOF");

subclean:
	\\rm -f @subobj

clean: 
	\\rm -f *.o *.xml *.a *.x *.mod *.optrpt 

tidy:
	\\rm -f *.xml *.optrpt
EOF


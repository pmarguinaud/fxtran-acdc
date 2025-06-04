#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use Getopt::Long;
use FileHandle;
use File::stat;
use File::Spec;
use File::Basename;
use List::MoreUtils qw (uniq);
use Data::Dumper;
use Cwd;

use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Fxtran::Common;
use Fxtran;
use Fxtran::Interface;
use Fxtran::PATH;

my %opts = qw (out Makefile include Makefile.inc SRC .);
my @opts_f = qw ();
my @opts_s = qw (out include SRC);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

$opts{include} = [split (m/,/o, $opts{include})];

my @sys = qw (NVTX OMP_LIB CUDAFOR IEEE_ARITHMETIC ISO_C_BINDING OPENACC ECCODES CUBLAS ISO_FORTRAN_ENV);
my @extern_mod = qw (YOMHOOK MPL_MODULE PARKIND1 OML_MOD EC_LUN MPL_MPIF MPL_MODULE MPL_BARRIER_MOD FIELD_MODULE 
                     FIELD_FACTORY_MODULE FIELD_ACCESS_MODULE);
my @extern_inc = qw (setup_trans.h setup_trans0.h inv_trans.h gath_grid.h gath_spec.h dist_spec.h abor1.intfb.h);

for my $mod (@extern_mod)
  {
    next if (-f $opts{SRC} . '/' . lc ($mod) . '.F90');
    push @sys, $mod;
  }

my %slurp;

sub slurp
{
  my $f = shift;
  unless ($slurp{$f})
    {
      $slurp{$f} = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); $fh or die ("Cannot open `$f' for reading\n"); <$fh> };
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

      my @use = map { uc ($_) } 
                (($code =~ m/\n\s*use\s+(\w+)/gomis), 
                 ($code =~ m/\n\s*submodule\*s\(\s*(\w+)\s*\)/gomis));
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

sub obj
{
  (my $f = &basename ($_[0])) =~ s/\.(?:F90|F|c)$//o;
  return "\$(BUILD)/$f.o";
}

sub exe
{
  (my $f = &basename ($_[0])) =~ s/\.(?:F90|F|c)$//o;
  return "\$(BUILD)/$f.x";
}

sub ok
{
  (my $f = &basename ($_[0])) =~ s/\.(?:h)$//o;
  return "\$(BUILD)/$f.ok";
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

for my $fypp (<$opts{SRC}/*.fypp>)
  {
    my $code = &slurp ($fypp);
    my @field_config = ($code =~ m/field_config/goms ? qw (-m -field_config) : ());
    (my $F90 = &basename ($fypp)) =~ s/\.fypp/.F90/o;
    next if (&newer ($F90, $fypp));
    system (qw (fypp -m os -M . -m yaml), @field_config, "./$fypp", $F90) and die ($fypp);
  }

my @F90 = map { 'File::Spec'->rel2abs ($_) } <$opts{SRC}/*.F90>;

my $fh = 'FileHandle'->new (">$opts{out}");

$fh->print (<< "EOF");

ifeq (\$(SRC), )
SRC=$opts{SRC}
endif

ifeq (\$(BUILD), )
BUILD=.
endif

all: exe

EOF

$fh->print (join ("\n", map ({ "include $_"  } @{ $opts{include} }), '', ''));

my (@obj, @prg);

for my $F90 (@F90)
  {
    my $obj = &obj ($F90);
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
        (my $intfb = &basename ($F90)) =~ s/\.F90$/.intfb.h/o;

        if ((! -f $intfb) || &newer ($F90, $intfb))
          {
            my @cmd = ('fxtran-f90', '--method', 'interface', 
                       (-f "$opts{SRC}/fxtran.conf" ?  ('--config', "$opts{SRC}/fxtran.conf") : ()),
                       '--dir', '.', '--dryrun', '--tmp', '.', '--', 'f90', '-c', $F90);
            system (@cmd)
              and die ("Command `@cmd' failed");
          }

        my $ok = &ok ($intfb);
        my @deps = &deps ($intfb);
        $fh->print (<< "EOF");
$ok: \$(BUILD)/$intfb @deps
	touch $ok

EOF
      }
    elsif ($kind eq 'PROGRAM')
      {
        push @prg, $F90;
      }
  }


for my $H (grep { ! m/\.intfb\.h$/o } <$opts{SRC}/*.h>)
  {
    my $ok = &ok ($H);
    my @deps = &deps ($H);
    my $h = &basename ($H);
    $fh->print (<< "EOF");
$ok: \$(SRC)/$h @deps
	touch $ok

EOF
  }

my @subobj;

for my $F90 (@F90)
  {
    my @deps = &deps ($F90);
    my @mods = &mods ($F90);
    my $kind = &kind ($F90);
    my $obj = &obj ($F90);
    my $f90 = &basename ($F90);

    if ($kind eq 'MODULE')
      {
        @deps = grep { ($_ ne $obj) } @deps;
      }

    $fh->print (<< "EOF");
$obj: \$(SRC)/$f90 @deps
	\$(FC) -o $obj -c \$(SRC)/$f90

EOF

    push @obj, $obj if ($kind ne 'PROGRAM');

    push @subobj, $obj if ($kind ne 'MODULE');
  }

for my $C (<$opts{SRC}/*.c>)
  {
    my $obj = &obj ($C);
    my $c = &basename ($C);
    $fh->print (<< "EOF");
$obj: \$(SRC)/$c
	\$(CC) -o $obj -c \$(SRC)/$c
EOF
    push @obj, $obj;
    push @subobj, $obj;
  }

my @exe;

for my $prg (@prg)
  {
    my $exe = &exe ($prg);
    my $obj = &obj ($prg);
    $fh->print (<< "EOF");
$exe: $obj @obj
	\$(FC) -o $exe $obj @obj \$(LIBS)

EOF
    push @exe, $exe;
  }


$fh->print (<< "EOF");

exe: @exe

subclean:
	\\rm -f @subobj

clean: 
	\\rm -f \$(BUILD)/*.o \$(BUILD)/*.xml \$(BUILD)/*.a \$(BUILD)/*.x \$(BUILD)/*.mod \$(BUILD)/*.optrpt \$(BUILD)/*.smod \$(BUILD)/*.ok

tidy:
	\\rm -f \$(BUILD)/*.xml \$(BUILD)/*.optrpt
EOF


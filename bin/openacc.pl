#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use Getopt::Long;
use File::stat;
use File::Basename;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use Stack;
use Associate;
use Loop;
use OpenACC;
use ReDim;
use Construct;
use DIR;
use Subroutine;
use Call;
use Canonic;
use DrHook;
use Identifier;

sub updateFile
{
  my ($F90, $code) = @_;

  my $c = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$F90"); $fh ? <$fh> : undef };
  
  if ((! defined ($c)) || ($c ne $code))
    {
      'FileHandle'->new (">$F90")->print ($code);
    }
}

my $SUFFIX = '_OPENACC';

my %opts = ();
my @opts_f = qw (help drhook only-if-newer jljk2jlonjlev);
my @opts_s = qw (dir nocompute);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

if ($opts{help})
  {
    print
     "Usage: " . &basename ($0) . "\n" .
      join ('', map { "  --$_\n" } @opts_f) .
      join ('', map { "  --$_=...\n" } @opts_f) .
     "\n";
    exit (0);
  }


$opts{nocompute} = [$opts{nocompute} ? split (m/,/o, $opts{nocompute}) : ()];

my $F90 = shift;

$opts{dir} ||= &dirname ($F90);

my $suffix = lc ($SUFFIX);
(my $F90out = $F90) =~ s/\.F90/$suffix.F90/;
$F90out = $opts{dir} . '/' . &basename ($F90out);


if ($opts{'only-if-newer'})
  {
    my $st = stat ($F90);
    my $stout = stat ($F90out);
    if ($st && $stout)
      {
        exit (0) unless ($st->mtime > $stout->mtime);
      }
  }


my $d = &Fxtran::parse (location => $F90, fopts => [qw (-canonic -construct-tag -no-include -no-cpp -line-length 500)]);

if ($opts{jljk2jlonjlev})
  {
    &Identifier::rename ($d, JL => 'JLON', JK => 'JLEV');
  }

&Associate::resolveAssociates ($d);

&Construct::changeIfStatementsInIfConstructs ($d);
&Construct::apply 
($d, 
  '//named-E[string(N)="LMUSCLFA"]',        &e ('.FALSE.'),
  '//named-E[string(.)="YDLDDH%LFLEXDIA"]', &e ('.FALSE.'),
);

&DIR::removeDIR ($d);

&Loop::removeJlonLoops ($d);

&ReDim::reDim ($d);

&Subroutine::addSuffix ($d, $SUFFIX);

&Call::addSuffix ($d, suffix => $SUFFIX, match => sub { my $proc = shift; ! grep ({ $_ eq $proc } @{ $opts{nocompute} })});

&OpenACC::routineSeq ($d);

&Stack::addStack ($d, skip => sub { my $proc = shift; grep ({ $_ eq $proc } @{ $opts{nocompute} }) });

&DrHook::remove ($d) unless ($opts{drhook});

&updateFile ($F90out, &Canonic::indent ($d));

&Fxtran::intfb ($F90out, $opts{dir});


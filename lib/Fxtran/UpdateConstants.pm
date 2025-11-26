package Fxtran::UpdateConstants;

use Data::Dumper;

use strict;

use fxtran;
use fxtran::xpath;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Style;
use Fxtran::Bt;
use Fxtran::Finder;
use Fxtran::Util;

my %intf;

sub getActualArgumentIntent
{
  my ($p, %opts) = @_;

  my $find = $opts{find};

  my $stmt = &Fxtran::stmt ($p);

  my ($name, $rank) = &F ('./arg-N', $p, 1);

  unless ($name)
    {
      my @arg = &F ('./arg-spec/arg', $stmt);

      for ($rank = 0; $rank < scalar (@arg); $rank++)
        {
          last if ($arg[$rank]->unique_key eq $p->unique_key);
        }

      die if ($rank == scalar (@arg));
    }
  
  my ($proc) = &F ('./procedure-designator', $stmt, 1);

  unless ($intf{$proc})
    {
      my $intf = $find->getInterface (name => $proc);
      
      die unless ($intf);
      
      $intf = &Fxtran::Util::slurp ($intf);
      
      ($intf) = &parse (fragment => $intf, fopts => [qw (-construct-tag -no-cpp -line-length 500 -canonic)]);
      
      ($intf) = &F ('.//program-unit[./subroutine-stmt[string(./subroutine-N)="?"]]', $proc, $intf);
      
      &Fxtran::Canonic::makeCanonic ($intf);

      $intf{$proc} = $intf;
    }

  unless ($name)
    {
      my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $intf{$proc});
      $name = $arg[$rank]->textContent;
    }

  my ($dp) = &F ('./specification-part/declaration-part', $intf{$proc});

  my ($intent) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(./EN-N)="?"]]/attribute/intent-spec', $name, $dp, 1);

  return $intent;
}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  my $find = $opts{find};

  my %list = map { ($_, 1) } @{ $opts{types} };

  my ($up) = &F ('./specification-part/use-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my @arg = &F ('./T-decl-stmt[./_T-spec_/derived-T-spec][./attribute[string(./intent-spec)="INOUT"]]/EN-decl-LT/EN-decl/EN-N', $dp);

  my %typ;

  for my $arg (@arg)
    {
      my $decl = &Fxtran::stmt ($arg);
      my ($ts) = &F ('./_T-spec_/derived-T-spec/T-N', $decl, 1);
      $typ{$arg->textContent} = $ts;
    }

  @arg = grep { $list{$typ{$_}} } map { $_->textContent } @arg;

  my %arg = map { ($_, 1) } @arg;
  
  my %N;
  
  for my $expr (&F ('.//named-E', $ep))
    {
      my ($N) = &F ('./N', $expr, 1);
  
      next unless ($arg{$N});
  
      my $p = $expr->parentNode;
  
      my $do = 0;
  
      if ($p->nodeName eq 'E-1')
        {
          $do = 1;
        }
      elsif ($p->nodeName eq 'arg')
        {
          my $intent = &getActualArgumentIntent ($p, find => $find);
          $do = $intent ne 'IN';
        }
  
      my $stmt = &Fxtran::stmt ($p);
  
      if ($do)
        {
          $stmt->parentNode->insertAfter ($_, $stmt) for (&s ("LLDO_$N = .TRUE."), &t ("\n"));
          $N{$N}++;
        }
    }
  
  
  $dp->appendChild (&t ("\n"));
  $ep->insertBefore (&t ("\n"), $ep->firstChild);
  
  for my $N (sort keys (%N))
    {
      $dp->appendChild ($_) for (&t ("\n"), &s ("LOGICAL :: LLDO_$N"));
      $ep->insertBefore ($_, $ep->firstChild) for (&t ("\n"), &s ("LLDO_$N = .FALSE."));
  
      my ($if) = &parse (fragment => << "EOF");
IF (LLDO_$N) THEN
CALL $opts{'method-prefix'}WIPE ($N)
CALL $opts{'method-prefix'}COPY ($N)
ENDIF
EOF
  
      $ep->appendChild ($_) for (&t ("\n"), $if);

      $up->appendChild ($_) for (&t ("\n"), &s ("USE UTIL_$typ{$N}_MOD"));
  
    }
  
  $dp->appendChild (&t ("\n"));


}

1;

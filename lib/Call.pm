package Call;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Subroutine;
use Data::Dumper;

sub addSuffix
{
  my ($d, %opts) = @_;

  my ($suffix, $match, $section) = @opts{qw (suffix match section)};

  $section ||= $d;

  my %contained = map { ($_, 1) } &F ('//subroutine-stmt[count(ancestor::program-unit)>1]/subroutine-N/N/n/text()', $d, 1);

  my %proc;
  for my $proc (&F ('.//call-stmt/procedure-designator', $section))
    {
      next if ($proc->textContent =~ m/%/o);
      next if ($proc->textContent eq 'DR_HOOK');
      ($proc) = &F ('./named-E/N/n/text()', $proc);
      next if ($contained{$proc->textContent});
      if ($match)
        {
          next unless $match->($proc);
        }
      $proc{$proc->textContent} = 1;
      $proc->setData ($proc->textContent . $suffix);
    }


           

  PROC: for my $proc (keys (%proc))
    {   
      for my $ext (qw (.intfb.h .h))
        {
          next unless (my ($include) = &F ('.//include[string(filename)="?"]', lc ($proc) . $ext, $d));
  
          if (&F ('.//call-stmt[string(procedure-designator)="?"]', $proc, $d))
            {
              my $include1 = $include->cloneNode (1);
              my ($t) = &F ('./filename/text()', $include1); 
              $t->setData (lc ($proc) . lc ($suffix) . $ext);
              $include->parentNode->insertBefore ($include1, $include);
              $include->parentNode->insertBefore (&t ("\n"), $include);
            }
          else
            {
              my ($t) = &F ('./filename/text()', $include); 
              $t->setData (lc ($proc) . lc ($suffix) . $ext);
            }
          next PROC;
        }

      # MesoNH style
      if (my ($mode) = &F ('.//use-stmt/module-N/N/n/text()[string(.)="?"]', "MODE_$proc", $d))
        {
          $mode->setData ("MODE_${proc}${suffix}");
          my $use = &Fxtran::stmt ($mode);
          my ($un) = &F ('./rename-LT/rename/use-N/N/n/text()[string(.)="?"]', $proc, $use);
          $un->setData ("${proc}${suffix}");
          next PROC;
        }

      if (my ($mode) = &F ('.//use-stmt[./rename-LT/rename/use-N/N/n/text()[string(.)="?"]]/module-N/N/n/text()', $proc, $d))
        {
          $mode->setData ($mode->data () . ${suffix}) unless ($mode->data () =~ m,$suffix$,);
          my $use = &Fxtran::stmt ($mode);
          my ($un) = &F ('./rename-LT/rename/use-N/N/n/text()[string(.)="?"]', $proc, $use);
          $un->setData ("${proc}${suffix}");
          next PROC;
        }

    }   


}

sub getArgumentIntent
{
  my ($call, $expr, $find) = @_;

  my ($proc) = &F ('./procedure-designator', $call, 1);

  my $intf = &Subroutine::getInterface ($proc, $find);

  my ($stmt) = &F ('.//program-unit/subroutine-stmt[string(subroutine-N)="?"]', $proc, $intf);

  my $unit = $stmt->parentNode;

  # Dummy arguments
  my @argd = &F ('./dummy-arg-LT/arg-N', $stmt, 1); 

  # Actual arguments
  my @arga = &F ('./arg-spec/arg', $call); 

  my $argn;

  for my $i (0 .. $#arga)
    {
      my $arga  = $arga[$i];
      my ($k) = &F ('./arg-N/k/text()', $arga, 1);
      my ($e) = &F ('./ANY-E', $arga);
      if ($expr->isEqual ($e))
        {
          $argn = $k || $argd[$i];
          last;
        }
    }

  return unless ($argn);

  my ($intent) = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]//intent-spec', $argn, $unit, 1);

  return $intent;
}

sub grokIntent
{
  my ($expr, $pintent, $find) = @_;
  
  my ($r, $w);

  if ($expr->parentNode->nodeName eq 'E-1')
    {
      $w = 1;
    }
  elsif ($expr->parentNode->nodeName eq 'arg')
    {
      my $stmt = &Fxtran::stmt ($expr);

      if ($stmt->nodeName eq 'call-stmt')
        {
          my $intent = &getArgumentIntent ($stmt, $expr, $find) || 'INOUT';
          if ($intent =~ m/IN/o)
            {
              $r = 1;
            }
          if ($intent =~ m/OUT/o)
            {
              $w = 1;
            }
        }
      else
        {
          $r = 1;
        }
    }
  else
    {
      $r = 1;
    }
  
  if (defined ($$pintent))
    {
      $$pintent = 'INOUT' if ($w);
    }
  else
    {
      if ($r)
        {
          $$pintent = 'IN';
        }
      if ($w)
        {
          $$pintent = 'INOUT';
        }
    }

  $$pintent ||= 'INOUT';

}

1;

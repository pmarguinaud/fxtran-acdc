package Fxtran::Call;

=head1 NAME

Fxtran::Call

=head1 DESCRIPTION

This package provides functions to work on call statements. 

=head1 FUNCTIONS

=cut

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Subroutine;

sub addSuffix
{
  my ($pu, %opts) = @_;

=head2 addSuffix

The C<addSuffix> function
works on a code section, and adds a suffix to the procedure designator of all
call statements.

Interface includes and interfaces provided by modules are updated accordingly.

Some procedure designators may be excluded from the processing, using the C<match>
callback, passed as argument. 

Contained subroutines may be excluded from the processing if the argument C<contained>
is present.

=cut

  my ($suffix, $match, $section, $contained) = @opts{qw (suffix match section contained)};

  my ($ep) = &F ('./execution-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($up) = &F ('./specification-part/use-part', $pu);

  $section ||= $pu;

  my %contained;

  unless ($contained)
    {
      %contained = map { ($_, 1) }
        &F ('//subroutine-stmt[count(ancestor::program-unit)>1]/subroutine-N/N/n/text()', $pu, 1);
    }

  my %proc;
  for my $proc (&F ('.//call-stmt/procedure-designator', $section))
    {
      my $name = $proc->textContent;
      next if ($name =~ m/%/o);
      next if ($name eq 'DR_HOOK');
      ($proc) = &F ('./named-E/N/n/text()', $proc);
      next if ($contained{$proc->textContent});

      if ($match)
        {
          next unless $match->($proc);
        }

      $proc{$proc->textContent} = 1;
      $proc->setData ($proc->textContent . $suffix);
    }

  PROC: for my $proc (sort keys (%proc))
    {   
      my @ext = qw (.intfb.h .h);

      shift (@ext) if ($opts{'merge-interfaces'});

      for my $ext (@ext)
        {
          next if (&F ('./include[string(filename)="?"]', lc ($proc) . lc ($suffix) . $ext, $dp));
          next unless (my ($include) = &F ('./include[string(filename)="?"]', lc ($proc) . $ext, $dp));
  
          if (&F ('.//call-stmt[string(procedure-designator)="?"]', $proc, $ep))
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
      if (my ($mode) = &F ('./use-stmt/module-N/N/n/text()[string(.)="?"]', "MODE_$proc", $up))
        {
          my $use = &Fxtran::stmt ($mode);
          my $use1 = $use->cloneNode (1);

          my ($mode1) = &F ('./module-N/N/n/text()', $use1);
          $mode1->setData ("MODE_${proc}${suffix}");
          my ($un) = &F ('./rename-LT/rename/use-N/N/n/text()[string(.)="?"]', $proc, $use1);
          $un->setData ("${proc}${suffix}");
 
          $up->insertAfter ($_, $use) for ($use1, &t ("\n"));
        
          next PROC;
        }

      if (my ($mode) = &F ('./use-stmt[./rename-LT/rename/use-N/N/n/text()[string(.)="?"]]/module-N/N/n/text()', $proc, $up))
        {
          my $use = &Fxtran::stmt ($mode);

          my ($mod) = &F ('./module-N', $use, 1);

          my $use1 = &s ("USE $mod$suffix, ONLY : $proc$suffix");

          $up->insertAfter ($_, $use) for ($use1, &t ("\n"));

          next PROC;
        }

    }   

}

sub getArgumentIntent
{
  my ($call, $expr, $find) = @_;

=head2 getArgumentIntent

This function returns the argument intent of an expression passed
as argument to a call statement.

We rely on the C<$find> argument of C<getArgumentIntent> to find
the subroutine interface and retrieve the dummy argument intent.

=cut

  my ($proc) = &F ('./procedure-designator', $call, 1);

  if ($proc eq 'PCRC')
    {
      return 'IN';
    }

  my $intf = &Fxtran::Subroutine::getInterface ($proc, $find);

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

=head1 grokIntent

This function finds the intent of an expression (IN or INOUT) in any statement.

=cut
  
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

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

1;

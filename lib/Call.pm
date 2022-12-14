package Call;

use strict;
use Fxtran;

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


  for my $proc (keys (%proc))
    {   
      next unless (my ($include) = &F ('.//include[string(filename)="?"]', lc ($proc) . '.intfb.h', $d));

      if (&F ('.//call-stmt[string(procedure-designator)="?"]', $proc, $d))
        {
          my $include1 = $include->cloneNode (1);
          my ($t) = &F ('./filename/text()', $include1); 
          $t->setData (lc ($proc) . lc ($suffix) . '.intfb.h');
          $include->parentNode->insertBefore ($include1, $include);
          $include->parentNode->insertBefore (&t ("\n"), $include);
        }
      else
        {
          my ($t) = &F ('./filename/text()', $include); 
          $t->setData (lc ($proc) . lc ($suffix) . '.intfb.h');
        }
    }   


}

1;

package Associate;
#
use strict;
use FileHandle;
use Data::Dumper;

use Fxtran;

sub resolveAssociates
{
  my $d = shift;
  my %args = @_;

  my @assoc = $args{outer} ? &F ('ancestor::associate-construct', $d) : &F ('.//associate-construct', $d);
  
  for my $assoc (@assoc)
    {
      my @as = &F ('./associate-stmt/associate-LT/associate', $assoc);
  
      # Apply association rules
  
      for my $as (@as)
        {
          my ($n) = &F ('./associate-N/n/text()', $as, 1);
          my ($e) = &F ('./selector/named-E', $as);
  
          my @expr = &F ('.//named-E[./N/n/text()="?"]', $n, $assoc);
  
          for my $expr (@expr)
            {
              # List of references for current expression
  
              my @r = &F ('./R-LT/node()', $expr);
              my $E = $e->cloneNode (1);
              if (@r)
                {
                  my ($rlt) = &F ('./R-LT', $E);
                  unless ($rlt)
                    {
                      $rlt = &n ('<R-LT/>');
                      $E->appendChild ($rlt);
                    }
  
                  # Append expression references to new expression
                  for (@r)
                    {
                      $rlt->appendChild ($_);
                    }
                }
  
              # Replace expression with its association
  
              $expr->replaceNode ($E);
            }
  
        }
   
      unless ($args{outer})
        {
          # Remove ASSOCIATE block & statements
  
          $assoc->firstChild->unbindNode ();
          $assoc->lastChild->unbindNode ();
  
          for (&F ('./node()', $assoc))
            {
              $assoc->parentNode->insertBefore ($_, $assoc);
            }
  
          $assoc->unbindNode ();
        }
    }
  
  
}

1;
  
  

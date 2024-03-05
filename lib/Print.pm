package Print;

use strict;
use Fxtran;

sub useABOR1_ACC
{
  my $d = shift;

  my @abor1 = &F ('.//call-stmt/procedure-designator/named-E/N/n/text()[string(.)="ABOR1"]', $d);

  for my $abor1 (@abor1)
    {
      $abor1->setData ('ABOR1_ACC');
    }
  my @include = &F ('.//include[string(filename)="abor1.intfb.h"]', $d);
  for (@include)
    {
      $_->unbindNode ();
    }
}

sub removeTRIM
{
  my $expr = shift;

  my @TRIM = &F ('.//named-E[string(N)="TRIM"]', $expr);

  for my $TRIM (@TRIM)
    {
      my ($str) = &F ('./R-LT/array-R/section-subscript-LT/section-subscript/lower-bound/ANY-E', $TRIM);
      $TRIM->replaceNode ($str);
    }

}

sub changeWRITEintoPRINT
{
  my $d = shift;

  my @write = &F ('.//write-stmt[string(./io-control-spec/io-control)="NULERR" or string(./io-control-spec/io-control)="NULOUT"]', $d);
  
  for my $write (@write)
    {
      my ($output) = &F ('./output-item-LT', $write, 1);
      $write->replaceNode (&s ("PRINT *, $output"));
    }

}

sub changePRINT_MSGintoPRINT
{
  my $d = shift;

  my @print_msg = &F ('.//call-stmt[string(procedure-designator)="PRINT_MSG"]', $d);
  
  for my $print_msg (@print_msg)
    {
      my @arg = &F ('./arg-spec/arg/ANY-E', $print_msg);

      my $mess = $arg[-1];
      &removeTRIM ($mess);

      if ($arg[0]->textContent eq 'NVERB_FATAL')
        {
          $print_msg->replaceNode (&s ("CALL ABOR1_ACC (" . $mess->textContent . ")"));
        }
      else
        {
          $print_msg->replaceNode (&s ("PRINT *, " . $mess->textContent . ")"));
        }
    }

}

1;

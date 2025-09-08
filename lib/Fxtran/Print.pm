package Fxtran::Print;

=head1 NAME

Fxtran::Print

=head1 DESCRIPTION

The purpose of this module is to provide functions to transform
C<PRINT> statements, C<WRITE> statements, calls to C<ABOR1>
or MesoNH C<PRINT_MSG>. These statements cannot be used
on the device, and need to be replaced by a simple C<PRINT>
statements and/or a C<STOP> statement.

=head1 FUNCTIONS

=cut

use strict;

use Fxtran;
use Data::Dumper;

sub useABOR1_ACC
{

=head2 useABOR1_ACC

Replace calls to C<ABOR1> by calls to C<FXTRAN_ACDC_ABORT>, which can be used 
on the device.

=cut

  my $d = shift;

  my ($ep, $dp);

  if ($d->nodeName eq 'program-unit')
    {
      ($ep) = &F ('./execution-part', $d);
      ($dp) = &F ('./specification-part/declaration-part', $d);
    }
  else
    {
      $ep = $d;
    }

  my @abor1 = &F ('.//call-stmt/procedure-designator/named-E/N/n/text()[string(.)="ABOR1"]', $ep);

  for my $abor1 (@abor1)
    {
      $abor1->setData ('FXTRAN_ACDC_ABORT');
    }

  if ($dp)
    {
      my @include = &F ('./include[string(filename)="abor1.intfb.h"]', $dp);

      for (@include)
        {
          $_->unbindNode ();
        }
    }
}

sub removeTRIM
{
  my $expr = shift;

  my @TRIM = &F ('.//named-E[string(N)="TRIM"]', $expr);

  for my $TRIM (@TRIM)
    {
      # element-LT/.. should be replaced by something else
      my ($str) = &F ('./R-LT/function-R/element-LT/element/ANY-E', $TRIM);
      $TRIM->replaceNode ($str);
    }

}

sub changeWRITEintoPRINT
{

=head2 changeWRITEintoPRINT

Change C<WRITE> statements (not supported on the device) into C<PRINT> statements.

=cut

  my $d = shift;

  my ($ep) = &F ('./execution-part', $d);

  my @write = &F ('.//write-stmt[string(./io-control-spec/io-control)="NULERR" or string(./io-control-spec/io-control)="NULOUT"]', $ep);
  
  for my $write (@write)
    {
      my ($output) = &F ('./output-item-LT', $write, 1);
      $write->replaceNode (&s ("PRINT *, $output"));
    }

}

sub changePRINT_MSGintoPRINT
{

=head2 changePRINT_MSGintoPRINT

Change calls to C<PRINT_MSG> into calls to C<ABOR1> or C<PRINT> statements.

=cut

  my $d = shift;

  my ($ep) = &F ('./execution-part', $d);

  my @print_msg = &F ('.//call-stmt[string(procedure-designator)="PRINT_MSG"]', $ep);
  
  for my $print_msg (@print_msg)
    {
      my @arg = &F ('./arg-spec/arg/ANY-E', $print_msg);

      my $mess = $arg[-1];
      &removeTRIM ($mess);

      if ($arg[0]->textContent eq 'NVERB_FATAL')
        {
          $print_msg->replaceNode (&s ("CALL FXTRAN_ACDC_ABORT (" . $mess->textContent . ")"));
        }
      else
        {
          $print_msg->replaceNode (&s ("PRINT *, " . $mess->textContent . ")"));
        }
    }

}

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;

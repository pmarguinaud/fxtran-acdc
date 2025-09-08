package Fxtran::ACPY;

=head1 NAME

Fxtran::ACPY

=head1 DESCRIPTION

This module provides functions to transform array assignments 
into calls to functions which will perform the assignements.

This is meant to avoid the compiler makeing assumptions about
aliasing, which leads to very conservative optimizations and
memory allocations.

=head1 EXAMPLE

  X (:,:) = Y (:,:)

is transformed into:

  CALL FXTRAN_ACDC_ARRAY_COPY (X, Y)

or, even better (for the NVHPC compiler):

  CALL FXTRAN_ACDC_ARRAY_COPY (X, SIZE (X (:,:), 1), SIZE (X (:,:), 2), &
                             & Y, SIZE (Y (:,:), 1), SIZE (Y (:,:), 2))

=cut

use Data::Dumper;

use strict;

use Fxtran;

sub useAcpy
{
  my $do_jlon = shift;
  my %opts = @_;

  my $jlon = $opts{style}->jlon ();

  my @acpy = &F ('.//a-stmt'
               . '[E-1/named-E/R-LT/array-R/section-subscript-LT/section-subscript[string(lower-bound)="?"]]' 
               . '[E-2/named-E/R-LT/array-R/section-subscript-LT/section-subscript[string(lower-bound)="?"]]'
               , $jlon, $jlon, $do_jlon);
  
  for my $acpy (@acpy)
    {
      my ($E1) = &F ('./E-1/named-E', $acpy);
      my ($E2) = &F ('./E-2/named-E', $acpy);

      my @lb1 = &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $E1); 
      my @lb2 = &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $E2); 

      my @dd1 = map { &F ('./text()[contains(string(.),":")]', $_) } @lb1;
      my @dd2 = map { &F ('./text()[contains(string(.),":")]', $_) } @lb2;

      die if (@dd1 && (! @dd2));
      die if (@dd2 && (! @dd1));
      next unless (@dd1 && @dd2);

      $lb1[0]->replaceNode (&n ('<section-subscript>:</section-subscript>'));
      $lb2[0]->replaceNode (&n ('<section-subscript>:</section-subscript>'));

      $acpy->replaceNode (&s ("CALL FXTRAN_ACDC_ARRAY_COPY ($jlon, " . $E1->textContent . ', ' . $E2->textContent . ')'));
    }
}

sub useBcpy
{
  my $do_jlon = shift;
  my %opts = @_;

  my $jlon = $opts{style}->jlon ();

  my @acpy = &F ('.//a-stmt'
               . '[E-1/named-E/R-LT/array-R/section-subscript-LT/section-subscript[string(lower-bound)="?"]]' 
               . '[E-2/named-E/R-LT/array-R/section-subscript-LT/section-subscript[string(lower-bound)="?"]]'
               , $jlon, $jlon, $do_jlon);
  
  for my $acpy (@acpy)
    {
      my ($E1) = &F ('./E-1/named-E', $acpy);
      my ($E2) = &F ('./E-2/named-E', $acpy);

      my @lb1 = &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $E1); 
      my @lb2 = &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $E2); 

      my @dd1 = map { &F ('./text()[contains(string(.),":")]', $_) } @lb1;
      my @dd2 = map { &F ('./text()[contains(string(.),":")]', $_) } @lb2;

      die if (@dd1 && (! @dd2));
      die if (@dd2 && (! @dd1));
      next unless (@dd1 && @dd2);

      $lb1[0]->replaceNode (&n ('<section-subscript>:</section-subscript>'));
      $lb2[0]->replaceNode (&n ('<section-subscript>:</section-subscript>'));

      $E1 = $E1->textContent;
      $E2 = $E2->textContent;

      my $call = "CALL FXTRAN_ACDC_ARRAY_COPY ($jlon, " . 
            join (', ', map { "SIZE ($E1, $_)" } (1 .. 1 + scalar (@dd1))) .  ", $E1" . ', ' .
            join (', ', map { "SIZE ($E2, $_)" } (1 .. 1 + scalar (@dd2))) .  ", $E2" . ')';

      $call = &s ($call);

      $acpy->replaceNode ($call);
    }
}

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

1;

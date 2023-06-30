package ACPY;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;

sub useAcpy
{
  my $do_jlon = shift;

  my @acpy = &F ('.//a-stmt'
               . '[E-1/named-E/R-LT/array-R/section-subscript-LT/section-subscript[string(lower-bound)="JLON"]]' 
               . '[E-2/named-E/R-LT/array-R/section-subscript-LT/section-subscript[string(lower-bound)="JLON"]]'
               , $do_jlon);
  
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

      $acpy->replaceNode (&s ('CALL ACPY (JLON, ' . $E1->textContent . ', ' . $E2->textContent . ')'));
    }
}

1;

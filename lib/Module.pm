package Module;

#
# Copyright 2024 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use FileHandle;
  
sub addSuffix
{
  my ($d, $suffix) = @_;

  my @sn = &F ('./module-stmt/module-N/N/n/text()|./end-module-stmt/module-N/N/n/text()', $d);

  for my $sn (@sn) 
    {
      $sn->setData ($sn->data . $suffix);
    }

}

sub rename
{
  my ($d, $sub) = @_; 

  my $d = &getProgramUnit ($d);

  my @name = (
               &F ('./module-stmt/module-N/N/n/text()', $d),
               &F ('./end-module-stmt/module-N/N/n/text()', $d),
             );
  my $name = $name[0]->textContent;

  my $name1 = $sub->($name);

  for (@name)
    {   
      $_->setData ($name1);
    }   

}

1;

package Module;

#
# Copyright 2024 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use FileHandle;
  
sub getProgramUnit
{
  my $d = shift;

  my $pu;
  if ($d->isa ('XML::LibXML::Document'))
    {
      ($pu) = &F ('./object/file/program-unit', $d);
    }
  elsif ($d->nodeName eq 'program-unit')
    {
      $pu = $d;
    }
  else
    {
      die $d->nodeName;
    }

  return $pu;
}

sub addSuffix
{
  my ($d, $suffix) = @_;

  my $pu = &getProgramUnit ($d);

  my @sn = &F ('./module-stmt/module-N/N/n/text()|./end-module-stmt/module-N/N/n/text()', $pu);

  for my $sn (@sn) 
    {
      $sn->setData ($sn->data . $suffix);
    }

}

sub rename
{
  my ($d, $sub) = @_; 

  my $pu = &getProgramUnit ($d);

  my @name = (
               &F ('./module-stmt/module-N/N/n/text()', $pu),
               &F ('./end-module-stmt/module-N/N/n/text()', $pu),
             );
  my $name = $name[0]->textContent;

  my $name1 = $sub->($name);

  for (@name)
    {   
      $_->setData ($name1);
    }   

}

1;

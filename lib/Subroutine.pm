package Subroutine;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use FileHandle;
  
sub addSuffix
{
  my ($d, $suffix) = @_;

  my @sn = &F ('./object/file/program-unit/subroutine-stmt/subroutine-N/N/n/text()|./object/file/program-unit/end-subroutine-stmt/subroutine-N/N/n/text()', $d);

  for my $sn (@sn) 
    {
      $sn->setData ($sn->data . $suffix);
    }

  my @drhook_name = &F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]/arg-spec/arg/string-E/S/text()', $d);

  for (@drhook_name)
    {
      (my $str = $_->data) =~ s/(["'])$/$suffix$1/go;
      $_->setData ($str);
    }

}

sub rename
{
  my ($doc, $sub) = @_; 

  my @name = &F ('.//subroutine-N/N/n/text()', $doc);
  my $name = $name[0]->textContent;

  my $name1 = $sub->($name);

  for (@name)
    {   
      $_->setData ($name1);
    }   

  my @drhook = &F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]', $doc);

  for my $drhook (@drhook)
    {   
      next unless (my ($S) = &F ('./arg-spec/arg/string-E/S/text()', $drhook));
      my $str = $S->textContent;
      $str =~ s/$name/$name1/;
      $S->setData ($str);
    }   
  
}

sub getInterface
{
  my ($name, $find) = @_;
  my $file = $find->getInterface (name => $name);
  $file or die ("Could not find interface for $name");
  my $code = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$file"); $fh or die ("Cannot open $file"); <$fh> };
  my ($intf) = &Fxtran::parse (fragment => $code);
  return $intf;
}

1;

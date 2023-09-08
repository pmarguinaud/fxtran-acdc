package Pointer::Object;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Data::Dumper;
use Storable;

{

my %decl;

sub getObjectDecl
{
  my ($key, $types, %opts) = @_;

  my $h = $types;

  unless ($decl{$key}) 
    {
      unless ($h->{$key})
        {
          if ($opts{allowConstant})
            {
              return;
            }
          else
            {
              die $key;
            }
        }
      ($decl{$key}) = &s ($h->{$key});
    }

  return $decl{$key};
}

my %type;

sub getObjectType
{
  my ($s, $obj) = @_;

  unless ($type{$obj})
    {
      ($type{$obj}) = &F ('./T-N', $s->{ts}, 1);
    }

  return $type{$obj};
}

}

sub asFromDecl
{
  my $decl = shift;

  my ($as) = &F ('.//EN-decl/array-spec', $decl);

  return $as;
}

sub getFieldFromExpr
{
  my ($expr) = @_;

  my @Ctl = &F ('./R-LT/component-R/ct/text()', $expr);
  my $ctl = @Ctl ? $Ctl[-1]->textContent : undef;

  my ($obj) = &F ('.//N', $expr, 1);

  if (($obj eq 'YDVARS') && $ctl && ($ctl eq 'RCP'))
     {
     }
  elsif (($obj eq 'YDVARS') && $ctl && ($ctl eq 'LT1'))
     {
     }
  elsif (($obj eq 'YDVARS') && $ctl && ($ctl eq 'P'))
     {
       $ctl = 'FT0';
     }
  elsif ($ctl =~ m/^(?:T[019]|(?:DM|DL)[019]?)$/o)
    {
      $ctl = 'F' . $ctl; 
    }
  elsif ($obj eq 'YDMF_PHYS_SURF') 
    {
      if ($ctl =~ m/^P(\w+)_T[019]$/o)
        {
          $ctl =~ s/^P/F_/o;
        }
      else
        {
          $ctl =~ s/^P/F_/o;
        }
    }
  else
    {
      $ctl = 'F_' . $ctl; 
    }

  my $e = $expr->cloneNode (1);
  my ($RLT) = &F ('./R-LT', $e);

  @Ctl = &F ('./component-R/ct/text()', $RLT);
  if ($ctl)
    {
      $Ctl[-1]->setData ($ctl);
    }

  @Ctl = &F ('./ANY-R', $RLT);

  while (my $Ctl = pop (@Ctl))
    {
      last unless (($Ctl->nodeName eq 'array-R') or ($Ctl->nodeName eq 'parens-R'));
      $Ctl->unbindNode ();
    }

  if ($RLT->lastChild->nodeName eq '#text')
    {
      $RLT->lastChild->unbindNode ();
    }

  return $e;
}

sub getFieldFromObjectComponents
{
  my ($obj, @ctl) = @_;

  if (($obj eq 'YDVARS') && @ctl && ($ctl[-1] eq 'RCP'))
     {
     }
  elsif (($obj eq 'YDVARS') && @ctl && ($ctl[-1] eq 'LT1'))
     {
     }
  elsif ($ctl[-1] =~ m/^(?:T[019]|(?:DM|DL)[019]?)$/o)
    {
      $ctl[-1] = 'F' . $ctl[-1]; 
    }
  elsif ($obj eq 'YDMF_PHYS_SURF') 
    {
      if ($ctl[-1] =~ m/^P(\w+)_T[019]$/o)
        {
          $ctl[-1] =~ s/^P/F_/o;
        }
      else
        {
          $ctl[-1] =~ s/^P/F_/o;
        }
    }
  else
    {
      $ctl[-1] = 'F_' . $ctl[-1]; 
    }

  my $n =  &n ("<named-E><N><n>$obj</n></N><R-LT>" 
             . join ('', map { "<component-R>%<ct>$_</ct></component-R>" } @ctl)
             . "</R-LT></named-E>");

  return $n;
}

1;

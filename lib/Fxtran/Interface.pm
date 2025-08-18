package Fxtran::Interface;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use File::Basename;
use FileHandle;
use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Util;

sub intfbBody
{
  my $doc = shift;

  my @pu = &F ('./object/file/program-unit', $doc);

  for my $n (&F ('.//n/text()', $doc))
    {
      my $t = $n->textContent;
      if ((my $T = uc ($t)) ne $t)
        {
          $n->setData ($T);
        }
    }

  for my $pu (@pu)
    {
      if (my ($ep) = &F ('./execution-part', $pu))
        {
          $ep->unbindNode ();
        }

      my ($up) = &F ('./specification-part/use-part', $pu);
      my ($dp) = &F ('./specification-part/declaration-part', $pu);
      my ($ip) = &F ('./specification-part/implicit-part', $pu);

      for (&F ('.//program-unit', $pu))
        {
          $_->unbindNode ();
        }

      for (&F ('.//cpp-section|.//cpp', $pu))
        {
          $_->unbindNode ();
        }

      my $first = $pu->firstChild;
   
      if ($first->nodeName =~ m/^(?:module|program)-stmt$/o)
        {
          $pu->unbindNode ();
          next;
        }
  
      # Strip blocks (these may contain use statements)
      
      for (&F ('.//ANY-construct', $pu))
        {
          $_->unbindNode ();
        }
  
      (my $kind = $first->nodeName ()) =~ s/-stmt$//o;
  
      my ($name) = &F ('./' . $kind . '-N/N/n/text()', $first, 1);
      my @args = &F ('.//dummy-arg-LT//arg-N/N/n/text()', $first, 1);
      
      my %stmt;
      
      # Keep first & last statements
      
      $stmt{$pu->firstChild} = $pu->firstChild;
      $stmt{$pu->lastChild}  = $pu->lastChild;
      
      # %symb holds symbols whose declaration should be kept
      # %undf holds symbols which are not defined with T-decl-stmt nor imported by use statements

      my (%symb, %undf);

      for my $arg (@args)
        {
          $symb{$arg} = 1;
        }
      
      # Keep result declaration (function)

      if ($first->nodeName eq 'function-stmt')
        {
          my ($result) = &F ('./result-spec/N', $first, 1);
           
          unless ($result)
            {
              ($result) = &F ('./function-N', $first, 1);
            }

          $symb{$result} = 1;
        }

      # Index decl statements

      my %s2d;

      for my $decl (&F ('./ANY-stmt[.//EN-decl]', $dp || $pu))
        {
          for my $symb (&F ('.//EN-N', $decl, 1))
            {
              push @{ $s2d{$symb} }, $decl;
            }
        }

      # Index use statements
      
      my %s2u;

      for my $use (&F ('./use-stmt', $up || $pu))
        {
          for my $symb (&F ('.//use-N', $use, 1))
            {
              push @{ $s2u{$symb} }, $use;
            }
        }
      
      
      # Symbols used in decl statements of arguments
      
      my @symb = sort keys (%symb);

      while (my $symb = shift (@symb))
        {
          if (my @decl = @{ $s2d{$symb} || [] })
            {
              for my $decl (@decl)
                {
                  $stmt{$decl} = $decl;
                  for my $s (&F ('.//named-E/N', $decl), &F ('.//T-N', $decl))
                    {
                      my $t = $s->parentNode;
                      if ($t->nodeName ne 'intrinsic-T-spec')
                        {
                          $s = $s->textContent;
                          push @symb, $s unless ($symb{$s});
                          $symb{$s} = 1; 
                        }
                    }
                }
            }
          elsif (my @use = @{ $s2u{$symb} || [] })
            {
              for my $use (@use)
                {
                  $stmt{$use} = $use; 
                }
            }
          else
            {
              $undf{$symb} = 1;
            }
        }

      if (%undf)
        {
          # Keep use statements without ONLY list : they may import some of the undefined symbols

          for my $use (&F ('./use-stmt[not(./rename-LT)]', $up || $pu))
            {
              $stmt{$use} = $use;
            }
        }

      my @stmt;
 
      if ($dp)
        {
          push @stmt,
            &F ('./ANY-stmt', $up),
            &F ('./ANY-stmt', $ip),
            &F ('./ANY-stmt', $dp);
        }
      else
        {
          @stmt = &F ('./ANY-stmt', $pu);
        }
      
      for my $stmt (@stmt)
        {
          next if ($stmt->nodeName eq 'implicit-none-stmt');
          $stmt->unbindNode () unless ($stmt{$stmt});
        }
  
    }
  

  # Strip labels
  for (&F ('.//label', $doc))
    {
      $_->unbindNode ();
    }
  
  # Strip comments
  
  for (&F ('.//C', $doc))
    {
      next if ($_->textContent =~ m/^!\$acc\s+routine/io);
      $_->unbindNode ();
    }
  
  # Strip includes
  
  for (&F ('.//include', $doc))
    {
      $_->unbindNode ();
    }

  # Strip defines

  for (&F ('.//cpp[starts-with (text(),"#define ")]', $doc))
    {
      $_->unbindNode ();
    }


  for (&F ('.//unseen', $doc))
    {
      $_->unbindNode ();
    }


  $doc->documentElement->normalize ();

  my @text = &F ('.//text()[translate(.," ?","")=""]', "\n", $doc);

  for my $text (@text)
    {
      if ($text->data =~ m/\n/goms)
        {
          $text->setData ("\n");
        }
    }

}

sub intfb
{
  my ($F90, $dir, $ext, %opts) = @_;

  $dir ||= '.';
  $ext ||= '.intfb.h';

  my $doc = &Fxtran::parse (location => $F90, fopts => ['-construct-tag', '-no-include', '-line-length' => 500]);
  
  my ($openacc) = &F ('.//C[starts-with(.,"!$ACDC singlecolumn")]', $doc);

  &intfbBody ($doc);

  my $text_acc = '';

  if ($openacc && $opts{'merge-interfaces'})
    {
      $openacc = $openacc->textContent;
      use FindBin qw ($Bin);
      'FileHandle'->new (">tmp.F90")->print ($doc->textContent);
      $openacc =~ s/^\!\$ACDC\s+//o;
      my @openacc = split (m/\s+/o, $openacc);

      my @cmd = ("$Bin/fxtran-gen", @openacc, 'tmp.F90');

      system (@cmd)
        and die ("Command `@cmd' failed");

      my $doc_acc = &Fxtran::parse (location => 'tmp_openacc.F90', fopts => ['-construct-tag', '-no-include', '-line-length' => 500]);
      $_->unbindNode () for (&F ('.//a-stmt', $doc_acc));
      $text_acc = $doc_acc->textContent ();
      $text_acc =~ s/^\s*\n$//goms;
      unlink ($_) for (qw (tmp.F90 tmp_openacc.F90 tmp.F90.xml tmp_openacc.F90.xml));
    }

  # Strip empty lines
  
  my $text = $doc->textContent ();
  
  $text =~ s/^\s*\n$//goms;
  
  my $sub = &basename ($F90, qw (.F90));
  
  &Fxtran::Util::updateFile ("$dir/$sub$ext", << "EOF");
INTERFACE
$text
$text_acc
END INTERFACE
EOF

  return "$dir/$sub$ext";
}

sub modi
{
  my ($F90, $dir) = @_;
  
  $dir ||= '.';

  my $doc = &Fxtran::parse (location => $F90, fopts => ['-construct-tag', '-no-include', '-line-length' => 500]);
  
  &intfbBody ($doc);

  # Strip empty lines
  
  my $text = $doc->textContent ();
  
  $text =~ s/^\s*\n$//goms;
  
  my $sub = &basename ($F90, qw (.F90));
  my $SUB = uc ($sub);
  
  &Fxtran::Util::updateFile ("$dir/modi_$sub.F90", << "EOF");
MODULE MODI_$SUB
INTERFACE
$text
END INTERFACE
END MODULE
EOF

  return "$dir/modi_$sub.F90";
}



1;

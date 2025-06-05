package Fxtran::ManyBlocks;

use Data::Dumper;

use strict;

use Fxtran::Common;
use Fxtran::Loop;
use Fxtran::Stack;
use Fxtran::Call;
use Fxtran::Subroutine;
use Fxtran::Pragma;
use Fxtran::Finder;
use Fxtran::Style;
use Fxtran::Decl;
use Fxtran::Inline;
use Fxtran;

sub processSingleRoutine
{
  my ($pu, $find, %opts) = @_;

  my $KGPBLKS = 'KGPBLKS';

  for my $in (@{ $opts{inlined} })
    {
      my $f90in = $find->resolve (file => $in);
      my $di = &Fxtran::parse (location => $f90in, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)], dir => $opts{tmp});
      &Fxtran::Canonic::makeCanonic ($di, %opts);
      &Fxtran::Inline::inlineExternalSubroutine ($pu, $di, %opts);
    }
      
  my $style = $opts{style};
  my $pragma = $opts{pragma};

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my @nproma = $style->nproma ();
  
  my $jlon = $style->jlon ();
  my $kidia = $style->kidia ();
  my $kfdia = $style->kfdia ();
  
  # Arrays dimensioned with KLON and their dimensions

  my $var2dim = &Fxtran::Loop::getVarToDim ($pu, style => $style);
  
  {
  # Derived types, assume they are present on the device
  my @type = &F ('./T-decl-stmt[./_T-spec_/derived-T-spec]/EN-decl-LT/EN-decl/EN-N', $dp, 1);
  my %type = map { ($_, 1) } @type;
  
  my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
  my %arg = map { ($_, 1) } @arg;
  
  my @present = grep { $var2dim->{$_} || $type{$_} } @arg;
  my @create = grep { ! $arg{$_} } sort (keys (%$var2dim));
  
  # Create local arrays, assume argument arrays are on the device
  $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@create);
  }
  
  # Parallel sections

  my @par = &F ('.//parallel-section', $pu);
  
  for my $par (@par)
    {

      # Make the section single column

      &Fxtran::Loop::removeNpromaLoopsInSection
      (
        $par, 
        style => $style, 
        var2dim => $var2dim,
      );

      # Get JLON indexed expressions and add JBLK as last index

      for my $sslt (&F ('.//array-R/section-subscript-LT[./section-subscript[string(.)="?"]]', $jlon, $par))
        {
          $sslt->appendChild ($_) for (&t (", "), &n ('<section-subscript><lower-bound>' . &e ('JBLK') . '</lower-bound></section-subscript>'));
        }

      # Move section contents into a DO loop over KLON

      my ($do_jlon) = &fxtran::parse (fragment => << "EOF");
DO $jlon = $kidia, $kfdia
ENDDO
EOF
  
      for my $x ($par->childNodes ())
        {
          $do_jlon->insertBefore ($x, $do_jlon->lastChild);
        }
  
      $par->replaceNode ($do_jlon);    
  
      # Use a stack

      if (&Fxtran::Stack::addStackInSection ($do_jlon))
        {
          &Fxtran::Stack::iniStackManyBlocks ($do_jlon, stack84 => 1, JBLKMIN => 1, KGPBLKS => $KGPBLKS);
        }

      # Replace KIDIA/KFDIA by JLON in call statements

      for my $call (&F ('.//call-stmt', $do_jlon))
        {
          for my $var ($kidia,, $kfdia)
            {
              for my $expr (&F ('.//named-E[string(.)="?"]', $var, $call))
                {
                  $expr->replaceNode (&e ($jlon));
                }
            }
        }

      # Add single column suffix to routines called in this section

      &Fxtran::Call::addSuffix 
      (
        $pu,
        section => $do_jlon,
        suffix => $opts{'suffix-singlecolumn'},
        'merge-interfaces' => $opts{'merge-interfaces'},
      );
  
      # Move loop over NPROMA into a loop over the blocks
  
      my ($do_jblk) = &fxtran::parse (fragment => << "EOF");
DO JBLK = 1, $KGPBLKS
ENDDO
EOF

      my $do_jlon1 = $do_jlon->cloneNode (); $do_jlon1->appendChild ($_) for ($do_jlon->childNodes ());

      $do_jblk->insertBefore ($_, $do_jblk->lastChild) 
        for ($do_jlon1, &t ("\n"));

      $do_jlon->replaceNode ($do_jblk); $do_jlon = $do_jlon1;

      # Find private variables

      my %priv;
      for my $expr (&F ('.//named-E', $do_jlon))
        {
          my ($n) = &F ('./N', $expr, 1);
          next if ($var2dim->{$n});
          my $p = $expr->parentNode;
          $priv{$n}++ if (($p->nodeName eq 'E-1') || ($p->nodeName eq 'do-V'));
        }

      # Add OpenACC directive  

      $pragma->insertLoopVector ($do_jlon, PRIVATE => [sort (keys (%priv))]);

      $pragma->insertParallelLoopGang ($do_jblk, PRIVATE => ['JBLK']);

    }
  
  # Add single block suffix to routines not called from within parallel sections

  &Fxtran::Call::addSuffix 
  (
    $pu,
    suffix => $opts{'suffix-manyblocks'},
    'merge-interfaces' => 1,
    match => sub { my $proc = shift; ! ($proc =~ m/$opts{'suffix-singlecolumn'}$/i) },
  );

  # Add KGPLKS argument to manyblock routines
  
  for my $call (&F ('.//call-stmt[contains(string(procedure-designator),"?")]', $opts{'suffix-manyblocks'}, $ep))
    {
      for my $nproma (@nproma)
        {
          next unless (my ($arg) = &F ('./arg-spec/arg[string(.)="?"]', $nproma, $call));
          $arg->parentNode->insertAfter ($_, $arg) for (&n ('<arg>' . &e ($KGPBLKS) . '</arg>'), &t (", "));
          last;
        }
    }
  
  # Add KGPBLKS dummy argument

  for my $nproma (@nproma)
    {
      next unless (my ($arg) = &F ('./subroutine-stmt/dummy-arg-LT/arg-N[string(.)="?"]', $nproma, $pu));
      $arg->parentNode->insertAfter ($_, $arg) for (&n ("<arg-N><N><n>$KGPBLKS</n></N></arg-N>"), &t (", "));      

      my ($decl_nproma) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $nproma, $dp);

      my $decl_kgpblks = $decl_nproma->cloneNode (1);

      my ($n) = &F ('./EN-decl-LT/EN-decl/EN-N/N/n/text()', $decl_kgpblks);

      $n->setData ($KGPBLKS);

      $dp->insertAfter ($_, $decl_nproma) for ($decl_kgpblks, &t ("\n"));

      last;
    }


  # Stack definition & declaration
  
  my ($implicit) = &F ('.//implicit-none-stmt', $pu);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);
 

  &Fxtran::Decl::declare ($pu, 'TYPE (STACK) :: YLSTACK');
  &Fxtran::Decl::use ($pu, 'USE STACK_MOD');

  # Add extra dimensions to all nproma arrays + make all array spec implicit


  for my $stmt (&F ('./T-decl-stmt', $dp))
    {
      next unless (my ($sslt) = &F ('./EN-decl-LT/EN-decl/array-spec/shape-spec-LT', $stmt));

      my @ss = &F ('./shape-spec', $sslt);

      for my $nproma (@nproma)
        {
          goto NPROMA if ($ss[0]->textContent eq $nproma);
        }

      next;

NPROMA:

     if (&F ('./attribute[string(attribute-N)="INTENT"]', $stmt))
       {
         # Dummy argument : use implicit shape

         my $iss = &n ('<shape-spec>:</shape-spec>');

         for my $ss (@ss)
           {
             next unless (my ($ub) = &F ('./upper-bound', $ss));  # Only for dimensions with upper-bound : (N1:N2) or (N)
             if (my ($lb) = &F ('./lower-bound', $ss))
               {
                 $ub->unbindNode ();                              # (N1:N2) -> (N1:)
               }
             else
               {
                 $ub->replaceNode ($iss->cloneNode (1));          # (N) -> (:)
               }
           }

         $sslt->appendChild ($_) for (&t (", "), $iss);
       }   
     else
       {
         # Local variable : add KGPBLKS dimension
         $sslt->appendChild ($_) for (&t (", "), &n ('<shape-spec>' . &e ($KGPBLKS) . '</shape-spec>'));
       }
    

    }

  &Fxtran::Decl::declare ($pu, 'INTEGER :: JBLK');

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-manyblocks'});

}

1;

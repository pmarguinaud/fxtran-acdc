package Fxtran::IO;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use Data::Dumper;
use File::Path;
use File::Spec;
use Cwd;
use Fxtran::Common;
use Fxtran;

sub callSubroutineMethod
{
  my ($opts, $object, $methodName, $methodNameLong, $isFieldAPI, @args) = @_;

  if ($isFieldAPI)
    {
      if ($methodName =~ m/^(?:LOAD|SAVE)$/io)
        {
          return "CALL $methodNameLong (" . join (', ', @args, $object) . ")";
        }
      else
        {
          return "CALL $methodNameLong (" . join (', ', $object, @args) . ")";
        }
    }
  else
    {
      if ($opts->{'type-bound-methods'})
        {
          return "CALL $object%$opts->{'method-prefix'}$methodName (" . join (', ', @args) . ")";
        }
      else
        {
          return "CALL $opts->{'method-prefix'}$methodNameLong (" . join (', ', $object, @args) . ")";
        }
    }
}

sub callFunctionMethod
{
  my ($opts, $object, $methodName, $methodNameLong, $isFieldAPI, @args) = @_;

  if ($isFieldAPI)
    {
      return "$methodNameLong (" . join (', ', @args, $object) . ")";
    }
  else
    {
      if ($opts->{'type-bound-methods'})
        {
          return "$object%$opts->{'method-prefix'}$methodName (" . join (', ', @args) . ")";
        }
      else
        {
          return "$opts->{'method-prefix'}$methodNameLong (" . join (', ', $object, @args) . ")";
        }
    }
}

sub processDecl
{
  my ($opts, $en_decl, $sname, $prefix, 
      $BODY_SAVE, $BODY_LOAD, $BODY_COPY, $BODY_WIPE, $BODY_SIZE, $BODY_HOST, $BODY_LEGACY, $BODY_CRC64,
      $U, $J, $L, $B, $T, $en_decl_hash) = @_;

  my (@BODY_SAVE, @BODY_LOAD, @BODY_COPY, @BODY_WIPE, @BODY_SIZE, @BODY_HOST, @BODY_LEGACY, @BODY_CRC64);
  my (%U, %J, %L, %B, %T);

  my $stmt = &Fxtran::stmt ($en_decl);

  return if ($stmt->nodeName eq 'final-stmt');

  my %attr = map { ($_, 1) } &F ('.//attribute/attribute-N/text()', $stmt);

  return if ($attr{PARAMETER});

  my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
  
  my $skip = $opts->{'skip-components'}->($sname, $name, \%attr, $en_decl_hash);

  if (my $fac = $opts->{'field-api-class'})
    {
      $fac = 'Fxtran::IO::' . $fac;
      eval "use $fac";
      if (my $c = $@)
        {
          die $c;
        }
      my $fam = $fac->getFieldAPIMember ($sname, $name, \%attr, $en_decl_hash);
      $skip = 1 if ($fam);
    }

  if ($skip)
    {
      if ($attr{POINTER})
        {
          push @BODY_LOAD, "NULLIFY ($prefix$name)";
#         push @BODY_COPY, "!\$acc serial present (SELF)",
#                          "NULLIFY ($prefix$name)",
#                          "!\$acc end serial",
#                          "\n";
                           
        }
      goto RETURN;
    }
  
  my ($tspec) = &F ('./_T-spec_', $stmt);

  die $stmt->toString unless ($tspec);

  (my $ttspec = uc ($tspec->textContent)) =~ s/\s+//go;

  return if ($ttspec =~ m/^PROCEDURE/o);

  my $tmp_jprb = $ttspec eq 'REAL(KIND=JPRB)';


  my ($intrinsic) = &F ('./intrinsic-T-spec', $tspec);
  my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec);
  
  $tname && ($U{$tname} = 1);
  
  my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl, 1);

  unless (@ss)
    {
      my $stmt = &Fxtran::stmt ($en_decl);
      @ss = &F ('.//attribute/array-spec/shape-spec-LT/shape-spec', $stmt, 1);
    }
  
  my $isFieldAPI      = $ttspec =~ m/^CLASS\(FIELD_\w+\)$/o;
  my $isFieldAPI_VIEW = $ttspec =~ m/^TYPE\(FIELD_\w+_VIEW\)$/o;
  my $isFieldAPI_PTR  = $ttspec =~ m/^TYPE\(FIELD_\w+_PTR\)$/o;

  my $hasCRC64  = (! $isFieldAPI_VIEW) && (! $isFieldAPI_PTR);
  my $hasLEGACY = (! $isFieldAPI_VIEW) && (! $isFieldAPI_PTR);
  my $hasWIPE   = (! $isFieldAPI_VIEW) && (! $isFieldAPI_PTR);
  my $hasCOPY   = (! $isFieldAPI_VIEW) && (! $isFieldAPI_PTR);
  my $hasSIZE   = (! $isFieldAPI_VIEW) && (! $isFieldAPI_PTR) && (! $isFieldAPI);

  if ($attr{POINTER} || $attr{ALLOCATABLE})
    {
      my $func = $attr{POINTER} ? 'ASSOCIATED' : 'ALLOCATED';
      push @BODY_SAVE       , "L$name = $func ($prefix$name)\n";
      push @BODY_COPY       , "L$name = $func ($prefix$name)\n";
      push @BODY_WIPE       , "L$name = $func ($prefix$name)\n";
      push @BODY_SIZE       , "L$name = $func ($prefix$name)\n" if (! $isFieldAPI);
      push @BODY_HOST       , "L$name = $func ($prefix$name)\n" unless ($intrinsic);
      push @BODY_LEGACY     , "L$name = $func ($prefix$name)\n" unless ($intrinsic);
      push @BODY_CRC64      , "L$name = $func ($prefix$name)\n" unless ($intrinsic);
      push @BODY_SAVE       , "WRITE (KLUN) L$name\n";
      push @BODY_LOAD       , "READ (KLUN) L$name\n";
      $L{$name} = 1;
      push @BODY_SAVE       , "IF (L$name) THEN\n";
      push @BODY_LOAD       , "IF (L$name) THEN\n";
      push @BODY_COPY       , $isFieldAPI ? "IF (L$name .AND. LLFIELDAPI) THEN\n" : "IF (L$name) THEN\n";
      push @BODY_WIPE       , $isFieldAPI ? "IF (L$name .AND. LLFIELDAPI) THEN\n" : "IF (L$name) THEN\n";
      push @BODY_SIZE       , "IF (L$name) THEN\n" if (! $isFieldAPI);
      push @BODY_HOST       , "IF (L$name) THEN\n" unless ($intrinsic);
      push @BODY_LEGACY     , "IF (L$name) THEN\n" unless ($intrinsic);
      push @BODY_CRC64      , "IF (L$name) THEN\n" unless ($intrinsic);
      if (@ss)
        {
          push @BODY_SAVE, "WRITE (KLUN) LBOUND ($prefix$name)\n";
          push @BODY_SAVE, "WRITE (KLUN) UBOUND ($prefix$name)\n";
          $B{scalar (@ss)} = 1;
          my $r = scalar (@ss);
          push @BODY_LOAD, "READ (KLUN) IL" . scalar (@ss) . "\n";
          push @BODY_LOAD, "READ (KLUN) IU" . scalar (@ss) . "\n";
          push @BODY_LOAD, "ALLOCATE ($prefix$name (" . join (', ', map { "IL$r($_):IU$r($_)" } (1 .. $#ss+1) ) . "))\n";
        }
      else
        {
          push @BODY_LOAD, "ALLOCATE ($prefix$name)\n" unless ($tname && grep { $_ eq $tname } @{ $opts->{'no-allocate'} });
        }

      if (! $isFieldAPI)
        {
          push @BODY_COPY, $opts->{pragma}->enterDataCreate ("$prefix$name") . "\n", 
                           $opts->{pragma}->updateDevice ("$prefix$name") . "\n";
          push @BODY_WIPE, $opts->{pragma}->exitDataDetach ("$prefix$name") . "\n";
        }
    }
  

  if ($intrinsic)
    {
      my ($tn) = &F ('./T-N', $intrinsic, 1);
      if ($tmp_jprb)
        {
          my $n = scalar (@ss);
          $T{$n} = 1;
          if (@ss)
            {
              push @BODY_LOAD, "ALLOCATE (ZTMP$n(" . join (', ', map { my $i = $_; "LBOUND($prefix$name,$i):UBOUND($prefix$name,$i)" } (1 .. $n)) . "))\n";
            }
          push @BODY_SAVE, "WRITE (KLUN) $prefix$name\n";
          push @BODY_LOAD, "READ (KLUN) ZTMP$n\n";
          push @BODY_LOAD, "$prefix$name = ZTMP$n\n";
          if (@ss)
            {
              push @BODY_LOAD, "DEALLOCATE (ZTMP$n)\n";
            }
        }
      else
        {
          push @BODY_SAVE, "WRITE (KLUN) $prefix$name\n";
          push @BODY_LOAD, "READ (KLUN) $prefix$name\n";
        }
      my $size = "ISIZE = KIND ($prefix$name)"; 
      $size .= " * SIZE ($prefix$name)" if (@ss);
      $size .= " * LEN ($prefix$name)" if ($tn eq 'CHARACTER');

      push @BODY_SIZE, $size . "\n", 
                       "IF (LLPRINT) THEN\n", 
                       "WRITE (*, '(I10,\" \")', ADVANCE='NO') ISIZE\n", 
                       "WRITE (*, *) TRIM (CLPATH)//'%$name'\n", 
                       "ENDIF\n", 
                       "KSIZE = KSIZE + ISIZE\n";

      push @BODY_CRC64, "WRITE (KLUN, '(Z16.16,\" \",A)') FCRC64 ($prefix$name), CDPATH//'%$name'\n";
    }
  else 
    {
      push @BODY_SIZE, "JSIZE = 0\n" if (! $isFieldAPI);
      for (my $i = $#ss+1; $i >= 1; $i--)
        {
          $J{"J$i"} = 1;
          my $do = "DO J$i = LBOUND ($prefix$name, $i), UBOUND ($prefix$name, $i)\n";
          push @BODY_SAVE       , $do;
          push @BODY_LOAD       , $do;
          push @BODY_COPY       , $do;
          push @BODY_WIPE       , $do;
          push @BODY_SIZE       , $do if (! $isFieldAPI);
          push @BODY_HOST       , $do;
          push @BODY_LEGACY     , $do;
          push @BODY_CRC64      , $do;
        }
      my @J = map { "J$_"  } (1 .. $#ss+1);
      my $J = @ss ? " (" . join (', ', @J) . ")" : '';

      if (@J)
        {
          push @BODY_CRC64, "WRITE (CLIND, '(\"(\"," . join (',",",', map { 'I0' } @J) . ",\")\")')" .  " " . join (', ', @J) . "\n";
        }
      else
        {
          push @BODY_CRC64, "CLIND = ''\n";
        }

      my $LLPRINT = '.FALSE.';

      my $indent = '  ' x scalar (@ss);

      push @BODY_SAVE  , $indent . &callSubroutineMethod ($opts, "$prefix$name$J", 'SAVE',   "SAVE_$tname",   $isFieldAPI || $isFieldAPI_VIEW || $isFieldAPI_PTR, 'KLUN');
      push @BODY_LOAD  , $indent . &callSubroutineMethod ($opts, "$prefix$name$J", 'LOAD',   "LOAD_$tname",   $isFieldAPI || $isFieldAPI_VIEW || $isFieldAPI_PTR, 'KLUN');
      push @BODY_HOST  , $indent . &callSubroutineMethod ($opts, "$prefix$name$J", 'HOST',   "HOST_$tname",   $isFieldAPI || $isFieldAPI_VIEW || $isFieldAPI_PTR);
      push @BODY_LEGACY, $indent . &callSubroutineMethod ($opts, "$prefix$name$J", 'LEGACY', "LEGACY_$tname", $isFieldAPI || $isFieldAPI_VIEW || $isFieldAPI_PTR, 'KADDRL', 'KADDRU', 'KDIR=KDIR');

      if ($isFieldAPI)
        {
          push @BODY_CRC64, $indent . "WRITE (KLUN, '(Z16.16,\" \",A)') $prefix$name$J%CRC64 (), CDPATH//'%$name'//TRIM(CLIND)\n";
        }
      else
        {
          push @BODY_CRC64, $indent . &callSubroutineMethod ($opts, "$prefix$name$J", 'CRC64', "CRC64_$tname", $isFieldAPI, 'KLUN', "CDPATH//'%$name'//TRIM(CLIND)");
        }

      if ($isFieldAPI || $isFieldAPI_VIEW)
        {
          push @BODY_COPY, $indent . "CALL COPY_$tname ($prefix$name" . $J . ")\n";
          push @BODY_WIPE, $indent . "CALL WIPE_$tname ($prefix$name" . $J . ")\n";
        }
      else
        {
          push @BODY_COPY, $indent . &callSubroutineMethod ($opts, "$prefix$name$J", 'COPY', "COPY_$tname", $isFieldAPI, 'LDCREATED=.TRUE.', 'LDFIELDAPI=LDFIELDAPI');
          push @BODY_WIPE, $indent . &callSubroutineMethod ($opts, "$prefix$name$J", 'WIPE', "WIPE_$tname", $isFieldAPI, 'LDDELETED=.TRUE.', 'LDFIELDAPI=LDFIELDAPI');
        }

      if (! $isFieldAPI)
        {
          push @BODY_SIZE, $indent
                       . "ISIZE = " . &callFunctionMethod ($opts, "$prefix$name$J", 'SIZE', "SIZE_$tname", $isFieldAPI, "CLPATH//'%$name'", $LLPRINT) . "\n", 
                         "JSIZE = JSIZE + ISIZE\n",
                         "KSIZE = KSIZE + ISIZE\n";
        }
      for (my $i = $#ss; $i >= 0; $i--)
        {
          push @BODY_SAVE       , "ENDDO\n";
          push @BODY_LOAD       , "ENDDO\n";
          push @BODY_COPY       , "ENDDO\n";
          push @BODY_HOST       , "ENDDO\n";
          push @BODY_LEGACY     , "ENDDO\n";
          push @BODY_CRC64      , "ENDDO\n";
          push @BODY_WIPE       , "ENDDO\n";
          push @BODY_SIZE       , "ENDDO\n" if (! $isFieldAPI);
        }

      if (! $isFieldAPI)
        {
          push @BODY_SIZE, 
                         "IF (LLPRINT) THEN\n", 
                         "WRITE (*, '(I10,\" \")', ADVANCE='NO') JSIZE\n",
                         "WRITE (*, *) TRIM (CLPATH)//'%$name'\n",
                         "ENDIF\n", 
        }
    }
  
  if ($attr{POINTER} || $attr{ALLOCATABLE})
    {
      push @BODY_SAVE, "ENDIF\n";
      if ($attr{POINTER})
        {
          push @BODY_LOAD, "ELSE\n", "NULLIFY ($prefix$name)\n";
        }
      push @BODY_LOAD, "ENDIF\n";
      push @BODY_COPY, $opts->{pragma}->enterDataAttach ("$prefix$name") . "\n",
                       "ENDIF\n";
      if (! $isFieldAPI)
        {
          push @BODY_WIPE, $opts->{pragma}->exitDataDetach ("$prefix$name") . "\n";
        }
      push @BODY_HOST       , "ENDIF\n" unless ($intrinsic);
      push @BODY_LEGACY     , "ENDIF\n" unless ($intrinsic);
      push @BODY_CRC64      , "ENDIF\n" unless ($intrinsic);
      push @BODY_WIPE       , "ENDIF\n";
      push @BODY_SIZE       , "ENDIF\n" if (! $isFieldAPI);
    }
  push @BODY_COPY       , "\n";
  push @BODY_HOST       , "\n";
  push @BODY_LEGACY     , "\n";
  push @BODY_CRC64      , "\n";
  push @BODY_WIPE       , "\n";

RETURN:

  push @$BODY_SAVE       , @BODY_SAVE;
  push @$BODY_LOAD       , @BODY_LOAD;
  push @$BODY_COPY       , @BODY_COPY   if ($hasCOPY);
  push @$BODY_HOST       , @BODY_HOST;
  push @$BODY_LEGACY     , @BODY_LEGACY if ($hasLEGACY);
  push @$BODY_CRC64      , @BODY_CRC64  if ($hasCRC64);
  push @$BODY_WIPE       , @BODY_WIPE   if ($hasWIPE);
  push @$BODY_SIZE       , @BODY_SIZE   if ($hasSIZE);

  %$U = (%$U, %U); %$J = (%$J, %J); 
  %$L = (%$L, %L); %$B = (%$B, %B); 
  %$T = (%$T, %T); 

}

sub indent
{
  my $n = 0;

  my @line;
  
  for my $line (@_)
    {
      chomp ($line);
      $line =~ s/^\s*//o;
      $n-- if ($line =~ m/^\s*(?:ELSE|ENDIF|ENDDO)\b/o);
      $line = ('  ' x $n) . $line;
      $n++ if ($line =~ m/^\s*(?:ELSE|IF|DO)\b/o);

      if (@line)
        {
          next if (($line =~ m/^\s*$/o) && ($line[-1] =~ m/^\s*$/o));
        }

      push @line, $line;
    }

  return @line;
}

sub r
{
  my $f = shift;
  return '' unless (-f $f);
  return do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
}

sub w
{
  my $f = shift;
  my $t = &r ($f);
  return if ($t eq $_[0]);
  'FileHandle'->new (">$f")->print ($_[0]);
}

sub processTypes1
{
  my ($doc, $opts) = @_;

  my (%code, @file, @meth);

  my ($mod) = &F ('.//module-stmt/module-N/N/n/text()', $doc);
  
  my @tconst = &F ('.//T-construct', $doc);

  for my $tconst (@tconst)
    {
      my ($name) = &F ('.//T-stmt/T-N/N/n/text()', $tconst, 1);
      my $tname = $name;
  
      next if ($opts->{'skip-types'}->($name));

      my ($abstract) = &F ('./T-stmt/attribute[string(attribute-N)="ABSTRACT"]', $tconst);
      my ($extends) = &F ('./T-stmt/attribute[string(attribute-N)="EXTENDS"]/N/n/text()', $tconst);

      my $GENERIC_SAVE   = '';
      my $GENERIC_LOAD   = '';
      my $GENERIC_COPY   = '';
      my $GENERIC_HOST   = '';
      my $GENERIC_LEGACY = '';
      my $GENERIC_CRC64  = '';
      my $GENERIC_WIPE   = '';
      my $GENERIC_SIZE   = '';
  
  
      $GENERIC_SAVE   .= "MODULE PROCEDURE $opts->{'method-prefix'}SAVE_$name\n";
      $GENERIC_LOAD   .= "MODULE PROCEDURE $opts->{'method-prefix'}LOAD_$name\n";
      $GENERIC_COPY   .= "MODULE PROCEDURE $opts->{'method-prefix'}COPY_$name\n";
      $GENERIC_HOST   .= "MODULE PROCEDURE $opts->{'method-prefix'}HOST_$name\n";
      $GENERIC_LEGACY .= "MODULE PROCEDURE $opts->{'method-prefix'}LEGACY_$name\n";
      $GENERIC_CRC64  .= "MODULE PROCEDURE $opts->{'method-prefix'}CRC64_$name\n";
      $GENERIC_WIPE   .= "MODULE PROCEDURE $opts->{'method-prefix'}WIPE_$name\n";
      $GENERIC_SIZE   .= "MODULE PROCEDURE $opts->{'method-prefix'}SIZE_$name\n";
  
      my (@BODY_SAVE, @BODY_LOAD, @BODY_COPY, @BODY_WIPE, @BODY_SIZE, @BODY_HOST, @BODY_LEGACY, @BODY_CRC64);

      push @BODY_WIPE,  
                       "LLFIELDAPI = .FALSE.\n",
                       "IF (PRESENT (LDFIELDAPI)) THEN\n",
                       "LLFIELDAPI = LDFIELDAPI\n",
                       "ENDIF\n";
      push @BODY_COPY,  
                       "LLFIELDAPI = .FALSE.\n",
                       "IF (PRESENT (LDFIELDAPI)) THEN\n",
                       "LLFIELDAPI = LDFIELDAPI\n",
                       "ENDIF\n";
      push @BODY_COPY,  
                       "LLCREATED = .FALSE.\n",
                       "IF (PRESENT (LDCREATED)) THEN\n",
                       "LLCREATED = LDCREATED\n",
                       "ENDIF\n",
                       "IF (.NOT. LLCREATED) THEN\n",
                       $opts->{pragma}->enterDataCreate ('SELF') . "\n",
                       $opts->{pragma}->updateDevice ('SELF') . "\n",
                       "ENDIF\n";
      push @BODY_SIZE, "LLPRINT = .FALSE.\n",
                       "IF (PRESENT (LDPRINT)) THEN\n",
                       "LLPRINT = LDPRINT\n",
                       "ENDIF\n",
                       "CLPATH=''\n",
                       "IF (PRESENT (CDPATH)) THEN\n",
                       "CLPATH = CDPATH\n",
                       "ENDIF\n",
                       "KSIZE = 0\n";

      if ($extends)
        {
          for (\@BODY_SAVE, \@BODY_LOAD, \@BODY_COPY, \@BODY_WIPE, \@BODY_SIZE, \@BODY_HOST, \@BODY_LEGACY, \@BODY_CRC64)
            {
              push @$_, "YLSUPER => SELF\n";
            }
          push @BODY_SAVE       , &callSubroutineMethod ($opts, 'YLSUPER', 'SAVE',   "SAVE_$extends",   0, 'KLUN');
          push @BODY_LOAD       , &callSubroutineMethod ($opts, 'YLSUPER', 'LOAD',   "LOAD_$extends",   0, 'KLUN');
          push @BODY_COPY       , &callSubroutineMethod ($opts, 'YLSUPER', 'COPY',   "COPY_$extends",   0, 'LDCREATED=.TRUE.', 'LDFIELDAPI=LDFIELDAPI');
          push @BODY_HOST       , &callSubroutineMethod ($opts, 'YLSUPER', 'HOST',   "HOST_$extends",   0);
          push @BODY_LEGACY     , &callSubroutineMethod ($opts, 'YLSUPER', 'LEGACY', "LEGACY_$extends", 0, 'KADDRL', 'KADDRU', 'KDIR=KDIR');
          push @BODY_CRC64      , &callSubroutineMethod ($opts, 'YLSUPER', 'CRC64',  "CRC64_$extends",  0, 'KLUN', 'CDPATH');
          push @BODY_WIPE       , &callSubroutineMethod ($opts, 'YLSUPER', 'WIPE',   "WIPE_$extends",   0, 'LDDELETED=.TRUE.', 'LDFIELDAPI=LDFIELDAPI');
          push @BODY_SIZE       , "KSIZE = KSIZE + " . &callFunctionMethod ($opts, 'YLSUPER', 'SIZE', "SIZE_$extends", 0, 'CLPATH', 'LLPRINT');
        }
    
      my (%U, %J, %L, %B, %T);

      my @en_decl = &F ('.//EN-decl', $tconst);
      my %en_decl;
      for my $en_decl (@en_decl)
        {
          my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
          $en_decl{$name} = $en_decl;
        }
      for my $en_decl (@en_decl)
        {
          &processDecl ($opts, $en_decl, "$tname%", 'SELF%', 
                        \@BODY_SAVE, \@BODY_LOAD, \@BODY_COPY, \@BODY_WIPE, \@BODY_SIZE, \@BODY_HOST, \@BODY_LEGACY, \@BODY_CRC64,
                        \%U, \%J, \%L, \%B, \%T, \%en_decl);

        }

      push @BODY_WIPE, "LLDELETED = .FALSE.\n",
                       "IF (PRESENT (LDDELETED)) THEN\n", 
                       "LLDELETED = LDDELETED\n",
                       "ENDIF",
                       "IF (.NOT. LLDELETED) THEN\n",
                       $opts->{pragma}->exitDataDelete ('SELF') . "\n",
                       "ENDIF\n";
                       
  
      my $DECL_SAVE        = '';
      my $DECL_LOAD        = '';
      my $DECL_COPY        = "LOGICAL :: LLCREATED\n";      $DECL_COPY .= "LOGICAL :: LLFIELDAPI\n";
      my $DECL_HOST        = '';
      my $DECL_LEGACY      = '';
      my $DECL_CRC64       = "CHARACTER(LEN=128) :: CLIND\n";
      my $DECL_WIPE        = "LOGICAL :: LLDELETED\n";      $DECL_WIPE .= "LOGICAL :: LLFIELDAPI\n";
      my $DECL_SIZE        = "INTEGER*8 :: ISIZE, JSIZE\n"; $DECL_SIZE .= "LOGICAL :: LLPRINT\nCHARACTER(LEN=128) :: CLPATH\n";


      if ($extends)
        {
          for ($DECL_SAVE, $DECL_LOAD, $DECL_COPY, $DECL_WIPE, $DECL_SIZE, $DECL_HOST, $DECL_LEGACY, $DECL_CRC64)
            {
              $_ .= "CLASS ($extends), POINTER :: YLSUPER\n";
            }
        }
  
      if (%J)
        {
          $DECL_SAVE        .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_LOAD        .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_COPY        .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_HOST        .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_LEGACY      .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_CRC64       .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_WIPE        .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_SIZE        .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
        }
      if (%B)
        {
          $DECL_LOAD        .= "INTEGER :: " . join (', ', map  { ("IL$_($_)", "IU$_($_)") } sort keys (%B)) . "\n";
        }
      if (%L)
        {
          my @L = sort keys (%L);
          while (my @l = splice (@L, 0, 10))
            {
              $DECL_SAVE        .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_LOAD        .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_HOST        .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_LEGACY      .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_CRC64       .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_COPY        .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_WIPE        .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_SIZE        .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
            }
        }
      if (%T)
        {
          if (exists $T{'0'})
            {
              $DECL_LOAD .= "REAL(KIND=JPRD) :: ZTMP0\n";
            }
          for my $i (grep { $_ } sort keys (%T))
            {
              $DECL_LOAD .= "REAL(KIND=JPRD), ALLOCATABLE :: ZTMP$i (" . join (',', (':') x $i) . ")\n";
            }
        }
  
      my @U;

      if ($opts->{'type-bound-methods'})
        {
          for my $T (sort keys (%U))
            {
              if (my $mod = $opts->{'module-map'}{"UTIL_${T}_MOD"})
                {
                  push @U, $mod;
                }
            }
        }
      else
        {
          @U = map { "UTIL_${_}_MOD" } sort keys (%U);
          @U = map { (exists ($opts->{'module-map'}{$_}) ? $opts->{'module-map'}{$_} : $_) } @U;
        }
      %U = map { ($_, 1) } @U;
      @U = sort keys (%U);

      my $USE_SAVE        = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
      my $USE_LOAD        = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
      my $USE_HOST        = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
      my $USE_LEGACY      = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
      my $USE_CRC64       = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
      my $USE_COPY        = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
      my $USE_WIPE        = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
      my $USE_SIZE        = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
  
      if (%T)
        {
          $USE_LOAD .= "USE PARKIND1, ONLY : JPRD\n";
        }

      if ($extends)
        {
          $USE_SAVE   .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}SAVE_$extends\n"    unless ($opts->{'type-bound-methods'});
          $USE_LOAD   .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}LOAD_$extends\n"    unless ($opts->{'type-bound-methods'});
          $USE_HOST   .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}HOST_$extends\n"    unless ($opts->{'type-bound-methods'});
          $USE_LEGACY .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}LEGACY_$extends\n"  unless ($opts->{'type-bound-methods'});
          $USE_CRC64  .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}CRC64_$extends\n"   unless ($opts->{'type-bound-methods'});
          $USE_COPY   .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}COPY_$extends\n"    unless ($opts->{'type-bound-methods'});
          $USE_WIPE   .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}WIPE_$extends\n"    unless ($opts->{'type-bound-methods'});
          $USE_SIZE   .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}SIZE_$extends\n"    unless ($opts->{'type-bound-methods'});
        }

      $USE_CRC64 .= "USE CRC64_INTRINSIC, ONLY : FCRC64 => CRC64\n";

      for ($USE_SAVE, $USE_SAVE, $USE_COPY, $USE_WIPE, $USE_SIZE, $DECL_SAVE, $DECL_LOAD, $DECL_HOST, $DECL_LEGACY, $DECL_CRC64)
        {
          chomp ($_);
        }
  
      my $type = 'TYPE';
      $type = 'CLASS' if ($abstract);
      $type = 'CLASS' if ($opts->{'type-bound-methods'});

      my $HEAD_SAVE = << "EOF";
SUBROUTINE $opts->{'method-prefix'}SAVE_$name (SELF, KLUN)
$USE_SAVE
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
EOF

      my $HEAD_LOAD = << "EOF";
SUBROUTINE $opts->{'method-prefix'}LOAD_$name (SELF, KLUN)
$USE_LOAD
IMPLICIT NONE
$type ($name), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
EOF

      my $HEAD_HOST = << "EOF";
SUBROUTINE $opts->{'method-prefix'}HOST_$name (SELF)
$USE_HOST
IMPLICIT NONE
$type ($name), TARGET :: SELF
EOF

      my $HEAD_LEGACY = << "EOF";
SUBROUTINE $opts->{'method-prefix'}LEGACY_$name (SELF, KADDRL, KADDRU, KDIR)
$USE_LEGACY
IMPLICIT NONE
$type ($name), TARGET :: SELF
INTEGER*8, INTENT (IN) :: KADDRL
INTEGER*8, INTENT (IN) :: KADDRU
INTEGER, INTENT (IN) :: KDIR
EOF

      my $HEAD_CRC64 = << "EOF";
SUBROUTINE $opts->{'method-prefix'}CRC64_$name (SELF, KLUN, CDPATH)
$USE_CRC64
IMPLICIT NONE
$type ($name), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
EOF

      my $HEAD_COPY = << "EOF";
SUBROUTINE $opts->{'method-prefix'}COPY_$name (SELF, LDCREATED, LDFIELDAPI)
$USE_COPY
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
EOF

      my $HEAD_WIPE = << "EOF";
SUBROUTINE $opts->{'method-prefix'}WIPE_$name (SELF, LDDELETED, LDFIELDAPI)
$USE_WIPE
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
EOF

      my $HEAD_SIZE = << "EOF";
FUNCTION $opts->{'method-prefix'}SIZE_$name (SELF, CDPATH, LDPRINT) RESULT (KSIZE)
$USE_SIZE
IMPLICIT NONE
$type ($name),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
EOF


      @BODY_SAVE   = &indent (@BODY_SAVE);
      @BODY_LOAD   = &indent (@BODY_LOAD);
      @BODY_HOST   = &indent (@BODY_HOST);
      @BODY_LEGACY = &indent (@BODY_LEGACY);
      @BODY_CRC64  = &indent (@BODY_CRC64);
      @BODY_COPY   = &indent (@BODY_COPY);
      @BODY_WIPE   = &indent (@BODY_WIPE);
      @BODY_SIZE   = &indent (@BODY_SIZE);


      my $IMPL_SAVE   = $HEAD_SAVE   . $DECL_SAVE   . "\n" . join ("\n", @BODY_SAVE  , '') . "END SUBROUTINE\n";
      my $IMPL_LOAD   = $HEAD_LOAD   . $DECL_LOAD   . "\n" . join ("\n", @BODY_LOAD  , '') . "END SUBROUTINE\n";
      my $IMPL_HOST   = $HEAD_HOST   . $DECL_HOST   . "\n" . join ("\n", @BODY_HOST  , '') . "END SUBROUTINE\n";
      my $IMPL_LEGACY = $HEAD_LEGACY . $DECL_LEGACY . "\n" . join ("\n", @BODY_LEGACY, '') . "END SUBROUTINE\n";
      my $IMPL_CRC64  = $HEAD_CRC64  . $DECL_CRC64  . "\n" . join ("\n", @BODY_CRC64 , '') . "END SUBROUTINE\n";
      my $IMPL_COPY   = $HEAD_COPY   . $DECL_COPY   . "\n" . join ("\n", @BODY_COPY  , '') . "END SUBROUTINE\n";
      my $IMPL_WIPE   = $HEAD_WIPE   . $DECL_WIPE   . "\n" . join ("\n", @BODY_WIPE  , '') . "END SUBROUTINE\n";
      my $IMPL_SIZE   = $HEAD_SIZE   . $DECL_SIZE   . "\n" . join ("\n", @BODY_SIZE  , '') . "END FUNCTION\n";

      $HEAD_SAVE   .= "END SUBROUTINE\n";
      $HEAD_LOAD   .= "END SUBROUTINE\n";
      $HEAD_HOST   .= "END SUBROUTINE\n";
      $HEAD_LEGACY .= "END SUBROUTINE\n";
      $HEAD_CRC64  .= "END SUBROUTINE\n";
      $HEAD_COPY   .= "END SUBROUTINE\n";
      $HEAD_WIPE   .= "END SUBROUTINE\n";
      $HEAD_SIZE   .= "END FUNCTION\n";

      for ($IMPL_SAVE, $IMPL_SAVE, $IMPL_COPY, $IMPL_WIPE, $IMPL_SIZE, $IMPL_HOST, $IMPL_LEGACY, $IMPL_CRC64,
           $GENERIC_SAVE, $GENERIC_LOAD, $GENERIC_COPY, $GENERIC_WIPE, $GENERIC_SIZE, $GENERIC_HOST, $GENERIC_LEGACY, $GENERIC_CRC64)
        {
          chomp ($_);
        }
  
      my $n = lc ($name);

      $IMPL_SAVE        = '' unless ($opts->{save});
      $IMPL_LOAD        = '' unless ($opts->{load});
      $IMPL_COPY        = '' unless ($opts->{copy});
      $IMPL_HOST        = '' unless ($opts->{host});
      $IMPL_LEGACY      = '' unless ($opts->{legacy});
      $IMPL_CRC64       = '' unless ($opts->{crc64});
      $IMPL_WIPE        = '' unless ($opts->{wipe});
      $IMPL_SIZE        = '' unless ($opts->{size});

      $GENERIC_SAVE        = "INTERFACE $opts->{'method-prefix'}SAVE\n$GENERIC_SAVE\nEND INTERFACE\n";
      $GENERIC_LOAD        = "INTERFACE $opts->{'method-prefix'}LOAD\n$GENERIC_LOAD\nEND INTERFACE\n";
      $GENERIC_COPY        = "INTERFACE $opts->{'method-prefix'}COPY\n$GENERIC_COPY\nEND INTERFACE\n";
      $GENERIC_HOST        = "INTERFACE $opts->{'method-prefix'}HOST\n$GENERIC_HOST\nEND INTERFACE\n";
      $GENERIC_LEGACY      = "INTERFACE $opts->{'method-prefix'}LEGACY\n$GENERIC_LEGACY\nEND INTERFACE\n";
      $GENERIC_CRC64       = "INTERFACE $opts->{'method-prefix'}CRC64\n$GENERIC_CRC64\nEND INTERFACE\n";
      $GENERIC_WIPE        = "INTERFACE $opts->{'method-prefix'}WIPE\n$GENERIC_WIPE\nEND INTERFACE\n";
      $GENERIC_SIZE        = "INTERFACE $opts->{'method-prefix'}SIZE\n$GENERIC_SIZE\nEND INTERFACE\n";

      if ($abstract || $opts->{'type-bound-methods'})
        {
          $GENERIC_SAVE        = "";
          $GENERIC_LOAD        = "";
          $GENERIC_COPY        = "";
          $GENERIC_HOST        = "";
          $GENERIC_LEGACY      = "";
          $GENERIC_CRC64       = "";
          $GENERIC_WIPE        = "";
          $GENERIC_SIZE        = "";
        }
  
      $GENERIC_SAVE        = '' unless ($opts->{save});
      $GENERIC_LOAD        = '' unless ($opts->{load});
      $GENERIC_COPY        = '' unless ($opts->{copy});
      $GENERIC_HOST        = '' unless ($opts->{host});
      $GENERIC_LEGACY      = '' unless ($opts->{legacy});
      $GENERIC_CRC64       = '' unless ($opts->{crc64});
      $GENERIC_WIPE        = '' unless ($opts->{wipe});
      $GENERIC_SIZE        = '' unless ($opts->{size});

      if ($opts->{'type-bound-methods'})
        {
          push @meth,
            { 
              name => $n,
              tconst => $tconst,
              methods =>
              {
                ($opts->{save}   ? (save   => {head => $HEAD_SAVE  , impl => $IMPL_SAVE  }) : ()),
                ($opts->{load}   ? (load   => {head => $HEAD_LOAD  , impl => $IMPL_LOAD  }) : ()),
                ($opts->{copy}   ? (copy   => {head => $HEAD_COPY  , impl => $IMPL_COPY  }) : ()),
                ($opts->{host}   ? (host   => {head => $HEAD_HOST  , impl => $IMPL_HOST  }) : ()),
                ($opts->{legacy} ? (legacy => {head => $HEAD_LEGACY, impl => $IMPL_LEGACY}) : ()),
                ($opts->{crc64}  ? (crc64  => {head => $HEAD_CRC64 , impl => $IMPL_CRC64 }) : ()),
                ($opts->{wipe}   ? (wipe   => {head => $HEAD_WIPE  , impl => $IMPL_WIPE  }) : ()),
                ($opts->{size}   ? (size   => {head => $HEAD_SIZE  , impl => $IMPL_SIZE  }) : ()),
              },
            };
        }
      else
        {
          push @file, "util_${n}_mod.F90";

          $code{"util_${n}_mod.F90"} = << "EOF";
MODULE UTIL_${name}_MOD

USE $mod, ONLY : $name

$GENERIC_SAVE
$GENERIC_LOAD
$GENERIC_COPY
$GENERIC_HOST
$GENERIC_LEGACY     
$GENERIC_CRC64
$GENERIC_WIPE
$GENERIC_SIZE

CONTAINS

$IMPL_SAVE

$IMPL_LOAD

$IMPL_COPY

$IMPL_HOST

$IMPL_LEGACY     

$IMPL_CRC64

$IMPL_WIPE

$IMPL_SIZE

END MODULE
EOF
        }

    
      if (my $dir = $opts->{'types-constant-dir'})
        {
          'FileHandle'->new (">$dir/$name.pl");
        }
    }

  return (\@file, \%code, \@meth);
}

sub processTypes
{
  my ($doc, $opts) = @_;

  my ($file, $code, $type) = &processTypes1 ($doc, $opts);

  if ($opts->{'type-bound-methods'})
    {
      my ($MOD) = &F ('.//module-N', $doc, 1); my $mod = lc ($MOD);

      my $count = 1;

      my $interface = "INTERFACE\n\n";

      for my $type (@$type)
        {
          my $methods = $type->{methods};
          my $tconst = $type->{tconst};

          my ($end) = &F ('./end-T-stmt', $tconst);

          unless (&F ('./contains-stmt', $tconst))
            {
              $tconst->insertBefore ($_, $end) for (&n ("<contains-stmt>CONTAINS</contains-stmt>"), &t ("\n"));
            }
 
          for my $methodName (sort keys (%$methods))
            {
              my $method = $methods->{$methodName};

              my $sub;
 
             
              my $sub = $opts->{'numbered-submodules'}
                       ? $mod . '_' . $count . '_smod'
                       : $mod . '_' . $type->{name} . '_' . $methodName . '_smod'; 


              $methodName = $opts->{'method-prefix'} . $methodName;

              my $SUB = uc ($sub);

              my $file = "$sub.F90";
     
              $file = sprintf ('%4.4d.', $count) . $file if ($opts->{sorted});

              $file = "$opts->{dir}/$file";
     
              &w ($file, << "EOF");
SUBMODULE ($MOD) $SUB

IMPLICIT NONE

CONTAINS

MODULE $method->{impl}

END SUBMODULE
EOF

              $interface .= "MODULE $method->{head}\n";

              $tconst->insertBefore (&t ('PROCEDURE :: ' . uc ($methodName) . ' => ' . uc ($methodName) . '_' . uc ($type->{name}) . "\n"), $end);

              $count++;
            }
     
        }

      my ($pu) = &F ('./object/file/program-unit', $doc);
      my ($stmt) = &F ('./contains-stmt', $pu);

      unless ($stmt)
        {
          ($stmt) = &F ('./end-module-stmt', $pu);
        }

      $interface .= "\nEND INTERFACE\n\n";

      $pu->insertBefore (&t ($interface), $stmt);


      if ($opts->{sorted})
        {
          &w ("$opts->{dir}/0000.$mod.F90", $doc->textContent);
        }
      else
        {
          &w ("$opts->{dir}/$mod.F90", $doc->textContent);
        }

    }
  elsif ($opts->{out})
    {
      &w ("$opts->{dir}/$opts->{out}", join ('', map { $code->{$_} } @$file));
    }
  elsif ($opts->{sorted})
    {
      my $i = 0;
      for my $f (@$file)
        {
          &w ("$opts->{dir}/" . sprintf ('%4.4d.', $i++) . $f, $code->{$f});
        }
    }
  else
    {
      for my $f (@$file)
        {
          &w ("$opts->{dir}/$f", $code->{$f});
        }
    }
}

sub process_module
{
  use File::Temp;

  my ($doc, $opts) = @_;

  my ($pu) = &F ('./object/file/program-unit', $doc);
  my ($mod) = &F ('./module-stmt/module-N', $pu, 1);


  my @decl = &F ('./T-decl-stmt', $pu);

  $pu->insertBefore (&t ("TYPE TYPE_MODULE\n"), $decl[0]);
  $pu->insertAfter (&t ("\nEND TYPE"), $decl[-1]);


  my $fh = 'File::Temp'->new (UNLINK => 1, SUFFIX => '.F90', DIR => '/tmp');
  $fh->print ($doc->textContent);
  $fh->close ();

  my $F90 = $fh->filename;

  $doc = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -line-length 800)]);

  my ($file, $code) = &processTypes1 ($doc, $opts);

  for ($code)
    {
      s/\bYD%//goms;
      s/_TYPE_MODULE/_$mod/goms;
      s/, YD//goms;
      s/TYPE \(TYPE_MODULE\), INTENT \(.*?\), TARGET :: YD//goms;
      s/INTERFACE.*?END INTERFACE//goms;
      s/, ONLY : TYPE_MODULE//goms;
    }

  if ($opts->{out})
    {
      &w ("$opts->{dir}/$opts->{out}", join ('', map { $code->{$_} } @$file));
    }
  else
    {
      for my $f (@$file)
        {
          &w ("$opts->{dir}/$f", $code->{$f});
        }
    }

}



1;

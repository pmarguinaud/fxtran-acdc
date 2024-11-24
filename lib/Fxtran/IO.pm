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
use Common;
use Fxtran;

sub process_decl
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
#         push @BODY_COPY, "!\$acc serial present (YD)",
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
          push @BODY_COPY, "!\$acc enter data create ($prefix$name)\n",
                           "!\$acc update device ($prefix$name)\n";
          push @BODY_WIPE, "!\$acc exit data detach ($prefix$name)\n";
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

      push @BODY_CRC64, "WRITE (KLUN, '(Z16.16,\" \",A)') CRC64 ($prefix$name), CDPATH//'%$name'\n";
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
      push @BODY_SAVE       , ('  ' x scalar (@ss)) 
                   . "CALL SAVE_$tname (KLUN, $prefix$name" . $J . ")\n";
      push @BODY_LOAD       , ('  ' x scalar (@ss)) 
                   . "CALL LOAD_$tname (KLUN, $prefix$name" . $J . ")\n";
      push @BODY_HOST       , ('  ' x scalar (@ss)) 
                   . "CALL HOST_$tname ($prefix$name" . $J . ")\n";
      push @BODY_LEGACY     , ('  ' x scalar (@ss)) 
                   . "CALL LEGACY_$tname ($prefix$name" . $J . ", KADDRL, KADDRU, KDIR=KDIR)\n";

      if ($isFieldAPI)
        {
          push @BODY_CRC64      , ('  ' x scalar (@ss)) 
                       . "WRITE (KLUN, '(Z16.16,\" \",A)') $prefix$name$J%CRC64 (), CDPATH//'%$name'//TRIM(CLIND)\n";
        }
      else
        {
          push @BODY_CRC64      , ('  ' x scalar (@ss)) 
                       . "CALL CRC64_$tname ($prefix$name" . $J . ", KLUN, CDPATH//'%$name'//TRIM(CLIND))\n";
        }

      if ($isFieldAPI || $isFieldAPI_VIEW)
        {
          push @BODY_COPY, ('  ' x scalar (@ss)) 
                       . "CALL COPY_$tname ($prefix$name" . $J . ")\n";
          push @BODY_WIPE, ('  ' x scalar (@ss)) 
                       . "CALL WIPE_$tname ($prefix$name" . $J . ")\n";
        }
      else
        {
          push @BODY_COPY, ('  ' x scalar (@ss)) 
                       . "CALL COPY_$tname ($prefix$name" . $J . ", LDCREATED=.TRUE., LDFIELDAPI=LDFIELDAPI)\n";
          push @BODY_WIPE, ('  ' x scalar (@ss)) 
                       . "CALL WIPE_$tname ($prefix$name" . $J . ", LDDELETED=.TRUE., LDFIELDAPI=LDFIELDAPI)\n";
        }

      if (! $isFieldAPI)
        {
          push @BODY_SIZE, ('  ' x scalar (@ss))
                       . "ISIZE = SIZE_$tname ($prefix$name" . $J . ", CLPATH//'%$name', $LLPRINT)\n", 
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
      push @BODY_COPY, "!\$acc enter data attach ($prefix$name)\n",
                       "ENDIF\n";
      if (! $isFieldAPI)
        {
          push @BODY_WIPE, "!\$acc exit data delete ($prefix$name)\n";
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
  for (@_)
    {
      chomp;
      s/^\s*//o;
      $n-- if (m/^\s*(?:ELSE|ENDIF|ENDDO)\b/o);
      $_ = ('  ' x $n) . $_;
      $n++ if (m/^\s*(?:ELSE|IF|DO)\b/o);
    }
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

  my %code;
  my @file;

  my ($mod) = &F ('.//module-stmt/module-N/N/n/text()', $doc);
  
  my @tconst = &F ('.//T-construct', $doc);

  for my $tconst (@tconst)
    {
      my ($name) = &F ('.//T-stmt/T-N/N/n/text()', $tconst, 1);
      my $tname = $name;
  
      next if ($opts->{'skip-types'}->($name));

      my ($abstract) = &F ('./T-stmt/attribute[string(attribute-N)="ABSTRACT"]', $tconst);
      my ($extends) = &F ('./T-stmt/attribute[string(attribute-N)="EXTENDS"]/N/n/text()', $tconst);

      my ($INTERFACE_SAVE       , $CONTAINS_SAVE       ) = ('', '');
      my ($INTERFACE_LOAD       , $CONTAINS_LOAD       ) = ('', '');
      my ($INTERFACE_COPY       , $CONTAINS_COPY       ) = ('', '');
      my ($INTERFACE_HOST       , $CONTAINS_HOST       ) = ('', '');
      my ($INTERFACE_LEGACY     , $CONTAINS_LEGACY     ) = ('', '');
      my ($INTERFACE_CRC64      , $CONTAINS_CRC64      ) = ('', '');
      my ($INTERFACE_WIPE       , $CONTAINS_WIPE       ) = ('', '');
      my ($INTERFACE_SIZE       , $CONTAINS_SIZE       ) = ('', '');
  
  
      $INTERFACE_SAVE        .= "MODULE PROCEDURE SAVE_$name\n";
      $INTERFACE_LOAD        .= "MODULE PROCEDURE LOAD_$name\n";
      $INTERFACE_COPY        .= "MODULE PROCEDURE COPY_$name\n";
      $INTERFACE_HOST        .= "MODULE PROCEDURE HOST_$name\n";
      $INTERFACE_LEGACY      .= "MODULE PROCEDURE LEGACY_$name\n";
      $INTERFACE_CRC64       .= "MODULE PROCEDURE CRC64_$name\n";
      $INTERFACE_WIPE        .= "MODULE PROCEDURE WIPE_$name\n";
      $INTERFACE_SIZE        .= "MODULE PROCEDURE SIZE_$name\n";
  
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
                       "!\$acc enter data create (YD)\n",
                       "!\$acc update device (YD)\n",
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
              push @$_, "YLSUPER => YD\n";
            }
          push @BODY_SAVE       , "CALL SAVE_$extends (KLUN, YLSUPER)\n";
          push @BODY_LOAD       , "CALL LOAD_$extends (KLUN, YLSUPER)\n";
          push @BODY_COPY       , "CALL COPY_$extends (YLSUPER, LDCREATED=.TRUE., LDFIELDAPI=LDFIELDAPI)\n";
          push @BODY_HOST       , "CALL HOST_$extends (YLSUPER)\n";
          push @BODY_LEGACY     , "CALL LEGACY_$extends (YLSUPER, KADDRL, KADDRU, KDIR=KDIR)\n";
          push @BODY_CRC64      , "CALL CRC64_$extends (YLSUPER, KLUN, CDPATH)\n";
          push @BODY_WIPE       , "CALL WIPE_$extends (YLSUPER, LDDELETED=.TRUE., LDFIELDAPI=LDFIELDAPI)\n";
          push @BODY_SIZE       , "KSIZE = KSIZE + SIZE_$extends (YLSUPER, CLPATH, LLPRINT)\n";
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
          &process_decl ($opts, $en_decl, "$tname%", 'YD%', 
                         \@BODY_SAVE, \@BODY_LOAD, \@BODY_COPY, \@BODY_WIPE, \@BODY_SIZE, \@BODY_HOST, \@BODY_LEGACY, \@BODY_CRC64,
                         \%U, \%J, \%L, \%B, \%T, \%en_decl);

        }

      push @BODY_WIPE, "LLDELETED = .FALSE.\n",
                       "IF (PRESENT (LDDELETED)) THEN\n", 
                       "LLDELETED = LDDELETED\n",
                       "ENDIF",
                       "IF (.NOT. LLDELETED) THEN\n",
                       "!\$acc exit data delete (YD)\n",
                       "ENDIF\n";
                       
  
      my $DECL_SAVE        = '';
      my $DECL_LOAD        = '';
      my $DECL_COPY        = "LOGICAL :: LLCREATED\n";      $DECL_COPY .= "LOGICAL :: LLFIELDAPI\n";
      my $DECL_HOST        = '';
      my $DECL_LEGACY      = '';
      my $DECL_CRC64       = '';
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
  
      my @U = map { "UTIL_${_}_MOD" } sort keys (%U);

      @U = map { (exists ($opts->{'module-map'}{$_}) ? $opts->{'module-map'}{$_} : $_) } @U;
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
          $USE_SAVE        .= "USE UTIL_${extends}_MOD, ONLY : $extends, SAVE_$extends\n";
          $USE_LOAD        .= "USE UTIL_${extends}_MOD, ONLY : $extends, LOAD_$extends\n";
          $USE_HOST        .= "USE UTIL_${extends}_MOD, ONLY : $extends, HOST_$extends\n";
          $USE_LEGACY      .= "USE UTIL_${extends}_MOD, ONLY : $extends, LEGACY_$extends\n";
          $USE_CRC64       .= "USE UTIL_${extends}_MOD, ONLY : $extends, CRC64_$extends\n";
          $USE_COPY        .= "USE UTIL_${extends}_MOD, ONLY : $extends, COPY_$extends\n";
          $USE_WIPE        .= "USE UTIL_${extends}_MOD, ONLY : $extends, WIPE_$extends\n";
          $USE_SIZE        .= "USE UTIL_${extends}_MOD, ONLY : $extends, SIZE_$extends\n";
        }

      $USE_CRC64 .= "USE CRC64_INTRINSIC\n";

      for ($USE_SAVE, $USE_SAVE, $USE_COPY, $USE_WIPE, $USE_SIZE, $DECL_SAVE, $DECL_LOAD, $DECL_HOST, $DECL_LEGACY, $DECL_CRC64)
        {
          chomp ($_);
        }
  
      my $type = $abstract ? 'CLASS' : 'TYPE';

      $CONTAINS_SAVE .= << "EOF";
SUBROUTINE SAVE_$name (KLUN, YD)
$USE_SAVE
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
$type ($name), INTENT (IN), TARGET :: YD
EOF

      $CONTAINS_LOAD .= << "EOF";
SUBROUTINE LOAD_$name (KLUN, YD)
$USE_LOAD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
$type ($name), INTENT (OUT), TARGET :: YD
EOF

      $CONTAINS_HOST .= << "EOF";
SUBROUTINE HOST_$name (YD)
$USE_HOST
IMPLICIT NONE
$type ($name), TARGET :: YD
EOF

      $CONTAINS_LEGACY .= << "EOF";
SUBROUTINE LEGACY_$name (YD, KADDRL, KADDRU, KDIR)
$USE_LEGACY
IMPLICIT NONE
$type ($name), TARGET :: YD
INTEGER*8, INTENT (IN) :: KADDRL
INTEGER*8, INTENT (IN) :: KADDRU
INTEGER, INTENT (IN) :: KDIR
EOF

      $CONTAINS_CRC64 .= << "EOF";
SUBROUTINE CRC64_$name (YD, KLUN, CDPATH)
$USE_CRC64
IMPLICIT NONE
$type ($name), TARGET :: YD
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
CHARACTER(LEN=128) :: CLIND
EOF

      $CONTAINS_COPY .= << "EOF";
SUBROUTINE COPY_$name (YD, LDCREATED, LDFIELDAPI)
$USE_COPY
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
EOF

      $CONTAINS_WIPE .= << "EOF";
SUBROUTINE WIPE_$name (YD, LDDELETED, LDFIELDAPI)
$USE_WIPE
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
EOF

      $CONTAINS_SIZE .= << "EOF";
INTEGER*8 FUNCTION SIZE_$name (YD, CDPATH, LDPRINT) RESULT (KSIZE)
$USE_SIZE
IMPLICIT NONE
$type ($name),     INTENT (IN), TARGET :: YD
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
EOF


      &indent (@BODY_SAVE);
      &indent (@BODY_LOAD);
      &indent (@BODY_HOST);
      &indent (@BODY_LEGACY);
      &indent (@BODY_CRC64);
      &indent (@BODY_COPY);
      &indent (@BODY_WIPE);
      &indent (@BODY_SIZE);


      $CONTAINS_SAVE        .= $DECL_SAVE   . "\n" . join ("\n", @BODY_SAVE  , '') . "END SUBROUTINE\n";
      $CONTAINS_LOAD        .= $DECL_LOAD   . "\n" . join ("\n", @BODY_LOAD  , '') . "END SUBROUTINE\n";
      $CONTAINS_HOST        .= $DECL_HOST   . "\n" . join ("\n", @BODY_HOST  , '') . "END SUBROUTINE\n";
      $CONTAINS_LEGACY      .= $DECL_LEGACY . "\n" . join ("\n", @BODY_LEGACY, '') . "END SUBROUTINE\n";
      $CONTAINS_CRC64       .= $DECL_CRC64  . "\n" . join ("\n", @BODY_CRC64 , '') . "END SUBROUTINE\n";
      $CONTAINS_COPY        .= $DECL_COPY   . "\n" . join ("\n", @BODY_COPY  , '') . "END SUBROUTINE\n";
      $CONTAINS_WIPE        .= $DECL_WIPE   . "\n" . join ("\n", @BODY_WIPE  , '') . "END SUBROUTINE\n";
      $CONTAINS_SIZE        .= $DECL_SIZE   . "\n" . join ("\n", @BODY_SIZE  , '') . "END FUNCTION\n";

      for ($CONTAINS_SAVE, $CONTAINS_SAVE, $CONTAINS_COPY, $CONTAINS_WIPE, $CONTAINS_SIZE, $CONTAINS_HOST, $CONTAINS_LEGACY, $CONTAINS_CRC64,
           $INTERFACE_SAVE, $INTERFACE_LOAD, $INTERFACE_COPY, $INTERFACE_WIPE, $INTERFACE_SIZE, $INTERFACE_HOST, $INTERFACE_LEGACY, $INTERFACE_CRC64)
        {
          chomp ($_);
        }
  
      my $n = lc ($name);

      $CONTAINS_SAVE        = '' unless ($opts->{save});
      $CONTAINS_LOAD        = '' unless ($opts->{load});
      $CONTAINS_COPY        = '' unless ($opts->{copy});
      $CONTAINS_HOST        = '' unless ($opts->{host});
      $CONTAINS_LEGACY      = '' unless ($opts->{legacy});
      $CONTAINS_CRC64       = '' unless ($opts->{crc64});
      $CONTAINS_WIPE        = '' unless ($opts->{wipe});
      $CONTAINS_SIZE        = '' unless ($opts->{size});

      $INTERFACE_SAVE        = "INTERFACE SAVE\n$INTERFACE_SAVE\nEND INTERFACE\n";
      $INTERFACE_LOAD        = "INTERFACE LOAD\n$INTERFACE_LOAD\nEND INTERFACE\n";
      $INTERFACE_COPY        = "INTERFACE COPY\n$INTERFACE_COPY\nEND INTERFACE\n";
      $INTERFACE_HOST        = "INTERFACE HOST\n$INTERFACE_HOST\nEND INTERFACE\n";
      $INTERFACE_LEGACY      = "INTERFACE LEGACY\n$INTERFACE_LEGACY\nEND INTERFACE\n";
      $INTERFACE_CRC64       = "INTERFACE CRC64\n$INTERFACE_CRC64\nEND INTERFACE\n";
      $INTERFACE_WIPE        = "INTERFACE WIPE\n$INTERFACE_WIPE\nEND INTERFACE\n";
      $INTERFACE_SIZE        = "INTERFACE SIZE\n$INTERFACE_SIZE\nEND INTERFACE\n";

      if ($abstract)
        {
          $INTERFACE_SAVE        = "";
          $INTERFACE_LOAD        = "";
          $INTERFACE_COPY        = "";
          $INTERFACE_HOST        = "";
          $INTERFACE_LEGACY      = "";
          $INTERFACE_CRC64       = "";
          $INTERFACE_WIPE        = "";
          $INTERFACE_SIZE        = "";
        }
  
      $INTERFACE_SAVE        = '' unless ($opts->{save});
      $INTERFACE_LOAD        = '' unless ($opts->{load});
      $INTERFACE_COPY        = '' unless ($opts->{copy});
      $INTERFACE_HOST        = '' unless ($opts->{host});
      $INTERFACE_LEGACY      = '' unless ($opts->{legacy});
      $INTERFACE_CRC64       = '' unless ($opts->{crc64});
      $INTERFACE_WIPE        = '' unless ($opts->{wipe});
      $INTERFACE_SIZE        = '' unless ($opts->{size});

      push @file, "util_${n}_mod.F90";

      $code{"util_${n}_mod.F90"} = << "EOF";
MODULE UTIL_${name}_MOD

USE $mod, ONLY : $name

$INTERFACE_SAVE
$INTERFACE_LOAD
$INTERFACE_COPY
$INTERFACE_HOST
$INTERFACE_LEGACY     
$INTERFACE_CRC64
$INTERFACE_WIPE
$INTERFACE_SIZE

CONTAINS

$CONTAINS_SAVE

$CONTAINS_LOAD

$CONTAINS_COPY

$CONTAINS_HOST

$CONTAINS_LEGACY     

$CONTAINS_CRC64

$CONTAINS_WIPE

$CONTAINS_SIZE

END MODULE
EOF

    
      if (my $dir = $opts->{'types-constant-dir'})
        {
          'FileHandle'->new (">$dir/$name.pl");
        }
    }

  return (\@file, \%code);
}

sub processTypes
{
  my ($doc, $opts) = @_;

  my ($file, $code) = &processTypes1 ($doc, $opts);

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

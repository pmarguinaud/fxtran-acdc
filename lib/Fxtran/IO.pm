package Fxtran::IO;

=head1 NAME

Fxtran::IO

=head1 DESCRIPTION

This module provides functions whose purpose is to generate code 
for handling FORTRAN derived types, in particular:

=over 4

=item SAVE

Writing to a FORTRAN logical unit.

=item LOAD

Reading from a FORTRAN logical unit.

=item COPY

Copying to a device using OpenACC or OpenMP directives.

=item WIPE

Destroying a structure on the device.

=item CRC64

Computing checksums on device members.

=item HOST

Copy back Field API data to the host.

=item DEVICE

Push Field API data to the device.

=item LEGACY

Copy Field API data to host arrays.

=back

=cut

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
  my ($opts, $en_decl, $sname, $prefix, $BODY, $U, $J, $L, $B, $T, $en_decl_hash) = @_;

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
          push @BODY_WIPE, $opts->{pragma}->exitDataDelete ("$prefix$name") . "\n";
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

  push @{ $BODY->{save}  }, @BODY_SAVE;
  push @{ $BODY->{load}  }, @BODY_LOAD;
  push @{ $BODY->{copy}  }, @BODY_COPY   if ($hasCOPY);
  push @{ $BODY->{host}  }, @BODY_HOST;
  push @{ $BODY->{legacy}}, @BODY_LEGACY if ($hasLEGACY);
  push @{ $BODY->{crc64} }, @BODY_CRC64  if ($hasCRC64);
  push @{ $BODY->{wipe}  }, @BODY_WIPE   if ($hasWIPE);
  push @{ $BODY->{size}  }, @BODY_SIZE   if ($hasSIZE);

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

sub w
{
  my ($f, $from, $opts) = @_;
  if ($opts->{'write-metadata'})
    {
      &Fxtran::Util::updateFile ($f, $_[3], from => $from, time => 1, version => 1);
    }
  else
    {
      &Fxtran::Util::updateFile ($f, $_[3]);
    }
}

sub processTypes1
{
  my ($doc, $opts) = @_;

  my (%code, @file, @meth);

  my ($mod) = &F ('.//module-stmt/module-N/N/n/text()', $doc);
  
  my @tconst = &F ('.//T-construct', $doc);

  my @method = qw (save load copy host legacy crc64 wipe size);

  for my $tconst (@tconst)
    {
      my ($name) = &F ('.//T-stmt/T-N/N/n/text()', $tconst, 1);
      my $tname = $name;
  
      next if ($opts->{'skip-types'}->($name));

      my ($abstract) = &F ('./T-stmt/attribute[string(attribute-N)="ABSTRACT"]', $tconst);
      my ($extends) = &F ('./T-stmt/attribute[string(attribute-N)="EXTENDS"]/N/n/text()', $tconst);


      my %GENERIC;

      $GENERIC{$_} = '' for (@method);

      for my $method (@method)
        {
          $GENERIC{$method} .= "MODULE PROCEDURE $opts->{'method-prefix'}\U${method}_$name\n";
        }

      my %BODY = map { ($_, []) } @method;

      push @{ $BODY{wipe} },  
                       "LLFIELDAPI = .FALSE.\n",
                       "IF (PRESENT (LDFIELDAPI)) THEN\n",
                       "LLFIELDAPI = LDFIELDAPI\n",
                       "ENDIF\n";
      push @{ $BODY{copy} },  
                       "LLFIELDAPI = .FALSE.\n",
                       "IF (PRESENT (LDFIELDAPI)) THEN\n",
                       "LLFIELDAPI = LDFIELDAPI\n",
                       "ENDIF\n",
                       "LLCREATED = .FALSE.\n",
                       "IF (PRESENT (LDCREATED)) THEN\n",
                       "LLCREATED = LDCREATED\n",
                       "ENDIF\n",
                       "IF (.NOT. LLCREATED) THEN\n",
                       $opts->{pragma}->enterDataCreate ('SELF') . "\n",
                       $opts->{pragma}->updateDevice ('SELF') . "\n",
                       "ENDIF\n";
      push @{ $BODY{size} }, 
                       "LLPRINT = .FALSE.\n",
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
          for my $body (values (%BODY))
            {
              push @$body, "YLSUPER => SELF\n";
            }
          push @{ $BODY{save}   }, &callSubroutineMethod ($opts, 'YLSUPER', 'SAVE',   "SAVE_$extends",   0, 'KLUN');
          push @{ $BODY{load}   }, &callSubroutineMethod ($opts, 'YLSUPER', 'LOAD',   "LOAD_$extends",   0, 'KLUN');
          push @{ $BODY{copy}   }, &callSubroutineMethod ($opts, 'YLSUPER', 'COPY',   "COPY_$extends",   0, 'LDCREATED=.TRUE.', 'LDFIELDAPI=LDFIELDAPI');
          push @{ $BODY{host}   }, &callSubroutineMethod ($opts, 'YLSUPER', 'HOST',   "HOST_$extends",   0);
          push @{ $BODY{legacy} }, &callSubroutineMethod ($opts, 'YLSUPER', 'LEGACY', "LEGACY_$extends", 0, 'KADDRL', 'KADDRU', 'KDIR=KDIR');
          push @{ $BODY{crc64}  }, &callSubroutineMethod ($opts, 'YLSUPER', 'CRC64',  "CRC64_$extends",  0, 'KLUN', 'CDPATH');
          push @{ $BODY{wipe}   }, &callSubroutineMethod ($opts, 'YLSUPER', 'WIPE',   "WIPE_$extends",   0, 'LDDELETED=.TRUE.', 'LDFIELDAPI=LDFIELDAPI');
          push @{ $BODY{size}   }, "KSIZE = KSIZE + " . &callFunctionMethod ($opts, 'YLSUPER', 'SIZE', "SIZE_$extends", 0, 'CLPATH', 'LLPRINT');
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
                        \%BODY, \%U, \%J, \%L, \%B, \%T, \%en_decl);

        }

      push @{ $BODY{wipe} }, 
                       "LLDELETED = .FALSE.\n",
                       "IF (PRESENT (LDDELETED)) THEN\n", 
                       "LLDELETED = LDDELETED\n",
                       "ENDIF",
                       "IF (.NOT. LLDELETED) THEN\n",
                       $opts->{pragma}->exitDataDelete ('SELF') . "\n",
                       "ENDIF\n";
                       
  
      my %DECL =
      (
         save   => '',
         load   => '',
         copy   => "LOGICAL :: LLCREATED\nLOGICAL :: LLFIELDAPI\n",
         host   => '',
         legacy => '',
         crc64  => "CHARACTER(LEN=128) :: CLIND\n",
         wipe   => "LOGICAL :: LLDELETED\nLOGICAL :: LLFIELDAPI\n",
         size   => "INTEGER*8 :: ISIZE, JSIZE\nLOGICAL :: LLPRINT\nCHARACTER(LEN=128) :: CLPATH\n",
      );


      if ($extends)
        {
          for (values (%DECL))
            {
              $_ .= "CLASS ($extends), POINTER :: YLSUPER\n";
            }
        }
  
      if (%J)
        {
          for (values (%DECL))
            {
              $_ .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
            }
        }
      if (%B)
        {
          $DECL{load} .= "INTEGER :: " . join (', ', map  { ("IL$_($_)", "IU$_($_)") } sort keys (%B)) . "\n";
        }
      if (%L)
        {
          my @L = sort keys (%L);
          while (my @l = splice (@L, 0, 10))
            {
              for (values (%DECL))
                {
                  $_ .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
                }
            }
        }
      if (%T)
        {
          if (exists $T{'0'})
            {
              $DECL{load} .= "REAL(KIND=JPRD) :: ZTMP0\n";
            }
          for my $i (grep { $_ } sort keys (%T))
            {
              $DECL{load} .= "REAL(KIND=JPRD), ALLOCATABLE :: ZTMP$i (" . join (',', (':') x $i) . ")\n";
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

          if ($tname =~ m/^FIELD_\d\w\w_ARRAY$/o)  # Field API arrays have to be processed differently; here we translate UTIL_FIELD_1IM_MOD into FIELD_1IM_UTIL_MODULE
            {
              for (@U)
                {
                  if (m/^UTIL_FIELD_(\d\w\w)_MOD$/o)
                    {
                      $_ = "FIELD_${1}_UTIL_MODULE";
                    }
                }
            }
          else
            {
              @U = map { (exists ($opts->{'module-map'}{$_}) ? $opts->{'module-map'}{$_} : $_) } @U;
            }
        }

      %U = map { ($_, 1) } @U;
      @U = sort keys (%U);

      my %USE;

      for my $method (@method)
        {
          $USE{$method} = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);
        }
  
      if (%T)
        {
          $USE{load} .= "USE PARKIND1, ONLY : JPRD\n";
        }

      if ($extends && (! $opts->{'type-bound-methods'}))
        {
          for my $method (@method)
            {
              $USE{$method} .= "USE UTIL_${extends}_MOD, ONLY : $extends, $opts->{'method-prefix'}${method}_$extends\n";
            }
        }

      $USE{crc64} .= "USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64\n";

      for (values (%USE), values (%DECL))
        {
          chomp ($_);
        }
  
      my $type = 'TYPE';
      $type = 'CLASS' if ($abstract);
      $type = 'CLASS' if ($opts->{'type-bound-methods'});

      my %HEAD;

      $HEAD{save} = << "EOF";
SUBROUTINE $opts->{'method-prefix'}SAVE_$name (SELF, KLUN)
$USE{save}
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
EOF

      $HEAD{load} = << "EOF";
SUBROUTINE $opts->{'method-prefix'}LOAD_$name (SELF, KLUN)
$USE{load}
IMPLICIT NONE
$type ($name), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
EOF

      $HEAD{host} = << "EOF";
SUBROUTINE $opts->{'method-prefix'}HOST_$name (SELF)
$USE{host}
IMPLICIT NONE
$type ($name), TARGET :: SELF
EOF

      $HEAD{legacy} = << "EOF";
SUBROUTINE $opts->{'method-prefix'}LEGACY_$name (SELF, KADDRL, KADDRU, KDIR)
$USE{legacy}
IMPLICIT NONE
$type ($name), TARGET :: SELF
INTEGER*8, INTENT (IN) :: KADDRL
INTEGER*8, INTENT (IN) :: KADDRU
INTEGER, INTENT (IN) :: KDIR
EOF

      $HEAD{crc64} = << "EOF";
SUBROUTINE $opts->{'method-prefix'}CRC64_$name (SELF, KLUN, CDPATH)
$USE{crc64}
IMPLICIT NONE
$type ($name), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
EOF

      $HEAD{copy} = << "EOF";
SUBROUTINE $opts->{'method-prefix'}COPY_$name (SELF, LDCREATED, LDFIELDAPI)
$USE{copy}
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
EOF

      $HEAD{wipe} = << "EOF";
SUBROUTINE $opts->{'method-prefix'}WIPE_$name (SELF, LDDELETED, LDFIELDAPI)
$USE{wipe}
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
EOF

      $HEAD{size} = << "EOF";
FUNCTION $opts->{'method-prefix'}SIZE_$name (SELF, CDPATH, LDPRINT) RESULT (KSIZE)
$USE{size}
IMPLICIT NONE
$type ($name),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
EOF


      for (values (%BODY))
        {
          @$_ = &indent (@$_);
        }

      my %IMPL;

      for my $method (@method)
        {
          $IMPL{$method} = $HEAD{$method} . $DECL{$method} . "\n" . join ("\n", @{ $BODY{$method} }, '');
          if ($IMPL{$method} =~ m/^SUBROUTINE/o)
            {
              $IMPL{$method} .= "END SUBROUTINE\n";
              $HEAD{$method} .= "END SUBROUTINE\n";
            }
          elsif ($IMPL{$method} =~ m/^FUNCTION/o)
            {
              $IMPL{$method} .= "END FUNCTION\n";
              $HEAD{$method} .= "END FUNCTION\n";
            }
        }

      for (values (%IMPL), values (%GENERIC))
        {
          chomp ($_);
        }
  
      my $n = lc ($name);

      for my $method (@method)
        {
          $IMPL{$method} = '' unless ($opts->{$method});
        }

      for my $method (@method)
        {
          $GENERIC{$method} = "INTERFACE $opts->{'method-prefix'}\U$method\n$GENERIC{$method}\nEND INTERFACE\n";
        }

      if ($abstract || $opts->{'type-bound-methods'})
        {
          for my $method (@method)
            {
              $GENERIC{$method} = '';
            }
        }
  
      for my $method (@method)
        {
          $GENERIC{$method} = '' unless ($opts->{$method});
        }

      if ($opts->{'type-bound-methods'})
        {
          push @meth,
            { 
              name => $n,
              tconst => $tconst,
              methods =>
              {
                map 
                {
                  my $method = $_;
                  ($opts->{$method} ? ($method => {head => $HEAD{$method}  , impl => $IMPL{$method}  }) : ())
                }
                @method
              },
            };
        }
      elsif ($opts->{'split-util'})
        {

          $code{"util_${n}_mod.F90"} = << "EOF";
MODULE UTIL_\U${name}_MOD

USE $mod, ONLY : $name

EOF

          for my $method (@method)
            {
              $code{"util_${n}_mod.F90"} .= "USE UTIL_${name}_${method}_MOD\n"   if ($IMPL{$method});
            }

          $code{"util_${n}_mod.F90"} .= "END MODULE\n";
    
          for my $method (@method)
            {
              $code{"util_${n}_${method}_mod.F90"} = << "EOF" if ($IMPL{$method});
MODULE UTIL_\U${name}_${method}_MOD
USE $mod, ONLY : $name
$GENERIC{$method}
CONTAINS
$IMPL{$method}
END MODULE
EOF
            }

          push @file, sort (keys (%code));
        }
      else
        {
          push @file, "util_${n}_mod.F90";

          $code{"util_${n}_mod.F90"} = << "EOF";
MODULE UTIL_\U${name}_MOD

USE $mod, ONLY : $name

EOF

          for my $method (@method)
            {
              $code{"util_${n}_mod.F90"} .= $GENERIC{$method} . "\n";
            }

          $code{"util_${n}_mod.F90"} .= "CONTAINS\n\n";

          for my $method (@method)
            {
              $code{"util_${n}_mod.F90"} .= $IMPL{$method} . "\n\n";
            }

          $code{"util_${n}_mod.F90"} .= "END MODULE\n";
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

  my ($F90) = &F ('./object/file/@name', $doc, 2);

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

              my $sub = $opts->{'numbered-submodules'}
                       ? $mod . '_' . $count . '_smod'
                       : $mod . '_' . $type->{name} . '_' . $methodName . '_smod'; 


              $methodName = $opts->{'method-prefix'} . $methodName;

              my $SUB = uc ($sub);

              my $file = "$sub.F90";
     
              $file = sprintf ('%4.4d.', $count) . $file if ($opts->{sorted});

              $file = "$opts->{dir}/$file";
     
              &w ($file, $F90, $opts, << "EOF");
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
          &w ("$opts->{dir}/0000.$mod.F90", $F90, $opts, $doc->textContent);
        }
      else
        {
          &w ("$opts->{dir}/$mod.F90", $F90, $opts, $doc->textContent);
        }

    }
  elsif ($opts->{out})
    {
      &w ("$opts->{dir}/$opts->{out}", $F90, $opts, join ('', map { $code->{$_} } @$file));
    }
  elsif ($opts->{sorted})
    {
      my $i = 0;
      for my $f (@$file)
        {
          &w ("$opts->{dir}/" . sprintf ('%4.4d.', $i++) . $f, $F90, $opts, $code->{$f});
        }
    }
  else
    {
      for my $f (@$file)
        {
          &w ("$opts->{dir}/$f", $F90, $opts, $code->{$f});
        }
    }
}

sub process_module
{
  use File::Temp;

  my ($doc, $opts) = @_;

  my ($F90) = &F ('./object/file/@name', $doc, 2);

  my ($pu) = &F ('./object/file/program-unit', $doc);
  my ($mod) = &F ('./module-stmt/module-N', $pu, 1);


  my @decl = &F ('./T-decl-stmt', $pu);

  $pu->insertBefore (&t ("TYPE TYPE_MODULE\n"), $decl[0]);
  $pu->insertAfter (&t ("\nEND TYPE"), $decl[-1]);


  my $fh = 'File::Temp'->new (UNLINK => 1, SUFFIX => '.F90', DIR => '/tmp');
  $fh->print ($doc->textContent);
  $fh->close ();

  $F90 = $fh->filename;

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
      &w ("$opts->{dir}/$opts->{out}", $F90, $opts, join ('', map { $code->{$_} } @$file));
    }
  else
    {
      for my $f (@$file)
        {
          &w ("$opts->{dir}/$f", $F90, $opts, $code->{$f});
        }
    }

}


=head1 COPYRIGHT

Meteo-France 2022

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=cut

1;



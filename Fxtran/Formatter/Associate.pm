package Fxtran::Formatter::Associate;

use base qw (Fxtran::Formatter);

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{
  shift;
  my ($stmt, $indent) = @_;

  ($stmt) = &parse (fragment => $stmt->textContent . "\nEND ASSOCIATE\n", fopts => [qw (-line-length 10000 -canonic)]);

  my @t = &F ('./associate-LT/associate', $stmt);

  @t = grep { $_->nodeName ne '#text' } @t;
  
  my %n2t;

  for my $t (@t)
    {
      my ($n) = &F ('./associate-N', $t, 1);
      $n2t{$n} = $t;
    }
  
  @t = @n2t{sort keys (%n2t)};

  $stmt = "ASSOCIATE ( &\n$indent  " .  join ("\n$indent, ", map { $_->textContent . " & " } @t) . "\n$indent)";

  ($stmt) = &parse (fragment => $stmt . "\nEND ASSOCIATE\n", fopts => [qw (-line-length 10000)]);

  return $stmt;
}

sub repack
{
  my $class = shift;
  my ($stmt, $indent) = @_;
  my @associate = &F ('./associate-LT/associate', $stmt, 1);
  $class->repackCallLikeStatement ("ASSOCIATE (", @associate, ")", $indent);
}

sub reparse
{
  shift;
  my $stmt = shift;
  ($stmt) = &parse (fragment => << "EOF", fopts => [qw (-line-length 10000)]);
$stmt
END ASSOCIATE
EOF
  return $stmt;
}

sub canonic
{
  shift;
  my $stmt = shift;
  $stmt = $stmt->textContent if (ref ($stmt));
  ($stmt) = &parse (fragment => << "EOF", fopts => [qw (-line-length 10000 -canonic)]);
$stmt
END ASSOCIATE
EOF
  return $stmt;
}

1;

package Style;

use strict;

sub new
{
  my $class = shift;
  my %args = @_;
  if ($args{style})
    {
      my $class = "Style::$args{style}";
      eval "use $class";
      $@ && die ($@);
      return $class->new ();
    }
  return bless \%args, $class;
}

sub removeUnusedIncludes
{
  return 0;
}

sub noComputeRoutine
{
  return 0;
}

sub preProcessForOpenACC
{

}

1;

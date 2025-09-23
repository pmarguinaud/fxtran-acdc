package Fxtran::Style;

=head1 NAME

Fxtran::Style

=head1 DESCRIPTION

This module implements a base class for style objects. These
are objects which contain some usefull properties to identify
the parallelism in FORTRAN source code.

Different parts of the code use slightly different notations;
for instance:

=over 4 

=item

In Meteo-France physics, the itetator and bounds of iterations are C<JLON>, C<KIDIA>, C<KFDIA>.

=item

In the dynamics, the itetator and bounds of iterations are C<JROF>, C<KST>, C<KEN>.

=item

In MesoNH physics, the iterator and bounds if iterations are C<JI> (or C<JIJ>), C<D%NIB>, C<D%NIE>.

=back

All other style classes derive from C<Fxtran::Style>.

This base class is also responsible for identifying the style of a FORTRAN source code 
document and create the corresponding style object.

=cut

use Data::Dumper;
use File::Basename;
use File::Spec;
use File::Find;

use strict;

use Fxtran;

sub newFromStyle
{
  my $class = shift;
  my %args = @_;
  $class = "Fxtran::Style::$args{style}";
  eval "use $class";
  $@ && die ($@);
  return $class->new ();
}

sub newFromDocument
{
  my $class = shift;
  my %args = @_;
  my $doc = $args{document};

  my $base = __PACKAGE__;
  (my $dir = __FILE__) =~ s/\.pm$//o;

  my @pm;

  &find ({wanted => sub { my $f = $File::Find::name; push @pm, $f if ($f =~ m/\.pm$/o) }, no_chdir => 1}, $dir);

  @pm = sort @pm;

  for my $pm (@pm)
    {
      $pm = 'File::Spec'->abs2rel ($pm, $dir);
      $pm =~ s,/,::,go;
      $pm =~ s/\.pm$//o;
      $pm = $base . '::' . $pm;
      eval "use $pm";
      $@ && die ($@);
    }

  my $rank;

  $rank = sub
  {
    return 0 unless (my $class = shift);

    my ($super) = do
    {
      no strict 'refs';
      @{ "$class\::ISA" }
    };

    return 1 + $rank->($super);
  };
 
  # Scan more specialized styles first

  my %rank = map { ($_, $rank->($_)) } @pm;

  @pm = sort { ($rank{$b} <=> $rank{$a}) || ($a cmp $b) } @pm;

  for my $pm (@pm)
    {
      my $canMatchDocument = do
      {
        no strict 'refs';
        defined (*{"$pm\::matchDocument"})
      };

      next unless ($canMatchDocument);

      if ($pm->matchDocument ($doc))
        {
          return $pm->new ();
        }
    }
}

sub new
{
  my $class = shift;
  my %args = @_;

  if ($args{style})
    {
      return $class->newFromStyle (%args);
    }
  elsif ($args{document})
    {
      return $class->newFromDocument (%args);
    }
  elsif ($class ne __PACKAGE__)
    {
      return bless \%args, $class;
    }
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

sub customIterator
{
}

sub updateCustomIterator
{
}

sub getActualNproma
{
  my $self = shift;
  my $pu = shift;

  my @arg = &F ('./dummy-arg-LT/arg-N', $pu->firstChild, 1);

  my @nproma = $self->nproma ();

  for my $nproma (@nproma)
    {
      my ($expr) = &e ($nproma);
      my ($n) = &F ('./N', $expr, 1);
      return $nproma if (grep { $_ eq $n } @arg);
    }

}

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;

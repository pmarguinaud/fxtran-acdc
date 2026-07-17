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

=head2 newFromStyle

Create a new style object from a style name.

=cut

  my $class = shift;
  my %args = @_;
  $class = "Fxtran::Style::$args{style}";
  eval "use $class";
  $@ && die ($@);
  return $class->new ();
}

sub newFromDocument
{

=head2 newFromDocument

Create a new style object by detecting the style from a FORTRAN source document.

=cut

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

=head2 new

Constructor. Dispatches to C<newFromStyle> or C<newFromDocument> depending on the arguments provided.

=cut

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

=head2 removeUnusedIncludes

Returns 0 by default. Subclasses may override this to enable removal of unused include statements.

=cut

  return 0;
}

sub noComputeRoutine
{

=head2 noComputeRoutine

Returns 0 by default. Subclasses may override this to suppress generation of compute routines.

=cut

  return 0;
}

sub preProcessForOpenACC
{

=head2 preProcessForOpenACC

Pre-process a document before OpenACC conversion. Base implementation is a no-op; subclasses may override.

=cut

}

sub customIterator
{

=head2 customIterator

Return a custom iterator expression for this style, if any. Base implementation returns nothing.

=cut

}

sub updateCustomIterator
{

=head2 updateCustomIterator

Update the custom iterator in a document. Base implementation is a no-op; subclasses may override.

=cut

}

sub getActualNproma
{

=head2 getActualNproma

Return the actual nproma argument used in a program unit, by matching dummy argument names against the style's nproma list.

=cut

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

sub getAbortStatement
{
  my $self = shift;
  my %args = @_;
  my $mess = $args{message};
  return &s ("CALL ABOR1 ('$mess')");
}

=head1 SEE ALSO

L<Fxtran::Style::MFPHYS>, L<Fxtran::Style::IAL>, L<Fxtran::Style::ECPHYS>, L<Fxtran::Style::MESONH>.

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;

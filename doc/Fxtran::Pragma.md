# NAME

[Fxtran::Pragma](../lib/Fxtran/Pragma.pm)

# SYNOPSIS

    use Fxtran::Pragma;

    my $pragma = 'Fxtran::Pragma'->new (pragma => 'OpenMP');

    # $do is a do construct

    $pragma->parallelDo ($do, PRIVATE => ['I', 'J']);

# DESCRIPTION

This module implements a base class for pragma annotations
languages such as `OpenMP` or `OpenACC`. Objects
of class `Fxtran::Pragma` have methods to generate such
annotations.

The `new` constructor also serves as an entry point for
creating such objects.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

# NAME

[click](../lib/click.pm)

# SYNOPSIS

Implement a `clickable` package:

    package clickable;

    use click;

    &click (<< "EOF");
      flag1                     -- This is a boolean option
      value1=s                  -- This is a string option        -- default
      list1=s@                  -- This is a list option          -- value1,value2
      hash1=s%                  -- This is a hash option          -- key1=vaule1,key2=value2
    EOF
    sub method1
    {
      my ($opts, @args) = @_;
      
      # $opts = {...} contains parsed options
      # @opts contains options after the double hyphen --

    }

then a `clickme.pl` script:

    #!/usr/bin/perl -w

    use clickable;

    clickable->method1 (@ARGV);

and run the script:

    $ ./clickme.pl --flag1 --value1=... ...

# DESCRIPTION

This module is similar to the python click module. It maps command line methods and options to module 
methods and arguments.

# SEE ALSO

Getopt::Long

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

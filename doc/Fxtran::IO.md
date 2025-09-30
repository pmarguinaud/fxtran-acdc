# NAME

[Fxtran::IO](../lib/Fxtran/IO.pm)

# DESCRIPTION

This module provides functions whose purpose is to generate code 
for handling FORTRAN derived types, in particular:

- SAVE

    Writing to a FORTRAN logical unit.

- LOAD

    Reading from a FORTRAN logical unit.

- COPY

    Copying to a device using OpenACC or OpenMP directives.

- WIPE

    Destroying a structure on the device.

- CRC64

    Computing checksums on device members.

- HOST

    Copy back Field API data to the host.

- DEVICE

    Push Field API data to the device.

- LEGACY

    Copy Field API data to host arrays.

# COPYRIGHT

Meteo-France 2022

# AUTHOR

philippe.marguinaud@meteo.fr

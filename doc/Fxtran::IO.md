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

## callSubroutineMethod

Return a Fortran CALL statement string for invoking a derived-type method as either a Field API procedure, a type-bound method, or a free-standing subroutine, depending on options.

## callFunctionMethod

Return a Fortran function-call expression string for invoking a derived-type method as either a Field API function, a type-bound function, or a free-standing function, depending on options.

## processDecl

Generate SAVE, LOAD, COPY, WIPE, HOST, LEGACY, CRC64, and SIZE code fragments for a single entity declaration of a derived type, appending them to the appropriate body arrays.

## indent

Apply simple indentation to a list of Fortran code lines, increasing indent after `IF`/`DO`/`ELSE` keywords and decreasing it before `ELSE`/`ENDIF`/`ENDDO`, and collapsing consecutive blank lines.

## w

Write content to a file, optionally embedding source-metadata (origin file, timestamp, version) via `Fxtran::Util::updateFile`.

## processTypes1

Process all derived-type constructs in a parsed module document and return the generated file names, code strings, and type-bound method descriptors.

## processTypes

Orchestrate code generation for all derived types in a document and write the resulting Fortran source files to disk, supporting regular, split-util, sorted, and type-bound-methods output modes.

## process\_module

Wrap a module's scalar declarations in a synthetic derived type, invoke `processTypes1` to generate IO code for that type, then strip the scaffolding and write the result.

# SEE ALSO

[Fxtran::FieldAPI](Fxtran%3A%3AFieldAPI.md), [Fxtran::IO::Link](Fxtran%3A%3AIO%3A%3ALink.md)

# COPYRIGHT

Meteo-France 2022

# AUTHOR

philippe.marguinaud@meteo.fr

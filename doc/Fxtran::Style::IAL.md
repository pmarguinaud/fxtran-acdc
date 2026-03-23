# NAME

[Fxtran::Style::IAL](../lib/Fxtran/Style/IAL.pm)

# DESCRIPTION

Intermediate base style class for IFS/Arpege/LACE (IAL) code. This class
derives from `Fxtran::Style` and provides shared behaviour for all IAL
style subclasses (`Fxtran::Style::MFPHYS`, `Fxtran::Style::ECPHYS`,
`Fxtran::Style::DYNAMICS`, etc.).

It provides the following common services:

- The include file extension `.intfb.h` used for interface blocks.
- A list of routines that are never treated as compute routines
(`ABOR1`, `DR_HOOK`, `PCRC`).
- Message-handling pre-processing (`ABOR1_ACC` conversion and
`WRITE`-to-`PRINT` transformation).
- Dimension-to-iterator and dimension-to-bounds mappings for `KLON` and
`KLEV`.
- OpenACC pre-processing that replaces explicit horizontal-range section
subscripts with bare colons.
- Interface generation via `Fxtran::Interface::intfb`.
- Logic for inserting OpenACC-variant interface include files.

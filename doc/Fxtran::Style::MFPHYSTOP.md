# NAME

[Fxtran::Style::MFPHYSTOP](../lib/Fxtran/Style/MFPHYSTOP.pm)

# DESCRIPTION

Style class for the top-level Meteo-France physics interface routines.
This class derives from `Fxtran::Style::MFPHYS` and overrides the loop
size (nproma) and loop bounds to use the `YDCPG_OPTS` and `YDCPG_BNDS`
structures that appear at the top of the Meteo-France physics call tree.
Specifically, nproma is taken from `YDGEOMETRY%YRDIM%NPROMA`,
`YDGEOMETRY%YRDIM%NPROMNH`, or `YDCPG_OPTS%KLON`, and the loop bounds
are `YDCPG_BNDS%KIDIA` and `YDCPG_BNDS%KFDIA`.

The `matchDocument` method identifies MFPHYSTOP source files by the
presence of `YDMF_PHYS`, `YDMF_PHYS_OUT`, or the combination of
`YDCPG_OPTS` and `YDCPG_BNDS` dummy arguments together with a `JLON`
local variable declaration.

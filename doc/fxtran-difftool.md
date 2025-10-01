Reformat FORTRAN code before invoking a diff utility such as `kdiff3` or `meld`.

This is meant to ease difference visualization; for instance, the two following `CALL`
statements are best diffed if they have a single argument per line:

    CALL ACDRAG (YDMODEL%YRCST, YDMODEL%YRML_PHY_MF, KIDIA, KFDIA, KLON, &
    & NTDRAG, KFLEVG, PAPRS, PAPRSF, PDELP, ZNBVNO, YDMF_PHYS_BASE_STATE%YCPG_PHY%XYB%RDELP, &
    & PU, PV, YDVARS%GEOMETRY%RCORI%T0, YDMF_PHYS_SURF%GSD_VF%PGETRL, ZGWDCS, YDMF_PHYS_SURF%GSD_VF%PVRLAN, &
    & YDMF_PHYS_SURF%GSD_VF%PVRLDI, YDMF_PHYS%OUT%STRDU, YDMF_PHYS%OUT%STRDV, ZTRAJGWD)

    CALL ACDRAG (YDMODEL%YRCST, YDMODEL%YRML_PHY_MF, KIDIA, KFDIA, KLON, &
    & NTDRAG, KFLEVG, PAPRS, PAPRSF, PDELP, ZNBVNO, YDMF_PHYS_BASE_STATE%YCPG_PHY%XYB%RDELP, &
    & PU, PV, YDVARS%GEOMETRY%RCORI%T0, YDMF_PHYS_SURF%GSD_VF%PGETRL, LLFLAG, ZGWDCS, YDMF_PHYS_SURF%GSD_VF%PVRLAN, &
    & YDMF_PHYS_SURF%GSD_VF%PVRLDI, YDMF_PHYS%OUT%STRDU, YDMF_PHYS%OUT%STRDV, ZTRAJGWD)

That is, reformatting the `CALL` statements make the diff trivial:

    CALL ACDRAG (                                 CALL ACDRAG (
    & YDMODEL%YRCST, &                            & YDMODEL%YRCST, &
    ...
    & PV, &                                       & PV, &
    & YDVARS%GEOMETRY%RCORI%T0, &                 & YDVARS%GEOMETRY%RCORI%T0, &
    & YDMF_PHYS_SURF%GSD_VF%PGETRL, &             & YDMF_PHYS_SURF%GSD_VF%PGETRL, &
    -------------------------------------------   & LLFLAG, &
    & ZGWDCS, &                                   & ZGWDCS, &
    ...
    & YDMF_PHYS%OUT%STRDV, &                      & YDMF_PHYS%OUT%STRDV, &
    & ZTRAJGWD)                                   & ZTRAJGWD)
# NAME

[fxtran-difftool](../bin/fxtran-difftool)

# SYNOPSIS

    $ fxtran-diffool file1.F90 file2.F90

# DESCRIPTION

Reformat FORTRAN code before invoking a diff utility such as `kdiff3` or `meld`.

This is meant to ease difference visualization; for instance, the two following `CALL`
statements are best diffed if they have a single argument per line:

    CALL ACDRAG (YDMODEL%YRCST, YDMODEL%YRML_PHY_MF, KIDIA, KFDIA, KLON, &
    & NTDRAG, KFLEVG, PAPRS, PAPRSF, PDELP, ZNBVNO, YDMF_PHYS_BASE_STATE%YCPG_PHY%XYB%RDELP, &
    & PU, PV, YDVARS%GEOMETRY%RCORI%T0, YDMF_PHYS_SURF%GSD_VF%PGETRL, ZGWDCS, YDMF_PHYS_SURF%GSD_VF%PVRLAN, &
    & YDMF_PHYS_SURF%GSD_VF%PVRLDI, YDMF_PHYS%OUT%STRDU, YDMF_PHYS%OUT%STRDV, ZTRAJGWD)

    CALL ACDRAG (YDMODEL%YRCST, YDMODEL%YRML_PHY_MF, KIDIA, KFDIA, KLON, &
    & NTDRAG, KFLEVG, PAPRS, PAPRSF, PDELP, ZNBVNO, YDMF_PHYS_BASE_STATE%YCPG_PHY%XYB%RDELP, &
    & PU, PV, YDVARS%GEOMETRY%RCORI%T0, YDMF_PHYS_SURF%GSD_VF%PGETRL, LLFLAG, ZGWDCS, YDMF_PHYS_SURF%GSD_VF%PVRLAN, &
    & YDMF_PHYS_SURF%GSD_VF%PVRLDI, YDMF_PHYS%OUT%STRDU, YDMF_PHYS%OUT%STRDV, ZTRAJGWD)

That is, reformatting the `CALL` statements make the diff trivial:

    CALL ACDRAG (                                 CALL ACDRAG (
    & YDMODEL%YRCST, &                            & YDMODEL%YRCST, &
    ...
    & PV, &                                       & PV, &
    & YDVARS%GEOMETRY%RCORI%T0, &                 & YDVARS%GEOMETRY%RCORI%T0, &
    & YDMF_PHYS_SURF%GSD_VF%PGETRL, &             & YDMF_PHYS_SURF%GSD_VF%PGETRL, &
    -------------------------------------------   & LLFLAG, &
    & ZGWDCS, &                                   & ZGWDCS, &
    ...
    & YDMF_PHYS%OUT%STRDV, &                      & YDMF_PHYS%OUT%STRDV, &
    & ZTRAJGWD)                                   & ZTRAJGWD)

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

[fxtran-mergetool](fxtran-mergetool.md), [Fxtran::Formatter](Fxtran%3A%3AFormatter.md)

# COPYRIGHT

Meteo-France 2025

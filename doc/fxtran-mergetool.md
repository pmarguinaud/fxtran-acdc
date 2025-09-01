Wrap kdiff3, meld or vimdiff. Expand the following FORTRAN statements before calling merge utilities:

- call
- associate
- subroutine

Repack after merging.
# NAME

[fxtran-mergetool](../bin/fxtran-mergetool)

# SYNOPSIS

    $ git mergetool -t fxtran-mergetool

# DESCRIPTION

Wrap kdiff3, meld or vimdiff. Expand the following FORTRAN statements before calling merge utilities:

- call
- associate
- subroutine

Repack after merging.

# CONFIGURATION

Add this in your ~/.gitconfig :

    [mergetool "fxtran-mergetool"]
            cmd = fxtran-mergetool --mergetool kdiff3 "$BASE" "$LOCAL" "$REMOTE" "$MERGED"
            trustExitCode = true

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

[fxtran](https://github.com/pmarguinaud/fxtran)

# COPYRIGHT

Meteo-France 2025

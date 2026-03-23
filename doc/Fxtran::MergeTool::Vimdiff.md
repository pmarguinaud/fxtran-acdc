# NAME

[Fxtran::MergeTool::Vimdiff](../lib/Fxtran/MergeTool/Vimdiff.pm)

# DESCRIPTION

Backend for the vimdiff merge tool. Implements the `merge` method from
[Fxtran::MergeTool](Fxtran%3A%3AMergeTool.md) by invoking the `vimdiff` external command to perform
a three-way merge inside Vim. The base file is first copied to the merged
output path, which is then opened alongside the local and remote files for
interactive editing.

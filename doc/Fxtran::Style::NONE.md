# NAME

[Fxtran::Style::NONE](../lib/Fxtran/Style/NONE.pm)

# DESCRIPTION

A no-op style class used as a neutral fallback when no specific coding
style is applicable or when style-specific behaviour should be suppressed.
This class derives from `Fxtran::Style` and overrides all style-sensitive
methods to return empty or false values: unused includes are never removed,
no routine is flagged as a non-compute routine, OpenACC pre-processing is
a no-op, and there is no custom iterator or actual nproma.

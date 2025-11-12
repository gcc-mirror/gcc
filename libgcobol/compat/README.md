# GCC COBOL Compatibility Functions

## Purpose

It seems every COBOL compiler includes a library of functions intended
to make the COBOL programer's life easier.  All of them, as we
demonstrate here, can be written in COBOL.  They are supplied in COBOL
form, not as a library. The user is free to compile them into a
utility library.

Some of the functions defined here require runtime support from libgcobol. 

## Fri Oct 10 16:01:58 2025

At the time of this writing, the functions of greatest concern are
those that are defined by Rocket Software (formerly MicroFocus) and
emulated by GnuCOBOL. Those are implemented in
`gcc/cobol/compat/lib/gnu`.  Any calls they would otherwise make to
the C library are effected through COBOL POSIX bindings supplied by
`gcc/cobol/posix/udf`.

As an aid to the developer, a simple example of how these functions
are used is found in `gcc/cobol/compat/t/smoke.cbl`. It may by
compiled using `gcc/cobol/compat/Makefile`.


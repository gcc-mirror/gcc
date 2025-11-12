# GCC COBOL Posix Functions and Adapter

## Purpose

GCC COBOL provides COBOL bindings for some POSIX functions.  Feel free
to contribute more.  Insofar as possible, the functions take the same
parameters and return the same values as defined by POSIX.  Among
others, they are used by the COBOL compatibility library (see
libgcobol/compat/lib/gnu). They are installed in source form.  The
user may choose to compile them to a library.

ISO COBOL does not specify any relationship to any particular
operating system, and does not reference POSIX. The raw capability is
there, of course, via the `CALL` statement.  But that's not very
convenient, and offers no parameter validation.

For simple functions, e.g. **unlink**(2), the UDFs simply call the
underlying C library.  More complex functions, though,
e.g. **stat**(2), pass or return a buffer.  That buffer is normally
defined by what members must exist, but its exact layout is left up to
the C implementation and defined by the C header files, which are not
parsed by GCC COBOL. Consequently we do not know, at the COBOL level,
how to define the `struct stat` buffer required by **stat**(2).  For
such functions, we use a C "shim" function that accepts a buffer
defined by GCC COBOL.  That buffer has the members defined by POSIX
and a layout defined by GCC COBOL.  The COBOL application calls the
COBOL POSIX binding, which uses the shim function to call the C
library.  

To take **stat**(2) as an example, 

    COBOL program uses 
        COPY posix-stat.
      01 stat-buf.
        COPY posix-statbuf. *> gcc/cobol/posix/cpy
      FUNCTION POSIX-STAT(filename, stat-buf)
    libgcobol/posix/udf/posix-stat.cbl 
        passes stat-buf to 
        posix_stat in libgcobol
    posix_stat calls stat(2), 
        and copies the returned values to its input buffer

## Contents

The installed POSIX bindings and associated copybooks are in `cpy` and `udf`: 

- `cpy/`  copybooks used by functions in `udf`
- `udf/`  COBOL POSIX bindings
- `t/`    simple tests demonstrating use of functions in `udf`

Any buffer shared between the COBOL application and a COBOL POSIX
function is defined in `cpy/`.  While these buffers meet the POSIX
descriptions -- meaning they have members matching the standard --
they probably do not match the buffer defined by the C library in
`/usr/include`. GCC COBOL does not parse C, and therefore does not
parse C header files, and so has no access to those C buffer definitions. 

The machine-shop tools are in `bin/`. 

- `bin/`  developer tools to aid creation of POSIX bindings
  - `scrape.awk` extracts function prototypes from the SYNOPSIS of a
    man page.
  - `udf-gen` reads function declarations and, for each one, produces
    a COBOL User Defined Function (UDF) that calls the function.

Finally, 

- `shim/` C support for POSIX bindings, incorporated in libgcobol

## Prerequisites
### for developers, to generate COBOL POSIX bindings

To use the POSIX bindings, just use the COPY statement. 

To create new ones, use `udf-gen`.  `udf-gen` is a Python program that
imports the [PLY pycparser module](http://www.dabeaz.com/ply/) module,
which must be installed.

`udf-gen` is lightly documented, use `udf-gen --help`. It can be a
little tedious to set up the first time, but if you want to use more a
few functions, it will be faster than doing the work by hand.

## Limitations

`udf-gen` does not

- generate a working UDF for function parameters of type `struct`,
  such as is used by **stat**(2).  This is because the information is
  not available in a standardized way in the SYNOPSIS of a man page.
- define helpful Level 88 values for "magic" numbers, such as
  permission bits in **chmod**(2).

None of this is particularly difficult; it's just a matter of time and
need. The `scrape.awk` script finds 560 functions in the Ubuntu LTS
22.04 manual.  Which of those is important is for users to decide.

## Other Options

IBM and MicroFocus both supply intrinsic functions to interface with
the OS, each in their own way. GnuCOBOL implements some of those functions.

## Portability

The UDF produced by `udf-gen` is pure ISO COBOL.  The code should be
compilable by any ISO COBOL compiler.

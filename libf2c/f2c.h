/* f2c.h file for GNU Fortran run-time library
   Copyright (C) 1998 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This file currently is just a stub through which g77's copy
   of netlib's libf2c, which g77 builds and installs as libg2c.a
   (to avoid conflict), #include's g77's version of f2c.h, named
   g2c.h.  That file is, in turn, produced via g77's library
   configuration process from g2c.h.in.

   By going through this extra "hoop", it is easy to provide for
   libg2c-specific configuration and typedefs that aren't appropriate
   in g2c.h itself (since that is intended to be installed so it can
   be shared with f2c users), without changing the libf2c (libg2c)
   routines themselves.  (They continue to #include "f2c.h", just
   like they do in netlib's version.)  */

#include "g2c.h"

/* For GNU Fortran (g77), we always enable the following behaviors for
   libf2c, to make things easy on the programmer.  The alternate
   behaviors have their uses, and g77 might provide them as compiler,
   rather than library, options, so only a single copy of a shared libf2c
   need be built for a system.  */

/* This makes unformatted I/O more consistent in relation to other
   systems.  It is not required by the F77 standard.  */

#define Pad_UDread

/* This makes ERR= and IOSTAT= returns work properly in disk-full
   situations, making things work more as expected.  It slows things
   down, so g77 will probably someday choose the original implementation
   on a case-by-case basis when it can be shown to not be necessary
   (e.g. no ERR= or IOSTAT=) or when it is given the appropriate
   compile-time option or, perhaps, source-code directive.

   (No longer defined, since it really slows down NFS access too much.)  */

/* #define ALWAYS_FLUSH */

/* Most Fortran implementations do this, so to make it easier
   to compare the output of g77-compiled programs to those compiled
   by most other compilers, tell libf2c to put leading zeros in
   appropriate places on output.  */

#define WANT_LEAD_0

/* Copyright (C) 2002-2023 Free Software Foundation, Inc.
   Contributed by Andy Vaught and Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"


/* Stupid function to be sure the constructor is always linked in, even
   in the case of static linking.  See PR libfortran/22298 for details.  */
void
stupid_function_name_for_static_linking (void)
{
  return;
}


static int argc_save;
static char **argv_save;


/* Set the saved values of the command line arguments.  */

void
set_args (int argc, char **argv)
{
  argc_save = argc;
  argv_save = argv;
}
iexport(set_args);


/* Retrieve the saved values of the command line arguments.  */

void
get_args (int *argc, char ***argv)
{
  *argc = argc_save;
  *argv = argv_save;
}


/* Initialize the runtime library.  */

static void __attribute__((constructor))
init (void)
{
  /* Must be first */
  init_variables ();

  init_units ();

  /* If (and only if) the user asked for it, set up the FPU state.  */
  if (options.fpe != 0)
    set_fpu ();

  init_compile_options ();
}


/* Cleanup the runtime library.  */

static void __attribute__((destructor))
cleanup (void)
{
  close_units ();
}

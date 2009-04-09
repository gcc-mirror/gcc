/* Copyright (C) 2002-2003, 2005, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Andy Vaught and Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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
#include <stdlib.h>
#include <string.h>
#include <limits.h>


#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Stupid function to be sure the constructor is always linked in, even
   in the case of static linking.  See PR libfortran/22298 for details.  */
void
stupid_function_name_for_static_linking (void)
{
  return;
}

/* This will be 0 for little-endian
   machines and 1 for big-endian machines.  */
int big_endian = 0;


/* Figure out endianness for this machine.  */

static void
determine_endianness (void)
{
  union
  {
    GFC_LOGICAL_8 l8;
    GFC_LOGICAL_4 l4[2];
  } u;

  u.l8 = 1;
  if (u.l4[0])
    big_endian = 0;
  else if (u.l4[1])
    big_endian = 1;
  else
    runtime_error ("Unable to determine machine endianness");
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

/* Retrieve the saved values of the command line arguments.  */

void
get_args (int *argc, char ***argv)
{
  *argc = argc_save;
  *argv = argv_save;
}


static const char *exe_path;
static int please_free_exe_path_when_done;

/* Save the path under which the program was called, for use in the
   backtrace routines.  */
void
store_exe_path (const char * argv0)
{
#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

#ifndef DIR_SEPARATOR   
#define DIR_SEPARATOR '/'
#endif

  char buf[PATH_MAX], *cwd, *path;

  /* On the simulator argv is not set.  */
  if (argv0 == NULL || argv0[0] == '/')
    {
      exe_path = argv0;
      please_free_exe_path_when_done = 0;
      return;
    }

  memset (buf, 0, sizeof (buf));
#ifdef HAVE_GETCWD
  cwd = getcwd (buf, sizeof (buf));
#else
  cwd = "";
#endif

  /* exe_path will be cwd + "/" + argv[0] + "\0" */
  path = malloc (strlen (cwd) + 1 + strlen (argv0) + 1);
  sprintf (path, "%s%c%s", cwd, DIR_SEPARATOR, argv0);
  exe_path = path;
  please_free_exe_path_when_done = 1;
}

/* Return the full path of the executable.  */
char *
full_exe_path (void)
{
  return (char *) exe_path;
}

/* Initialize the runtime library.  */

static void __attribute__((constructor))
init (void)
{
  /* Figure out the machine endianness.  */
  determine_endianness ();

  /* Must be first */
  init_variables ();

  init_units ();
  set_fpu ();
  init_compile_options ();

#ifdef DEBUG
  /* Check for special command lines.  */

  if (argc > 1 && strcmp (argv[1], "--help") == 0)
    show_variables ();

  /* if (argc > 1 && strcmp(argv[1], "--resume") == 0) resume();  */
#endif

  random_seed_i4 (NULL, NULL, NULL);
}


/* Cleanup the runtime library.  */

static void __attribute__((destructor))
cleanup (void)
{
  close_units ();
  
  if (please_free_exe_path_when_done)
    free ((char *) exe_path);
}

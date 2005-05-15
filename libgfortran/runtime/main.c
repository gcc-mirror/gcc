/* Copyright (C) 2002-2003, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught and Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stddef.h>

#include "libgfortran.h"

/* This is the offset (in bytes) required to cast from logical(8)* to
   logical(4)*. and still get the same result.  Will be 0 for little-endian
   machines and 4 for big-endian machines.  */
int l8_to_l4_offset = 0;


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
    l8_to_l4_offset = 0;
  else if (u.l4[1])
    l8_to_l4_offset = 1;
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


/* Initialize the runtime library.  */

static void __attribute__((constructor))
init (void)
{
  /* Figure out the machine endianness.  */
  determine_endianness ();

  /* Must be first */
  init_variables ();

  init_units ();

#ifdef DEBUG
  /* Check for special command lines.  */

  if (argc > 1 && strcmp (argv[1], "--help") == 0)
    show_variables ();

  /* if (argc > 1 && strcmp(argv[1], "--resume") == 0) resume();  */
#endif

  random_seed(NULL,NULL,NULL);
}


/* Cleanup the runtime library.  */

static void __attribute__((destructor))
cleanup (void)
{
  close_units ();
}

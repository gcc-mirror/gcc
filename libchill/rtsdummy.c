/* Implement runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#define __CHILL_LIB__

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
/*#include "gvarargs.h"	Gcc source and runtime libs use gvarargs.h */

#include "rtltypes.h"

typedef void (*init_ptr) ();
typedef int * tasking_ptr;

/* Dummy functions for rts access. When we come here we have an error. */

typedef char *(*fetch_names) (int number);
typedef int (*fetch_numbers) (char *name);

static void __rts_main_loop ()
{
  /* do nothing in case of no run time system */
}
init_ptr	__RTS_MAIN_LOOP__ = __rts_main_loop;

static void __rts_init ()
{
  /* do nothing in case of no run time system */
}
init_ptr	__RTS_INIT__ = __rts_init;

static char *__fetch_name (int number)
{
    fprintf (stderr, "ChillLib: fetch_name: no runtime system library linked.\n");
    fflush (stderr);
    abort ();
}
fetch_names	__RTS_FETCH_NAMES__ = __fetch_name;

static int __fetch_number (char *name)
{
    fprintf (stderr, "ChillLib: fetch_number: no runtime system library linked.\n");
    fflush (stderr);
    abort ();
}
fetch_numbers	__RTS_FETCH_NUMBERS__ = __fetch_number;

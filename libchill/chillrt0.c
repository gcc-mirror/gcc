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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "rtltypes.h"
#include "iomodes.h"

/* type definitions */
typedef void (*init_ptr) ();
typedef void (*rts_init_ptr) (int *argc, char *argv []);

typedef struct INIT_LIST
{
  init_ptr             code;
  struct INIT_LIST     *forward;
} InitList;

InitList    *_ch_init_list = 0;

/* force linker to get correct RTS functions */
extern rts_init_ptr	__RTS_INIT__;
extern init_ptr		__RTS_MAIN_LOOP__;
extern init_ptr		__RTS_FETCH_NUMBERS__;
extern init_ptr		__RTS_FETCH_NAMES__;
static init_ptr		*rts_dummies[4] = 
{
  &__RTS_INIT__,
  &__RTS_MAIN_LOOP__,
  &__RTS_FETCH_NUMBERS__,
  &__RTS_FETCH_NAMES__,
};

/* chill argc and argv */
int			chill_argc = 0;
TVaryingCharType	**chill_argv = NULL;

/* the program name for debugging purpose */
char 			*progname = 0;

extern void *__xmalloc_ ();

/*
 * function __xrealloc_
 *
 * parameter:
 *   ptr		pointer to reallocate
 *   size		new number of bytes
 *
 * returns:
 *  void*
 *
 * abstract:
 *  This is the general reallocation routine for libchill
 *
 */

void *
__xrealloc_ (ptr, size)
void *ptr;
int size;
{
  void	*tmp = realloc (ptr, size);
  
  if (!tmp)
    {
      fprintf (stderr, "ChillLib: Out of heap space.\n");
      fflush (stderr);
      exit (ENOMEM);
    }
  return (tmp);
} /* __xrealloc_ */

static void
setup_argc_argv (argc, argv)
int argc;
char *argv[];
{
  int		i;
  
  chill_argv = __xmalloc_ ((argc + 1) * sizeof (TVaryingCharType *));
  for (i = 0; i < argc; i++)
    {
      chill_argv[i] = __xmalloc_ (sizeof (TVaryingCharType) + strlen (argv[i]) + 1);
      chill_argv[i]->len = strlen (argv[i]);
      strcpy (chill_argv[i]->body, argv[i]);
    }
  chill_argv[chill_argc = argc] = NULL;
  
  if ((progname = strrchr (argv[0], '/')) == 0)
    progname = argv[0];
  else
    progname++;
  
} /* setup_argc_argv */

extern void __setexceptionStack ();

/*--------- main entry for each CHILL - program ----------*/
int
main (argc, argv)
     int argc;
     char *argv [];
{
  /* call look up for tasking */
  (*__RTS_INIT__) (&argc, argv);

  /* setup argc and argv */
  setup_argc_argv (argc, argv);

  /* clear exception stack */
  __setexceptionStack (0);

  /* now call code at module level */
  while (_ch_init_list)
    {
      if (_ch_init_list->code)
	(*(_ch_init_list->code)) ();
      _ch_init_list = _ch_init_list->forward;
    }

  /* if we have rts linked, something will be done, else just return */
  (*__RTS_MAIN_LOOP__) ();
  
  return (0);
  
} /* main */

/* memory allocation routines with error checking.
   Copyright 1989, 90, 91, 92, 93, 94 Free Software Foundation, Inc.
   
This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "ansidecl.h"
#include "libiberty.h"

#include <stdio.h>

#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#define ptrdiff_t long
#endif

#if VMS
#include <stdlib.h>
#include <unixlib.h>
#else
/* For systems with larger pointers than ints, these must be declared.  */
PTR malloc PARAMS ((size_t));
PTR realloc PARAMS ((PTR, size_t));
PTR calloc PARAMS ((size_t, size_t));
PTR sbrk PARAMS ((ptrdiff_t));
#endif

/* The program name if set.  */
static const char *name = "";

#ifdef HAVE_SBRK
/* The initial sbrk, set when the program name is set. Not used for win32
   ports other than cygwin32.  */
static char *first_break = NULL;
#endif /* HAVE_SBRK */

void
xmalloc_set_program_name (s)
     const char *s;
{
  name = s;
#ifdef HAVE_SBRK
  /* Win32 ports other than cygwin32 don't have brk() */
  if (first_break == NULL)
    first_break = (char *) sbrk (0);
#endif /* HAVE_SBRK */
}

PTR
xmalloc (size)
    size_t size;
{
  PTR newmem;

  if (size == 0)
    size = 1;
  newmem = malloc (size);
  if (!newmem)
    {
#ifdef HAVE_SBRK
      extern char **environ;
      size_t allocated;

      if (first_break != NULL)
	allocated = (char *) sbrk (0) - first_break;
      else
	allocated = (char *) sbrk (0) - (char *) &environ;
      fprintf (stderr,
	       "\n%s%sCan not allocate %lu bytes after allocating %lu bytes\n",
	       name, *name ? ": " : "",
	       (unsigned long) size, (unsigned long) allocated);
#else /* HAVE_SBRK */
      fprintf (stderr,
              "\n%s%sCan not allocate %lu bytes\n",
              name, *name ? ": " : "",
              (unsigned long) size);
#endif /* HAVE_SBRK */
      xexit (1);
    }
  return (newmem);
}

PTR
xcalloc (nelem, elsize)
  size_t nelem, elsize;
{
  PTR newmem;

  if (nelem == 0 || elsize == 0)
    nelem = elsize = 1;

  newmem = calloc (nelem, elsize);
  if (!newmem)
    {
#ifdef HAVE_SBRK
      extern char **environ;
      size_t allocated;

      if (first_break != NULL)
	allocated = (char *) sbrk (0) - first_break;
      else
	allocated = (char *) sbrk (0) - (char *) &environ;
      fprintf (stderr,
	       "\n%s%sCan not allocate %lu bytes after allocating %lu bytes\n",
	       name, *name ? ": " : "",
	       (unsigned long) (nelem * elsize), (unsigned long) allocated);
#else /* HAVE_SBRK */
      fprintf (stderr,
              "\n%s%sCan not allocate %lu bytes\n",
              name, *name ? ": " : "",
              (unsigned long) (nelem * elsize));
#endif /* HAVE_SBRK */
      xexit (1);
    }
  return (newmem);
}

PTR
xrealloc (oldmem, size)
    PTR oldmem;
    size_t size;
{
  PTR newmem;

  if (size == 0)
    size = 1;
  if (!oldmem)
    newmem = malloc (size);
  else
    newmem = realloc (oldmem, size);
  if (!newmem)
    {
#ifdef HAVE_SBRK
      extern char **environ;
      size_t allocated;

      if (first_break != NULL)
	allocated = (char *) sbrk (0) - first_break;
      else
	allocated = (char *) sbrk (0) - (char *) &environ;
      fprintf (stderr,
	       "\n%s%sCan not reallocate %lu bytes after allocating %lu bytes\n",
	       name, *name ? ": " : "",
	       (unsigned long) size, (unsigned long) allocated);
#else /* HAVE_SBRK */
      fprintf (stderr,
              "\n%s%sCan not reallocate %lu bytes\n",
              name, *name ? ": " : "",
              (unsigned long) size);
#endif /* HAVE_SBRK */
      xexit (1);
    }
  return (newmem);
}

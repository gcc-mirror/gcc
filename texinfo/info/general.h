/* general.h -- Some generally useful defines. */

/* This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

   Written by Brian Fox (bfox@ai.mit.edu). */

#if !defined (_GENERAL_H_)
#define _GENERAL_H_

extern void *xmalloc (), *xrealloc ();

#if defined (HAVE_UNISTD_H)
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if defined (HAVE_STRING_H)
#  include <string.h>
#else
#  include <strings.h>
#endif /* !HAVE_STRING_H */

#include "clib.h"

#define info_toupper(x) (islower (x) ? toupper (x) : x)
#define info_tolower(x) (isupper (x) ? tolower (x) : x)

#if !defined (whitespace)
#  define whitespace(c) ((c == ' ') || (c == '\t'))
#endif /* !whitespace */

#if !defined (whitespace_or_newline)
#  define whitespace_or_newline(c) (whitespace (c) || (c == '\n'))
#endif /* !whitespace_or_newline */

#if !defined (__FUNCTION_DEF)
#  define __FUNCTION_DEF
typedef int Function ();
typedef void VFunction ();
typedef char *CFunction ();
#endif /* _FUNCTION_DEF */

/* Add POINTER to the list of pointers found in ARRAY.  SLOTS is the number
   of slots that have already been allocated.  INDEX is the index into the
   array where POINTER should be added.  GROW is the number of slots to grow
   ARRAY by, in the case that it needs growing.  TYPE is a cast of the type
   of object stored in ARRAY (e.g., NODE_ENTRY *. */
#define add_pointer_to_array(pointer, idx, array, slots, grow, type) \
  do { \
    if (idx + 2 >= slots) \
      array = (type *)(xrealloc (array, (slots += grow) * sizeof (type))); \
    array[idx++] = (type)pointer; \
    array[idx] = (type)NULL; \
  } while (0)

#define maybe_free(x) do { if (x) free (x); } while (0)

#if !defined (zero_mem) && defined (HAVE_MEMSET)
#  define zero_mem(mem, length) memset (mem, 0, length)
#endif /* !zero_mem && HAVE_MEMSET */

#if !defined (zero_mem) && defined (HAVE_BZERO)
#  define zero_mem(mem, length) bzero (mem, length)
#endif /* !zero_mem && HAVE_BZERO */

#if !defined (zero_mem)
#  define zero_mem(mem, length) \
  do {					\
	register int zi;		\
	register unsigned char *place;	\
					\
	place = (unsigned char *)mem;	\
	for (zi = 0; zi < length; zi++)	\
	  place[zi] = 0;		\
      } while (0)
#endif /* !zero_mem */

#endif /* !_GENERAL_H_ */

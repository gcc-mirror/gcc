/* clib.c: Functions which we normally expect to find in the C library.
   $Id: clib.c,v 1.1.1.1 1997/08/21 22:57:59 jason Exp $

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1995 Free Software Foundation, Inc.

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

#include <stdio.h>

#if defined (HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined (HAVE_STDLIB_H)
#include <stdlib.h>
#endif

#if defined (HAVE_STRING_H)
#include <string.h>
#endif

#include <sys/errno.h>

extern void *xmalloc (), *xrealloc ();
#include "general.h"

#if !defined (errno)
extern int errno;
#endif

#if !defined (HAVE_STRERROR)
extern char *sys_errlist[];
extern int sys_nerr;

char *
strerror (num)
     int num;
{
  if (num >= sys_nerr)
    return ("");
  else
    return (sys_errlist[num]);
}
#endif /* !HAVE_STRERROR */

#if !defined (HAVE_STRCASECMP)
/* This Unix doesn't have the strcasecmp () function. */
int
strcasecmp (string1, string2)
     char *string1, *string2;
{
  char ch1, ch2;

  for (;;)
    {
      ch1 = *string1++;
      ch2 = *string2++;

      if (!(ch1 | ch2))
	return (0);

      ch1 = info_toupper (ch1);
      ch2 = info_toupper (ch2);

      if (ch1 != ch2)
	return (ch1 - ch2);
    }
}

/* Compare at most COUNT characters from string1 to string2.  Case
   doesn't matter. */
int
strncasecmp (string1, string2, count)
     char *string1, *string2;
     int count;
{
  register char ch1, ch2;

  while (count)
    {
      ch1 = *string1++;
      ch2 = *string2++;

      ch1 = info_toupper (ch1);
      ch2 = info_toupper (ch2);

      if (ch1 == ch2)
	count--;
      else
	break;
    }
  return (count);
}
#endif /* !STRCASECMP */


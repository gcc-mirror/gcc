/* strdup.c -- return a newly allocated copy of a string
   Copyright (C) 1990 Free Software Foundation, Inc.

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
   Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#else
char *malloc ();
char *strcpy ();
#endif

/* Return a newly allocated copy of STR,
   or 0 if out of memory. */

char *
strdup (str)
     const char *str;
{
  char *newstr;

  newstr = (char *) malloc (strlen (str) + 1);
  if (newstr)
    strcpy (newstr, str);
  return newstr;
}

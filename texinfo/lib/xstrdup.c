/* xstrdup.c -- copy a string with out of memory checking
   Copyright (C) 1990, 1996 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
# include <string.h>
#else
# include <strings.h>
#endif

#if defined (__STDC__) && __STDC__
char *xmalloc (size_t);
char *xstrdup (char *string);
#else
char *xmalloc ();
#endif

/* Return a newly allocated copy of STRING.  */

char *
xstrdup (string)
     char *string;
{
  return strcpy (xmalloc (strlen (string) + 1), string);
}

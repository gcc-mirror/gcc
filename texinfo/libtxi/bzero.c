/*
 * Copyright (C) 1993 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can either send email to this
 * program's author (see below) or write to: The Free Software Foundation,
 * Inc.; 59 Temple Place - Suite 330. Boston, MA 02111-1307, USA.
 */

#if !defined (HAVE_MEMSET) && !defined (HAVE_BZERO)

void
bzero (b, length)
     register char *b;
     register int length;
{
#ifdef VMS /* but this is definitely VMS-specific */
  short zero = 0;
  long max_str = 65535;

  while (length > max_str)
    {
      (void) LIB$MOVC5 (&zero, &zero, &zero, &max_str, b);
      length -= max_str;
      b += max_str;
    }
  (void) LIB$MOVC5 (&zero, &zero, &zero, &length, b);
#else
  while (length-- > 0)
    *b++ = 0;
#endif /* not VMS */
}

#endif /* not HAVE_MEMSET && not HAVE_BZERO */

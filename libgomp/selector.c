/* Copyright (C) 2022 Free Software Foundation, Inc.
   Contributed by Mentor, a Siemens Business.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains a placeholder implementation of
   GOMP_evaluate_current_device.  */

#include "libgomp.h"

/* The selectors are passed as strings, but are actually sets of multiple
   trait property names, separated by '\0' and with an extra '\0' at
   the end.  Match such a string SELECTORS against an array of strings
   CHOICES, that is terminated by a null pointer.
   matches.  */
static bool
gomp_match_selectors (const char *selectors, const char **choices)
{
  while (*selectors != '\0')
    {
      bool match = false;
      for (int i = 0; !match && choices[i]; i++)
	match = !strcmp (selectors, choices[i]);
      if (!match)
	return false;
      selectors += strlen (selectors) + 1;
    }
  return true;
}

bool
GOMP_evaluate_current_device (const char *kind, const char *arch,
			      const char *isa)
{
  static const char *kind_choices[] = { "cpu", "host", NULL };

  if (kind && !gomp_match_selectors (kind, kind_choices))
    return false;

  if (arch || isa)
    return false;

  return true;
}

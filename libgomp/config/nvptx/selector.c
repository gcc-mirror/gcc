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

/* This file contains an implementation of GOMP_evaluate_current_device for
   a Nvidia GPU.  */

#include "libgomp.h"
#include <string.h>

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
  static const char *kind_choices[] = { "gpu", "nohost", NULL };
  static const char *arch_choices[] = { "nvptx", NULL };
  static const char *isa_choices[]
    = {
       "sm_30",
#if __PTX_SM__ >= 350
       "sm_35",
#endif
#if __PTX_SM__ >= 530
       "sm_53",
#endif
#if __PTX_SM__ >= 750
       "sm_75",
#endif
#if __PTX_SM__ >= 800
       "sm_80",
#endif
       NULL };

  if (kind && !gomp_match_selectors (kind, kind_choices))
    return false;
  if (arch && !gomp_match_selectors (arch, arch_choices))
    return false;
  if (isa && !gomp_match_selectors (isa, isa_choices))
    return false;
  return true;
}

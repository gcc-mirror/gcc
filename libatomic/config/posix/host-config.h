/* Copyright (C) 2012-2020 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
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

/* Included after all more target-specific host-config.h.  */


#ifndef protect_start_end
# ifdef HAVE_ATTRIBUTE_VISIBILITY
#  pragma GCC visibility push(hidden)
# endif

void libat_lock_1 (void *ptr);
void libat_unlock_1 (void *ptr);

static inline UWORD
protect_start (void *ptr)
{
  libat_lock_1 (ptr);
  return 0;
}

static inline void
protect_end (void *ptr, UWORD dummy UNUSED)
{
  libat_unlock_1 (ptr);
}

# define protect_start_end 1
# ifdef HAVE_ATTRIBUTE_VISIBILITY
#  pragma GCC visibility pop
# endif
#endif /* protect_start_end */

#include_next <host-config.h>

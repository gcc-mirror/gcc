/* Copyright (C) 2023-2025 Free Software Foundation, Inc.

   Contributed by Siemens.

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


/* This file is used to create a hash table on the host that is supposed
   to get use on the device - that's for the 'indirect' clause feature.

   In order to habe host initialization work, the pointer sizes must be
   the same - and either the the endianess or the host-device memcopy
   has to take of it.  */

typedef unsigned __int128 hash_entry_type;
#define INDIRECT_HOST_ADDR(p) ((void *) (uintptr_t) p)
#define INDIRECT_DEV_ADDR(p) ((void*) (uintptr_t) (p >> 64))
#define SET_INDIRECT_ADDRS(p, h, d) \
  p = (((unsigned __int128) h) + (((unsigned __int128) d) << 64))

_Static_assert (sizeof (unsigned __int128) == 2 * sizeof (void*),
		"hash_entry_type size mismatch");

static inline void *htab_alloc (size_t size) {
  return malloc (size);
}

static inline void htab_free (void *ptr) { free (ptr); }

#include "hashtab.h"

static inline hashval_t
htab_hash (hash_entry_type element)
{
  return hash_pointer (INDIRECT_HOST_ADDR (element));
}

static inline bool
htab_eq (hash_entry_type x, hash_entry_type y)
{
  return INDIRECT_HOST_ADDR (x) == INDIRECT_HOST_ADDR (x);
}

void*
create_target_indirect_map (size_t *h_size, size_t count,
			    uint64_t *host_addrs, uint64_t *device_addrs)
{
  (void) htab_find;  /* Silence -Werror=unused-function.  */

  htab_t indirect_htab = htab_create (count);

  hash_entry_type element;
  for (int i = 0; i < count; i++)
    {
      SET_INDIRECT_ADDRS (element, host_addrs[i], device_addrs[i]);
      hash_entry_type *slot = htab_find_slot (&indirect_htab, element,
					      INSERT);
      *slot = element;
    }
  *h_size = (sizeof (struct htab)
	     + htab_size (indirect_htab) * sizeof (hash_entry_type));
  return (void*) indirect_htab;
}

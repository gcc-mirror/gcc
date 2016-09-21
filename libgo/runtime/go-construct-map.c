/* go-construct-map.c -- construct a map from an initializer.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "runtime.h"

extern void *makemap (const struct __go_map_type *, int64_t hint,
		      void *, void *)
  __asm__ (GOSYM_PREFIX "runtime.makemap");

extern void mapassign1 (const struct __go_map_type *, void *hmap,
			const void *key, const void *val)
  __asm__ (GOSYM_PREFIX "runtime.mapassign1");

void *
__go_construct_map (const struct __go_map_type *type,
		    uintptr_t count, uintptr_t entry_size,
		    uintptr_t val_offset, const void *ventries)
{
  void *ret;
  const unsigned char *entries;
  uintptr_t i;

  ret = makemap(type, (int64_t) count, NULL, NULL);

  entries = (const unsigned char *) ventries;
  for (i = 0; i < count; ++i)
    {
      mapassign1 (type, ret, entries, entries + val_offset);
      entries += entry_size;
    }

  return ret;
}

/* go-construct-map.c -- construct a map from an initializer.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "runtime.h"
#include "go-type.h"

extern void *makemap (const struct __go_map_type *, intgo hint,
		      void *)
  __asm__ (GOSYM_PREFIX "runtime.makemap");

extern void *mapassign (const struct __go_map_type *, void *hmap,
			const void *key)
  __asm__ (GOSYM_PREFIX "runtime.mapassign");

void *
__go_construct_map (const struct __go_map_type *type,
		    uintptr_t count, uintptr_t entry_size,
		    uintptr_t val_offset, const void *ventries)
{
  void *ret;
  const unsigned char *entries;
  uintptr_t i;
  void *p;

  ret = makemap(type, (intgo) count, NULL);

  entries = (const unsigned char *) ventries;
  for (i = 0; i < count; ++i)
    {
      p = mapassign (type, ret, entries);
      typedmemmove (type->__val_type, p, entries + val_offset);
      entries += entry_size;
    }

  return ret;
}

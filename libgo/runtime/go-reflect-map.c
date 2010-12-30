/* go-reflect-map.c -- map reflection support for Go.

   Copyright 2009, 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdlib.h>
#include <stdint.h>

#include "go-alloc.h"
#include "go-type.h"
#include "map.h"

/* This file implements support for reflection on maps.  These
   functions are called from reflect/value.go.  */

extern _Bool mapaccess (unsigned char *, unsigned char *, unsigned char *)
  asm ("libgo_reflect.reflect.mapaccess");

_Bool
mapaccess (unsigned char *m, unsigned char *key, unsigned char *val)
{
  struct __go_map *map = (struct __go_map *) m;
  void *p;
  const struct __go_type_descriptor *val_descriptor;

  p = __go_map_index (map, key, 0);
  if (p == NULL)
    return 0;
  else
    {
      val_descriptor = map->__descriptor->__map_descriptor->__val_type;
      __builtin_memcpy (val, p, val_descriptor->__size);
      return 1;
    }
}

extern void mapassign (unsigned char *, unsigned char *, unsigned char *)
  asm ("libgo_reflect.reflect.mapassign");

void
mapassign (unsigned char *m, unsigned char *key, unsigned char *val)
{
  struct __go_map *map = (struct __go_map *) m;

  if (val == NULL)
    __go_map_delete (map, key);
  else
    {
      void *p;
      const struct __go_type_descriptor *val_descriptor;

      p = __go_map_index (map, key, 1);
      val_descriptor = map->__descriptor->__map_descriptor->__val_type;
      __builtin_memcpy (p, val, val_descriptor->__size);
    }
}

extern int32_t maplen (unsigned char *)
  asm ("libgo_reflect.reflect.maplen");

int32_t
maplen (unsigned char *m __attribute__ ((unused)))
{
  struct __go_map *map = (struct __go_map *) m;
  return (int32_t) map->__element_count;
}

extern unsigned char *mapiterinit (unsigned char *)
  asm ("libgo_reflect.reflect.mapiterinit");

unsigned char *
mapiterinit (unsigned char *m)
{
  struct __go_hash_iter *it;

  it = __go_alloc (sizeof (struct __go_hash_iter));
  __go_mapiterinit ((struct __go_map *) m, it);
  return (unsigned char *) it;
}

extern void mapiternext (unsigned char *)
  asm ("libgo_reflect.reflect.mapiternext");

void
mapiternext (unsigned char *it)
{
  __go_mapiternext ((struct __go_hash_iter *) it);
}

extern _Bool mapiterkey (unsigned char *, unsigned char *)
  asm ("libgo_reflect.reflect.mapiterkey");

_Bool
mapiterkey (unsigned char *ita, unsigned char *key)
{
  struct __go_hash_iter *it = (struct __go_hash_iter *) ita;

  if (it->entry == NULL)
    return 0;
  else
    {
      __go_mapiter1 (it, key);
      return 1;
    }
}

/* Make a new map.  We have to build our own map descriptor.  */

extern unsigned char *makemap (const struct __go_map_type *)
  asm ("libgo_reflect.reflect.makemap");

unsigned char *
makemap (const struct __go_map_type *t)
{
  struct __go_map_descriptor *md;
  unsigned int o;
  const struct __go_type_descriptor *kt;
  const struct __go_type_descriptor *vt;

  /* FIXME: Reference count.  */
  md = (struct __go_map_descriptor *) __go_alloc (sizeof (*md));
  md->__map_descriptor = t;
  o = sizeof (void *);
  kt = t->__key_type;
  o = (o + kt->__field_align - 1) & ~ (kt->__field_align - 1);
  md->__key_offset = o;
  o += kt->__size;
  vt = t->__val_type;
  o = (o + vt->__field_align - 1) & ~ (vt->__field_align - 1);
  md->__val_offset = o;
  o += vt->__size;
  o = (o + sizeof (void *) - 1) & ~ (sizeof (void *) - 1);
  o = (o + kt->__field_align - 1) & ~ (kt->__field_align - 1);
  o = (o + vt->__field_align - 1) & ~ (vt->__field_align - 1);
  md->__entry_size = o;

  return (unsigned char *) __go_new_map (md, 0);
}

/* go-reflect-map.c -- map reflection support for Go.

   Copyright 2009, 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdlib.h>
#include <stdint.h>

#include "runtime.h"
#include "go-alloc.h"
#include "go-assert.h"
#include "go-type.h"
#include "map.h"

/* This file implements support for reflection on maps.  These
   functions are called from reflect/value.go.  */

struct mapaccess_ret
{
  uintptr_t val;
  _Bool pres;
};

extern struct mapaccess_ret mapaccess (struct __go_map_type *, uintptr_t,
				       uintptr_t)
  __asm__ (GOSYM_PREFIX "reflect.mapaccess");

struct mapaccess_ret
mapaccess (struct __go_map_type *mt, uintptr_t m, uintptr_t key_i)
{
  struct __go_map *map = (struct __go_map *) m;
  void *key;
  const struct __go_type_descriptor *key_descriptor;
  void *p;
  const struct __go_type_descriptor *val_descriptor;
  struct mapaccess_ret ret;
  void *val;
  void *pv;

  __go_assert (mt->__common.__code == GO_MAP);

  key_descriptor = mt->__key_type;
  if (__go_is_pointer_type (key_descriptor))
    key = &key_i;
  else
    key = (void *) key_i;

  if (map == NULL)
    p = NULL;
  else
    p = __go_map_index (map, key, 0);

  val_descriptor = mt->__val_type;
  if (__go_is_pointer_type (val_descriptor))
    {
      val = NULL;
      pv = &val;
    }
  else
    {
      val = __go_alloc (val_descriptor->__size);
      pv = val;
    }

  if (p == NULL)
    ret.pres = 0;
  else
    {
      __builtin_memcpy (pv, p, val_descriptor->__size);
      ret.pres = 1;
    }

  ret.val = (uintptr_t) val;
  return ret;
}

extern void mapassign (struct __go_map_type *, uintptr_t, uintptr_t,
		       uintptr_t, _Bool)
  __asm__ (GOSYM_PREFIX "reflect.mapassign");

void
mapassign (struct __go_map_type *mt, uintptr_t m, uintptr_t key_i,
	   uintptr_t val_i, _Bool pres)
{
  struct __go_map *map = (struct __go_map *) m;
  const struct __go_type_descriptor *key_descriptor;
  void *key;

  __go_assert (mt->__common.__code == GO_MAP);

  if (map == NULL)
    runtime_panicstring ("assignment to entry in nil map");

  key_descriptor = mt->__key_type;
  if (__go_is_pointer_type (key_descriptor))
    key = &key_i;
  else
    key = (void *) key_i;

  if (!pres)
    __go_map_delete (map, key);
  else
    {
      void *p;
      const struct __go_type_descriptor *val_descriptor;
      void *pv;

      p = __go_map_index (map, key, 1);

      val_descriptor = mt->__val_type;
      if (__go_is_pointer_type (val_descriptor))
	pv = &val_i;
      else
	pv = (void *) val_i;
      __builtin_memcpy (p, pv, val_descriptor->__size);
    }
}

extern int32_t maplen (uintptr_t)
  __asm__ (GOSYM_PREFIX "reflect.maplen");

int32_t
maplen (uintptr_t m)
{
  struct __go_map *map = (struct __go_map *) m;

  if (map == NULL)
    return 0;
  return (int32_t) map->__element_count;
}

extern unsigned char *mapiterinit (struct __go_map_type *, uintptr_t)
  __asm__ (GOSYM_PREFIX "reflect.mapiterinit");

unsigned char *
mapiterinit (struct __go_map_type *mt, uintptr_t m)
{
  struct __go_hash_iter *it;

  __go_assert (mt->__common.__code == GO_MAP);
  it = __go_alloc (sizeof (struct __go_hash_iter));
  __go_mapiterinit ((struct __go_map *) m, it);
  return (unsigned char *) it;
}

extern void mapiternext (unsigned char *)
  __asm__ (GOSYM_PREFIX "reflect.mapiternext");

void
mapiternext (unsigned char *it)
{
  __go_mapiternext ((struct __go_hash_iter *) it);
}

struct mapiterkey_ret
{
  uintptr_t key;
  _Bool ok;
};

extern struct mapiterkey_ret mapiterkey (unsigned char *)
  __asm__ (GOSYM_PREFIX "reflect.mapiterkey");

struct mapiterkey_ret
mapiterkey (unsigned char *ita)
{
  struct __go_hash_iter *it = (struct __go_hash_iter *) ita;
  struct mapiterkey_ret ret;

  if (it->entry == NULL)
    {
      ret.key = 0;
      ret.ok = 0;
    }
  else
    {
      const struct __go_type_descriptor *key_descriptor;
      void *key;
      void *pk;

      key_descriptor = it->map->__descriptor->__map_descriptor->__key_type;
      if (__go_is_pointer_type (key_descriptor))
	{
	  key = NULL;
	  pk = &key;
	}
      else
	{
	  key = __go_alloc (key_descriptor->__size);
	  pk = key;
	}

      __go_mapiter1 (it, pk);

      ret.key = (uintptr_t) key;
      ret.ok = 1;
    }

  return ret;
}

/* Make a new map.  We have to build our own map descriptor.  */

extern uintptr_t makemap (const struct __go_map_type *)
  __asm__ (GOSYM_PREFIX "reflect.makemap");

uintptr_t
makemap (const struct __go_map_type *t)
{
  struct __go_map_descriptor *md;
  unsigned int o;
  const struct __go_type_descriptor *kt;
  const struct __go_type_descriptor *vt;
  struct __go_map* map;
  void *ret;

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

  map = __go_new_map (md, 0);

  ret = __go_alloc (sizeof (void *));
  __builtin_memcpy (ret, &map, sizeof (void *));
  return (uintptr_t) ret;
}

extern _Bool ismapkey (const struct __go_type_descriptor *)
  __asm__ (GOSYM_PREFIX "reflect.ismapkey");

_Bool
ismapkey (const struct __go_type_descriptor *typ)
{
  return typ != NULL && typ->__hashfn != __go_type_hash_error;
}

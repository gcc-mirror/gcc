/* go-type-float.c -- hash and equality float functions.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "go-type.h"

/* The 32-bit and 64-bit types.  */

typedef unsigned int SItype __attribute__ ((mode (SI)));
typedef unsigned int DItype __attribute__ ((mode (DI)));

/* Hash function for float types.  */

uintptr_t
__go_type_hash_float (const void *vkey, uintptr_t key_size)
{
  if (key_size == 4)
    {
      union
      {
	unsigned char a[4];
	float f;
	SItype si;
      } uf;
      float f;

      __builtin_memcpy (uf.a, vkey, 4);
      f = uf.f;
      if (__builtin_isinff (f) || __builtin_isnanf (f) || f == 0)
	return 0;
      return (uintptr_t) uf.si;
    }
  else if (key_size == 8)
    {
      union
      {
	unsigned char a[8];
	double d;
	DItype di;
      } ud;
      double d;

      __builtin_memcpy (ud.a, vkey, 8);
      d = ud.d;
      if (__builtin_isinf (d) || __builtin_isnan (d) || d == 0)
	return 0;
      return (uintptr_t) ud.di;
    }
  else
    runtime_throw ("__go_type_hash_float: invalid float size");
}

/* Equality function for float types.  */

_Bool
__go_type_equal_float (const void *vk1, const void *vk2, uintptr_t key_size)
{
  if (key_size == 4)
    {
      union
      {
	unsigned char a[4];
	float f;
      } uf;
      float f1;
      float f2;

      __builtin_memcpy (uf.a, vk1, 4);
      f1 = uf.f;
      __builtin_memcpy (uf.a, vk2, 4);
      f2 = uf.f;
      return f1 == f2;
    }
  else if (key_size == 8)
    {
      union
      {
	unsigned char a[8];
	double d;
	DItype di;
      } ud;
      double d1;
      double d2;

      __builtin_memcpy (ud.a, vk1, 8);
      d1 = ud.d;
      __builtin_memcpy (ud.a, vk2, 8);
      d2 = ud.d;
      return d1 == d2;
    }
  else
    runtime_throw ("__go_type_equal_float: invalid float size");
}

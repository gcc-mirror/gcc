/* go-type-complex.c -- hash and equality complex functions.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "go-type.h"

/* The 64-bit type.  */

typedef unsigned int DItype __attribute__ ((mode (DI)));

/* Hash function for float types.  */

uintptr_t
__go_type_hash_complex (const void *vkey, uintptr_t key_size)
{
  if (key_size == 8)
    {
      union
      {
	unsigned char a[8];
	__complex float cf;
	DItype di;
      } ucf;
      __complex float cf;
      float cfr;
      float cfi;

      __builtin_memcpy (ucf.a, vkey, 8);
      cf = ucf.cf;
      cfr = __builtin_crealf (cf);
      cfi = __builtin_cimagf (cf);
      if (__builtin_isinff (cfr) || __builtin_isinff (cfi)
	  || __builtin_isnanf (cfr) || __builtin_isnanf (cfi))
	return 0;

      /* Avoid negative zero.  */
      if (cfr == 0 && cfi == 0)
	return 0;
      else if (cfr == 0)
	ucf.cf = cfi * 1.0iF;
      else if (cfi == 0)
	ucf.cf = cfr;

      return ucf.di;
    }
  else if (key_size == 16)
    {
      union
      {
	unsigned char a[16];
	__complex double cd;
	DItype adi[2];
      } ucd;
      __complex double cd;
      double cdr;
      double cdi;

      __builtin_memcpy (ucd.a, vkey, 16);
      cd = ucd.cd;
      cdr = __builtin_crealf (cd);
      cdi = __builtin_cimagf (cd);
      if (__builtin_isinf (cdr) || __builtin_isinf (cdi)
	  || __builtin_isnan (cdr) || __builtin_isnan (cdi))
	return 0;

      /* Avoid negative zero.  */
      if (cdr == 0 && cdi == 0)
	return 0;
      else if (cdr == 0)
	ucd.cd = cdi * 1.0i;
      else if (cdi == 0)
	ucd.cd = cdr;

      return ucd.adi[0] ^ ucd.adi[1];
    }
  else
    runtime_throw ("__go_type_hash_complex: invalid complex size");
}

/* Equality function for complex types.  */

_Bool
__go_type_equal_complex (const void *vk1, const void *vk2, uintptr_t key_size)
{
  if (key_size == 8)
    {
      union
      {
	unsigned char a[8];
	__complex float cf;
      } ucf;
      __complex float cf1;
      __complex float cf2;

      __builtin_memcpy (ucf.a, vk1, 8);
      cf1 = ucf.cf;
      __builtin_memcpy (ucf.a, vk2, 8);
      cf2 = ucf.cf;
      return cf1 == cf2;
    }
  else if (key_size == 16)
    {
      union
      {
	unsigned char a[16];
	__complex double cd;
      } ucd;
      __complex double cd1;
      __complex double cd2;

      __builtin_memcpy (ucd.a, vk1, 16);
      cd1 = ucd.cd;
      __builtin_memcpy (ucd.a, vk2, 16);
      cd2 = ucd.cd;
      return cd1 == cd2;
    }
  else
    runtime_throw ("__go_type_equal_complex: invalid complex size");
}

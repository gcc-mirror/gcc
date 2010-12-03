/* go-type-identity.c -- hash and equality identity functions.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "go-type.h"

/* Typedefs for accesses of different sizes.  */

typedef int QItype __attribute__ ((mode (QI)));
typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));

/* An identity hash function for a type.  This is used for types where
   we can simply use the type value itself as a hash code.  This is
   true of, e.g., integers and pointers.  */

size_t
__go_type_hash_identity (const void *key, size_t key_size)
{
  switch (key_size)
    {
    case 1:
      return *(const QItype *) key;
    case 2:
      return *(const HItype *) key;
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
      return *(const SItype *) key;
    default:
      return *(const DItype *) key;
    }
}

/* An identity equality function for a type.  This is used for types
   where we can check for equality by checking that the values have
   the same bits.  */

_Bool
__go_type_equal_identity (const void *k1, const void *k2, size_t key_size)
{
  return __builtin_memcmp (k1, k2, key_size) == 0;
}

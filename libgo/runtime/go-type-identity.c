/* go-type-identity.c -- hash and equality identity functions.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "runtime.h"
#include "go-type.h"

/* The hash functions for types that can compare as identity is
   written in Go.  */

extern uintptr runtime_memhash(void *, uintptr, uintptr)
  __asm__ (GOSYM_PREFIX "runtime.memhash");

const FuncVal __go_type_hash_identity_descriptor =
  { (void *) runtime_memhash };

/* An identity equality function for a type.  This is used for types
   where we can check for equality by checking that the values have
   the same bits.  */

_Bool
__go_type_equal_identity (const void *k1, const void *k2, uintptr_t key_size)
{
  return __builtin_memcmp (k1, k2, key_size) == 0;
}

const FuncVal __go_type_equal_identity_descriptor =
  { (void *) __go_type_equal_identity };

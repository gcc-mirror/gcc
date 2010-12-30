/* go-unsafe-pointer.c -- unsafe.Pointer type descriptor for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "go-string.h"
#include "go-type.h"

/* This file provides the type descriptor for the unsafe.Pointer type.
   The unsafe package is defined by the compiler itself, which means
   that there is no package to compile to define the type
   descriptor.  */

extern const struct __go_type_descriptor unsafe_Pointer
  asm ("__go_tdn_libgo_unsafe.unsafe.Pointer");

/* Used to determine the field alignment.  */
struct field_align
{
  char c;
  void *p;
};

/* The reflection string.  */
#define REFLECTION "unsafe.Pointer"
static const struct __go_string reflection_string =
{
  (const unsigned char *) REFLECTION,
  sizeof REFLECTION - 1
};

const struct __go_type_descriptor unsafe_Pointer =
{
  /* __code */
  GO_UNSAFE_POINTER,
  /* __align */
  __alignof (void *),
  /* __field_align */
  offsetof (struct field_align, p) - 1,
  /* __size */
  sizeof (void *),
  /* __hash */
  78501163U,
  /* __hashfn */
  __go_type_hash_identity,
  /* __equalfn */
  __go_type_equal_identity,
  /* __reflection */
  &reflection_string,
  /* __uncommon */
  NULL
};

/* We also need the type descriptor for the pointer to unsafe.Pointer,
   since any package which refers to that type descriptor will expect
   it to be defined elsewhere.  */

extern const struct __go_ptr_type pointer_unsafe_Pointer
  asm ("__go_td_pN27_libgo_unsafe.unsafe.Pointer");

/* The reflection string.  */
#define PREFLECTION "*unsafe.Pointer"
static const struct __go_string preflection_string =
{
  (const unsigned char *) PREFLECTION,
  sizeof PREFLECTION - 1,
};

const struct __go_ptr_type pointer_unsafe_Pointer =
{
  /* __common */
  {
    /* __code */
    GO_PTR,
    /* __align */
    __alignof (void *),
    /* __field_align */
    offsetof (struct field_align, p) - 1,
    /* __size */
    sizeof (void *),
    /* __hash */
    1256018616U,
    /* __hashfn */
    __go_type_hash_identity,
    /* __equalfn */
    __go_type_equal_identity,
    /* __reflection */
    &preflection_string,
    /* __uncommon */
    NULL
  },
  /* __element_type */
  &unsafe_Pointer
};

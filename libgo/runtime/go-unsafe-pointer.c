/* go-unsafe-pointer.c -- unsafe.Pointer type descriptor for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "runtime.h"
#include "go-type.h"

/* This file provides the type descriptor for the unsafe.Pointer type.
   The unsafe package is defined by the compiler itself, which means
   that there is no package to compile to define the type
   descriptor.  */

extern const struct __go_type_descriptor unsafe_Pointer
  __asm__ (GOSYM_PREFIX "unsafe.Pointer..d");

extern const byte unsafe_Pointer_gc[]
  __asm__ (GOSYM_PREFIX "unsafe.Pointer..g");

/* Used to determine the field alignment.  */
struct field_align
{
  char c;
  void *p;
};

/* The reflection string.  */
#define REFLECTION "unsafe.Pointer"
static const String reflection_string =
{
  (const byte *) REFLECTION,
  sizeof REFLECTION - 1
};

const byte unsafe_Pointer_gc[] = { 1 };

extern const FuncVal runtime_pointerhash_descriptor
  __asm__ (GOSYM_PREFIX "runtime.pointerhash..f");
extern const FuncVal runtime_pointerequal_descriptor
  __asm__ (GOSYM_PREFIX "runtime.pointerequal..f");

const struct __go_type_descriptor unsafe_Pointer =
{
  /* __size */
  sizeof (void *),
  /* __ptrdata */
  sizeof (void *),
  /* __hash */
  78501163U,
  /* __code */
  GO_UNSAFE_POINTER | GO_DIRECT_IFACE,
  /* __align */
  __alignof (void *),
  /* __field_align */
  offsetof (struct field_align, p) - 1,
  /* __hashfn */
  &runtime_pointerhash_descriptor,
  /* __equalfn */
  &runtime_pointerequal_descriptor,
  /* __gcdata */
  unsafe_Pointer_gc,
  /* __reflection */
  &reflection_string,
  /* __uncommon */
  NULL,
  /* __pointer_to_this */
  NULL
};

/* We also need the type descriptor for the pointer to unsafe.Pointer,
   since any package which refers to that type descriptor will expect
   it to be defined elsewhere.  */

extern const struct __go_ptr_type pointer_unsafe_Pointer
  __asm__ (GOSYM_PREFIX "type...1unsafe.Pointer");

/* The reflection string.  */
#define PREFLECTION "*unsafe.Pointer"
static const String preflection_string =
{
  (const byte *) PREFLECTION,
  sizeof PREFLECTION - 1,
};

extern const byte pointer_unsafe_Pointer_gc[]
  __asm__ (GOSYM_PREFIX "type...1unsafe.Pointer..g");

const byte pointer_unsafe_Pointer_gc[] = { 1 };

const struct __go_ptr_type pointer_unsafe_Pointer =
{
  /* __common */
  {
    /* __size */
    sizeof (void *),
    /* __ptrdata */
    sizeof (void *),
    /* __hash */
    1256018616U,
    /* __code */
    GO_PTR | GO_DIRECT_IFACE,
    /* __align */
    __alignof (void *),
    /* __field_align */
    offsetof (struct field_align, p) - 1,
    /* __hashfn */
    &runtime_pointerhash_descriptor,
    /* __equalfn */
    &runtime_pointerequal_descriptor,
    /* __gcdata */
    pointer_unsafe_Pointer_gc,
    /* __reflection */
    &preflection_string,
    /* __uncommon */
    NULL,
    /* __pointer_to_this */
    NULL
  },
  /* __element_type */
  &unsafe_Pointer
};

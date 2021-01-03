/* go-unsafe-pointer.c -- unsafe.Pointer type descriptor for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "runtime.h"

/* This file provides the type descriptor for the unsafe.Pointer type.
   The unsafe package is defined by the compiler itself, which means
   that there is no package to compile to define the type
   descriptor.  */

extern const struct _type unsafe_Pointer
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

extern const FuncVal runtime_pointerequal_descriptor
  __asm__ (GOSYM_PREFIX "runtime.pointerequal..f");

const struct _type unsafe_Pointer =
{
  /* size */
  sizeof (void *),
  /* ptrdata */
  sizeof (void *),
  /* hash */
  78501163U,
  /* tflag */
  tflagRegularMemory,
  /* align */
  __alignof (void *),
  /* fieldAlign */
  offsetof (struct field_align, p) - 1,
  /* kind */
  kindUnsafePointer | kindDirectIface,
  /* equal */
  &runtime_pointerequal_descriptor,
  /* gcdata */
  unsafe_Pointer_gc,
  /* _string */
  &reflection_string,
  /* uncommontype */
  NULL,
  /* ptrToThis */
  NULL
};

/* We also need the type descriptor for the pointer to unsafe.Pointer,
   since any package which refers to that type descriptor will expect
   it to be defined elsewhere.  */

extern const struct ptrtype pointer_unsafe_Pointer
  __asm__ (GOSYM_PREFIX "unsafe.Pointer..p");

/* The reflection string.  */
#define PREFLECTION "*unsafe.Pointer"
static const String preflection_string =
{
  (const byte *) PREFLECTION,
  sizeof PREFLECTION - 1,
};

extern const byte pointer_unsafe_Pointer_gc[]
  __asm__ (GOSYM_PREFIX "unsafe.Pointer..p..g");

const byte pointer_unsafe_Pointer_gc[] = { 1 };

const struct ptrtype pointer_unsafe_Pointer =
{
  /* type */
  {
    /* size */
    sizeof (void *),
    /* ptrdata */
    sizeof (void *),
    /* hash */
    1256018616U,
    /* tflag */
    tflagRegularMemory,
    /* align */
    __alignof (void *),
    /* fieldAlign */
    offsetof (struct field_align, p) - 1,
    /* kind */
    kindPtr | kindDirectIface,
    /* equalfn */
    &runtime_pointerequal_descriptor,
    /* gcdata */
    pointer_unsafe_Pointer_gc,
    /* _string */
    &preflection_string,
    /* uncommontype */
    NULL,
    /* ptrToThis */
    NULL
  },
  /* elem */
  &unsafe_Pointer
};

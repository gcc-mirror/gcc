/* go-unsafe-new.c -- unsafe.New function for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "arch.h"
#include "malloc.h"
#include "go-type.h"
#include "interface.h"

/* Implement unsafe_New, called from the reflect package.  */

void *unsafe_New (const struct __go_type_descriptor *)
  __asm__ (GOSYM_PREFIX "reflect.unsafe_New");

/* The dynamic type of the argument will be a pointer to a type
   descriptor.  */

void *
unsafe_New (const struct __go_type_descriptor *descriptor)
{
  uint32 flag;
  void *ret;

  flag = (descriptor->__code & GO_NO_POINTERS) != 0 ? FlagNoPointers : 0;
  ret = runtime_mallocgc (descriptor->__size, flag, 1, 1);

  if (UseSpanType && flag == 0)
    runtime_settype (ret, (uintptr) descriptor | TypeInfo_SingleObject);

  return ret;
}

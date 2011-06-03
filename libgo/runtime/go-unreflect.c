/* go-unreflect.c -- implement unsafe.Unreflect for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-alloc.h"
#include "go-panic.h"
#include "go-type.h"
#include "interface.h"

/* Implement unsafe.Unreflect.  */

struct __go_empty_interface Unreflect (struct __go_empty_interface type,
				       void *object)
  asm ("libgo_unsafe.unsafe.Unreflect");

struct __go_empty_interface
Unreflect (struct __go_empty_interface type, void *object)
{
  struct __go_empty_interface ret;

  if (((uintptr_t) type.__type_descriptor & reflectFlags) != 0)
    __go_panic_msg ("invalid interface value");

  /* FIXME: We should check __type_descriptor to verify that this is
     really a type descriptor.  */
  ret.__type_descriptor = type.__object;
  if (__go_is_pointer_type (ret.__type_descriptor))
    ret.__object = *(void **) object;
  else
    ret.__object = object;
  return ret;
}

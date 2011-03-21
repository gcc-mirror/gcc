/* go-reflect.c -- implement unsafe.Reflect and unsafe.Typeof for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdlib.h>
#include <stdint.h>

#include "interface.h"
#include "go-alloc.h"
#include "go-panic.h"
#include "go-string.h"
#include "go-type.h"

/* For field alignment.  */

struct field_align
{
  char c;
  struct __go_type_descriptor *p;
};

/* The type descriptors in the runtime package.  */

extern const struct __go_type_descriptor ptr_bool_descriptor
  asm ("__go_td_pN30_libgo_runtime.runtime.BoolType");
extern const struct __go_type_descriptor ptr_float_descriptor
  asm ("__go_td_pN31_libgo_runtime.runtime.FloatType");
extern const struct __go_type_descriptor ptr_complex_descriptor
  asm ("__go_td_pN33_libgo_runtime.runtime.ComplexType");
extern const struct __go_type_descriptor ptr_int_descriptor
  asm ("__go_td_pN29_libgo_runtime.runtime.IntType");
extern const struct __go_type_descriptor ptr_uint_descriptor
  asm ("__go_td_pN30_libgo_runtime.runtime.UintType");
extern const struct __go_type_descriptor ptr_string_descriptor
  asm ("__go_td_pN32_libgo_runtime.runtime.StringType");
extern const struct __go_type_descriptor ptr_unsafe_pointer_decriptor
  asm ("__go_td_pN39_libgo_runtime.runtime.UnsafePointerType");
extern const struct __go_type_descriptor ptr_array_descriptor
  asm ("__go_td_pN31_libgo_runtime.runtime.ArrayType");
extern const struct __go_type_descriptor ptr_slice_descriptor
  asm ("__go_td_pN31_libgo_runtime.runtime.SliceType");
extern const struct __go_type_descriptor ptr_chan_descriptor
  asm ("__go_td_pN30_libgo_runtime.runtime.ChanType");
extern const struct __go_type_descriptor ptr_func_descriptor
  asm ("__go_td_pN30_libgo_runtime.runtime.FuncType");
extern const struct __go_type_descriptor ptr_interface_descriptor
  asm ("__go_td_pN35_libgo_runtime.runtime.InterfaceType");
extern const struct __go_type_descriptor ptr_map_descriptor
  asm ("__go_td_pN29_libgo_runtime.runtime.MapType");
extern const struct __go_type_descriptor ptr_ptr_descriptor
  asm ("__go_td_pN29_libgo_runtime.runtime.PtrType");
extern const struct __go_type_descriptor ptr_struct_descriptor
  asm ("__go_td_pN32_libgo_runtime.runtime.StructType");

const struct __go_type_descriptor *
get_descriptor (int code)
{
  switch (code)
    {
    case GO_BOOL:
      return &ptr_bool_descriptor;
    case GO_FLOAT32:
    case GO_FLOAT64:
      return &ptr_float_descriptor;
    case GO_COMPLEX64:
    case GO_COMPLEX128:
      return &ptr_complex_descriptor;
    case GO_INT16:
    case GO_INT32:
    case GO_INT64:
    case GO_INT8:
    case GO_INT:
      return &ptr_int_descriptor;
    case GO_UINT16:
    case GO_UINT32:
    case GO_UINT64:
    case GO_UINT8:
    case GO_UINTPTR:
    case GO_UINT:
      return &ptr_uint_descriptor;
    case GO_STRING:
      return &ptr_string_descriptor;
    case GO_UNSAFE_POINTER:
      return &ptr_unsafe_pointer_decriptor;
    case GO_ARRAY:
      return &ptr_array_descriptor;
    case GO_SLICE:
      return &ptr_slice_descriptor;
    case GO_CHAN:
      return &ptr_chan_descriptor;
    case GO_FUNC:
      return &ptr_func_descriptor;
    case GO_INTERFACE:
      return &ptr_interface_descriptor;
    case GO_MAP:
      return &ptr_map_descriptor;
    case GO_PTR:
      return &ptr_ptr_descriptor;
    case GO_STRUCT:
      return &ptr_struct_descriptor;
    default:
      abort ();
    }
}

/* Implement unsafe.Reflect.  */

struct reflect_ret
{
  struct __go_empty_interface rettype;
  void *addr;
};

struct reflect_ret Reflect (struct __go_empty_interface)
  asm ("libgo_unsafe.unsafe.Reflect");

struct reflect_ret
Reflect (struct __go_empty_interface e)
{
  struct reflect_ret ret;

  if (e.__type_descriptor == NULL)
    {
      ret.rettype.__type_descriptor = NULL;
      ret.rettype.__object = NULL;
      ret.addr = NULL;
    }
  else
    {
      size_t size;

      ret.rettype.__type_descriptor =
	get_descriptor (e.__type_descriptor->__code);

      /* This memcpy is really just an assignment of a const pointer
	 to a non-const pointer.  FIXME: We should canonicalize this
	 pointer, so that for a given type we always return the same
	 pointer.  */
      __builtin_memcpy (&ret.rettype.__object, &e.__type_descriptor,
			sizeof (void *));

      /* Make a copy of the value.  */
      size = e.__type_descriptor->__size;
      if (size <= sizeof (uint64_t))
	ret.addr = __go_alloc (sizeof (uint64_t));
      else
	ret.addr = __go_alloc (size);
      if (__go_is_pointer_type (e.__type_descriptor))
	*(void **) ret.addr = e.__object;
      else
	__builtin_memcpy (ret.addr, e.__object, size);
    }

  return ret;
}

/* Implement unsafe.Typeof.  */

struct __go_empty_interface Typeof (struct __go_empty_interface)
  asm ("libgo_unsafe.unsafe.Typeof");

struct __go_empty_interface
Typeof (const struct __go_empty_interface e)
{
  struct __go_empty_interface ret;

  if (e.__type_descriptor == NULL)
    {
      ret.__type_descriptor = NULL;
      ret.__object = NULL;
    }
  else
    {
      ret.__type_descriptor = get_descriptor (e.__type_descriptor->__code);

      /* This memcpy is really just an assignment of a const pointer
	 to a non-const pointer.  FIXME: We should canonicalize this
	 pointer, so that for a given type we always return the same
	 pointer.  */
      __builtin_memcpy (&ret.__object, &e.__type_descriptor, sizeof (void *));
    }

  return ret;
}

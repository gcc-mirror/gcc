/* go-reflect-call.c -- call reflection support for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdio.h>
#include <stdlib.h>

#include "ffi.h"

#include "go-alloc.h"
#include "go-assert.h"
#include "go-type.h"
#include "runtime.h"

/* Forward declaration.  */

static ffi_type *go_type_to_ffi (const struct __go_type_descriptor *);

/* Return an ffi_type for a Go array type.  The libffi library does
   not have any builtin support for passing arrays as values.  We work
   around this by pretending that the array is a struct.  */

static ffi_type *
go_array_to_ffi (const struct __go_array_type *descriptor)
{
  ffi_type *ret;
  uintptr_t len;
  ffi_type *element;
  uintptr_t i;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  __builtin_memset (ret, 0, sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  len = descriptor->__len;
  ret->elements = (ffi_type **) __go_alloc ((len + 1) * sizeof (ffi_type *));
  element = go_type_to_ffi (descriptor->__element_type);
  for (i = 0; i < len; ++i)
    ret->elements[i] = element;
  ret->elements[len] = NULL;
  return ret;
}

/* Return an ffi_type for a Go slice type.  This describes the
   __go_open_array type defines in array.h.  */

static ffi_type *
go_slice_to_ffi (
    const struct __go_slice_type *descriptor __attribute__ ((unused)))
{
  ffi_type *ret;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  __builtin_memset (ret, 0, sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  ret->elements = (ffi_type **) __go_alloc (4 * sizeof (ffi_type *));
  ret->elements[0] = &ffi_type_pointer;
  ret->elements[1] = &ffi_type_sint;
  ret->elements[2] = &ffi_type_sint;
  ret->elements[3] = NULL;
  return ret;
}

/* Return an ffi_type for a Go struct type.  */

static ffi_type *
go_struct_to_ffi (const struct __go_struct_type *descriptor)
{
  ffi_type *ret;
  int field_count;
  const struct __go_struct_field *fields;
  int i;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  __builtin_memset (ret, 0, sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  field_count = descriptor->__fields.__count;
  fields = (const struct __go_struct_field *) descriptor->__fields.__values;
  ret->elements = (ffi_type **) __go_alloc ((field_count + 1)
					    * sizeof (ffi_type *));
  for (i = 0; i < field_count; ++i)
    ret->elements[i] = go_type_to_ffi (fields[i].__type);
  ret->elements[field_count] = NULL;
  return ret;
}

/* Return an ffi_type for a Go string type.  This describes the
   __go_string struct.  */

static ffi_type *
go_string_to_ffi (void)
{
  ffi_type *ret;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  ret->elements = (ffi_type **) __go_alloc (3 * sizeof (ffi_type *));
  ret->elements[0] = &ffi_type_pointer;
  ret->elements[1] = &ffi_type_sint;
  ret->elements[2] = NULL;
  return ret;
}

/* Return an ffi_type for a Go interface type.  This describes the
   __go_interface and __go_empty_interface structs.  */

static ffi_type *
go_interface_to_ffi (void)
{
  ffi_type *ret;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  ret->elements = (ffi_type **) __go_alloc (3 * sizeof (ffi_type *));
  ret->elements[0] = &ffi_type_pointer;
  ret->elements[1] = &ffi_type_pointer;
  ret->elements[2] = NULL;
  return ret;
}

/* Return an ffi_type for a Go complex type.  */

static ffi_type *
go_complex_to_ffi (ffi_type *float_type)
{
  ffi_type *ret;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  ret->elements = (ffi_type **) __go_alloc (3 * sizeof (ffi_type *));
  ret->elements[0] = float_type;
  ret->elements[1] = float_type;
  ret->elements[2] = NULL;
  return ret;
}

/* Return an ffi_type for a type described by a
   __go_type_descriptor.  */

static ffi_type *
go_type_to_ffi (const struct __go_type_descriptor *descriptor)
{
  switch (descriptor->__code)
    {
    case GO_BOOL:
      if (sizeof (_Bool) == 1)
	return &ffi_type_uint8;
      else if (sizeof (_Bool) == sizeof (int))
	return &ffi_type_uint;
      abort ();
    case GO_FLOAT32:
      if (sizeof (float) == 4)
	return &ffi_type_float;
      abort ();
    case GO_FLOAT64:
      if (sizeof (double) == 8)
	return &ffi_type_double;
      abort ();
    case GO_COMPLEX64:
      if (sizeof (float) == 4)
	return go_complex_to_ffi (&ffi_type_float);
      abort ();
    case GO_COMPLEX128:
      if (sizeof (double) == 8)
	return go_complex_to_ffi (&ffi_type_double);
      abort ();
    case GO_INT16:
      return &ffi_type_sint16;
    case GO_INT32:
      return &ffi_type_sint32;
    case GO_INT64:
      return &ffi_type_sint64;
    case GO_INT8:
      return &ffi_type_sint8;
    case GO_INT:
      return &ffi_type_sint;
    case GO_UINT16:
      return &ffi_type_uint16;
    case GO_UINT32:
      return &ffi_type_uint32;
    case GO_UINT64:
      return &ffi_type_uint64;
    case GO_UINT8:
      return &ffi_type_uint8;
    case GO_UINT:
      return &ffi_type_uint;
    case GO_UINTPTR:
      if (sizeof (void *) == 2)
	return &ffi_type_uint16;
      else if (sizeof (void *) == 4)
	return &ffi_type_uint32;
      else if (sizeof (void *) == 8)
	return &ffi_type_uint64;
      abort ();
    case GO_ARRAY:
      return go_array_to_ffi ((const struct __go_array_type *) descriptor);
    case GO_SLICE:
      return go_slice_to_ffi ((const struct __go_slice_type *) descriptor);
    case GO_STRUCT:
      return go_struct_to_ffi ((const struct __go_struct_type *) descriptor);
    case GO_STRING:
      return go_string_to_ffi ();
    case GO_INTERFACE:
      return go_interface_to_ffi ();
    case GO_CHAN:
    case GO_FUNC:
    case GO_MAP:
    case GO_PTR:
    case GO_UNSAFE_POINTER:
      /* These types are always pointers, and for FFI purposes nothing
	 else matters.  */
      return &ffi_type_pointer;
    default:
      abort ();
    }
}

/* Return the return type for a function, given the number of out
   parameters and their types.  */

static ffi_type *
go_func_return_ffi (const struct __go_func_type *func)
{
  int count;
  const struct __go_type_descriptor **types;
  ffi_type *ret;
  int i;

  count = func->__out.__count;
  if (count == 0)
    return &ffi_type_void;

  types = (const struct __go_type_descriptor **) func->__out.__values;

  if (count == 1)
    return go_type_to_ffi (types[0]);

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  __builtin_memset (ret, 0, sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  ret->elements = (ffi_type **) __go_alloc ((count + 1) * sizeof (ffi_type *));
  for (i = 0; i < count; ++i)
    ret->elements[i] = go_type_to_ffi (types[i]);
  ret->elements[count] = NULL;
  return ret;
}

/* Build an ffi_cif structure for a function described by a
   __go_func_type structure.  */

static void
go_func_to_cif (const struct __go_func_type *func, _Bool is_interface,
		ffi_cif *cif)
{
  int num_params;
  const struct __go_type_descriptor **in_types;
  size_t num_args;
  ffi_type **args;
  int off;
  int i;
  ffi_type *rettype;
  ffi_status status;

  num_params = func->__in.__count;
  in_types = ((const struct __go_type_descriptor **)
	      func->__in.__values);

  num_args = num_params + (is_interface ? 1 : 0);
  args = (ffi_type **) __go_alloc (num_args * sizeof (ffi_type *));
  if (is_interface)
    args[0] = &ffi_type_pointer;
  off = is_interface ? 1 : 0;
  for (i = 0; i < num_params; ++i)
    args[i + off] = go_type_to_ffi (in_types[i]);

  rettype = go_func_return_ffi (func);

  status = ffi_prep_cif (cif, FFI_DEFAULT_ABI, num_args, rettype, args);
  __go_assert (status == FFI_OK);
}

/* Get the total size required for the result parameters of a
   function.  */

static size_t
go_results_size (const struct __go_func_type *func)
{
  int count;
  const struct __go_type_descriptor **types;
  size_t off;
  size_t maxalign;
  int i;

  count = func->__out.__count;
  if (count == 0)
    return 0;

  types = (const struct __go_type_descriptor **) func->__out.__values;

  off = 0;
  maxalign = 0;
  for (i = 0; i < count; ++i)
    {
      size_t align;

      align = types[i]->__field_align;
      if (align > maxalign)
	maxalign = align;
      off = (off + align - 1) & ~ (align - 1);
      off += types[i]->__size;
    }

  off = (off + maxalign - 1) & ~ (maxalign - 1);

  return off;
}

/* Copy the results of calling a function via FFI from CALL_RESULT
   into the addresses in RESULTS.  */

static void
go_set_results (const struct __go_func_type *func, unsigned char *call_result,
		void **results)
{
  int count;
  const struct __go_type_descriptor **types;
  size_t off;
  int i;

  count = func->__out.__count;
  if (count == 0)
    return;

  types = (const struct __go_type_descriptor **) func->__out.__values;

  off = 0;
  for (i = 0; i < count; ++i)
    {
      size_t align;
      size_t size;

      align = types[i]->__field_align;
      size = types[i]->__size;
      off = (off + align - 1) & ~ (align - 1);
      __builtin_memcpy (results[i], call_result + off, size);
      off += size;
    }
}

/* Call a function.  The type of the function is FUNC_TYPE, and the
   address is FUNC_ADDR.  PARAMS is an array of parameter addresses.
   RESULTS is an array of result addresses.  */

void
reflect_call (const struct __go_func_type *func_type, const void *func_addr,
	      _Bool is_interface, void **params, void **results)
{
  ffi_cif cif;
  unsigned char *call_result;

  __go_assert (func_type->__common.__code == GO_FUNC);
  go_func_to_cif (func_type, is_interface, &cif);

  call_result = (unsigned char *) malloc (go_results_size (func_type));

  ffi_call (&cif, func_addr, call_result, params);

  /* Some day we may need to free result values if RESULTS is
     NULL.  */
  if (results != NULL)
    go_set_results (func_type, call_result, results);

  free (call_result);
}

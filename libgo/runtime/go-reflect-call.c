/* go-reflect-call.c -- call reflection support for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "runtime.h"
#include "go-alloc.h"
#include "go-assert.h"
#include "go-type.h"

#ifdef USE_LIBFFI

#include "ffi.h"

/* The functions in this file are only called from reflect_call.  As
   reflect_call calls a libffi function, which will be compiled
   without -fsplit-stack, it will always run with a large stack.  */

static ffi_type *go_array_to_ffi (const struct __go_array_type *)
  __attribute__ ((no_split_stack));
static ffi_type *go_slice_to_ffi (const struct __go_slice_type *)
  __attribute__ ((no_split_stack));
static ffi_type *go_struct_to_ffi (const struct __go_struct_type *)
  __attribute__ ((no_split_stack));
static ffi_type *go_string_to_ffi (void) __attribute__ ((no_split_stack));
static ffi_type *go_interface_to_ffi (void) __attribute__ ((no_split_stack));
static ffi_type *go_complex_to_ffi (ffi_type *)
  __attribute__ ((no_split_stack, unused));
static ffi_type *go_type_to_ffi (const struct __go_type_descriptor *)
  __attribute__ ((no_split_stack));
static ffi_type *go_func_return_ffi (const struct __go_func_type *)
  __attribute__ ((no_split_stack));
static void go_func_to_cif (const struct __go_func_type *, _Bool, _Bool,
			    ffi_cif *)
  __attribute__ ((no_split_stack));
static size_t go_results_size (const struct __go_func_type *)
  __attribute__ ((no_split_stack));
static void go_set_results (const struct __go_func_type *, unsigned char *,
			    void **)
  __attribute__ ((no_split_stack));

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
  ffi_type *ffi_intgo;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  ret->elements = (ffi_type **) __go_alloc (4 * sizeof (ffi_type *));
  ret->elements[0] = &ffi_type_pointer;
  ffi_intgo = sizeof (intgo) == 4 ? &ffi_type_sint32 : &ffi_type_sint64;
  ret->elements[1] = ffi_intgo;
  ret->elements[2] = ffi_intgo;
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

/* Return an ffi_type for a Go string type.  This describes the String
   struct.  */

static ffi_type *
go_string_to_ffi (void)
{
  ffi_type *ret;
  ffi_type *ffi_intgo;

  ret = (ffi_type *) __go_alloc (sizeof (ffi_type));
  ret->type = FFI_TYPE_STRUCT;
  ret->elements = (ffi_type **) __go_alloc (3 * sizeof (ffi_type *));
  ret->elements[0] = &ffi_type_pointer;
  ffi_intgo = sizeof (intgo) == 4 ? &ffi_type_sint32 : &ffi_type_sint64;
  ret->elements[1] = ffi_intgo;
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
  switch (descriptor->__code & GO_CODE_MASK)
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
#ifdef __alpha__
      runtime_throw("the libffi library does not support Complex64 type with "
		    "reflect.Call or runtime.SetFinalizer");
#else
      if (sizeof (float) == 4)
	return go_complex_to_ffi (&ffi_type_float);
      abort ();
#endif
    case GO_COMPLEX128:
#ifdef __alpha__
      runtime_throw("the libffi library does not support Complex128 type with "
		    "reflect.Call or runtime.SetFinalizer");
#else
      if (sizeof (double) == 8)
	return go_complex_to_ffi (&ffi_type_double);
      abort ();
#endif
    case GO_INT16:
      return &ffi_type_sint16;
    case GO_INT32:
      return &ffi_type_sint32;
    case GO_INT64:
      return &ffi_type_sint64;
    case GO_INT8:
      return &ffi_type_sint8;
    case GO_INT:
      return sizeof (intgo) == 4 ? &ffi_type_sint32 : &ffi_type_sint64;
    case GO_UINT16:
      return &ffi_type_uint16;
    case GO_UINT32:
      return &ffi_type_uint32;
    case GO_UINT64:
      return &ffi_type_uint64;
    case GO_UINT8:
      return &ffi_type_uint8;
    case GO_UINT:
      return sizeof (uintgo) == 4 ? &ffi_type_uint32 : &ffi_type_uint64;
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
		_Bool is_method, ffi_cif *cif)
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

  num_args = (num_params
	      + (is_interface ? 1 : 0)
	      + (!is_interface && !is_method ? 1 : 0));
  args = (ffi_type **) __go_alloc (num_args * sizeof (ffi_type *));
  i = 0;
  off = 0;
  if (is_interface)
    {
      args[0] = &ffi_type_pointer;
      off = 1;
    }
  else if (is_method)
    {
      args[0] = &ffi_type_pointer;
      i = 1;
    }
  for (; i < num_params; ++i)
    args[i + off] = go_type_to_ffi (in_types[i]);

  if (!is_interface && !is_method)
    {
      // There is a closure argument, a pointer.
      args[i + off] = &ffi_type_pointer;
    }

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

  /* A single integer return value is always promoted to a full
     word.  */
  if (count == 1)
    {
      switch (types[0]->__code & GO_CODE_MASK)
	{
	case GO_BOOL:
	case GO_INT8:
	case GO_INT16:
	case GO_INT32:
	case GO_UINT8:
	case GO_UINT16:
	case GO_UINT32:
	case GO_INT:
	case GO_UINT:
	  return sizeof (ffi_arg);

	default:
	  break;
	}
    }

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

  /* A single integer return value is always promoted to a full
     word.  */
  if (count == 1)
    {
      switch (types[0]->__code & GO_CODE_MASK)
	{
	case GO_BOOL:
	case GO_INT8:
	case GO_INT16:
	case GO_INT32:
	case GO_UINT8:
	case GO_UINT16:
	case GO_UINT32:
	case GO_INT:
	case GO_UINT:
	  {
	    union
	    {
	      unsigned char buf[sizeof (ffi_arg)];
	      ffi_arg v;
	    } u;
	    ffi_arg v;

	    __builtin_memcpy (&u.buf, call_result, sizeof (ffi_arg));
	    v = u.v;

	    switch (types[0]->__size)
	      {
	      case 1:
		{
		  uint8_t b;

		  b = (uint8_t) v;
		  __builtin_memcpy (results[0], &b, 1);
		}
		break;

	      case 2:
		{
		  uint16_t s;

		  s = (uint16_t) v;
		  __builtin_memcpy (results[0], &s, 2);
		}
		break;

	      case 4:
		{
		  uint32_t w;

		  w = (uint32_t) v;
		  __builtin_memcpy (results[0], &w, 4);
		}
		break;

	      case 8:
		{
		  uint64_t d;

		  d = (uint64_t) v;
		  __builtin_memcpy (results[0], &d, 8);
		}
		break;

	      default:
		abort ();
	      }
	  }
	  return;

	default:
	  break;
	}
    }

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
   closure is FUNC_VAL.  PARAMS is an array of parameter addresses.
   RESULTS is an array of result addresses.

   If IS_INTERFACE is true this is a call to an interface method and
   the first argument is the receiver, which is always a pointer.
   This argument, the receiver, is not described in FUNC_TYPE.

   If IS_METHOD is true this is a call to a method expression.  The
   first argument is the receiver.  It is described in FUNC_TYPE, but
   regardless of FUNC_TYPE, it is passed as a pointer.

   If neither IS_INTERFACE nor IS_METHOD is true then we are calling a
   function indirectly, and the caller is responsible for passing a
   trailing closure argument, a pointer, which is not described in
   FUNC_TYPE.  */

void
reflect_call (const struct __go_func_type *func_type, FuncVal *func_val,
	      _Bool is_interface, _Bool is_method, void **params,
	      void **results)
{
  ffi_cif cif;
  unsigned char *call_result;

  __go_assert ((func_type->__common.__code & GO_CODE_MASK) == GO_FUNC);
  go_func_to_cif (func_type, is_interface, is_method, &cif);

  call_result = (unsigned char *) malloc (go_results_size (func_type));

  ffi_call (&cif, func_val->fn, call_result, params);

  /* Some day we may need to free result values if RESULTS is
     NULL.  */
  if (results != NULL)
    go_set_results (func_type, call_result, results);

  free (call_result);
}

#else /* !defined(USE_LIBFFI) */

void
reflect_call (const struct __go_func_type *func_type __attribute__ ((unused)),
	      FuncVal *func_val __attribute__ ((unused)),
	      _Bool is_interface __attribute__ ((unused)),
	      _Bool is_method __attribute__ ((unused)),
	      void **params __attribute__ ((unused)),
	      void **results __attribute__ ((unused)))
{
  /* Without FFI there is nothing we can do.  */
  runtime_throw("libgo built without FFI does not support "
		"reflect.Call or runtime.SetFinalizer");
}

#endif /* !defined(USE_LIBFFI) */

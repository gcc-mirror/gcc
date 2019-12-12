/* go-runtime-error.c -- Go runtime error.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

/* The compiler generates calls to this function.  This enum values
   are known to the compiler and used by compiled code.  Any change
   here must be reflected in the compiler.  */

enum
{
  /* Slice index out of bounds: negative or larger than the length of
     the slice.  */
  SLICE_INDEX_OUT_OF_BOUNDS = 0,

  /* Array index out of bounds.  */
  ARRAY_INDEX_OUT_OF_BOUNDS = 1,

  /* String index out of bounds.  */
  STRING_INDEX_OUT_OF_BOUNDS = 2,

  /* Slice slice out of bounds: negative or larger than the length of
     the slice or high bound less than low bound.  */
  SLICE_SLICE_OUT_OF_BOUNDS = 3,

  /* Array slice out of bounds.  */
  ARRAY_SLICE_OUT_OF_BOUNDS = 4,

  /* String slice out of bounds.  */
  STRING_SLICE_OUT_OF_BOUNDS = 5,

  /* Dereference of nil pointer.  This is used when there is a
     dereference of a pointer to a very large struct or array, to
     ensure that a gigantic array is not used a proxy to access random
     memory locations.  */
  NIL_DEREFERENCE = 6,

  /* Slice length out of bounds in make: negative or overflow or length
     greater than capacity.  */
  MAKE_SLICE_LEN_OUT_OF_BOUNDS = 7,

  /* Slice capacity out of bounds in make: negative.  */
  MAKE_SLICE_CAP_OUT_OF_BOUNDS = 8,

  /* Map capacity out of bounds in make: negative or overflow.  */
  MAKE_MAP_OUT_OF_BOUNDS = 9,

  /* Channel capacity out of bounds in make: negative or overflow.  */
  MAKE_CHAN_OUT_OF_BOUNDS = 10,

  /* Integer division by zero.  */
  DIVISION_BY_ZERO = 11,

  /* Go statement with nil function.  */
  GO_NIL = 12,

  /* Shift by negative value.  */
  SHIFT_BY_NEGATIVE = 13
};

extern void __go_runtime_error (int32) __attribute__ ((noreturn));

void
__go_runtime_error (int32 i)
{
  struct funcfileline_return fileline;
  bool in_runtime;

  fileline = runtime_funcfileline ((uintptr) runtime_getcallerpc()-1, 0);
  in_runtime = (fileline.retfn.len > 0
		&& (__builtin_strncmp ((const char *) fileline.retfn.str,
				      "runtime.", 8)
		    == 0));

  switch (i)
    {
    case SLICE_INDEX_OUT_OF_BOUNDS:
    case ARRAY_INDEX_OUT_OF_BOUNDS:
    case STRING_INDEX_OUT_OF_BOUNDS:
      if (in_runtime)
	runtime_throw ("index out of range");
      runtime_panicstring ("index out of range");

    case SLICE_SLICE_OUT_OF_BOUNDS:
    case ARRAY_SLICE_OUT_OF_BOUNDS:
    case STRING_SLICE_OUT_OF_BOUNDS:
      if (in_runtime)
	runtime_throw ("slice bounds out of range");
      runtime_panicstring ("slice bounds out of range");

    case NIL_DEREFERENCE:
      runtime_panicstring ("nil pointer dereference");

    case MAKE_SLICE_LEN_OUT_OF_BOUNDS:
      runtime_panicstring ("make slice len out of range");

    case MAKE_SLICE_CAP_OUT_OF_BOUNDS:
      runtime_panicstring ("make slice cap out of range");

    case MAKE_MAP_OUT_OF_BOUNDS:
      runtime_panicstring ("make map len out of range");

    case MAKE_CHAN_OUT_OF_BOUNDS:
      runtime_panicstring ("make chan len out of range");

    case DIVISION_BY_ZERO:
      runtime_panicstring ("integer divide by zero");

    case GO_NIL:
      /* This one is a throw, rather than a panic.  Set throwing to
	 not dump full stacks.  */
      runtime_g()->m->throwing = -1;
      runtime_throw ("go of nil func value");

    case SHIFT_BY_NEGATIVE:
      runtime_panicstring ("negative shift amount");

    default:
      runtime_panicstring ("unknown runtime error");
    }
}

// Copyright 2014 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"

#ifdef USE_LIBFFI

#include "ffi.h"

#if FFI_GO_CLOSURES
#define USE_LIBFFI_CLOSURES
#endif

#endif /* defined(USE_LIBFFI) */

/* Declare C functions with the names used to call from Go.  */

void makeFuncFFI(void *cif, void *impl)
  __asm__ (GOSYM_PREFIX "reflect.makeFuncFFI");

#ifdef USE_LIBFFI_CLOSURES

/* The function that we pass to ffi_prep_closure_loc.  This calls the Go
   function ffiCall with the pointer to the arguments, the results area,
   and the closure structure.  */

extern void ffiCallbackGo(void *result, void **args, ffi_go_closure *closure,
                          int32 wordsize, _Bool big_endian)
  __asm__ (GOSYM_PREFIX "reflect.ffiCallbackGo");

extern void makefuncfficanrecover(Slice)
  __asm__ (GOSYM_PREFIX "runtime.makefuncfficanrecover");

extern void makefuncreturning(void)
  __asm__ (GOSYM_PREFIX "runtime.makefuncreturning");

static void ffi_callback (ffi_cif *, void *, void **, void *)
  __asm__ ("reflect.ffi_callback");

static void
ffi_callback (ffi_cif* cif __attribute__ ((unused)), void *results,
	      void **args, void *closure)
{
  Location locs[8];
  int n;
  int i;

  /* This function is called from some series of FFI closure functions
     called by a Go function.  We want to see whether the caller of
     the closure functions can recover.  Look up the stack and skip
     the FFI functions.  */
  n = runtime_callers (1, &locs[0], sizeof locs / sizeof locs[0], true);
  for (i = 0; i < n; i++)
    {
      const byte *name;

      if (locs[i].function.len == 0)
	continue;
      if (locs[i].function.len < 4)
	break;
      name = locs[i].function.str;
      if (name[0] != 'f' || name[1] != 'f' || name[2] != 'i' || name[3] != '_')
	break;
    }
  if (i < n)
    {
      Slice s;

      s.__values = (void *) &locs[i];
      s.__count = n - i;
      s.__capacity = n - i;
      makefuncfficanrecover (s);
    }

  ffiCallbackGo(results, args, closure, sizeof(ffi_arg),
                __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__);

  if (i < n)
    makefuncreturning ();
}

/* Allocate an FFI closure and arrange to call ffi_callback.  */

void
makeFuncFFI(void *cif, void *impl)
{
  ffi_prep_go_closure(impl, (ffi_cif*)cif, ffi_callback);
}

#else /* !defined(USE_LIBFFI_CLOSURES) */

void
makeFuncFFI(void *cif __attribute__ ((unused)),
	    void *impl __attribute__ ((unused)))
{
  runtime_panicstring ("libgo built without FFI does not support "
		       "reflect.MakeFunc");
}

#endif

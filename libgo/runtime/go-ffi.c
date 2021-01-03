/* go-ffi.c -- libffi support functions.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdlib.h>

#include "runtime.h"

#ifdef USE_LIBFFI

#include "ffi.h"

/* The functions in this file are called by the Go runtime code to get
   the libffi type values.  */

ffi_type *go_ffi_type_pointer(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_pointer(void) __asm__ ("runtime.ffi__type__pointer");
ffi_type *go_ffi_type_sint8(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_sint8(void) __asm__ ("runtime.ffi__type__sint8");
ffi_type *go_ffi_type_sint16(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_sint16(void) __asm__ ("runtime.ffi__type__sint16");
ffi_type *go_ffi_type_sint32(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_sint32(void) __asm__ ("runtime.ffi__type__sint32");
ffi_type *go_ffi_type_sint64(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_sint64(void) __asm__ ("runtime.ffi__type__sint64");
ffi_type *go_ffi_type_uint8(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_uint8(void) __asm__ ("runtime.ffi__type__uint8");
ffi_type *go_ffi_type_uint16(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_uint16(void) __asm__ ("runtime.ffi__type__uint16");
ffi_type *go_ffi_type_uint32(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_uint32(void) __asm__ ("runtime.ffi__type__uint32");
ffi_type *go_ffi_type_uint64(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_uint64(void) __asm__ ("runtime.ffi__type__uint64");
ffi_type *go_ffi_type_float(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_float(void) __asm__ ("runtime.ffi__type__float");
ffi_type *go_ffi_type_double(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_double(void) __asm__ ("runtime.ffi__type__double");
ffi_type *go_ffi_type_complex_float(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_complex_float(void) __asm__ ("runtime.ffi__type__complex__float");
ffi_type *go_ffi_type_complex_double(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_complex_double(void) __asm__ ("runtime.ffi__type__complex__double");
ffi_type *go_ffi_type_void(void) __attribute__ ((no_split_stack));
ffi_type *go_ffi_type_void(void) __asm__ ("runtime.ffi__type__void");

_Bool go_ffi_supports_complex(void) __attribute__ ((no_split_stack));
_Bool go_ffi_supports_complex(void) __asm__ ("runtime.ffi__supports__complex");

ffi_type *
go_ffi_type_pointer(void)
{
	return &ffi_type_pointer;
}

ffi_type *
go_ffi_type_sint8(void)
{
	return &ffi_type_sint8;
}

ffi_type *
go_ffi_type_sint16(void)
{
	return &ffi_type_sint16;
}

ffi_type *
go_ffi_type_sint32(void)
{
	return &ffi_type_sint32;
}

ffi_type *
go_ffi_type_sint64(void)
{
	return &ffi_type_sint64;
}

ffi_type *
go_ffi_type_uint8(void)
{
	return &ffi_type_uint8;
}

ffi_type *
go_ffi_type_uint16(void)
{
	return &ffi_type_uint16;
}

ffi_type *
go_ffi_type_uint32(void)
{
	return &ffi_type_uint32;
}

ffi_type *
go_ffi_type_uint64(void)
{
	return &ffi_type_uint64;
}

ffi_type *
go_ffi_type_float(void)
{
	return &ffi_type_float;
}

ffi_type *
go_ffi_type_double(void)
{
	return &ffi_type_double;
}

_Bool
go_ffi_supports_complex(void)
{
#ifdef FFI_TARGET_HAS_COMPLEX_TYPE
	return true;
#else
	return false;
#endif
}

ffi_type *
go_ffi_type_complex_float(void)
{
#ifdef FFI_TARGET_HAS_COMPLEX_TYPE
	return &ffi_type_complex_float;
#else
	abort();
#endif
}

ffi_type *
go_ffi_type_complex_double(void)
{
#ifdef FFI_TARGET_HAS_COMPLEX_TYPE
	return &ffi_type_complex_double;
#else
	abort();
#endif
}

ffi_type *
go_ffi_type_void(void)
{
	return &ffi_type_void;
}

#endif /* defined(USE_LIBFFI) */

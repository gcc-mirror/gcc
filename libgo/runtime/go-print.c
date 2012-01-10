/* go-print.c -- support for the go print statement.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>
#include <stdio.h>

#include "array.h"
#include "go-panic.h"
#include "go-string.h"
#include "interface.h"

/* This implements the various little functions which are called by
   the predeclared functions print/println/panic/panicln.  */

void
__go_print_space ()
{
  putc (' ', stderr);
}

void
__go_print_nl ()
{
  putc ('\n', stderr);
}

void
__go_print_string (struct __go_string val)
{
  fprintf (stderr, "%.*s", (int) val.__length, (const char *) val.__data);
}

void
__go_print_uint64 (uint64_t val)
{
  fprintf (stderr, "%llu", (unsigned long long) val);
}

void
__go_print_int64 (int64_t val)
{
  fprintf (stderr, "%lld", (long long) val);
}

void
__go_print_double (double val)
{
  fprintf (stderr, "%.24g", val);
}

void
__go_print_complex (__complex double val)
{
  fprintf (stderr, "(%.24g%s%.24gi)",
	  __builtin_creal (val),
	  (__builtin_cimag (val) >= 0 || __builtin_isnan (__builtin_cimag(val))
	   ? "+"
	   : ""),
	  __builtin_cimag (val));
}

void
__go_print_bool (_Bool val)
{
  fputs (val ? "true" : "false", stderr);
}

void
__go_print_pointer (void *val)
{
  fprintf (stderr, "%p", val);
}

void
__go_print_empty_interface (struct __go_empty_interface e)
{
  fprintf (stderr, "(%p,%p)", e.__type_descriptor, e.__object);
}

void
__go_print_interface (struct __go_interface i)
{
  fprintf (stderr, "(%p,%p)", i.__methods, i.__object);
}

void
__go_print_slice (struct __go_open_array val)
{
  fprintf (stderr, "[%d/%d]%p", val.__count, val.__capacity, val.__values);
}

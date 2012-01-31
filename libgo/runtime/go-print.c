/* go-print.c -- support for the go print statement.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <math.h>
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
__go_print_double (double v)
{
  char buf[20];
  int e, s, i, n;
  double h;

  if (isnan (v))
    {
      fputs ("NaN", stderr);
      return;
    }
  if (__builtin_isinf (v))
    {
      putc (v < 0 ? '-' : '+', stderr);
      fputs ("Inf", stderr);
      return;
    }

  /* The number of digits printed.  */
  n = 7;
  /* The exponent.  */
  e = 0;
  /* The sign.  */
  s = 0;
  if (v != 0)
    {
      if (v < 0)
	{
	  v = -v;
	  s = 1;
	}

      /* Normalize.  */
      while (v >= 10)
	{
	  ++e;
	  v /= 10;
	}
      while (v < 1)
	{
	  --e;
	  v *= 10;
	}

      /* Round.  */
      h = 5;
      for (i = 0; i < n; ++i)
	h /= 10;

      v += h;
      if (v >= 10)
	{
	  ++e;
	  v /= 10;
	}
    }

  /* The format is +d.dddd+edd.  */
  buf[0] = s ? '-' : '+';
  for (i = 0; i < n; ++i)
    {
      int d;

      d = v;
      buf[i + 2] = d + '0';
      v -= d;
      v *= 10;
    }
  buf[1] = buf[2];
  buf[2] = '.';

  buf[n + 2] = 'e';
  buf[n + 3] = e < 0 ? '-' : '+';
  if (e < 0)
    e = - e;
  buf[n + 4] = e / 100 + '0';
  buf[n + 5] = (e / 10) % 10 + '0';
  buf[n + 6] = e % 10 + '0';
  buf[n + 7] = '\0';
  fputs (buf, stderr);
}

void
__go_print_complex (__complex double val)
{
  putc ('(', stderr);
  __go_print_double (__builtin_creal (val));
  __go_print_double (__builtin_cimag (val));
  fputs ("i)", stderr);
}

void
__go_print_bool (_Bool val)
{
  fputs (val ? "true" : "false", stderr);
}

void
__go_print_pointer (void *val)
{
  fprintf (stderr, "0x%lx", (unsigned long) (uintptr_t) val);
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
  fprintf (stderr, "[%d/%d]", val.__count, val.__capacity);
  __go_print_pointer (val.__values);
}

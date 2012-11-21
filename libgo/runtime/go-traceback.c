/* go-traceback.c -- stack backtrace for Go.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include "runtime.h"

/* Print a stack trace for the current goroutine.  */

void
runtime_traceback ()
{
  uintptr pcbuf[100];
  int32 c;

  c = runtime_callers (1, pcbuf, sizeof pcbuf / sizeof pcbuf[0]);
  runtime_printtrace (pcbuf, c);
}

void
runtime_printtrace (uintptr *pcbuf, int32 c)
{
  int32 i;

  for (i = 0; i < c; ++i)
    {
      String fn;
      String file;
      intgo line;

      if (__go_file_line (pcbuf[i], &fn, &file, &line)
	  && runtime_showframe (fn))
	{
	  runtime_printf ("%S\n", fn);
	  runtime_printf ("\t%S:%D\n", file, (int64) line);
	}
    }
}

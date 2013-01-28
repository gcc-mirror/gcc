/* go-callers.c -- get callers for Go.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include "backtrace.h"

#include "runtime.h"
#include "array.h"

/* Argument passed to callback function.  */

struct callers_data
{
  uintptr *pcbuf;
  int index;
  int max;
};

/* Callback function for backtrace_simple.  Just collect the PC
   values.  Return zero to continue, non-zero to stop.  */

static int
callback (void *data, uintptr_t pc)
{
  struct callers_data *arg = (struct callers_data *) data;

  arg->pcbuf[arg->index] = pc;
  ++arg->index;
  return arg->index >= arg->max;
}

/* Error callback.  */

static void
error_callback (void *data __attribute__ ((unused)),
		const char *msg, int errnum)
{
  if (errnum != 0)
    runtime_printf ("%s errno %d\n", msg, errnum);
  runtime_throw (msg);
}

/* Gather caller PC's.  */

int32
runtime_callers (int32 skip, uintptr *pcbuf, int32 m)
{
  struct callers_data data;

  data.pcbuf = pcbuf;
  data.index = 0;
  data.max = m;
  backtrace_simple (__go_get_backtrace_state (), skip + 1, callback,
		    error_callback, &data);
  return data.index;
}

int Callers (int, struct __go_open_array)
  __asm__ (GOSYM_PREFIX "runtime.Callers");

int
Callers (int skip, struct __go_open_array pc)
{
  /* In the Go 1 release runtime.Callers has an off-by-one error,
     which we can not correct because it would break backward
     compatibility.  Adjust SKIP here to be compatible.  */
  return runtime_callers (skip - 1, (uintptr *) pc.__values, pc.__count);
}

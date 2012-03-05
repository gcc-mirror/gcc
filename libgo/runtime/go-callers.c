/* go-callers.c -- get callers for Go.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include "unwind.h"

#include "runtime.h"

/* Argument passed to backtrace function.  */

struct callers_data
{
  int skip;
  uintptr *pcbuf;
  int index;
  int max;
};

static _Unwind_Reason_Code
backtrace (struct _Unwind_Context *context, void *varg)
{
  struct callers_data *arg = (struct callers_data *) varg;
  uintptr pc;

  pc = _Unwind_GetIP (context);

  /* FIXME: If PC is in the __morestack routine, we should ignore
     it.  */

  if (arg->skip > 0)
    --arg->skip;
  else if (arg->index >= arg->max)
    return _URC_END_OF_STACK;
  else
    {
      arg->pcbuf[arg->index] = pc;
      ++arg->index;
    }
  return _URC_NO_REASON;
}

int32
runtime_callers (int32 skip, uintptr *pcbuf, int32 m)
{
  struct callers_data arg;

  arg.skip = skip;
  arg.pcbuf = pcbuf;
  arg.index = 0;
  arg.max = m;
  _Unwind_Backtrace (backtrace, &arg);
  return arg.index;
}

int Callers (int, struct __go_open_array)
  __asm__ ("libgo_runtime.runtime.Callers");

int
Callers (int skip, struct __go_open_array pc)
{
  return runtime_callers (skip, (uintptr *) pc.__values, pc.__count);
}

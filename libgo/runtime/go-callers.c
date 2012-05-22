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
  int ip_before_insn = 0;

#ifdef HAVE_GETIPINFO
  pc = _Unwind_GetIPInfo (context, &ip_before_insn);
#else
  pc = _Unwind_GetIP (context);
#endif

  /* FIXME: If PC is in the __morestack routine, we should ignore
     it.  */

  if (arg->skip > 0)
    --arg->skip;
  else if (arg->index >= arg->max)
    return _URC_END_OF_STACK;
  else
    {
      /* Here PC will be the return address.  We actually want the
	 address of the call instruction, so back up one byte and
	 count on the lookup routines handling that correctly.  */
      if (!ip_before_insn)
	--pc;
      arg->pcbuf[arg->index] = pc;
      ++arg->index;
    }
  return _URC_NO_REASON;
}

int32
runtime_callers (int32 skip, uintptr *pcbuf, int32 m)
{
  struct callers_data arg;

  arg.skip = skip + 1;
  arg.pcbuf = pcbuf;
  arg.index = 0;
  arg.max = m;
  _Unwind_Backtrace (backtrace, &arg);
  return arg.index;
}

int Callers (int, struct __go_open_array)
  __asm__ ("runtime.Callers");

int
Callers (int skip, struct __go_open_array pc)
{
  /* In the Go 1 release runtime.Callers has an off-by-one error,
     which we can not correct because it would break backward
     compatibility.  Adjust SKIP here to be compatible.  */
  return runtime_callers (skip - 1, (uintptr *) pc.__values, pc.__count);
}

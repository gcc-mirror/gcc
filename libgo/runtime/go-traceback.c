/* go-traceback.c -- stack backtrace for Go.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include "unwind.h"

#include "runtime.h"
#include "go-string.h"

static _Unwind_Reason_Code
traceback (struct _Unwind_Context *context, void *varg)
{
  int *parg = (int *) varg;
  uintptr pc;
  int ip_before_insn = 0;
  struct __go_string fn;
  struct __go_string file;
  int line;

#ifdef HAVE_GETIPINFO
  pc = _Unwind_GetIPInfo (context, &ip_before_insn);
#else
  pc = _Unwind_GetIP (context);
#endif

  if (*parg > 100)
    return _URC_END_OF_STACK;
  ++*parg;

  /* FIXME: If PC is in the __morestack routine, we should ignore
     it.  */

  /* Back up to the call instruction.  */
  if (!ip_before_insn)
    --pc;

  if (!__go_file_line (pc, &fn, &file, &line))
    return _URC_END_OF_STACK;

  if (runtime_showframe (fn.__data))
    {
      runtime_printf ("%s\n", fn.__data);
      runtime_printf ("\t%s:%d\n", file.__data, line);
    }

  return _URC_NO_REASON;
}

/* Print a stack trace for the current goroutine.  */

void
runtime_traceback ()
{
  int c;

  c = 0;
  _Unwind_Backtrace (traceback, &c);
}

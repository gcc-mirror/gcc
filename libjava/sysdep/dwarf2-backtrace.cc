/* dwarf2-backtrac.cc - backtrace implementation driven by the dwarf2
 exception unwinder.  */

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Written by David Daney <ddaney@avtrex.com> */

/*
  Although this in theory could be 'C' instead of C++, saying that it
  is C++ and including jvm.h makes it easier to insure that the proper
  compiler options are used.  There must be unwind tables for
  backtrace because it is on the stack when _Unwind_Backtrace is
  called.  Compiling as C++ insures this.

*/

#include <config.h>

#include <unwind.h>

#include <jvm.h>


extern "C"
{
  int backtrace (void **, int);
}

struct backtrace_state
{
  int skip_count;
  int current_level;
  int max_level;
  void **locations;
};

static _Unwind_Reason_Code
my_trace_fn (struct _Unwind_Context *uc, void *arg)
{

  struct backtrace_state *bs = (struct backtrace_state *) arg;

  if (bs->skip_count)
    {
      bs->skip_count--;
      return _URC_NO_REASON;
    }

  _Unwind_Ptr loc = _Unwind_GetIP (uc);

  if (bs->current_level < bs->max_level)
    bs->locations[bs->current_level++] = (void *) loc;

  if (bs->current_level >= bs->max_level)
    return _URC_END_OF_STACK;
  else
    return _URC_NO_REASON;
}

/*
 * backtrace is defined in (some versions of) libc.  This definition
 * must match so that it can replace the libc version at link time.
 *
 * Fill the locations array with at most len back trace locations.
 *
 * Returns the number of locations actually filled in.
 *
 */
int
backtrace (void **locations, int len)
{
  struct backtrace_state bs;
  bs.skip_count = 1;		/* Don't log the call to backtrace itself. */
  bs.current_level = 0;
  bs.max_level = len;
  bs.locations = locations;

  _Unwind_Backtrace (my_trace_fn, &bs);
  return bs.current_level;
}

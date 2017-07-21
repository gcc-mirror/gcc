/* xcoff.c -- Get debug data from a XCOFFF file for backtraces.
   Copyright (C) 2017 Free Software Foundation, Inc.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    (1) Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    (2) Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

    (3) The name of the author may not be used to
    endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.  */

#include "config.h"

#include <sys/types.h>

#include "backtrace.h"
#include "internal.h"

/* A trivial routine that always fails to find fileline data.  */

static int
xcoff_fileline (struct backtrace_state *state ATTRIBUTE_UNUSED,
		uintptr_t pc, backtrace_full_callback callback,
		backtrace_error_callback error_callback ATTRIBUTE_UNUSED,
		void *data)

{
  static char buf[32];

  snprintf (buf, sizeof(buf), "pc=0x%llx", (unsigned long long) pc);
  return callback (data, pc, "unknown_file", 123, buf);
}

static void
xcoff_syminfo (struct backtrace_state *state ATTRIBUTE_UNUSED, uintptr_t addr,
               backtrace_syminfo_callback callback,
               backtrace_error_callback error_callback ATTRIBUTE_UNUSED,
               void *data)
{
  callback (data, addr, "unknown", 0, 0);
}

/* Initialize the backtrace data when we don't know how to read the
   debug info.  */

int
backtrace_initialize (struct backtrace_state *state ATTRIBUTE_UNUSED,
		      int descriptor ATTRIBUTE_UNUSED,
		      backtrace_error_callback error_callback ATTRIBUTE_UNUSED,
		      void *data ATTRIBUTE_UNUSED, fileline *fileline_fn)
{
  state->syminfo_fn = xcoff_syminfo;
  state->fileline_data = NULL;
  *fileline_fn = xcoff_fileline;
  return 1;
}

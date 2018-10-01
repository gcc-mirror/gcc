/* go-caller.c -- look up function/file/line/entry info

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* Implement runtime.Caller.  */

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "backtrace.h"

#include "runtime.h"

/* Get the function name, file name, and line number for a PC value.
   We use the backtrace library to get this.  */

/* Data structure to gather file/line information.  */

struct caller
{
  String fn;
  String file;
  intgo line;
  intgo index;
};

/* Collect file/line information for a PC value.  If this is called
   more than once, due to inlined functions, we use the last call, as
   that is usually the most useful one.  */

static int
callback (void *data, uintptr_t pc __attribute__ ((unused)),
	  const char *filename, int lineno, const char *function)
{
  struct caller *c = (struct caller *) data;

  /* The libbacktrace library says that these strings might disappear,
     but with the current implementation they won't.  We can't easily
     allocate memory here, so for now assume that we can save a
     pointer to the strings.  */
  c->fn = runtime_gostringnocopy ((const byte *) function);
  c->file = runtime_gostringnocopy ((const byte *) filename);
  c->line = lineno;

  if (c->index == 0)
    return 1;

  if (c->index > 0)
    --c->index;

  return 0;
}

/* The error callback for backtrace_pcinfo and backtrace_syminfo.  */

static void
error_callback (void *data __attribute__ ((unused)),
		const char *msg, int errnum)
{
  if (errnum == -1)
    return;
  if (errnum > 0)
    runtime_printf ("%s errno %d\n", msg, errnum);
  runtime_throw (msg);
}

/* The backtrace library state.  */

static void *back_state;

/* A lock to control creating back_state.  */

static uint32 back_state_lock;

/* The program arguments.  */

extern Slice runtime_get_args(void);

/* Fetch back_state, creating it if necessary.  */

struct backtrace_state *
__go_get_backtrace_state ()
{
  uint32 set;

  /* We may not have a g here, so we can't use runtime_lock.  */
  set = 0;
  while (!__atomic_compare_exchange_n (&back_state_lock, &set, 1, false, __ATOMIC_ACQUIRE, __ATOMIC_RELAXED))
    {
      runtime_osyield ();
      set = 0;
    }
  if (back_state == NULL)
    {
      Slice args;
      const char *filename;
      struct stat s;

      args = runtime_get_args();
      filename = NULL;
      if (args.__count > 0)
	filename = (const char*)((String*)args.__values)[0].str;

      /* If there is no '/' in FILENAME, it was found on PATH, and
	 might not be the same as the file with the same name in the
	 current directory.  */
      if (filename != NULL && __builtin_strchr (filename, '/') == NULL)
	filename = NULL;

      /* If the file is small, then it's not the real executable.
	 This is specifically to deal with Docker, which uses a bogus
	 argv[0] (http://gcc.gnu.org/PR61895).  It would be nice to
	 have a better check for whether this file is the real
	 executable.  */
      if (filename != NULL && (stat (filename, &s) < 0 || s.st_size < 1024))
	filename = NULL;

      back_state = backtrace_create_state (filename, 1, error_callback, NULL);
    }
  __atomic_store_n (&back_state_lock, 0, __ATOMIC_RELEASE);
  return back_state;
}

/* Return function/file/line information for PC.  The index parameter
   is the entry on the stack of inlined functions; -1 means the last
   one.  */

static _Bool
__go_file_line (uintptr pc, int index, String *fn, String *file, intgo *line)
{
  struct caller c;
  struct backtrace_state *state;

  runtime_memclr (&c, sizeof c);
  c.index = index;
  state = __go_get_backtrace_state ();
  backtrace_pcinfo (state, pc, callback, error_callback, &c);
  *fn = c.fn;
  *file = c.file;
  *line = c.line;

  // If backtrace_pcinfo didn't get the function name from the debug
  // info, try to get it from the symbol table.
  if (fn->len == 0)
    backtrace_syminfo (state, pc, __go_syminfo_fnname_callback,
		       error_callback, fn);

  return c.file.len > 0;
}

/* Collect symbol information.  */

static void
syminfo_callback (void *data, uintptr_t pc __attribute__ ((unused)),
		  const char *symname __attribute__ ((unused)),
		  uintptr_t address, uintptr_t size __attribute__ ((unused)))
{
  uintptr_t *pval = (uintptr_t *) data;

  *pval = address;
}

/* Set *VAL to the value of the symbol for PC.  */

static _Bool
__go_symbol_value (uintptr pc, uintptr *val)
{
  *val = 0;
  backtrace_syminfo (__go_get_backtrace_state (), pc, syminfo_callback,
		     error_callback, val);
  return *val != 0;
}

/* The values returned by runtime.Caller.  */

struct caller_ret
{
  uintptr_t pc;
  String file;
  intgo line;
  _Bool ok;
};

struct caller_ret Caller (int n) __asm__ (GOSYM_PREFIX "runtime.Caller");

/* Implement runtime.Caller.  */

struct caller_ret
Caller (int skip)
{
  struct caller_ret ret;
  Location loc;
  int32 n;

  runtime_memclr (&ret, sizeof ret);
  n = runtime_callers (skip + 1, &loc, 1, false);
  if (n < 1 || loc.pc == 0)
    return ret;
  ret.pc = loc.pc;
  ret.file = loc.filename;
  ret.line = loc.lineno;
  ret.ok = 1;
  return ret;
}

/* Look up the function name, file name, and line number for a PC.  */

struct funcfileline_return
runtime_funcfileline (uintptr targetpc, int32 index)
{
  struct funcfileline_return ret;

  if (!__go_file_line (targetpc, index, &ret.retfn, &ret.retfile,
		       &ret.retline))
    runtime_memclr (&ret, sizeof ret);
  return ret;
}

/* Return the entry point of a function.  */
uintptr runtime_funcentry(uintptr)
  __asm__ (GOSYM_PREFIX "runtime.funcentry");

uintptr
runtime_funcentry (uintptr pc)
{
  uintptr val;

  if (!__go_symbol_value (pc, &val))
    return 0;
  return val;
}

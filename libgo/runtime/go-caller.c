/* go-caller.c -- runtime.Caller and runtime.FuncForPC for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* Implement runtime.Caller.  */

#include <stdint.h>

#include "runtime.h"
#include "go-string.h"

/* Get the function name, file name, and line number for a PC value.
   We use the DWARF debug information to get this.  Rather than write
   a whole new library in C, we use the existing Go library.
   Unfortunately, the Go library is only available if the debug/elf
   package is imported (we use debug/elf for both ELF and Mach-O, in
   this case).  We arrange for the debug/elf package to register
   itself, and tweak the various packages that need this information
   to import debug/elf where possible.  */

/* The function that returns function/file/line information.  */

typedef _Bool (*infofn_type) (uintptr_t, struct __go_string *,
			      struct __go_string *, int *);
static infofn_type infofn;

/* The function that returns the value of a symbol, used to get the
   entry address of a function.  */

typedef _Bool (*symvalfn_type) (struct __go_string, uintptr_t *);
static symvalfn_type symvalfn;

/* This is called by debug/elf to register the function that returns
   function/file/line information.  */

void RegisterDebugLookup (infofn_type, symvalfn_type)
  __asm__ ("libgo_runtime.runtime.RegisterDebugLookup");

void
RegisterDebugLookup (infofn_type pi, symvalfn_type ps)
{
  infofn = pi;
  symvalfn = ps;
}

/* Return function/file/line information for PC.  */

_Bool
__go_file_line (uintptr_t pc, struct __go_string *fn, struct __go_string *file,
		int *line)
{
  if (infofn == NULL)
    return 0;
  return infofn (pc, fn, file, line);
}

/* Return the value of a symbol.  */

_Bool
__go_symbol_value (struct __go_string sym, uintptr_t *val)
{
  if (symvalfn == NULL)
    return 0;
  return symvalfn (sym, val);
}

/* The values returned by runtime.Caller.  */

struct caller_ret
{
  uintptr_t pc;
  struct __go_string file;
  int line;
  _Bool ok;
};

struct caller_ret Caller (int n) asm ("libgo_runtime.runtime.Caller");

Func *FuncForPC (uintptr_t) asm ("libgo_runtime.runtime.FuncForPC");

/* Implement runtime.Caller.  */

struct caller_ret
Caller (int skip)
{
  struct caller_ret ret;
  uintptr pc;
  int32 n;
  struct __go_string fn;

  runtime_memclr (&ret, sizeof ret);
  n = runtime_callers (skip + 1, &pc, 1);
  if (n < 1)
    return ret;
  ret.pc = pc;
  ret.ok = __go_file_line (pc, &fn, &ret.file, &ret.line);
  return ret;
}

/* Implement runtime.FuncForPC.  */

Func *
FuncForPC (uintptr_t pc)
{
  Func *ret;
  struct __go_string fn;
  struct __go_string file;
  int line;
  uintptr_t val;

  if (!__go_file_line (pc, &fn, &file, &line))
    return NULL;
  if (!__go_symbol_value (fn, &val))
    return NULL;

  ret = (Func *) runtime_malloc (sizeof (*ret));
  ret->name = fn;
  ret->entry = val;
  return ret;
}

/* Look up the file and line information for a PC within a
   function.  */

struct funcline_go_return
{
  struct __go_string retfile;
  int retline;
};

struct funcline_go_return
runtime_funcline_go (Func *f, uintptr targetpc)
  __asm__ ("libgo_runtime.runtime.funcline_go");

struct funcline_go_return
runtime_funcline_go (Func *f __attribute__((unused)), uintptr targetpc)
{
  struct funcline_go_return ret;
  struct __go_string fn;

  if (!__go_file_line (targetpc, &fn, &ret.retfile,  &ret.retline))
    runtime_memclr (&ret, sizeof ret);
  return ret;
}

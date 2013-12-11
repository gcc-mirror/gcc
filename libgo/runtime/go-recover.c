/* go-recover.c -- support for the go recover function.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "interface.h"
#include "go-panic.h"
#include "go-defer.h"

/* This is called by a thunk to see if the real function should be
   permitted to recover a panic value.  Recovering a value is
   permitted if the thunk was called directly by defer.  RETADDR is
   the return address of the function which is calling
   __go_can_recover--this is, the thunk.  */

_Bool
__go_can_recover (const void *retaddr)
{
  G *g;
  struct __go_defer_stack *d;
  const char* ret;
  const char* dret;
  Location loc;
  const byte *name;

  g = runtime_g ();

  d = g->defer;
  if (d == NULL)
    return 0;

  /* The panic which this function would recover is the one on the top
     of the panic stack.  We do not want to recover it if that panic
     was on the top of the panic stack when this function was
     deferred.  */
  if (d->__panic == g->panic)
    return 0;

  /* D->__RETADDR is the address of a label immediately following the
     call to the thunk.  We can recover a panic if that is the same as
     the return address of the thunk.  We permit a bit of slack in
     case there is any code between the function return and the label,
     such as an instruction to adjust the stack pointer.  */

  ret = (const char *) retaddr;

#ifdef __sparc__
  /* On SPARC the address we get, from __builtin_return_address, is
     the address of the call instruction.  Adjust forward, also
     skipping the delayed instruction following the call.  */
  ret += 8;
#endif

  dret = (const char *) d->__retaddr;
  if (ret <= dret && ret + 16 >= dret)
    return 1;

  /* If the function calling recover was created by reflect.MakeFunc,
     then RETADDR will be somewhere in libffi.  Our caller is
     permitted to recover if it was called from libffi.  */
  if (!d->__makefunc_can_recover)
    return 0;

  if (runtime_callers (2, &loc, 1) < 1)
    return 0;

  /* If we have no function name, then we weren't called by Go code.
     Guess that we were called by libffi.  */
  if (loc.function.len == 0)
    return 1;

  if (loc.function.len < 4)
    return 0;
  name = loc.function.str;
  if (*name == '_')
    {
      if (loc.function.len < 5)
	return 0;
      ++name;
    }

  if (name[0] == 'f' && name[1] == 'f' && name[2] == 'i' && name[3] == '_')
    return 1;

  return 0;
}

/* This function is called when code is about to enter a function
   created by reflect.MakeFunc.  It is called by the function stub
   used by MakeFunc.  If the stub is permitted to call recover, then a
   real MakeFunc function is permitted to call recover.  */

void
__go_makefunc_can_recover (const void *retaddr)
{
  struct __go_defer_stack *d;

  d = runtime_g ()->defer;
  if (d != NULL
      && !d->__makefunc_can_recover
      && __go_can_recover (retaddr))
    d->__makefunc_can_recover = 1;
}

/* This function is called when code is about to exit a function
   created by reflect.MakeFunc.  It is called by the function stub
   used by MakeFunc.  It clears the __makefunc_can_recover field.
   It's OK to always clear this field, because __go_can_recover will
   only be called by a stub created for a function that calls recover.
   That stub will not call a function created by reflect.MakeFunc, so
   by the time we get here any caller higher up on the call stack no
   longer needs the information.  */

void
__go_makefunc_returning (void)
{
  struct __go_defer_stack *d;

  d = runtime_g ()->defer;
  if (d != NULL)
    d->__makefunc_can_recover = 0;
}

/* This is only called when it is valid for the caller to recover the
   value on top of the panic stack, if there is one.  */

struct __go_empty_interface
__go_recover ()
{
  G *g;
  struct __go_panic_stack *p;

  g = runtime_g ();

  if (g->panic == NULL || g->panic->__was_recovered)
    {
      struct __go_empty_interface ret;

      ret.__type_descriptor = NULL;
      ret.__object = NULL;
      return ret;
    }
  p = g->panic;
  p->__was_recovered = 1;
  return p->__arg;
}

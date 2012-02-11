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
__go_can_recover (const void* retaddr)
{
  G *g;
  struct __go_defer_stack *d;
  const char* ret;
  const char* dret;

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
  return ret <= dret && ret + 16 >= dret;
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

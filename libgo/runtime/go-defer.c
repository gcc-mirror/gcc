/* go-defer.c -- manage the defer stack.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "runtime.h"
#include "go-alloc.h"
#include "go-panic.h"

/* This function is called each time we need to defer a call.  */

void
__go_defer (_Bool *frame, void (*pfn) (void *), void *arg)
{
  G *g;
  Defer *n;

  g = runtime_g ();
  n = runtime_newdefer ();
  n->next = g->_defer;
  n->frame = frame;
  n->_panic = g->_panic;
  n->pfn = (uintptr) pfn;
  n->arg = arg;
  n->retaddr = 0;
  n->makefunccanrecover = 0;
  n->special = 0;
  g->_defer = n;
}

/* This function is called when we want to undefer the stack.  */

void
__go_undefer (_Bool *frame)
{
  G *g;

  g = runtime_g ();
  while (g->_defer != NULL && g->_defer->frame == frame)
    {
      Defer *d;
      void (*pfn) (void *);

      d = g->_defer;
      pfn = (void (*) (void *)) d->pfn;
      d->pfn = 0;

      if (pfn != NULL)
	(*pfn) (d->arg);

      g->_defer = d->next;

      /* This may be called by a cgo callback routine to defer the
	 call to syscall.CgocallBackDone, in which case we will not
	 have a memory context.  Don't try to free anything in that
	 case--the GC will release it later.  */
      if (runtime_m () != NULL)
	runtime_freedefer (d);

      /* Since we are executing a defer function here, we know we are
	 returning from the calling function.  If the calling
	 function, or one of its callees, paniced, then the defer
	 functions would be executed by __go_panic.  */
      *frame = 1;
    }
}

/* This function is called to record the address to which the deferred
   function returns.  This may in turn be checked by __go_can_recover.
   The frontend relies on this function returning false.  */

_Bool
__go_set_defer_retaddr (void *retaddr)
{
  G *g;

  g = runtime_g ();
  if (g->_defer != NULL)
    g->_defer->retaddr = (uintptr) __builtin_extract_return_addr (retaddr);
  return 0;
}

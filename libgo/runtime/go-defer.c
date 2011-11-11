/* go-defer.c -- manage the defer stack.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "runtime.h"
#include "go-alloc.h"
#include "go-panic.h"
#include "go-defer.h"

/* This function is called each time we need to defer a call.  */

void
__go_defer (_Bool *frame, void (*pfn) (void *), void *arg)
{
  struct __go_defer_stack *n;

  n = (struct __go_defer_stack *) __go_alloc (sizeof (struct __go_defer_stack));
  n->__next = g->defer;
  n->__frame = frame;
  n->__panic = g->panic;
  n->__pfn = pfn;
  n->__arg = arg;
  n->__retaddr = NULL;
  g->defer = n;
}

/* This function is called when we want to undefer the stack.  */

void
__go_undefer (_Bool *frame)
{
  while (g->defer != NULL && g->defer->__frame == frame)
    {
      struct __go_defer_stack *d;
      void (*pfn) (void *);

      d = g->defer;
      pfn = d->__pfn;
      d->__pfn = NULL;

      if (pfn != NULL)
	(*pfn) (d->__arg);

      g->defer = d->__next;
      __go_free (d);

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
  if (g->defer != NULL)
    g->defer->__retaddr = retaddr;
  return 0;
}

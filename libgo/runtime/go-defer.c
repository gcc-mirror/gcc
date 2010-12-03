/* go-defer.c -- manage the defer stack.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "go-alloc.h"
#include "go-panic.h"
#include "go-defer.h"

/* This function is called each time we need to defer a call.  */

void
__go_defer (void *frame, void (*pfn) (void *), void *arg)
{
  struct __go_defer_stack *n;

  if (__go_panic_defer == NULL)
    __go_panic_defer = ((struct __go_panic_defer_struct *)
			__go_alloc (sizeof (struct __go_panic_defer_struct)));

  n = (struct __go_defer_stack *) __go_alloc (sizeof (struct __go_defer_stack));
  n->__next = __go_panic_defer->__defer;
  n->__frame = frame;
  n->__panic = __go_panic_defer->__panic;
  n->__pfn = pfn;
  n->__arg = arg;
  n->__retaddr = NULL;
  __go_panic_defer->__defer = n;
}

/* This function is called when we want to undefer the stack.  */

void
__go_undefer (void *frame)
{
  if (__go_panic_defer == NULL)
    return;
  while (__go_panic_defer->__defer != NULL
	 && __go_panic_defer->__defer->__frame == frame)
    {
      struct __go_defer_stack *d;
      void (*pfn) (void *);

      d = __go_panic_defer->__defer;
      pfn = d->__pfn;
      d->__pfn = NULL;

      if (pfn != NULL)
	(*pfn) (d->__arg);

      __go_panic_defer->__defer = d->__next;
      __go_free (d);
    }
}

/* This function is called to record the address to which the deferred
   function returns.  This may in turn be checked by __go_can_recover.
   The frontend relies on this function returning false.  */

_Bool
__go_set_defer_retaddr (void *retaddr)
{
  if (__go_panic_defer != NULL && __go_panic_defer->__defer != NULL)
    __go_panic_defer->__defer->__retaddr = retaddr;
  return 0;
}

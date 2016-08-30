/* go-panic.c -- support for the go panic function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"
#include "arch.h"
#include "malloc.h"
#include "go-alloc.h"
#include "go-panic.h"
#include "interface.h"

/* Print the panic stack.  This is used when there is no recover.  */

static void
__printpanics (Panic *p)
{
  if (p->next != NULL)
    {
      __printpanics (p->next);
      runtime_printf ("\t");
    }
  runtime_printf ("panic: ");
  runtime_printany (p->arg);
  if (p->recovered)
    runtime_printf (" [recovered]");
  runtime_printf ("\n");
}

/* This implements __go_panic which is used for the panic
   function.  */

void
__go_panic (struct __go_empty_interface arg)
{
  G *g;
  Panic *n;

  g = runtime_g ();

  n = (Panic *) __go_alloc (sizeof (Panic));
  n->arg = arg;
  n->next = g->_panic;
  g->_panic = n;

  /* Run all the defer functions.  */

  while (1)
    {
      Defer *d;
      void (*pfn) (void *);

      d = g->_defer;
      if (d == NULL)
	break;

      pfn = (void (*) (void *)) d->pfn;
      d->pfn = 0;

      if (pfn != NULL)
	{
	  (*pfn) (d->arg);

	  if (n->recovered)
	    {
	      /* Some defer function called recover.  That means that
		 we should stop running this panic.  */

	      g->_panic = n->next;
	      __go_free (n);

	      /* Now unwind the stack by throwing an exception.  The
		 compiler has arranged to create exception handlers in
		 each function which uses a defer statement.  These
		 exception handlers will check whether the entry on
		 the top of the defer stack is from the current
		 function.  If it is, we have unwound the stack far
		 enough.  */
	      __go_unwind_stack ();

	      /* __go_unwind_stack should not return.  */
	      abort ();
	    }

	  /* Because we executed that defer function by a panic, and
	     it did not call recover, we know that we are not
	     returning from the calling function--we are panicing
	     through it.  */
	  *d->frame = 0;
	}

      g->_defer = d->next;

      /* This may be called by a cgo callback routine to defer the
	 call to syscall.CgocallBackDone, in which case we will not
	 have a memory context.  Don't try to free anything in that
	 case--the GC will release it later.  */
      if (runtime_m () != NULL)
	runtime_freedefer (d);
    }

  /* The panic was not recovered.  */

  runtime_startpanic ();
  __printpanics (g->_panic);
  runtime_dopanic (0);
}

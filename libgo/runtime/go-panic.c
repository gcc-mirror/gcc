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
#include "go-defer.h"
#include "go-panic.h"
#include "go-string.h"
#include "interface.h"

/* Print the panic stack.  This is used when there is no recover.  */

static void
__printpanics (struct __go_panic_stack *p)
{
  if (p->__next != NULL)
    {
      __printpanics (p->__next);
      fprintf (stderr, "\t");
    }
  fprintf (stderr, "panic: ");
  printany (p->__arg);
  if (p->__was_recovered)
    fprintf (stderr, " [recovered]");
  fputc ('\n', stderr);
}

/* This implements __go_panic which is used for the panic
   function.  */

void
__go_panic (struct __go_empty_interface arg)
{
  G *g;
  struct __go_panic_stack *n;

  g = runtime_g ();

  n = (struct __go_panic_stack *) __go_alloc (sizeof (struct __go_panic_stack));
  n->__arg = arg;
  n->__next = g->panic;
  g->panic = n;

  /* Run all the defer functions.  */

  while (1)
    {
      struct __go_defer_stack *d;
      void (*pfn) (void *);

      d = g->defer;
      if (d == NULL)
	break;

      pfn = d->__pfn;
      d->__pfn = NULL;

      if (pfn != NULL)
	{
	  (*pfn) (d->__arg);

	  if (n->__was_recovered)
	    {
	      /* Some defer function called recover.  That means that
		 we should stop running this panic.  */

	      g->panic = n->__next;
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
	  *d->__frame = 0;
	}

      g->defer = d->__next;
      __go_free (d);
    }

  /* The panic was not recovered.  */

  runtime_startpanic ();
  __printpanics (g->panic);
  runtime_dopanic (0);
}

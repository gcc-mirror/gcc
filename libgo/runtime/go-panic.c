/* go-panic.c -- support for the go panic function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"
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
      printf ("\t");
    }
  printf ("panic: ");
  printany (p->__arg);
  if (p->__was_recovered)
    printf (" [recovered]");
  putchar ('\n');
}

/* This implements __go_panic which is used for the panic
   function.  */

void
__go_panic (struct __go_empty_interface arg)
{
  struct __go_panic_stack *n;

  if (__go_panic_defer == NULL)
    __go_panic_defer = ((struct __go_panic_defer_struct *)
			__go_alloc (sizeof (struct __go_panic_defer_struct)));

  n = (struct __go_panic_stack *) __go_alloc (sizeof (struct __go_panic_stack));
  n->__arg = arg;
  n->__next = __go_panic_defer->__panic;
  __go_panic_defer->__panic = n;

  /* Run all the defer functions.  */

  while (1)
    {
      struct __go_defer_stack *d;
      void (*pfn) (void *);

      d = __go_panic_defer->__defer;
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

	      __go_panic_defer->__panic = n->__next;
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
	}

      __go_panic_defer->__defer = d->__next;
      __go_free (d);
    }

  /* The panic was not recovered.  */

  __printpanics (__go_panic_defer->__panic);

  /* FIXME: We should dump a call stack here.  */
  abort ();
}

/* This is used by the runtime library.  */

void
__go_panic_msg (const char* msg)
{
  size_t len;
  unsigned char *sdata;
  struct __go_string s;
  struct __go_empty_interface arg;

  len = __builtin_strlen (msg);
  sdata = runtime_mallocgc (len, RefNoPointers, 0, 0);
  __builtin_memcpy (sdata, msg, len);
  s.__data = sdata;
  s.__length = len;
  newErrorString(s, &arg);
  __go_panic (arg);
}

/* go-trampoline.c -- allocate a trampoline for a nested function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include <stddef.h>
#include <stdint.h>
#include <unistd.h>

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include "runtime.h"
#include "arch.h"
#include "malloc.h"
#include "go-assert.h"

/* Trampolines need to run in memory that is both writable and
   executable.  In order to implement them, we grab a page of memory
   and mprotect it.  We fill in the page with trampolines as they are
   required.  When we run out of space, we drop the pointer to the
   page and allocate a new one.  The page will be freed by the garbage
   collector when there are no more variables of type func pointing to
   it.  */

/* A lock to control access to the page of closures.  */

static Lock trampoline_lock;

/* The page of closures.  */

static unsigned char *trampoline_page;

/* The size of trampoline_page.  */

static uintptr_t trampoline_page_size;

/* The number of bytes we have used on trampoline_page.  */

static uintptr_t trampoline_page_used;

/* Allocate a trampoline of SIZE bytes that will use the closure in
   CLOSURE.  */

void *
__go_allocate_trampoline (uintptr_t size, void *closure)
{
  uintptr_t ptr_size;
  uintptr_t full_size;
  unsigned char *ret;

  /* Because the garbage collector only looks at aligned addresses, we
     need to store the closure at an aligned address to ensure that it
     sees it.  */
  ptr_size = sizeof (void *);
  full_size = (((size + ptr_size - 1) / ptr_size) * ptr_size);
  full_size += ptr_size;

  runtime_lock (&trampoline_lock);

  if (full_size < trampoline_page_size - trampoline_page_used)
    trampoline_page = NULL;

  if (trampoline_page == NULL)
    {
      uintptr_t page_size;
      unsigned char *page;

      page_size = getpagesize ();
      __go_assert (page_size >= full_size);
      page = (unsigned char *) runtime_mallocgc (2 * page_size - 1, 0, 0, 0);
      page = (unsigned char *) (((uintptr_t) page + page_size - 1)
				& ~ (page_size - 1));

#ifdef HAVE_SYS_MMAN_H
      {
	int i;

	i = mprotect (page, page_size, PROT_READ | PROT_WRITE | PROT_EXEC);
	__go_assert (i == 0);
      }
#endif

      trampoline_page = page;
      trampoline_page_size = page_size;
      trampoline_page_used = 0;
    }

  ret = trampoline_page + trampoline_page_used;
  trampoline_page_used += full_size;

  runtime_unlock (&trampoline_lock);

  __builtin_memcpy (ret + full_size - ptr_size, &closure, ptr_size);

  return (void *) ret;
}

/* Scan the trampoline page when running the garbage collector.  This
   just makes sure that the garbage collector sees the pointer in
   trampoline_page, so that the page itself is not freed if there are
   no other references to it.  */

void
runtime_trampoline_scan (void (*scan) (byte *, int64))
{
  if (trampoline_page != NULL)
    scan ((byte *) &trampoline_page, sizeof trampoline_page);
}

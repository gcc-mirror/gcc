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

#include "go-alloc.h"
#include "go-assert.h"

/* In order to build a trampoline we need space which is both writable
   and executable.  We currently just allocate a whole page.  This
   needs to be more system dependent.  */

void *
__go_allocate_trampoline (uintptr_t size, void *closure)
{
  unsigned int page_size;
  void *ret;
  size_t off;

  page_size = getpagesize ();
  __go_assert (page_size >= size);
  ret = __go_alloc (2 * page_size - 1);
  ret = (void *) (((uintptr_t) ret + page_size - 1)
		  & ~ ((uintptr_t) page_size - 1));

  /* Because the garbage collector only looks at correct address
     offsets, we need to ensure that it will see the closure
     address.  */
  off = ((size + sizeof (void *) - 1) / sizeof (void *)) * sizeof (void *);
  __go_assert (size + off + sizeof (void *) <= page_size);
  __builtin_memcpy (ret + off, &closure, sizeof (void *));

#ifdef HAVE_SYS_MMAN_H
  {
    int i;
    i = mprotect (ret, size, PROT_READ | PROT_WRITE | PROT_EXEC);
    __go_assert (i == 0);
  }
#endif

  return ret;
}

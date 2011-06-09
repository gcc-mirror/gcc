/* Implement __enable_execute_stack using mprotect(2).
   Copyright (C) 2011 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>

#define STACK_PROT_RWX (PROT_READ | PROT_WRITE | PROT_EXEC)

static int need_enable_exec_stack;

static void check_enabling (void) __attribute__ ((unused));
extern void __enable_execute_stack (void *);

#if defined __FreeBSD__
#include <sys/sysctl.h>

static void __attribute__ ((constructor))
check_enabling (void)
{
  int prot = 0;
  size_t len = sizeof (prot);

  sysctlbyname ("kern.stackprot", &prot, &len, NULL, 0);
  if (prot != STACK_PROT_RWX)
    need_enable_exec_stack = 1;
}
#elif defined __sun__ && defined __svr4__
static void __attribute__ ((constructor))
check_enabling (void)
{
  int prot = (int) sysconf (_SC_STACK_PROT);

  if (prot != STACK_PROT_RWX)
    need_enable_exec_stack = 1;
}
#else
/* There is no way to query the execute permission of the stack, so
   we always issue the mprotect() call.  */

static int need_enable_exec_stack = 1;
#endif

#if defined __NetBSD__
/* Note that we go out of our way to use namespace-non-invasive calls
   here.  Unfortunately, there is no libc-internal name for mprotect().  */

#include <sys/sysctl.h>

extern int __sysctl (int *, unsigned int, void *, size_t *, void *, size_t);

static int
getpagesize (void)
{
  static int size;

  if (size == 0)
    {
      int mib[2];
      size_t len;

      mib[0] = CTL_HW;
      mib[1] = HW_PAGESIZE;
      len = sizeof (size);
      (void) __sysctl (mib, 2, &size, &len, NULL, 0);
    }
  return size;
}
#endif /* __NetBSD__ */

/* Attempt to turn on access permissions for the stack.  Unfortunately it
   is not possible to make this namespace-clean.*/

void
__enable_execute_stack (void *addr)
{
  if (!need_enable_exec_stack)
    return;
  else
    {
      static long size, mask;

      if (size == 0) {
	size = getpagesize ();
	mask = ~(size - 1);
      }

      char *page = (char *) (((long) addr) & mask);
      char *end  = (char *)
	((((long) (addr + __LIBGCC_TRAMPOLINE_SIZE__)) & mask) + size);

      if (mprotect (page, end - page, STACK_PROT_RWX) < 0)
	/* Note that no errors should be emitted by this code; it is
	   considered dangerous for library calls to send messages to
	   stdout/stderr.  */
	abort ();
    }
}

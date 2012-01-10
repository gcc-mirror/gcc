/* Library support for -fsplit-stack.  */
/* Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Ian Lance Taylor <iant@google.com>.

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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

/* If inhibit_libc is defined, we can not compile this file.  The
   effect is that people will not be able to use -fsplit-stack.  That
   is much better than failing the build particularly since people
   will want to define inhibit_libc while building a compiler which
   can build glibc.  */

#ifndef inhibit_libc

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/uio.h>

#include "generic-morestack.h"

typedef unsigned uintptr_type __attribute__ ((mode (pointer)));

/* This file contains subroutines that are used by code compiled with
   -fsplit-stack.  */

/* Declare functions to avoid warnings--there is no header file for
   these internal functions.  We give most of these functions the
   flatten attribute in order to minimize their stack usage--here we
   must minimize stack usage even at the cost of code size, and in
   general inlining everything will do that.  */

extern void
__generic_morestack_set_initial_sp (void *sp, size_t len)
  __attribute__ ((no_split_stack, flatten, visibility ("hidden")));

extern void *
__generic_morestack (size_t *frame_size, void *old_stack, size_t param_size)
  __attribute__ ((no_split_stack, flatten, visibility ("hidden")));

extern void *
__generic_releasestack (size_t *pavailable)
  __attribute__ ((no_split_stack, flatten, visibility ("hidden")));

extern void
__morestack_block_signals (void)
  __attribute__ ((no_split_stack, flatten, visibility ("hidden")));

extern void
__morestack_unblock_signals (void)
  __attribute__ ((no_split_stack, flatten, visibility ("hidden")));

extern size_t
__generic_findstack (void *stack)
  __attribute__ ((no_split_stack, flatten, visibility ("hidden")));

extern void
__morestack_load_mmap (void)
  __attribute__ ((no_split_stack, visibility ("hidden")));

extern void *
__morestack_allocate_stack_space (size_t size)
  __attribute__ ((visibility ("hidden")));

/* These are functions which -fsplit-stack code can call.  These are
   not called by the compiler, and are not hidden.  FIXME: These
   should be in some header file somewhere, somehow.  */

extern void *
__splitstack_find (void *, void *, size_t *, void **, void **, void **)
  __attribute__ ((visibility ("default")));

extern void
__splitstack_block_signals (int *, int *)
  __attribute__ ((visibility ("default")));

extern void
__splitstack_getcontext (void *context[10])
  __attribute__ ((no_split_stack, visibility ("default")));

extern void
__splitstack_setcontext (void *context[10])
  __attribute__ ((no_split_stack, visibility ("default")));

extern void *
__splitstack_makecontext (size_t, void *context[10], size_t *)
  __attribute__ ((visibility ("default")));

extern void *
__splitstack_resetcontext (void *context[10], size_t *)
  __attribute__ ((visibility ("default")));

extern void
__splitstack_releasecontext (void *context[10])
  __attribute__ ((visibility ("default")));

extern void
__splitstack_block_signals_context (void *context[10], int *, int *)
  __attribute__ ((visibility ("default")));

extern void *
__splitstack_find_context (void *context[10], size_t *, void **, void **,
			   void **)
  __attribute__ ((visibility ("default")));

/* These functions must be defined by the processor specific code.  */

extern void *__morestack_get_guard (void)
  __attribute__ ((no_split_stack, visibility ("hidden")));

extern void __morestack_set_guard (void *)
  __attribute__ ((no_split_stack, visibility ("hidden")));

extern void *__morestack_make_guard (void *, size_t)
  __attribute__ ((no_split_stack, visibility ("hidden")));

/* When we allocate a stack segment we put this header at the
   start.  */

struct stack_segment
{
  /* The previous stack segment--when a function running on this stack
     segment returns, it will run on the previous one.  */
  struct stack_segment *prev;
  /* The next stack segment, if it has been allocated--when a function
     is running on this stack segment, the next one is not being
     used.  */
  struct stack_segment *next;
  /* The total size of this stack segment.  */
  size_t size;
  /* The stack address when this stack was created.  This is used when
     popping the stack.  */
  void *old_stack;
  /* A list of memory blocks allocated by dynamic stack
     allocation.  */
  struct dynamic_allocation_blocks *dynamic_allocation;
  /* A list of dynamic memory blocks no longer needed.  */
  struct dynamic_allocation_blocks *free_dynamic_allocation;
  /* An extra pointer in case we need some more information some
     day.  */
  void *extra;
};

/* This structure holds the (approximate) initial stack pointer and
   size for the system supplied stack for a thread.  This is set when
   the thread is created.  We also store a sigset_t here to hold the
   signal mask while splitting the stack, since we don't want to store
   that on the stack.  */

struct initial_sp
{
  /* The initial stack pointer.  */
  void *sp;
  /* The stack length.  */
  size_t len;
  /* A signal mask, put here so that the thread can use it without
     needing stack space.  */
  sigset_t mask;
  /* Non-zero if we should not block signals.  This is a reversed flag
     so that the default zero value is the safe value.  The type is
     uintptr_type because it replaced one of the void * pointers in
     extra.  */
  uintptr_type dont_block_signals;
  /* Some extra space for later extensibility.  */
  void *extra[4];
};

/* A list of memory blocks allocated by dynamic stack allocation.
   This is used for code that calls alloca or uses variably sized
   arrays.  */

struct dynamic_allocation_blocks
{
  /* The next block in the list.  */
  struct dynamic_allocation_blocks *next;
  /* The size of the allocated memory.  */
  size_t size;
  /* The allocated memory.  */
  void *block;
};

/* These thread local global variables must be shared by all split
   stack code across shared library boundaries.  Therefore, they have
   default visibility.  They have extensibility fields if needed for
   new versions.  If more radical changes are needed, new code can be
   written using new variable names, while still using the existing
   variables in a backward compatible manner.  Symbol versioning is
   also used, although, since these variables are only referenced by
   code in this file and generic-morestack-thread.c, it is likely that
   simply using new names will suffice.  */

/* The first stack segment allocated for this thread.  */

__thread struct stack_segment *__morestack_segments
  __attribute__ ((visibility ("default")));

/* The stack segment that we think we are currently using.  This will
   be correct in normal usage, but will be incorrect if an exception
   unwinds into a different stack segment or if longjmp jumps to a
   different stack segment.  */

__thread struct stack_segment *__morestack_current_segment
  __attribute__ ((visibility ("default")));

/* The initial stack pointer and size for this thread.  */

__thread struct initial_sp __morestack_initial_sp
  __attribute__ ((visibility ("default")));

/* A static signal mask, to avoid taking up stack space.  */

static sigset_t __morestack_fullmask;

/* Convert an integer to a decimal string without using much stack
   space.  Return a pointer to the part of the buffer to use.  We this
   instead of sprintf because sprintf will require too much stack
   space.  */

static char *
print_int (int val, char *buf, int buflen, size_t *print_len)
{
  int is_negative;
  int i;
  unsigned int uval;

  uval = (unsigned int) val;
  if (val >= 0)
    is_negative = 0;
  else
    {
      is_negative = 1;
      uval = - uval;
    }

  i = buflen;
  do
    {
      --i;
      buf[i] = '0' + (uval % 10);
      uval /= 10;
    }
  while (uval != 0 && i > 0);

  if (is_negative)
    {
      if (i > 0)
	--i;
      buf[i] = '-';
    }

  *print_len = buflen - i;
  return buf + i;
}

/* Print the string MSG/LEN, the errno number ERR, and a newline on
   stderr.  Then crash.  */

void
__morestack_fail (const char *, size_t, int) __attribute__ ((noreturn));

void
__morestack_fail (const char *msg, size_t len, int err)
{
  char buf[24];
  static const char nl[] = "\n";
  struct iovec iov[3];
  union { char *p; const char *cp; } const_cast;

  const_cast.cp = msg;
  iov[0].iov_base = const_cast.p;
  iov[0].iov_len = len;
  /* We can't call strerror, because it may try to translate the error
     message, and that would use too much stack space.  */
  iov[1].iov_base = print_int (err, buf, sizeof buf, &iov[1].iov_len);
  const_cast.cp = &nl[0];
  iov[2].iov_base = const_cast.p;
  iov[2].iov_len = sizeof nl - 1;
  /* FIXME: On systems without writev we need to issue three write
     calls, or punt on printing errno.  For now this is irrelevant
     since stack splitting only works on GNU/Linux anyhow.  */
  writev (2, iov, 3);
  abort ();
}

/* Allocate a new stack segment.  FRAME_SIZE is the required frame
   size.  */

static struct stack_segment *
allocate_segment (size_t frame_size)
{
  static unsigned int static_pagesize;
  static int use_guard_page;
  unsigned int pagesize;
  unsigned int overhead;
  unsigned int allocate;
  void *space;
  struct stack_segment *pss;

  pagesize = static_pagesize;
  if (pagesize == 0)
    {
      unsigned int p;

      pagesize = getpagesize ();

#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
      p = __sync_val_compare_and_swap (&static_pagesize, 0, pagesize);
#else
      /* Just hope this assignment is atomic.  */
      static_pagesize = pagesize;
      p = 0;
#endif

      use_guard_page = getenv ("SPLIT_STACK_GUARD") != 0;

      /* FIXME: I'm not sure this assert should be in the released
	 code.  */
      assert (p == 0 || p == pagesize);
    }

  overhead = sizeof (struct stack_segment);

  allocate = pagesize;
  if (allocate < MINSIGSTKSZ)
    allocate = ((MINSIGSTKSZ + overhead + pagesize - 1)
		& ~ (pagesize - 1));
  if (allocate < frame_size)
    allocate = ((frame_size + overhead + pagesize - 1)
		& ~ (pagesize - 1));

  if (use_guard_page)
    allocate += pagesize;

  /* FIXME: If this binary requires an executable stack, then we need
     to set PROT_EXEC.  Unfortunately figuring that out is complicated
     and target dependent.  We would need to use dl_iterate_phdr to
     see if there is any object which does not have a PT_GNU_STACK
     phdr, though only for architectures which use that mechanism.  */
  space = mmap (NULL, allocate, PROT_READ | PROT_WRITE,
		MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  if (space == MAP_FAILED)
    {
      static const char msg[] =
	"unable to allocate additional stack space: errno ";
      __morestack_fail (msg, sizeof msg - 1, errno);
    }

  if (use_guard_page)
    {
      void *guard;

#ifdef STACK_GROWS_DOWNWARD
      guard = space;
      space = (char *) space + pagesize;
#else
      guard = space + allocate - pagesize;
#endif

      mprotect (guard, pagesize, PROT_NONE);
      allocate -= pagesize;
    }

  pss = (struct stack_segment *) space;

  pss->prev = NULL;
  pss->next = NULL;
  pss->size = allocate - overhead;
  pss->dynamic_allocation = NULL;
  pss->free_dynamic_allocation = NULL;
  pss->extra = NULL;

  return pss;
}

/* Free a list of dynamic blocks.  */

static void
free_dynamic_blocks (struct dynamic_allocation_blocks *p)
{
  while (p != NULL)
    {
      struct dynamic_allocation_blocks *next;

      next = p->next;
      free (p->block);
      free (p);
      p = next;
    }
}

/* Merge two lists of dynamic blocks.  */

static struct dynamic_allocation_blocks *
merge_dynamic_blocks (struct dynamic_allocation_blocks *a,
		      struct dynamic_allocation_blocks *b)
{
  struct dynamic_allocation_blocks **pp;

  if (a == NULL)
    return b;
  if (b == NULL)
    return a;
  for (pp = &a->next; *pp != NULL; pp = &(*pp)->next)
    ;
  *pp = b;
  return a;
}

/* Release stack segments.  If FREE_DYNAMIC is non-zero, we also free
   any dynamic blocks.  Otherwise we return them.  */

struct dynamic_allocation_blocks *
__morestack_release_segments (struct stack_segment **pp, int free_dynamic)
{
  struct dynamic_allocation_blocks *ret;
  struct stack_segment *pss;

  ret = NULL;
  pss = *pp;
  while (pss != NULL)
    {
      struct stack_segment *next;
      unsigned int allocate;

      next = pss->next;

      if (pss->dynamic_allocation != NULL
	  || pss->free_dynamic_allocation != NULL)
	{
	  if (free_dynamic)
	    {
	      free_dynamic_blocks (pss->dynamic_allocation);
	      free_dynamic_blocks (pss->free_dynamic_allocation);
	    }
	  else
	    {
	      ret = merge_dynamic_blocks (pss->dynamic_allocation, ret);
	      ret = merge_dynamic_blocks (pss->free_dynamic_allocation, ret);
	    }
	}

      allocate = pss->size + sizeof (struct stack_segment);
      if (munmap (pss, allocate) < 0)
	{
	  static const char msg[] = "munmap of stack space failed: errno ";
	  __morestack_fail (msg, sizeof msg - 1, errno);
	}

      pss = next;
    }
  *pp = NULL;

  return ret;
}

/* This function is called by a processor specific function to set the
   initial stack pointer for a thread.  The operating system will
   always create a stack for a thread.  Here we record a stack pointer
   near the base of that stack.  The size argument lets the processor
   specific code estimate how much stack space is available on this
   initial stack.  */

void
__generic_morestack_set_initial_sp (void *sp, size_t len)
{
  /* The stack pointer most likely starts on a page boundary.  Adjust
     to the nearest 512 byte boundary.  It's not essential that we be
     precise here; getting it wrong will just leave some stack space
     unused.  */
#ifdef STACK_GROWS_DOWNWARD
  sp = (void *) ((((__UINTPTR_TYPE__) sp + 511U) / 512U) * 512U);
#else
  sp = (void *) ((((__UINTPTR_TYPE__) sp - 511U) / 512U) * 512U);
#endif

  __morestack_initial_sp.sp = sp;
  __morestack_initial_sp.len = len;
  sigemptyset (&__morestack_initial_sp.mask);

  sigfillset (&__morestack_fullmask);
#ifdef __GLIBC__
  /* In glibc, the first two real time signals are used by the NPTL
     threading library.  By taking them out of the set of signals, we
     avoiding copying the signal mask in pthread_sigmask.  More
     importantly, pthread_sigmask uses less stack space on x86_64.  */
  sigdelset (&__morestack_fullmask, __SIGRTMIN);
  sigdelset (&__morestack_fullmask, __SIGRTMIN + 1);
#endif
}

/* This function is called by a processor specific function which is
   run in the prologue when more stack is needed.  The processor
   specific function handles the details of saving registers and
   frobbing the actual stack pointer.  This function is responsible
   for allocating a new stack segment and for copying a parameter
   block from the old stack to the new one.  On function entry
   *PFRAME_SIZE is the size of the required stack frame--the returned
   stack must be at least this large.  On function exit *PFRAME_SIZE
   is the amount of space remaining on the allocated stack.  OLD_STACK
   points at the parameters the old stack (really the current one
   while this function is running).  OLD_STACK is saved so that it can
   be returned by a later call to __generic_releasestack.  PARAM_SIZE
   is the size in bytes of parameters to copy to the new stack.  This
   function returns a pointer to the new stack segment, pointing to
   the memory after the parameters have been copied.  The returned
   value minus the returned *PFRAME_SIZE (or plus if the stack grows
   upward) is the first address on the stack which should not be used.

   This function is running on the old stack and has only a limited
   amount of stack space available.  */

void *
__generic_morestack (size_t *pframe_size, void *old_stack, size_t param_size)
{
  size_t frame_size = *pframe_size;
  struct stack_segment *current;
  struct stack_segment **pp;
  struct dynamic_allocation_blocks *dynamic;
  char *from;
  char *to;
  void *ret;
  size_t i;

  current = __morestack_current_segment;

  pp = current != NULL ? &current->next : &__morestack_segments;
  if (*pp != NULL && (*pp)->size < frame_size)
    dynamic = __morestack_release_segments (pp, 0);
  else
    dynamic = NULL;
  current = *pp;

  if (current == NULL)
    {
      current = allocate_segment (frame_size + param_size);
      current->prev = __morestack_current_segment;
      *pp = current;
    }

  current->old_stack = old_stack;

  __morestack_current_segment = current;

  if (dynamic != NULL)
    {
      /* Move the free blocks onto our list.  We don't want to call
	 free here, as we are short on stack space.  */
      current->free_dynamic_allocation =
	merge_dynamic_blocks (dynamic, current->free_dynamic_allocation);
    }

  *pframe_size = current->size - param_size;

#ifdef STACK_GROWS_DOWNWARD
  {
    char *bottom = (char *) (current + 1) + current->size;
    to = bottom - param_size;
    ret = bottom - param_size;
  }
#else
  to = current + 1;
  ret = (char *) (current + 1) + param_size;
#endif

  /* We don't call memcpy to avoid worrying about the dynamic linker
     trying to resolve it.  */
  from = (char *) old_stack;
  for (i = 0; i < param_size; i++)
    *to++ = *from++;

  return ret;
}

/* This function is called by a processor specific function when it is
   ready to release a stack segment.  We don't actually release the
   stack segment, we just move back to the previous one.  The current
   stack segment will still be available if we need it in
   __generic_morestack.  This returns a pointer to the new stack
   segment to use, which is the one saved by a previous call to
   __generic_morestack.  The processor specific function is then
   responsible for actually updating the stack pointer.  This sets
   *PAVAILABLE to the amount of stack space now available.  */

void *
__generic_releasestack (size_t *pavailable)
{
  struct stack_segment *current;
  void *old_stack;

  current = __morestack_current_segment;
  old_stack = current->old_stack;
  current = current->prev;
  __morestack_current_segment = current;

  if (current != NULL)
    {
#ifdef STACK_GROWS_DOWNWARD
      *pavailable = (char *) old_stack - (char *) (current + 1);
#else
      *pavailable = (char *) (current + 1) + current->size - (char *) old_stack;
#endif
    }
  else
    {
      size_t used;

      /* We have popped back to the original stack.  */
#ifdef STACK_GROWS_DOWNWARD
      if ((char *) old_stack >= (char *) __morestack_initial_sp.sp)
	used = 0;
      else
	used = (char *) __morestack_initial_sp.sp - (char *) old_stack;
#else
      if ((char *) old_stack <= (char *) __morestack_initial_sp.sp)
	used = 0;
      else
	used = (char *) old_stack - (char *) __morestack_initial_sp.sp;
#endif

      if (used > __morestack_initial_sp.len)
	*pavailable = 0;
      else
	*pavailable = __morestack_initial_sp.len - used;
    }

  return old_stack;
}

/* Block signals while splitting the stack.  This avoids trouble if we
   try to invoke a signal handler which itself wants to split the
   stack.  */

extern int pthread_sigmask (int, const sigset_t *, sigset_t *)
  __attribute__ ((weak));

void
__morestack_block_signals (void)
{
  if (__morestack_initial_sp.dont_block_signals)
    ;
  else if (pthread_sigmask)
    pthread_sigmask (SIG_BLOCK, &__morestack_fullmask,
		     &__morestack_initial_sp.mask);
  else
    sigprocmask (SIG_BLOCK, &__morestack_fullmask,
		 &__morestack_initial_sp.mask);
}

/* Unblock signals while splitting the stack.  */

void
__morestack_unblock_signals (void)
{
  if (__morestack_initial_sp.dont_block_signals)
    ;
  else if (pthread_sigmask)
    pthread_sigmask (SIG_SETMASK, &__morestack_initial_sp.mask, NULL);
  else
    sigprocmask (SIG_SETMASK, &__morestack_initial_sp.mask, NULL);
}

/* This function is called to allocate dynamic stack space, for alloca
   or a variably sized array.  This is a regular function with
   sufficient stack space, so we just use malloc to allocate the
   space.  We attach the allocated blocks to the current stack
   segment, so that they will eventually be reused or freed.  */

void *
__morestack_allocate_stack_space (size_t size)
{
  struct stack_segment *seg, *current;
  struct dynamic_allocation_blocks *p;

  /* We have to block signals to avoid getting confused if we get
     interrupted by a signal whose handler itself uses alloca or a
     variably sized array.  */
  __morestack_block_signals ();

  /* Since we don't want to call free while we are low on stack space,
     we may have a list of already allocated blocks waiting to be
     freed.  Release them all, unless we find one that is large
     enough.  We don't look at every block to see if one is large
     enough, just the first one, because we aren't trying to build a
     memory allocator here, we're just trying to speed up common
     cases.  */

  current = __morestack_current_segment;
  p = NULL;
  for (seg = __morestack_segments; seg != NULL; seg = seg->next)
    {
      p = seg->free_dynamic_allocation;
      if (p != NULL)
	{
	  if (p->size >= size)
	    {
	      seg->free_dynamic_allocation = p->next;
	      break;
	    }

	  free_dynamic_blocks (p);
	  seg->free_dynamic_allocation = NULL;
	  p = NULL;
	}
    }

  if (p == NULL)
    {
      /* We need to allocate additional memory.  */
      p = malloc (sizeof (*p));
      if (p == NULL)
	abort ();
      p->size = size;
      p->block = malloc (size);
      if (p->block == NULL)
	abort ();
    }

  /* If we are still on the initial stack, then we have a space leak.
     FIXME.  */
  if (current != NULL)
    {
      p->next = current->dynamic_allocation;
      current->dynamic_allocation = p;
    }

  __morestack_unblock_signals ();

  return p->block;
}

/* Find the stack segment for STACK and return the amount of space
   available.  This is used when unwinding the stack because of an
   exception, in order to reset the stack guard correctly.  */

size_t
__generic_findstack (void *stack)
{
  struct stack_segment *pss;
  size_t used;

  for (pss = __morestack_current_segment; pss != NULL; pss = pss->prev)
    {
      if ((char *) pss < (char *) stack
	  && (char *) pss + pss->size > (char *) stack)
	{
	  __morestack_current_segment = pss;
#ifdef STACK_GROWS_DOWNWARD
	  return (char *) stack - (char *) (pss + 1);
#else
	  return (char *) (pss + 1) + pss->size - (char *) stack;
#endif
	}
    }

  /* We have popped back to the original stack.  */

  if (__morestack_initial_sp.sp == NULL)
    return 0;

#ifdef STACK_GROWS_DOWNWARD
  if ((char *) stack >= (char *) __morestack_initial_sp.sp)
    used = 0;
  else
    used = (char *) __morestack_initial_sp.sp - (char *) stack;
#else
  if ((char *) stack <= (char *) __morestack_initial_sp.sp)
    used = 0;
  else
    used = (char *) stack - (char *) __morestack_initial_sp.sp;
#endif

  if (used > __morestack_initial_sp.len)
    return 0;
  else
    return __morestack_initial_sp.len - used;
}

/* This function is called at program startup time to make sure that
   mmap, munmap, and getpagesize are resolved if linking dynamically.
   We want to resolve them while we have enough stack for them, rather
   than calling into the dynamic linker while low on stack space.  */

void
__morestack_load_mmap (void)
{
  /* Call with bogus values to run faster.  We don't care if the call
     fails.  Pass __MORESTACK_CURRENT_SEGMENT to make sure that any
     TLS accessor function is resolved.  */
  mmap (__morestack_current_segment, 0, PROT_READ, MAP_ANONYMOUS, -1, 0);
  mprotect (NULL, 0, 0);
  munmap (0, getpagesize ());
}

/* This function may be used to iterate over the stack segments.
   This can be called like this.
     void *next_segment = NULL;
     void *next_sp = NULL;
     void *initial_sp = NULL;
     void *stack;
     size_t stack_size;
     while ((stack = __splitstack_find (next_segment, next_sp, &stack_size,
                                        &next_segment, &next_sp,
					&initial_sp)) != NULL)
       {
         // Stack segment starts at stack and is stack_size bytes long.
       }

   There is no way to iterate over the stack segments of a different
   thread.  However, what is permitted is for one thread to call this
   with the first two values NULL, to pass next_segment, next_sp, and
   initial_sp to a different thread, and then to suspend one way or
   another.  A different thread may run the subsequent
   __morestack_find iterations.  Of course, this will only work if the
   first thread is suspended during the __morestack_find iterations.
   If not, the second thread will be looking at the stack while it is
   changing, and anything could happen.

   FIXME: This should be declared in some header file, but where?  */

void *
__splitstack_find (void *segment_arg, void *sp, size_t *len,
		   void **next_segment, void **next_sp,
		   void **initial_sp)
{
  struct stack_segment *segment;
  void *ret;
  char *nsp;

  if (segment_arg == (void *) (uintptr_type) 1)
    {
      char *isp = (char *) *initial_sp;

      if (isp == NULL)
	return NULL;

      *next_segment = (void *) (uintptr_type) 2;
      *next_sp = NULL;
#ifdef STACK_GROWS_DOWNWARD
      if ((char *) sp >= isp)
	return NULL;
      *len = (char *) isp - (char *) sp;
      return sp;
#else
      if ((char *) sp <= (char *) isp)
	return NULL;
      *len = (char *) sp - (char *) isp;
      return (void *) isp;
#endif
    }
  else if (segment_arg == (void *) (uintptr_type) 2)
    return NULL;
  else if (segment_arg != NULL)
    segment = (struct stack_segment *) segment_arg;
  else
    {
      *initial_sp = __morestack_initial_sp.sp;
      segment = __morestack_current_segment;
      sp = (void *) &segment;
      while (1)
	{
	  if (segment == NULL)
	    return __splitstack_find ((void *) (uintptr_type) 1, sp, len,
				      next_segment, next_sp, initial_sp);
	  if ((char *) sp >= (char *) (segment + 1)
	      && (char *) sp <= (char *) (segment + 1) + segment->size)
	    break;
	  segment = segment->prev;
	}
    }

  if (segment->prev == NULL)
    *next_segment = (void *) (uintptr_type) 1;
  else
    *next_segment = segment->prev;

  /* The old_stack value is the address of the function parameters of
     the function which called __morestack.  So if f1 called f2 which
     called __morestack, the stack looks like this:

         parameters       <- old_stack
         return in f1
	 return in f2
	 registers pushed by __morestack

     The registers pushed by __morestack may not be visible on any
     other stack, if we are being called by a signal handler
     immediately after the call to __morestack_unblock_signals.  We
     want to adjust our return value to include those registers.  This
     is target dependent.  */

  nsp = (char *) segment->old_stack;

  if (nsp == NULL)
    {
      /* We've reached the top of the stack.  */
      *next_segment = (void *) (uintptr_type) 2;
    }
  else
    {
#if defined (__x86_64__)
      nsp -= 12 * sizeof (void *);
#elif defined (__i386__)
      nsp -= 6 * sizeof (void *);
#else
#error "unrecognized target"
#endif

      *next_sp = (void *) nsp;
    }

#ifdef STACK_GROWS_DOWNWARD
  *len = (char *) (segment + 1) + segment->size - (char *) sp;
  ret = (void *) sp;
#else
  *len = (char *) sp - (char *) (segment + 1);
  ret = (void *) (segment + 1);
#endif

  return ret;
}

/* Tell the split stack code whether it has to block signals while
   manipulating the stack.  This is for programs in which some threads
   block all signals.  If a thread already blocks signals, there is no
   need for the split stack code to block them as well.  If NEW is not
   NULL, then if *NEW is non-zero signals will be blocked while
   splitting the stack, otherwise they will not.  If OLD is not NULL,
   *OLD will be set to the old value.  */

void
__splitstack_block_signals (int *new, int *old)
{
  if (old != NULL)
    *old = __morestack_initial_sp.dont_block_signals ? 0 : 1;
  if (new != NULL)
    __morestack_initial_sp.dont_block_signals = *new ? 0 : 1;
}

/* The offsets into the arrays used by __splitstack_getcontext and
   __splitstack_setcontext.  */

enum __splitstack_context_offsets
{
  MORESTACK_SEGMENTS = 0,
  CURRENT_SEGMENT = 1,
  CURRENT_STACK = 2,
  STACK_GUARD = 3,
  INITIAL_SP = 4,
  INITIAL_SP_LEN = 5,
  BLOCK_SIGNALS = 6,

  NUMBER_OFFSETS = 10
};

/* Get the current split stack context.  This may be used for
   coroutine switching, similar to getcontext.  The argument should
   have at least 10 void *pointers for extensibility, although we
   don't currently use all of them.  This would normally be called
   immediately before a call to getcontext or swapcontext or
   setjmp.  */

void
__splitstack_getcontext (void *context[NUMBER_OFFSETS])
{
  memset (context, 0, NUMBER_OFFSETS * sizeof (void *));
  context[MORESTACK_SEGMENTS] = (void *) __morestack_segments;
  context[CURRENT_SEGMENT] = (void *) __morestack_current_segment;
  context[CURRENT_STACK] = (void *) &context;
  context[STACK_GUARD] = __morestack_get_guard ();
  context[INITIAL_SP] = (void *) __morestack_initial_sp.sp;
  context[INITIAL_SP_LEN] = (void *) (uintptr_type) __morestack_initial_sp.len;
  context[BLOCK_SIGNALS] = (void *) __morestack_initial_sp.dont_block_signals;
}

/* Set the current split stack context.  The argument should be a
   context previously passed to __splitstack_getcontext.  This would
   normally be called immediately after a call to getcontext or
   swapcontext or setjmp if something jumped to it.  */

void
__splitstack_setcontext (void *context[NUMBER_OFFSETS])
{
  __morestack_segments = (struct stack_segment *) context[MORESTACK_SEGMENTS];
  __morestack_current_segment =
    (struct stack_segment *) context[CURRENT_SEGMENT];
  __morestack_set_guard (context[STACK_GUARD]);
  __morestack_initial_sp.sp = context[INITIAL_SP];
  __morestack_initial_sp.len = (size_t) context[INITIAL_SP_LEN];
  __morestack_initial_sp.dont_block_signals =
    (uintptr_type) context[BLOCK_SIGNALS];
}

/* Create a new split stack context.  This will allocate a new stack
   segment which may be used by a coroutine.  STACK_SIZE is the
   minimum size of the new stack.  The caller is responsible for
   actually setting the stack pointer.  This would normally be called
   before a call to makecontext, and the returned stack pointer and
   size would be used to set the uc_stack field.  A function called
   via makecontext on a stack created by __splitstack_makecontext may
   not return.  Note that the returned pointer points to the lowest
   address in the stack space, and thus may not be the value to which
   to set the stack pointer.  */

void *
__splitstack_makecontext (size_t stack_size, void *context[NUMBER_OFFSETS],
			  size_t *size)
{
  struct stack_segment *segment;
  void *initial_sp;

  memset (context, 0, NUMBER_OFFSETS * sizeof (void *));
  segment = allocate_segment (stack_size);
  context[MORESTACK_SEGMENTS] = segment;
  context[CURRENT_SEGMENT] = segment;
#ifdef STACK_GROWS_DOWNWARD
  initial_sp = (void *) ((char *) (segment + 1) + segment->size);
#else
  initial_sp = (void *) (segment + 1);
#endif
  context[STACK_GUARD] = __morestack_make_guard (initial_sp, segment->size);
  context[INITIAL_SP] = NULL;
  context[INITIAL_SP_LEN] = 0;
  *size = segment->size;
  return (void *) (segment + 1);
}

/* Given an existing split stack context, reset it back to the start
   of the stack.  Return the stack pointer and size, appropriate for
   use with makecontext.  This may be used if a coroutine exits, in
   order to reuse the stack segments for a new coroutine.  */

void *
__splitstack_resetcontext (void *context[10], size_t *size)
{
  struct stack_segment *segment;
  void *initial_sp;
  size_t initial_size;
  void *ret;

  /* Reset the context assuming that MORESTACK_SEGMENTS, INITIAL_SP
     and INITIAL_SP_LEN are correct.  */

  segment = context[MORESTACK_SEGMENTS];
  context[CURRENT_SEGMENT] = segment;
  context[CURRENT_STACK] = NULL;
  if (segment == NULL)
    {
      initial_sp = context[INITIAL_SP];
      initial_size = (uintptr_type) context[INITIAL_SP_LEN];
      ret = initial_sp;
#ifdef STACK_GROWS_DOWNWARD
      ret = (void *) ((char *) ret - initial_size);
#endif
    }
  else
    {
#ifdef STACK_GROWS_DOWNWARD
      initial_sp = (void *) ((char *) (segment + 1) + segment->size);
#else
      initial_sp = (void *) (segment + 1);
#endif
      initial_size = segment->size;
      ret = (void *) (segment + 1);
    }
  context[STACK_GUARD] = __morestack_make_guard (initial_sp, initial_size);
  context[BLOCK_SIGNALS] = NULL;
  *size = initial_size;
  return ret;
}

/* Release all the memory associated with a splitstack context.  This
   may be used if a coroutine exits and the associated stack should be
   freed.  */

void
__splitstack_releasecontext (void *context[10])
{
  __morestack_release_segments (context[MORESTACK_SEGMENTS], 1);
}

/* Like __splitstack_block_signals, but operating on CONTEXT, rather
   than on the current state.  */

void
__splitstack_block_signals_context (void *context[NUMBER_OFFSETS], int *new,
				    int *old)
{
  if (old != NULL)
    *old = ((uintptr_type) context[BLOCK_SIGNALS]) != 0 ? 0 : 1;
  if (new != NULL)
    context[BLOCK_SIGNALS] = (void *) (uintptr_type) (*new ? 0 : 1);
}

/* Find the stack segments associated with a split stack context.
   This will return the address of the first stack segment and set
   *STACK_SIZE to its size.  It will set next_segment, next_sp, and
   initial_sp which may be passed to __splitstack_find to find the
   remaining segments.  */

void *
__splitstack_find_context (void *context[NUMBER_OFFSETS], size_t *stack_size,
			   void **next_segment, void **next_sp,
			   void **initial_sp)
{
  void *sp;
  struct stack_segment *segment;

  *initial_sp = context[INITIAL_SP];

  sp = context[CURRENT_STACK];
  if (sp == NULL)
    {
      /* Most likely this context was created but was never used.  The
	 value 2 is a code used by __splitstack_find to mean that we
	 have reached the end of the list of stacks.  */
      *next_segment = (void *) (uintptr_type) 2;
      *next_sp = NULL;
      *initial_sp = NULL;
      return NULL;
    }

  segment = context[CURRENT_SEGMENT];
  if (segment == NULL)
    {
      /* Most likely this context was saved by a thread which was not
	 created using __splistack_makecontext and which has never
	 split the stack.  The value 1 is a code used by
	 __splitstack_find to look at the initial stack.  */
      segment = (struct stack_segment *) (uintptr_type) 1;
    }

  return __splitstack_find (segment, sp, stack_size, next_segment, next_sp,
			    initial_sp);
}

#endif /* !defined (inhibit_libc) */

/* go-cgo.c -- SWIG support routines for libgo.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "go-alloc.h"
#include "interface.h"
#include "go-panic.h"
#include "go-string.h"

/* Go memory allocated by code not written in Go.  We keep a linked
   list of these allocations so that the garbage collector can see
   them.  */

struct cgoalloc
{
  struct cgoalloc *next;
  void *alloc;
};

/* Prepare to call from code written in Go to code written in C or
   C++.  This takes the current goroutine out of the Go scheduler, as
   though it were making a system call.  Otherwise the program can
   lock up if the C code goes to sleep on a mutex or for some other
   reason.  This idea is to call this function, then immediately call
   the C/C++ function.  After the C/C++ function returns, call
   syscall_cgocalldone.  The usual Go code would look like

       syscall.Cgocall()
       defer syscall.Cgocalldone()
       cfunction()

   */

/* We let Go code call these via the syscall package.  */
void syscall_cgocall(void) __asm__ ("syscall.Cgocall");
void syscall_cgocalldone(void) __asm__ ("syscall.CgocallDone");
void syscall_cgocallback(void) __asm__ ("syscall.CgocallBack");
void syscall_cgocallbackdone(void) __asm__ ("syscall.CgocallBackDone");

void
syscall_cgocall ()
{
  M* m;
  G* g;

  m = runtime_m ();
  ++m->ncgocall;
  g = runtime_g ();
  ++g->ncgo;
  runtime_entersyscall ();
}

/* Prepare to return to Go code from C/C++ code.  */

void
syscall_cgocalldone ()
{
  G* g;

  g = runtime_g ();
  __go_assert (g != NULL);
  --g->ncgo;
  if (g->ncgo == 0)
    {
      /* We are going back to Go, and we are not in a recursive call.
	 Let the garbage collector clean up any unreferenced
	 memory.  */
      g->cgoalloc = NULL;
    }

  /* If we are invoked because the C function called _cgo_panic, then
     _cgo_panic will already have exited syscall mode.  */
  if (g->status == Gsyscall)
    runtime_exitsyscall ();
}

/* Call back from C/C++ code to Go code.  */

void
syscall_cgocallback ()
{
  runtime_exitsyscall ();
}

/* Prepare to return to C/C++ code from a callback to Go code.  */

void
syscall_cgocallbackdone ()
{
  runtime_entersyscall ();
}

/* Allocate memory and save it in a list visible to the Go garbage
   collector.  */

void *
alloc_saved (size_t n)
{
  void *ret;
  G *g;
  struct cgoalloc *c;

  ret = __go_alloc (n);

  g = runtime_g ();
  c = (struct cgoalloc *) __go_alloc (sizeof (struct cgoalloc));
  c->next = g->cgoalloc;
  c->alloc = ret;
  g->cgoalloc = c;

  return ret;
}

/* These are routines used by SWIG.  The gc runtime library provides
   the same routines under the same name, though in that case the code
   is required to import runtime/cgo.  */

void *
_cgo_allocate (size_t n)
{
  void *ret;

  runtime_exitsyscall ();
  ret = alloc_saved (n);
  runtime_entersyscall ();
  return ret;
}

extern const struct __go_type_descriptor string_type_descriptor
  asm ("__go_tdn_string");

void
_cgo_panic (const char *p)
{
  int len;
  unsigned char *data;
  struct __go_string *ps;
  struct __go_empty_interface e;

  runtime_exitsyscall ();
  len = __builtin_strlen (p);
  data = alloc_saved (len);
  __builtin_memcpy (data, p, len);
  ps = alloc_saved (sizeof *ps);
  ps->__data = data;
  ps->__length = len;
  e.__type_descriptor = &string_type_descriptor;
  e.__object = ps;

  /* We don't call runtime_entersyscall here, because normally what
     will happen is that we will walk up the stack to a Go deferred
     function that calls recover.  However, this will do the wrong
     thing if this panic is recovered and the stack unwinding is
     caught by a C++ exception handler.  It might be possible to
     handle this by calling runtime_entersyscall in the personality
     function in go-unwind.c.  FIXME.  */

  __go_panic (e);
}

/* Return the number of CGO calls.  */

int64 runtime_NumCgoCall (void) __asm__ ("runtime.NumCgoCall");

int64
runtime_NumCgoCall (void)
{
  int64 ret;
  M* m;

  ret = 0;
  for (m = runtime_atomicloadp (&runtime_allm); m != NULL; m = m->alllink)
    ret += m->ncgocall;
  return ret;
}

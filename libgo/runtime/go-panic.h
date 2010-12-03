/* go-panic.h -- declare the go panic functions.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#ifndef LIBGO_GO_PANIC_H
#define LIBGO_GO_PANIC_H

#include "interface.h"

struct __go_string;
struct __go_type_descriptor;
struct __go_defer_stack;

/* The stack of panic calls.  */

struct __go_panic_stack
{
  /* The next entry in the stack.  */
  struct __go_panic_stack *__next;

  /* The value associated with this panic.  */
  struct __go_empty_interface __arg;

  /* Whether this panic has been recovered.  */
  _Bool __was_recovered;

  /* Whether this panic was pushed on the stack because of an
     exception thrown in some other language.  */
  _Bool __is_foreign;
};

/* The panic and defer stacks, grouped together into a single thread
   local variable for convenience for systems without TLS.  */

struct __go_panic_defer_struct
{
  /* The list of defers to execute.  */
  struct __go_defer_stack *__defer;

  /* The list of currently active panics.  There will be more than one
     if a deferred function calls panic.  */
  struct __go_panic_stack *__panic;

  /* The current exception being thrown when unwinding after a call to
     panic .  This is really struct _UnwindException *.  */
  void *__exception;

  /* Whether the current exception is from some other language.  */
  _Bool __is_foreign;
};

#ifdef __rtems__
#define __thread
#endif

extern __thread struct __go_panic_defer_struct *__go_panic_defer;

#ifdef __rtems__
#undef __thread
#endif

extern void __go_panic (struct __go_empty_interface)
  __attribute__ ((noreturn));

extern void __go_panic_msg (const char* msg)
  __attribute__ ((noreturn));

extern void __go_print_string (struct __go_string);

extern struct __go_empty_interface __go_recover (void);

extern void __go_unwind_stack (void);

/* Functions defined in libgo/go/runtime/error.go.  */

extern void newTypeAssertionError(const struct __go_type_descriptor *pt1,
				  const struct __go_type_descriptor *pt2,
				  const struct __go_type_descriptor *pt3,
				  const struct __go_string *ps1,
				  const struct __go_string *ps2,
				  const struct __go_string *ps3,
				  const struct __go_string *pmeth,
				  struct __go_empty_interface *ret)
  __asm__ ("libgo_runtime.runtime.NewTypeAssertionError");

extern void newErrorString(struct __go_string, struct __go_empty_interface *)
  __asm__ ("libgo_runtime.runtime.NewErrorString");

extern void printany(struct __go_empty_interface)
  __asm__ ("libgo_runtime.runtime.Printany");

#endif /* !defined(LIBGO_GO_PANIC_H) */

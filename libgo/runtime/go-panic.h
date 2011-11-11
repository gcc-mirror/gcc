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

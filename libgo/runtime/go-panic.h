/* go-panic.h -- declare the go panic functions.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#ifndef LIBGO_GO_PANIC_H
#define LIBGO_GO_PANIC_H

#include "interface.h"

struct String;
struct __go_type_descriptor;

extern void __go_panic (struct __go_empty_interface)
  __attribute__ ((noreturn));

extern void __go_print_string (struct String);

extern struct __go_empty_interface __go_recover (void);

extern _Bool __go_can_recover (void *);

extern void __go_makefunc_can_recover (void *retaddr);

struct location;
extern void __go_makefunc_ffi_can_recover (struct location *, int);

extern void __go_makefunc_returning (void);

extern void __go_unwind_stack (void);

#endif /* !defined(LIBGO_GO_PANIC_H) */

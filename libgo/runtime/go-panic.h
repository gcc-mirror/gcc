/* go-panic.h -- declare the go panic functions.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#ifndef LIBGO_GO_PANIC_H
#define LIBGO_GO_PANIC_H

extern void __go_panic (Eface)
  __attribute__ ((noreturn));

extern void __go_print_string (String);

extern Eface __go_recover (void);

extern _Bool __go_can_recover (void *);

extern void __go_makefunc_can_recover (void *retaddr);

extern void __go_makefunc_ffi_can_recover (Location*, int);

extern void __go_makefunc_returning (void);

extern void __go_unwind_stack (void);

#endif /* !defined(LIBGO_GO_PANIC_H) */

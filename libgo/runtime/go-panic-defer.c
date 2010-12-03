/* go-panic-stack.c -- The panic/defer stack.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-panic.h"

#ifdef __rtems__
#define __thread
#endif

__thread struct __go_panic_defer_struct *__go_panic_defer;

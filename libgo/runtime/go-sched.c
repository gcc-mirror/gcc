/* go-sched.c -- the runtime.Gosched function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <sched.h>

void Gosched (void) asm ("libgo_runtime.runtime.Gosched");

void
Gosched (void)
{
  sched_yield ();
}

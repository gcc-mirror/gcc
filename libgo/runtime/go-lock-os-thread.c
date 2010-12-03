/* go-lock-os-thread.c -- the LockOSThread and UnlockOSThread functions.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* The runtime.LockOSThread and runtime.UnlockOSThread functions are
   meaningless in the current implementation, since for us a goroutine
   always stays on a single OS thread.  */

extern void LockOSThread (void) __asm__ ("libgo_runtime.runtime.LockOSThread");

void
LockOSThread (void)
{
}

extern void UnlockOSThread (void)
  __asm__ ("libgo_runtime.runtime.UnlockOSThread");

void
UnlockOSThread (void)
{
}

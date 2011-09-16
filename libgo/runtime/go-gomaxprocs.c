/* go-gomaxprocs.c -- runtime.GOMAXPROCS.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* This is the runtime.GOMAXPROCS function.  This currently does
   nothing, since each goroutine runs in a separate thread anyhow.  */

extern int GOMAXPROCS (int) asm ("libgo_runtime.runtime.GOMAXPROCS");

static int set = 1;

int
GOMAXPROCS (int n)
{
  int ret;

  ret = set;
  if (n > 0)
    set = n;
  return ret;
}

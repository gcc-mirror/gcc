// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Return current time in nanoseconds.

#include <sys/time.h>

#include "runtime.h"

int64 runtime_nanotime (void)
  __attribute__ ((no_split_stack));

int64
runtime_nanotime (void)
{
  struct timespec ts;

  clock_gettime (CLOCK_MONOTONIC, &ts);
  return (int64) ts.tv_sec * 1000000000 + (int64) ts.tv_nsec;
}

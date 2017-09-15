// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stddef.h>
#include <stdint.h>
#include <sys/time.h>

#include "runtime.h"

// Return current time.  This is the implementation of time.walltime().

struct walltime_ret
{
  int64_t sec;
  int32_t nsec;
};

struct walltime_ret now() __asm__ (GOSYM_PREFIX "runtime.walltime")
  __attribute__ ((no_split_stack));

struct walltime_ret
now()
{
  struct timespec ts;
  struct walltime_ret ret;

  clock_gettime (CLOCK_REALTIME, &ts);
  ret.sec = ts.tv_sec;
  ret.nsec = ts.tv_nsec;
  return ret;
}

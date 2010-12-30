// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "malloc.h"	/* so that acid generated from proc.c includes malloc data structures */

typedef struct Sched Sched;

M	m0;

#ifdef __rtems__
#define __thread
#endif

__thread M *m = &m0;

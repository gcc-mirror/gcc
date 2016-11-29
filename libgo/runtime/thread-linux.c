// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "defs.h"

// Linux futex.

#include <unistd.h>
#include <syscall.h>
#include <linux/futex.h>

void
runtime_osinit(void)
{
	runtime_ncpu = getproccount();
}

void
runtime_goenvs(void)
{
	runtime_goenvs_unix();
}

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "config.h"
#include "runtime.h"

#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <semaphore.h>

void
runtime_osinit (void)
{
  runtime_ncpu = getproccount();
}

void
runtime_goenvs (void)
{
  runtime_goenvs_unix ();
}

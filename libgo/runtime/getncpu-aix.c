// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <sys/types.h>
#include <sys/systemcfg.h>

#include "runtime.h"
#include "defs.h"

int32_t
getproccount(void)
{
	return _system_configuration.ncpus;
}

// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package net

/*
#include <netdb.h>
*/

import "syscall"

func cgoAddrInfoMask() C.int {
	return syscall.AI_MASK
}

// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package net

/*
#include <netdb.h>
*/

import "syscall"

func cgoAddrInfoMask() int {
	return syscall.AI_CANONNAME | syscall.AI_V4MAPPED | syscall.AI_ALL
}

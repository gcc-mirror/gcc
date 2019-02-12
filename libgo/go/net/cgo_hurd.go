// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// This file is derived from cgo_bsd.go

// +build cgo,!netgo
// +build hurd

package net

/*
#include <netdb.h>
*/

import "syscall"

const cgoAddrInfoFlags = syscall.AI_CANONNAME | syscall.AI_V4MAPPED | syscall.AI_ALL

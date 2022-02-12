// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build cgo && !netgo

package net

import (
	"syscall"
)

const cgoAddrInfoFlags = syscall.AI_CANONNAME

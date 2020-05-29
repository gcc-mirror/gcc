// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !aix,gccgo

package runtime

import (
	_ "unsafe"
)

// go:linkname is required as eqtype is a compiler-called
// function on some OSes.
//
//go:linkname eqtype

// Return whether two type descriptors are equal.
func eqtype(t1, t2 *_type) bool {
	return t1 == t2
}

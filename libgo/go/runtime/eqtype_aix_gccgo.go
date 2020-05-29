// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix,gccgo

package runtime

import (
	_ "unsafe"
)

// eqtype is a compiler-called function.
//
//go:linkname eqtype

// Return whether two type descriptors are equal.
// This is gccgo-specific, as some linkers are not able
// to merge identical type descriptors coming from
// different object or shared object files.
func eqtype(t1, t2 *_type) bool {
	switch {
	case t1 == t2:
		return true
	case t1 == nil || t2 == nil:
		return false
	case t1.kind != t2.kind || t1.hash != t2.hash:
		return false
	default:
		return t1.string() == t2.string()
	}
}

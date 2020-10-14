// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//+build aix,gccgo

// AIX linker isn't able to merge identical type descriptors coming from
// different objects. Thus, two rtypes might have two different pointers
// even if they are the same. Thus, instead of pointer equality, string
// field is checked.

package reflectlite

// rtypeEqual returns true if both types are identical.
func rtypeEqual(t1, t2 *rtype) bool {
	switch {
	case t1 == t2:
		return true
	case t1 == nil || t2 == nil:
		return false
	case t1.kind != t2.kind || t1.hash != t2.hash:
		return false
	default:
		return t1.String() == t2.String()
	}
}

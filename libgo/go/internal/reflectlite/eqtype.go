// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//+build !aix !gccgo

package reflectlite

// rtypeEqual returns true if both types are identical.
func rtypeEqual(t1, t2 *rtype) bool {
	return t1 == t2
}

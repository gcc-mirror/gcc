// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !aix !gccgo

package reflect

// rtypeEqual returns true if both rtypes are identical.
func rtypeEqual(t1, t2 *rtype) bool {
	return t1 == t2
}

// typeEqual returns true if both Types are identical.
func typeEqual(t1, t2 Type) bool {
	return t1 == t2
}

func toType(p *rtype) Type {
	if p == nil {
		return nil
	}
	return p
}

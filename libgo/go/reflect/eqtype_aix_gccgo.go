// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//+build aix,gccgo

// AIX linker isn't able to merge identical type descriptors coming from
// different objects. Thus, two rtypes might have two different pointers
// even if they are the same. Thus, instead of pointer equality, string
// field is checked.

package reflect

import (
	"sync"
)

// rtypeEqual returns true if both rtypes are identical.
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

// typeEqual returns true if both Types are identical.
func typeEqual(t1, t2 Type) bool {
	return rtypeEqual(t1.common(), t2.common())
}

// toType converts from a *rtype to a Type that can be returned
// to the client of package reflect. The only concern is that
// a nil *rtype must be replaced by a nil Type.
// On AIX, as type duplications can occur, it also ensure that
// multiple *rtype for the same  type are coalesced into a single
// Type.

var canonicalType = make(map[string]Type)

var canonicalTypeLock sync.RWMutex

func canonicalize(t Type) Type {
	if t == nil {
		return nil
	}
	s := t.rawString()
	canonicalTypeLock.RLock()
	if r, ok := canonicalType[s]; ok {
		canonicalTypeLock.RUnlock()
		return r
	}
	canonicalTypeLock.RUnlock()
	canonicalTypeLock.Lock()
	if r, ok := canonicalType[s]; ok {
		canonicalTypeLock.Unlock()
		return r
	}
	canonicalType[s] = t
	canonicalTypeLock.Unlock()
	return t
}

func toType(p *rtype) Type {
	if p == nil {
		return nil
	}
	return canonicalize(p)
}

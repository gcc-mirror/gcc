// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package reflect

// MakeRO returns a copy of v with the read-only flag set.
func MakeRO(v Value) Value {
	v.flag |= flagStickyRO
	return v
}

// IsRO reports whether v's read-only flag is set.
func IsRO(v Value) bool {
	return v.flag&flagStickyRO != 0
}

var CallGC = &callGC

// FuncLayout calls funcLayout and returns a subset of the results for testing.
//
// Bitmaps like stack, gc, inReg, and outReg are expanded such that each bit
// takes up one byte, so that writing out test cases is a little clearer.
// If ptrs is false, gc will be nil.
func FuncLayout(t Type, rcvr Type) (frametype Type, argSize, retOffset uintptr, stack, gc, inReg, outReg []byte, ptrs bool) {
	return
}

func TypeLinks() []string {
	return nil
}

var GCBits = gcbits

// Will be provided by runtime eventually.
func gcbits(interface{}) []byte {
	return nil
}

func MapBucketOf(x, y Type) Type {
	return nil
}

func CachedBucketOf(m Type) Type {
	return nil
}

type EmbedWithUnexpMeth struct{}

func (EmbedWithUnexpMeth) f() {}

type pinUnexpMeth interface {
	f()
}

var pinUnexpMethI = pinUnexpMeth(EmbedWithUnexpMeth{})

/*
func FirstMethodNameBytes(t Type) *byte {
	_ = pinUnexpMethI

	ut := t.uncommon()
	if ut == nil {
		panic("type has no methods")
	}
	m := ut.methods()[0]
	mname := t.(*rtype).nameOff(m.name)
	if *mname.data(0, "name flag field")&(1<<2) == 0 {
		panic("method name does not have pkgPath *string")
	}
	return mname.bytes
}
*/

type OtherPkgFields struct {
	OtherExported   int
	otherUnexported int
}

func IsExported(t Type) bool {
	return t.PkgPath() == ""
}

/*
func ResolveReflectName(s string) {
	resolveReflectName(newName(s, "", false))
}
*/

type Buffer struct {
	buf []byte
}

var MethodValueCallCodePtr = methodValueCallCodePtr

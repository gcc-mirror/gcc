// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Runtime type representation.

package runtime

import "unsafe"

type _type struct {
	kind       uint8
	align      int8
	fieldAlign uint8
	_          uint8
	size       uintptr
	hash       uint32

	hashfn  func(unsafe.Pointer, uintptr) uintptr
	equalfn func(unsafe.Pointer, unsafe.Pointer, uintptr) bool

	gc     unsafe.Pointer
	string *string
	*uncommonType
	ptrToThis *_type
}

type method struct {
	name    *string
	pkgPath *string
	mtyp    *_type
	typ     *_type
	tfn     unsafe.Pointer
}

type uncommonType struct {
	name    *string
	pkgPath *string
	methods []method
}

type imethod struct {
	name    *string
	pkgPath *string
	typ     *_type
}

type interfaceType struct {
	typ     _type
	methods []imethod
}

type mapType struct {
	typ  _type
	key  *_type
	elem *_type
}

type arrayType struct {
	typ   _type
	elem  *_type
	slice *_type
	len   uintptr
}

type chanType struct {
	typ  _type
	elem *_type
	dir  uintptr
}

type slicetype struct {
	typ  _type
	elem *_type
}

type functype struct {
	typ       _type
	dotdotdot bool
	in        []*_type
	out       []*_type
}

type ptrtype struct {
	typ  _type
	elem *_type
}

type structfield struct {
	name    *string // nil for embedded fields
	pkgPath *string // nil for exported Names; otherwise import path
	typ     *_type  // type of field
	tag     *string // nil if no tag
	offset  uintptr // byte offset of field within struct
}

type structtype struct {
	typ    _type
	fields []structfield
}

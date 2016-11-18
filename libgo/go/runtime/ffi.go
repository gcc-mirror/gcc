// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Only build this file if libffi is supported.

// +build libffi

package runtime

import "unsafe"

// This file contains the code that converts a Go type to an FFI type.
// This has to be written in Go because it allocates memory in the Go heap.

// C functions to return pointers to libffi variables.

func ffi_type_pointer() *__ffi_type
func ffi_type_sint8() *__ffi_type
func ffi_type_sint16() *__ffi_type
func ffi_type_sint32() *__ffi_type
func ffi_type_sint64() *__ffi_type
func ffi_type_uint8() *__ffi_type
func ffi_type_uint16() *__ffi_type
func ffi_type_uint32() *__ffi_type
func ffi_type_uint64() *__ffi_type
func ffi_type_float() *__ffi_type
func ffi_type_double() *__ffi_type
func ffi_supports_complex() bool
func ffi_type_complex_float() *__ffi_type
func ffi_type_complex_double() *__ffi_type
func ffi_type_void() *__ffi_type

// C functions defined in libffi.

//extern ffi_prep_cif
func ffi_prep_cif(*_ffi_cif, _ffi_abi, uint32, *__ffi_type, **__ffi_type) _ffi_status

// ffiFuncToCIF is called from C code.
//go:linkname ffiFuncToCIF runtime.ffiFuncToCIF

// ffiFuncToCIF builds an _ffi_cif struct for function described by ft.
func ffiFuncToCIF(ft *functype, isInterface bool, isMethod bool, cif *_ffi_cif) {
	nparams := len(ft.in)
	nargs := nparams
	if isInterface {
		nargs++
	}
	args := make([]*__ffi_type, nargs)
	i := 0
	off := 0
	if isInterface {
		args[0] = ffi_type_pointer()
		off = 1
	} else if isMethod {
		args[0] = ffi_type_pointer()
		i = 1
	}
	for ; i < nparams; i++ {
		args[i+off] = typeToFFI(ft.in[i])
	}

	rettype := funcReturnFFI(ft)

	var pargs **__ffi_type
	if len(args) > 0 {
		pargs = &args[0]
	}
	status := ffi_prep_cif(cif, _FFI_DEFAULT_ABI, uint32(nargs), rettype, pargs)
	if status != _FFI_OK {
		throw("ffi_prep_cif failed")
	}
}

// funcReturnFFI returns the FFI definition of the return type of ft.
func funcReturnFFI(ft *functype) *__ffi_type {
	c := len(ft.out)
	if c == 0 {
		return ffi_type_void()
	}

	// Compile a function that returns a zero-sized value as
	// though it returns void. This works around a problem in
	// libffi: it can't represent a zero-sized value.
	var size uintptr
	for _, v := range ft.out {
		size += v.size
	}
	if size == 0 {
		return ffi_type_void()
	}

	if c == 1 {
		return typeToFFI(ft.out[0])
	}

	elements := make([]*__ffi_type, c+1)
	for i, v := range ft.out {
		elements[i] = typeToFFI(v)
	}
	elements[c] = nil

	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &elements[0],
	}
}

// typeToFFI returns the __ffi_type for a Go type.
func typeToFFI(typ *_type) *__ffi_type {
	switch typ.kind & kindMask {
	case kindBool:
		switch unsafe.Sizeof(false) {
		case 1:
			return ffi_type_uint8()
		case 4:
			return ffi_type_uint32()
		default:
			throw("bad bool size")
			return nil
		}
	case kindInt:
		return intToFFI()
	case kindInt8:
		return ffi_type_sint8()
	case kindInt16:
		return ffi_type_sint16()
	case kindInt32:
		return ffi_type_sint32()
	case kindInt64:
		return ffi_type_sint64()
	case kindUint:
		switch unsafe.Sizeof(uint(0)) {
		case 4:
			return ffi_type_uint32()
		case 8:
			return ffi_type_uint64()
		default:
			throw("bad uint size")
			return nil
		}
	case kindUint8:
		return ffi_type_uint8()
	case kindUint16:
		return ffi_type_uint16()
	case kindUint32:
		return ffi_type_uint32()
	case kindUint64:
		return ffi_type_uint64()
	case kindUintptr:
		switch unsafe.Sizeof(uintptr(0)) {
		case 4:
			return ffi_type_uint32()
		case 8:
			return ffi_type_uint64()
		default:
			throw("bad uinptr size")
			return nil
		}
	case kindFloat32:
		return ffi_type_float()
	case kindFloat64:
		return ffi_type_double()
	case kindComplex64:
		if ffi_supports_complex() {
			return ffi_type_complex_float()
		} else {
			return complexToFFI(ffi_type_float())
		}
	case kindComplex128:
		if ffi_supports_complex() {
			return ffi_type_complex_double()
		} else {
			return complexToFFI(ffi_type_double())
		}
	case kindArray:
		return arrayToFFI((*arraytype)(unsafe.Pointer(typ)))
	case kindChan, kindFunc, kindMap, kindPtr, kindUnsafePointer:
		// These types are always simple pointers, and for FFI
		// purposes nothing else matters.
		return ffi_type_pointer()
	case kindInterface:
		return interfaceToFFI()
	case kindSlice:
		return sliceToFFI((*slicetype)(unsafe.Pointer(typ)))
	case kindString:
		return stringToFFI()
	case kindStruct:
		return structToFFI((*structtype)(unsafe.Pointer(typ)))
	default:
		throw("unknown type kind")
		return nil
	}
}

// interfaceToFFI returns an ffi_type for a Go interface type.
// This is used for both empty and non-empty interface types.
func interfaceToFFI() *__ffi_type {
	elements := make([]*__ffi_type, 3)
	elements[0] = ffi_type_pointer()
	elements[1] = elements[0]
	elements[2] = nil
	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &elements[0],
	}
}

// stringToFFI returns an ffi_type for a Go string type.
func stringToFFI() *__ffi_type {
	elements := make([]*__ffi_type, 3)
	elements[0] = ffi_type_pointer()
	elements[1] = intToFFI()
	elements[2] = nil
	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &elements[0],
	}
}

// structToFFI returns an ffi_type for a Go struct type.
func structToFFI(typ *structtype) *__ffi_type {
	c := len(typ.fields)
	if c == 0 {
		return emptyStructToFFI()
	}

	fields := make([]*__ffi_type, c+1)
	for i, v := range typ.fields {
		fields[i] = typeToFFI(v.typ)
	}
	fields[c] = nil
	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &fields[0],
	}
}

// sliceToFFI returns an ffi_type for a Go slice type.
func sliceToFFI(typ *slicetype) *__ffi_type {
	elements := make([]*__ffi_type, 4)
	elements[0] = ffi_type_pointer()
	elements[1] = intToFFI()
	elements[2] = elements[1]
	elements[3] = nil
	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &elements[0],
	}
}

// complexToFFI returns an ffi_type for a Go complex type.
// This is only used if libffi does not support complex types internally
// for this target.
func complexToFFI(ffiFloatType *__ffi_type) *__ffi_type {
	elements := make([]*__ffi_type, 3)
	elements[0] = ffiFloatType
	elements[1] = ffiFloatType
	elements[2] = nil
	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &elements[0],
	}
}

// arrayToFFI returns an ffi_type for a Go array type.
func arrayToFFI(typ *arraytype) *__ffi_type {
	if typ.len == 0 {
		return emptyStructToFFI()
	}
	elements := make([]*__ffi_type, typ.len+1)
	et := typeToFFI(typ.elem)
	for i := uintptr(0); i < typ.len; i++ {
		elements[i] = et
	}
	elements[typ.len] = nil
	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &elements[0],
	}
}

// intToFFI returns an ffi_type for the Go int type.
func intToFFI() *__ffi_type {
	switch unsafe.Sizeof(0) {
	case 4:
		return ffi_type_sint32()
	case 8:
		return ffi_type_sint64()
	default:
		throw("bad int size")
		return nil
	}
}

// emptyStructToFFI returns an ffi_type for an empty struct.
// The libffi library won't accept a struct with no fields.
func emptyStructToFFI() *__ffi_type {
	elements := make([]*__ffi_type, 2)
	elements[0] = ffi_type_void()
	elements[1] = nil
	return &__ffi_type{
		_type:    _FFI_TYPE_STRUCT,
		elements: &elements[0],
	}
}

//go:linkname makeCIF reflect.makeCIF

// makeCIF is used by the reflect package to allocate a CIF.
func makeCIF(ft *functype) *_ffi_cif {
	cif := new(_ffi_cif)
	ffiFuncToCIF(ft, false, false, cif)
	return cif
}

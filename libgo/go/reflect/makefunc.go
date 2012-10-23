// Copyright 2012 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// MakeFunc implementation.

package reflect

import (
	"unsafe"
)

// MakeFunc returns a new function of the given Type
// that wraps the function fn. When called, that new function
// does the following:
//
//	- converts its arguments to a list of Values args.
//	- runs results := fn(args).
//	- returns the results as a slice of Values, one per formal result.
//
// The implementation fn can assume that the argument Value slice
// has the number and type of arguments given by typ.
// If typ describes a variadic function, the final Value is itself
// a slice representing the variadic arguments, as in the
// body of a variadic function. The result Value slice returned by fn
// must have the number and type of results given by typ.
//
// The Value.Call method allows the caller to invoke a typed function
// in terms of Values; in contrast, MakeFunc allows the caller to implement
// a typed function in terms of Values.
//
// The Examples section of the documentation includes an illustration
// of how to use MakeFunc to build a swap function for different types.
//
func MakeFunc(typ Type, fn func(args []Value) (results []Value)) Value {
	if typ.Kind() != Func {
		panic("reflect: call of MakeFunc with non-Func type")
	}

	ft := (*funcType)(unsafe.Pointer(typ.common()))

	// We will build a function that uses the C stdarg routines to
	// pull out the arguments.  Since the stdarg routines require
	// the first parameter to be available, we need to switch on
	// the possible first parameter types.  Note that this assumes
	// that the calling ABI for a stdarg function is the same as
	// that for a non-stdarg function.  The C standard does not
	// require this, but it is true for most implementations in
	// practice.

	// Handling result types is a different problem.  There are a
	// few cases to handle:
	//   * No results.
	//   * One result.
	//   * More than one result, which is returned in a struct.
	//     + Struct returned in registers.
	//     + Struct returned in memory.

	var result Kind
	var resultSize uintptr
	switch len(ft.out) {
	case 0:
		result = Invalid
	case 1:
		result = Kind(ft.out[0].kind)
		resultSize = ft.out[0].size
	default:
		result = Struct
	}

	panic("reflect MakeFunc not implemented")

	// stub := func(i int) {
	// 	var args __gnuc_va_list
	// 	__builtin_va_start(args, i)
	// 	v := makeInt(0, uint64(i), ft.in[0])
	// 	return callReflect(ft, fn, v, args)
	// }

	// return Value{t, unsafe.Pointer(&impl.code[0]), flag(Func) << flagKindShift}
}

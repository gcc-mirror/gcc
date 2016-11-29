// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"unsafe"
)

// For gccgo, use go:linkname to rename compiler-called functions to
// themselves, so that the compiler will export them.
//
//go:linkname requireitab runtime.requireitab
//go:linkname assertitab runtime.assertitab
//go:linkname assertI2T runtime.assertI2T
//go:linkname ifacetypeeq runtime.ifacetypeeq
//go:linkname efacetype runtime.efacetype
//go:linkname ifacetype runtime.ifacetype
//go:linkname ifaceE2E2 runtime.ifaceE2E2
//go:linkname ifaceI2E2 runtime.ifaceI2E2
//go:linkname ifaceE2I2 runtime.ifaceE2I2
//go:linkname ifaceI2I2 runtime.ifaceI2I2
//go:linkname ifaceE2T2P runtime.ifaceE2T2P
//go:linkname ifaceI2T2P runtime.ifaceI2T2P
//go:linkname ifaceE2T2 runtime.ifaceE2T2
//go:linkname ifaceI2T2 runtime.ifaceI2T2
//go:linkname ifaceT2Ip runtime.ifaceT2Ip
// Temporary for C code to call:
//go:linkname getitab runtime.getitab

// The gccgo itab structure is different than the gc one.
//
// Both gccgo and gc represent empty interfaces the same way:
// a two field struct, where the first field points to a type descriptor
// (a *_type) and the second field is the data pointer.
//
// Non-empty interfaces are also two-field structs, and the second
// field is the data pointer. However, for gccgo, the first field, the
// itab field, is different. The itab field points to the interface
// method table, which is the implemention of a specific interface
// type for a specific dynamic non-interface type.  An interface
// method table is a list of pointer values. The first pointer is the
// type descriptor (a *_type) for the dynamic type. The subsequent
// pointers are pointers to function code, which implement the methods
// required by the interface. The pointers are sorted by name.
//
// The method pointers in the itab are C function pointers, not Go
// function pointers; they may be called directly, and they have no
// closures. The receiver is always passed as a pointer, and it is
// always the same pointer stored in the interface value. A value
// method starts by copying the receiver value out of the pointer into
// a local variable.
//
// A method call on an interface value is by definition calling a
// method at a known index m in the list of methods. Given a non-empty
// interface value i, the call i.m(args) looks like
//     i.itab[m+1](i.iface, args)

// Both an empty interface and a non-empty interface have a data
// pointer field. The meaning of this field is determined by the
// kindDirectIface bit in the `kind` field of the type descriptor of
// the value stored in the interface. If kindDirectIface is set, then
// the data pointer field in the interface value is exactly the value
// stored in the interface. Otherwise, the data pointer field is a
// pointer to memory that holds the value. It follows from this that
// kindDirectIface can only be set for a type whose representation is
// simply a pointer. In the current gccgo implementation, this is set
// only for pointer types (including unsafe.Pointer). In the future it
// could also be set for other types: channels, maps, functions,
// single-field structs and single-element arrays whose single field
// is simply a pointer.

// For a nil interface value both fields in the interface struct are nil.

// Return the interface method table for a value of type rhs converted
// to an interface of type lhs.
func getitab(lhs, rhs *_type, canfail bool) unsafe.Pointer {
	if rhs == nil {
		return nil
	}

	if lhs.kind&kindMask != kindInterface {
		throw("getitab called for non-interface type")
	}

	lhsi := (*interfacetype)(unsafe.Pointer(lhs))

	if len(lhsi.methods) == 0 {
		throw("getitab called for empty interface type")
	}

	if rhs.uncommontype == nil || len(rhs.methods) == 0 {
		if canfail {
			return nil
		}
		panic(&TypeAssertionError{"", *rhs.string, *lhs.string, *lhsi.methods[0].name})
	}

	methods := make([]unsafe.Pointer, len(lhsi.methods)+1)
	methods[0] = unsafe.Pointer(rhs)

	ri := 0
	for li := range lhsi.methods {
		lhsMethod := &lhsi.methods[li]
		var rhsMethod *method

		for {
			if ri >= len(rhs.methods) {
				if canfail {
					return nil
				}
				panic(&TypeAssertionError{"", *rhs.string, *lhs.string, *lhsMethod.name})
			}

			rhsMethod = &rhs.methods[ri]
			if (lhsMethod.name == rhsMethod.name || *lhsMethod.name == *rhsMethod.name) &&
				(lhsMethod.pkgPath == rhsMethod.pkgPath || *lhsMethod.pkgPath == *rhsMethod.pkgPath) {
				break
			}

			ri++
		}

		if !eqtype(lhsMethod.typ, rhsMethod.mtyp) {
			if canfail {
				return nil
			}
			panic(&TypeAssertionError{"", *rhs.string, *lhs.string, *lhsMethod.name})
		}

		methods[li+1] = unsafe.Pointer(rhsMethod.tfn)
		ri++
	}

	return unsafe.Pointer(&methods[0])
}

// Return the interface method table for a value of type rhs converted
// to an interface of type lhs.  Panics if the conversion is impossible.
func requireitab(lhs, rhs *_type) unsafe.Pointer {
	return getitab(lhs, rhs, false)
}

// Return the interface method table for a value of type rhs converted
// to an interface of type lhs.  Panics if the conversion is
// impossible or if the rhs type is nil.
func assertitab(lhs, rhs *_type) unsafe.Pointer {
	if rhs == nil {
		panic(&TypeAssertionError{"", "", *lhs.string, ""})
	}

	if lhs.kind&kindMask != kindInterface {
		throw("assertitab called for non-interface type")
	}

	lhsi := (*interfacetype)(unsafe.Pointer(lhs))

	if len(lhsi.methods) == 0 {
		return unsafe.Pointer(rhs)
	}

	return getitab(lhs, rhs, false)
}

// Check whether an interface type may be converted to a non-interface
// type, panicing if not.
func assertI2T(lhs, rhs, inter *_type) {
	if rhs == nil {
		panic(&TypeAssertionError{"", "", *lhs.string, ""})
	}
	if !eqtype(lhs, rhs) {
		panic(&TypeAssertionError{*inter.string, *rhs.string, *lhs.string, ""})
	}
}

// Compare two type descriptors for equality.
func ifacetypeeq(a, b *_type) bool {
	return eqtype(a, b)
}

// Return the type descriptor of an empty interface.
// FIXME: This should be inlined by the compiler.
func efacetype(e eface) *_type {
	return e._type
}

// Return the type descriptor of a non-empty interface.
// FIXME: This should be inlined by the compiler.
func ifacetype(i iface) *_type {
	if i.tab == nil {
		return nil
	}
	return *(**_type)(i.tab)
}

// Convert an empty interface to an empty interface, for a comma-ok
// type assertion.
func ifaceE2E2(e eface) (eface, bool) {
	return e, e._type != nil
}

// Convert a non-empty interface to an empty interface, for a comma-ok
// type assertion.
func ifaceI2E2(i iface) (eface, bool) {
	if i.tab == nil {
		return eface{nil, nil}, false
	} else {
		return eface{*(**_type)(i.tab), i.data}, true
	}
}

// Convert an empty interface to a non-empty interface, for a comma-ok
// type assertion.
func ifaceE2I2(inter *_type, e eface) (iface, bool) {
	if e._type == nil {
		return iface{nil, nil}, false
	} else {
		itab := getitab(inter, e._type, true)
		if itab == nil {
			return iface{nil, nil}, false
		} else {
			return iface{itab, e.data}, true
		}
	}
}

// Convert a non-empty interface to a non-empty interface, for a
// comma-ok type assertion.
func ifaceI2I2(inter *_type, i iface) (iface, bool) {
	if i.tab == nil {
		return iface{nil, nil}, false
	} else {
		itab := getitab(inter, *(**_type)(i.tab), true)
		if itab == nil {
			return iface{nil, nil}, false
		} else {
			return iface{itab, i.data}, true
		}
	}
}

// Convert an empty interface to a pointer non-interface type.
func ifaceE2T2P(t *_type, e eface) (unsafe.Pointer, bool) {
	if !eqtype(t, e._type) {
		return nil, false
	} else {
		return e.data, true
	}
}

// Convert a non-empty interface to a pointer non-interface type.
func ifaceI2T2P(t *_type, i iface) (unsafe.Pointer, bool) {
	if i.tab == nil || !eqtype(t, *(**_type)(i.tab)) {
		return nil, false
	} else {
		return i.data, true
	}
}

// Convert an empty interface to a non-pointer non-interface type.
func ifaceE2T2(t *_type, e eface, ret unsafe.Pointer) bool {
	if !eqtype(t, e._type) {
		memclr(ret, t.size)
		return false
	} else {
		typedmemmove(t, ret, e.data)
		return true
	}
}

// Convert a non-empty interface to a non-pointer non-interface type.
func ifaceI2T2(t *_type, i iface, ret unsafe.Pointer) bool {
	if i.tab == nil || !eqtype(t, *(**_type)(i.tab)) {
		memclr(ret, t.size)
		return false
	} else {
		typedmemmove(t, ret, i.data)
		return true
	}
}

// Return whether we can convert a type to an interface type.
func ifaceT2Ip(to, from *_type) bool {
	if from == nil {
		return false
	}

	if to.kind&kindMask != kindInterface {
		throw("ifaceT2Ip called with non-interface type")
	}
	toi := (*interfacetype)(unsafe.Pointer(to))

	if from.uncommontype == nil || len(from.methods) == 0 {
		return len(toi.methods) == 0
	}

	ri := 0
	for li := range toi.methods {
		toMethod := &toi.methods[li]
		var fromMethod *method
		for {
			if ri >= len(from.methods) {
				return false
			}

			fromMethod = &from.methods[ri]
			if (toMethod.name == fromMethod.name || *toMethod.name == *fromMethod.name) &&
				(toMethod.pkgPath == fromMethod.pkgPath || *toMethod.pkgPath == *fromMethod.pkgPath) {
				break
			}

			ri++
		}

		if !eqtype(fromMethod.mtyp, toMethod.typ) {
			return false
		}

		ri++
	}

	return true
}

//go:linkname reflect_ifaceE2I reflect.ifaceE2I
func reflect_ifaceE2I(inter *interfacetype, e eface, dst *iface) {
	t := e._type
	if t == nil {
		panic(TypeAssertionError{"", "", *inter.typ.string, ""})
	}
	dst.tab = requireitab((*_type)(unsafe.Pointer(inter)), t)
	dst.data = e.data
}

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package reflect

import (
	"errors"
	"internal/goarch"
	"internal/itoa"
	"internal/unsafeheader"
	"math"
	"runtime"
	"unsafe"
)

// Value is the reflection interface to a Go value.
//
// Not all methods apply to all kinds of values. Restrictions,
// if any, are noted in the documentation for each method.
// Use the Kind method to find out the kind of value before
// calling kind-specific methods. Calling a method
// inappropriate to the kind of type causes a run time panic.
//
// The zero Value represents no value.
// Its IsValid method returns false, its Kind method returns Invalid,
// its String method returns "<invalid Value>", and all other methods panic.
// Most functions and methods never return an invalid value.
// If one does, its documentation states the conditions explicitly.
//
// A Value can be used concurrently by multiple goroutines provided that
// the underlying Go value can be used concurrently for the equivalent
// direct operations.
//
// To compare two Values, compare the results of the Interface method.
// Using == on two Values does not compare the underlying values
// they represent.
type Value struct {
	// typ holds the type of the value represented by a Value.
	typ *rtype

	// Pointer-valued data or, if flagIndir is set, pointer to data.
	// Valid when either flagIndir is set or typ.pointers() is true.
	ptr unsafe.Pointer

	// flag holds metadata about the value.
	// The lowest bits are flag bits:
	//	- flagStickyRO: obtained via unexported not embedded field, so read-only
	//	- flagEmbedRO: obtained via unexported embedded field, so read-only
	//	- flagIndir: val holds a pointer to the data
	//	- flagAddr: v.CanAddr is true (implies flagIndir)
	//	- flagMethod: v is a method value.
	// The next five bits give the Kind of the value.
	// This repeats typ.Kind() except for method values.
	// The remaining 23+ bits give a method number for method values.
	// If flag.kind() != Func, code can assume that flagMethod is unset.
	// If ifaceIndir(typ), code can assume that flagIndir is set.
	flag

	// A method value represents a curried method invocation
	// like r.Read for some receiver r. The typ+val+flag bits describe
	// the receiver r, but the flag's Kind bits say Func (methods are
	// functions), and the top bits of the flag give the method number
	// in r's type's method table.
}

type flag uintptr

const (
	flagKindWidth        = 5 // there are 27 kinds
	flagKindMask    flag = 1<<flagKindWidth - 1
	flagStickyRO    flag = 1 << 5
	flagEmbedRO     flag = 1 << 6
	flagIndir       flag = 1 << 7
	flagAddr        flag = 1 << 8
	flagMethod      flag = 1 << 9
	flagMethodFn    flag = 1 << 10 // gccgo: first fn parameter is always pointer
	flagMethodShift      = 11
	flagRO          flag = flagStickyRO | flagEmbedRO
)

func (f flag) kind() Kind {
	return Kind(f & flagKindMask)
}

func (f flag) ro() flag {
	if f&flagRO != 0 {
		return flagStickyRO
	}
	return 0
}

// pointer returns the underlying pointer represented by v.
// v.Kind() must be Pointer, Map, Chan, Func, or UnsafePointer
// if v.Kind() == Pointer, the base type must not be go:notinheap.
func (v Value) pointer() unsafe.Pointer {
	if v.typ.size != goarch.PtrSize || !v.typ.pointers() {
		panic("can't call pointer on a non-pointer Value")
	}
	if v.flag&flagIndir != 0 {
		return *(*unsafe.Pointer)(v.ptr)
	}
	return v.ptr
}

// packEface converts v to the empty interface.
func packEface(v Value) any {
	t := v.typ
	var i any
	e := (*emptyInterface)(unsafe.Pointer(&i))
	// First, fill in the data portion of the interface.
	switch {
	case ifaceIndir(t):
		if v.flag&flagIndir == 0 {
			panic("bad indir")
		}
		// Value is indirect, and so is the interface we're making.
		ptr := v.ptr
		if v.flag&flagAddr != 0 {
			// TODO: pass safe boolean from valueInterface so
			// we don't need to copy if safe==true?
			c := unsafe_New(t)
			typedmemmove(t, c, ptr)
			ptr = c
		}
		e.word = ptr
	case v.flag&flagIndir != 0:
		// Value is indirect, but interface is direct. We need
		// to load the data at v.ptr into the interface data word.
		e.word = *(*unsafe.Pointer)(v.ptr)
	default:
		// Value is direct, and so is the interface.
		e.word = v.ptr
	}
	// Now, fill in the type portion. We're very careful here not
	// to have any operation between the e.word and e.typ assignments
	// that would let the garbage collector observe the partially-built
	// interface value.
	e.typ = t
	return i
}

// unpackEface converts the empty interface i to a Value.
func unpackEface(i any) Value {
	e := (*emptyInterface)(unsafe.Pointer(&i))
	// NOTE: don't read e.word until we know whether it is really a pointer or not.
	t := e.typ
	if t == nil {
		return Value{}
	}
	f := flag(t.Kind())
	if ifaceIndir(t) {
		f |= flagIndir
	}
	return Value{t, e.word, f}
}

// A ValueError occurs when a Value method is invoked on
// a Value that does not support it. Such cases are documented
// in the description of each method.
type ValueError struct {
	Method string
	Kind   Kind
}

func (e *ValueError) Error() string {
	if e.Kind == 0 {
		return "reflect: call of " + e.Method + " on zero Value"
	}
	return "reflect: call of " + e.Method + " on " + e.Kind.String() + " Value"
}

// methodName returns the name of the calling method,
// assumed to be two stack frames above.
func methodName() string {
	pc, _, _, _ := runtime.Caller(2)
	f := runtime.FuncForPC(pc)
	if f == nil {
		return "unknown method"
	}
	return f.Name()
}

// methodNameSkip is like methodName, but skips another stack frame.
// This is a separate function so that reflect.flag.mustBe will be inlined.
func methodNameSkip() string {
	pc, _, _, _ := runtime.Caller(3)
	f := runtime.FuncForPC(pc)
	if f == nil {
		return "unknown method"
	}
	return f.Name()
}

// emptyInterface is the header for an interface{} value.
type emptyInterface struct {
	typ  *rtype
	word unsafe.Pointer
}

// nonEmptyInterface is the header for an interface value with methods.
type nonEmptyInterface struct {
	// see ../runtime/iface.go:/Itab
	itab *struct {
		typ *rtype                 // dynamic concrete type
		fun [100000]unsafe.Pointer // method table
	}
	word unsafe.Pointer
}

// mustBe panics if f's kind is not expected.
// Making this a method on flag instead of on Value
// (and embedding flag in Value) means that we can write
// the very clear v.mustBe(Bool) and have it compile into
// v.flag.mustBe(Bool), which will only bother to copy the
// single important word for the receiver.
func (f flag) mustBe(expected Kind) {
	// TODO(mvdan): use f.kind() again once mid-stack inlining gets better
	if Kind(f&flagKindMask) != expected {
		panic(&ValueError{methodName(), f.kind()})
	}
}

// mustBeExported panics if f records that the value was obtained using
// an unexported field.
func (f flag) mustBeExported() {
	if f == 0 || f&flagRO != 0 {
		f.mustBeExportedSlow()
	}
}

func (f flag) mustBeExportedSlow() {
	if f == 0 {
		panic(&ValueError{methodNameSkip(), Invalid})
	}
	if f&flagRO != 0 {
		panic("reflect: " + methodNameSkip() + " using value obtained using unexported field")
	}
}

// mustBeAssignable panics if f records that the value is not assignable,
// which is to say that either it was obtained using an unexported field
// or it is not addressable.
func (f flag) mustBeAssignable() {
	if f&flagRO != 0 || f&flagAddr == 0 {
		f.mustBeAssignableSlow()
	}
}

func (f flag) mustBeAssignableSlow() {
	if f == 0 {
		panic(&ValueError{methodNameSkip(), Invalid})
	}
	// Assignable if addressable and not read-only.
	if f&flagRO != 0 {
		panic("reflect: " + methodNameSkip() + " using value obtained using unexported field")
	}
	if f&flagAddr == 0 {
		panic("reflect: " + methodNameSkip() + " using unaddressable value")
	}
}

// Addr returns a pointer value representing the address of v.
// It panics if CanAddr() returns false.
// Addr is typically used to obtain a pointer to a struct field
// or slice element in order to call a method that requires a
// pointer receiver.
func (v Value) Addr() Value {
	if v.flag&flagAddr == 0 {
		panic("reflect.Value.Addr of unaddressable value")
	}
	// Preserve flagRO instead of using v.flag.ro() so that
	// v.Addr().Elem() is equivalent to v (#32772)
	fl := v.flag & flagRO
	return Value{v.typ.ptrTo(), v.ptr, fl | flag(Pointer)}
}

// Bool returns v's underlying value.
// It panics if v's kind is not Bool.
func (v Value) Bool() bool {
	v.mustBe(Bool)
	return *(*bool)(v.ptr)
}

// Bytes returns v's underlying value.
// It panics if v's underlying value is not a slice of bytes.
func (v Value) Bytes() []byte {
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Uint8 {
		panic("reflect.Value.Bytes of non-byte slice")
	}
	// Slice is always bigger than a word; assume flagIndir.
	return *(*[]byte)(v.ptr)
}

// runes returns v's underlying value.
// It panics if v's underlying value is not a slice of runes (int32s).
func (v Value) runes() []rune {
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Int32 {
		panic("reflect.Value.Bytes of non-rune slice")
	}
	// Slice is always bigger than a word; assume flagIndir.
	return *(*[]rune)(v.ptr)
}

// CanAddr reports whether the value's address can be obtained with Addr.
// Such values are called addressable. A value is addressable if it is
// an element of a slice, an element of an addressable array,
// a field of an addressable struct, or the result of dereferencing a pointer.
// If CanAddr returns false, calling Addr will panic.
func (v Value) CanAddr() bool {
	return v.flag&flagAddr != 0
}

// CanSet reports whether the value of v can be changed.
// A Value can be changed only if it is addressable and was not
// obtained by the use of unexported struct fields.
// If CanSet returns false, calling Set or any type-specific
// setter (e.g., SetBool, SetInt) will panic.
func (v Value) CanSet() bool {
	return v.flag&(flagAddr|flagRO) == flagAddr
}

// Call calls the function v with the input arguments in.
// For example, if len(in) == 3, v.Call(in) represents the Go call v(in[0], in[1], in[2]).
// Call panics if v's Kind is not Func.
// It returns the output results as Values.
// As in Go, each input argument must be assignable to the
// type of the function's corresponding input parameter.
// If v is a variadic function, Call creates the variadic slice parameter
// itself, copying in the corresponding values.
func (v Value) Call(in []Value) []Value {
	v.mustBe(Func)
	v.mustBeExported()
	return v.call("Call", in)
}

// CallSlice calls the variadic function v with the input arguments in,
// assigning the slice in[len(in)-1] to v's final variadic argument.
// For example, if len(in) == 3, v.CallSlice(in) represents the Go call v(in[0], in[1], in[2]...).
// CallSlice panics if v's Kind is not Func or if v is not variadic.
// It returns the output results as Values.
// As in Go, each input argument must be assignable to the
// type of the function's corresponding input parameter.
func (v Value) CallSlice(in []Value) []Value {
	v.mustBe(Func)
	v.mustBeExported()
	return v.call("CallSlice", in)
}

var callGC bool // for testing; see TestCallMethodJump and TestCallArgLive

const debugReflectCall = false

func (v Value) call(op string, in []Value) []Value {
	// Get function pointer, type.
	t := (*funcType)(unsafe.Pointer(v.typ))
	var (
		fn   unsafe.Pointer
		rcvr Value
	)
	if v.flag&flagMethod != 0 {
		rcvr = v
		_, t, fn = methodReceiver(op, v, int(v.flag)>>flagMethodShift)
	} else if v.flag&flagIndir != 0 {
		fn = *(*unsafe.Pointer)(v.ptr)
	} else {
		fn = v.ptr
	}

	if fn == nil {
		panic("reflect.Value.Call: call of nil function")
	}

	isSlice := op == "CallSlice"
	n := t.NumIn()
	isVariadic := t.IsVariadic()
	if isSlice {
		if !isVariadic {
			panic("reflect: CallSlice of non-variadic function")
		}
		if len(in) < n {
			panic("reflect: CallSlice with too few input arguments")
		}
		if len(in) > n {
			panic("reflect: CallSlice with too many input arguments")
		}
	} else {
		if isVariadic {
			n--
		}
		if len(in) < n {
			panic("reflect: Call with too few input arguments")
		}
		if !isVariadic && len(in) > n {
			panic("reflect: Call with too many input arguments")
		}
	}
	for _, x := range in {
		if x.Kind() == Invalid {
			panic("reflect: " + op + " using zero Value argument")
		}
	}
	for i := 0; i < n; i++ {
		if xt, targ := in[i].Type(), t.In(i); !xt.AssignableTo(targ) {
			panic("reflect: " + op + " using " + xt.String() + " as type " + targ.String())
		}
	}
	if !isSlice && isVariadic {
		// prepare slice for remaining values
		m := len(in) - n
		slice := MakeSlice(t.In(n), m, m)
		elem := t.In(n).Elem()
		for i := 0; i < m; i++ {
			x := in[n+i]
			if xt := x.Type(); !xt.AssignableTo(elem) {
				panic("reflect: cannot use " + xt.String() + " as type " + elem.String() + " in " + op)
			}
			slice.Index(i).Set(x)
		}
		origIn := in
		in = make([]Value, n+1)
		copy(in[:n], origIn)
		in[n] = slice
	}

	nin := len(in)
	if nin != t.NumIn() {
		panic("reflect.Value.Call: wrong argument count")
	}
	nout := t.NumOut()

	if v.flag&flagMethod != 0 {
		nin++
	}
	firstPointer := len(in) > 0 && ifaceIndir(t.In(0).common()) && v.flag&flagMethodFn != 0
	params := make([]unsafe.Pointer, nin)
	off := 0
	if v.flag&flagMethod != 0 {
		// Hard-wired first argument.
		p := new(unsafe.Pointer)
		if rcvr.typ.Kind() == Interface {
			*p = unsafe.Pointer((*nonEmptyInterface)(v.ptr).word)
		} else if rcvr.typ.Kind() == Ptr || rcvr.typ.Kind() == UnsafePointer {
			*p = rcvr.pointer()
		} else {
			*p = rcvr.ptr
		}
		params[0] = unsafe.Pointer(p)
		off = 1
	}
	for i, pv := range in {
		pv.mustBeExported()
		targ := t.In(i).(*rtype)
		pv = pv.assignTo("reflect.Value.Call", targ, nil)
		if pv.flag&flagIndir == 0 {
			p := new(unsafe.Pointer)
			*p = pv.ptr
			params[off] = unsafe.Pointer(p)
		} else {
			params[off] = pv.ptr
		}
		if i == 0 && firstPointer {
			p := new(unsafe.Pointer)
			*p = params[off]
			params[off] = unsafe.Pointer(p)
		}
		off++
	}

	ret := make([]Value, nout)
	results := make([]unsafe.Pointer, nout)
	for i := 0; i < nout; i++ {
		tv := t.Out(i)
		v := New(tv)
		results[i] = v.pointer()
		fl := flagIndir | flag(tv.Kind())
		ret[i] = Value{tv.common(), v.pointer(), fl}
	}

	var pp *unsafe.Pointer
	if len(params) > 0 {
		pp = &params[0]
	}
	var pr *unsafe.Pointer
	if len(results) > 0 {
		pr = &results[0]
	}

	call(t, fn, v.flag&flagMethod != 0, firstPointer, pp, pr)

	// For testing; see TestCallMethodJump.
	if callGC {
		runtime.GC()
	}

	return ret
}

// methodReceiver returns information about the receiver
// described by v. The Value v may or may not have the
// flagMethod bit set, so the kind cached in v.flag should
// not be used.
// The return value rcvrtype gives the method's actual receiver type.
// The return value t gives the method type signature (without the receiver).
// The return value fn is a pointer to the method code.
func methodReceiver(op string, v Value, methodIndex int) (rcvrtype *rtype, t *funcType, fn unsafe.Pointer) {
	i := methodIndex
	if v.typ.Kind() == Interface {
		tt := (*interfaceType)(unsafe.Pointer(v.typ))
		if uint(i) >= uint(len(tt.methods)) {
			panic("reflect: internal error: invalid method index")
		}
		m := &tt.methods[i]
		if m.pkgPath != nil {
			panic("reflect: " + op + " of unexported method")
		}
		iface := (*nonEmptyInterface)(v.ptr)
		if iface.itab == nil {
			panic("reflect: " + op + " of method on nil interface value")
		}
		rcvrtype = iface.itab.typ
		fn = unsafe.Pointer(&iface.itab.fun[i])
		t = (*funcType)(unsafe.Pointer(m.typ))
	} else {
		rcvrtype = v.typ
		ms := v.typ.exportedMethods()
		if uint(i) >= uint(len(ms)) {
			panic("reflect: internal error: invalid method index")
		}
		m := ms[i]
		if m.pkgPath != nil {
			panic("reflect: " + op + " of unexported method")
		}
		fn = unsafe.Pointer(&m.tfn)
		t = (*funcType)(unsafe.Pointer(m.mtyp))
	}
	return
}

// v is a method receiver. Store at p the word which is used to
// encode that receiver at the start of the argument list.
// Reflect uses the "interface" calling convention for
// methods, which always uses one word to record the receiver.
func storeRcvr(v Value, p unsafe.Pointer) {
	t := v.typ
	if t.Kind() == Interface {
		// the interface data word becomes the receiver word
		iface := (*nonEmptyInterface)(v.ptr)
		*(*unsafe.Pointer)(p) = iface.word
	} else if v.flag&flagIndir != 0 && !ifaceIndir(t) {
		*(*unsafe.Pointer)(p) = *(*unsafe.Pointer)(v.ptr)
	} else {
		*(*unsafe.Pointer)(p) = v.ptr
	}
}

// align returns the result of rounding x up to a multiple of n.
// n must be a power of two.
func align(x, n uintptr) uintptr {
	return (x + n - 1) &^ (n - 1)
}

// funcName returns the name of f, for use in error messages.
func funcName(f func([]Value) []Value) string {
	pc := *(*uintptr)(unsafe.Pointer(&f))
	rf := runtime.FuncForPC(pc)
	if rf != nil {
		return rf.Name()
	}
	return "closure"
}

// Cap returns v's capacity.
// It panics if v's Kind is not Array, Chan, or Slice.
func (v Value) Cap() int {
	k := v.kind()
	switch k {
	case Array:
		return v.typ.Len()
	case Chan:
		return chancap(v.pointer())
	case Slice:
		// Slice is always bigger than a word; assume flagIndir.
		return (*unsafeheader.Slice)(v.ptr).Cap
	}
	panic(&ValueError{"reflect.Value.Cap", v.kind()})
}

// Close closes the channel v.
// It panics if v's Kind is not Chan.
func (v Value) Close() {
	v.mustBe(Chan)
	v.mustBeExported()
	chanclose(v.pointer())
}

// CanComplex reports whether Complex can be used without panicking.
func (v Value) CanComplex() bool {
	switch v.kind() {
	case Complex64, Complex128:
		return true
	default:
		return false
	}
}

// Complex returns v's underlying value, as a complex128.
// It panics if v's Kind is not Complex64 or Complex128
func (v Value) Complex() complex128 {
	k := v.kind()
	switch k {
	case Complex64:
		return complex128(*(*complex64)(v.ptr))
	case Complex128:
		return *(*complex128)(v.ptr)
	}
	panic(&ValueError{"reflect.Value.Complex", v.kind()})
}

// Elem returns the value that the interface v contains
// or that the pointer v points to.
// It panics if v's Kind is not Interface or Pointer.
// It returns the zero Value if v is nil.
func (v Value) Elem() Value {
	k := v.kind()
	switch k {
	case Interface:
		var eface any
		if v.typ.NumMethod() == 0 {
			eface = *(*any)(v.ptr)
		} else {
			eface = (any)(*(*interface {
				M()
			})(v.ptr))
		}
		x := unpackEface(eface)
		if x.flag != 0 {
			x.flag |= v.flag.ro()
		}
		return x
	case Pointer:
		ptr := v.ptr
		if v.flag&flagIndir != 0 {
			if ifaceIndir(v.typ) {
				// This is a pointer to a not-in-heap object. ptr points to a uintptr
				// in the heap. That uintptr is the address of a not-in-heap object.
				// In general, pointers to not-in-heap objects can be total junk.
				// But Elem() is asking to dereference it, so the user has asserted
				// that at least it is a valid pointer (not just an integer stored in
				// a pointer slot). So let's check, to make sure that it isn't a pointer
				// that the runtime will crash on if it sees it during GC or write barriers.
				// Since it is a not-in-heap pointer, all pointers to the heap are
				// forbidden! That makes the test pretty easy.
				// See issue 48399.
				if !verifyNotInHeapPtr(*(*uintptr)(ptr)) {
					panic("reflect: reflect.Value.Elem on an invalid notinheap pointer")
				}
			}
			ptr = *(*unsafe.Pointer)(ptr)
		}
		// The returned value's address is v's value.
		if ptr == nil {
			return Value{}
		}
		tt := (*ptrType)(unsafe.Pointer(v.typ))
		typ := tt.elem
		fl := v.flag&flagRO | flagIndir | flagAddr
		fl |= flag(typ.Kind())
		return Value{typ, ptr, fl}
	}
	panic(&ValueError{"reflect.Value.Elem", v.kind()})
}

// Field returns the i'th field of the struct v.
// It panics if v's Kind is not Struct or i is out of range.
func (v Value) Field(i int) Value {
	if v.kind() != Struct {
		panic(&ValueError{"reflect.Value.Field", v.kind()})
	}
	tt := (*structType)(unsafe.Pointer(v.typ))
	if uint(i) >= uint(len(tt.fields)) {
		panic("reflect: Field index out of range")
	}
	field := &tt.fields[i]
	typ := field.typ

	// Inherit permission bits from v, but clear flagEmbedRO.
	fl := v.flag&(flagStickyRO|flagIndir|flagAddr) | flag(typ.Kind())
	// Using an unexported field forces flagRO.
	if field.pkgPath != nil {
		if field.embedded() {
			fl |= flagEmbedRO
		} else {
			fl |= flagStickyRO
		}
	}
	// Either flagIndir is set and v.ptr points at struct,
	// or flagIndir is not set and v.ptr is the actual struct data.
	// In the former case, we want v.ptr + offset.
	// In the latter case, we must have field.offset = 0,
	// so v.ptr + field.offset is still the correct address.
	ptr := add(v.ptr, field.offset(), "same as non-reflect &v.field")
	return Value{typ, ptr, fl}
}

// FieldByIndex returns the nested field corresponding to index.
// It panics if evaluation requires stepping through a nil
// pointer or a field that is not a struct.
func (v Value) FieldByIndex(index []int) Value {
	if len(index) == 1 {
		return v.Field(index[0])
	}
	v.mustBe(Struct)
	for i, x := range index {
		if i > 0 {
			if v.Kind() == Pointer && v.typ.Elem().Kind() == Struct {
				if v.IsNil() {
					panic("reflect: indirection through nil pointer to embedded struct")
				}
				v = v.Elem()
			}
		}
		v = v.Field(x)
	}
	return v
}

// FieldByIndexErr returns the nested field corresponding to index.
// It returns an error if evaluation requires stepping through a nil
// pointer, but panics if it must step through a field that
// is not a struct.
func (v Value) FieldByIndexErr(index []int) (Value, error) {
	if len(index) == 1 {
		return v.Field(index[0]), nil
	}
	v.mustBe(Struct)
	for i, x := range index {
		if i > 0 {
			if v.Kind() == Ptr && v.typ.Elem().Kind() == Struct {
				if v.IsNil() {
					return Value{}, errors.New("reflect: indirection through nil pointer to embedded struct field " + v.typ.Elem().Name())
				}
				v = v.Elem()
			}
		}
		v = v.Field(x)
	}
	return v, nil
}

// FieldByName returns the struct field with the given name.
// It returns the zero Value if no field was found.
// It panics if v's Kind is not struct.
func (v Value) FieldByName(name string) Value {
	v.mustBe(Struct)
	if f, ok := v.typ.FieldByName(name); ok {
		return v.FieldByIndex(f.Index)
	}
	return Value{}
}

// FieldByNameFunc returns the struct field with a name
// that satisfies the match function.
// It panics if v's Kind is not struct.
// It returns the zero Value if no field was found.
func (v Value) FieldByNameFunc(match func(string) bool) Value {
	if f, ok := v.typ.FieldByNameFunc(match); ok {
		return v.FieldByIndex(f.Index)
	}
	return Value{}
}

// CanFloat reports whether Float can be used without panicking.
func (v Value) CanFloat() bool {
	switch v.kind() {
	case Float32, Float64:
		return true
	default:
		return false
	}
}

// Float returns v's underlying value, as a float64.
// It panics if v's Kind is not Float32 or Float64
func (v Value) Float() float64 {
	k := v.kind()
	switch k {
	case Float32:
		return float64(*(*float32)(v.ptr))
	case Float64:
		return *(*float64)(v.ptr)
	}
	panic(&ValueError{"reflect.Value.Float", v.kind()})
}

var uint8Type = TypeOf(uint8(0)).(*rtype)

// Index returns v's i'th element.
// It panics if v's Kind is not Array, Slice, or String or i is out of range.
func (v Value) Index(i int) Value {
	switch v.kind() {
	case Array:
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		if uint(i) >= uint(tt.len) {
			panic("reflect: array index out of range")
		}
		typ := tt.elem
		offset := uintptr(i) * typ.size

		// Either flagIndir is set and v.ptr points at array,
		// or flagIndir is not set and v.ptr is the actual array data.
		// In the former case, we want v.ptr + offset.
		// In the latter case, we must be doing Index(0), so offset = 0,
		// so v.ptr + offset is still the correct address.
		val := add(v.ptr, offset, "same as &v[i], i < tt.len")
		fl := v.flag&(flagIndir|flagAddr) | v.flag.ro() | flag(typ.Kind()) // bits same as overall array
		return Value{typ, val, fl}

	case Slice:
		// Element flag same as Elem of Pointer.
		// Addressable, indirect, possibly read-only.
		s := (*unsafeheader.Slice)(v.ptr)
		if uint(i) >= uint(s.Len) {
			panic("reflect: slice index out of range")
		}
		tt := (*sliceType)(unsafe.Pointer(v.typ))
		typ := tt.elem
		val := arrayAt(s.Data, i, typ.size, "i < s.Len")
		fl := flagAddr | flagIndir | v.flag.ro() | flag(typ.Kind())
		return Value{typ, val, fl}

	case String:
		s := (*unsafeheader.String)(v.ptr)
		if uint(i) >= uint(s.Len) {
			panic("reflect: string index out of range")
		}
		p := arrayAt(s.Data, i, 1, "i < s.Len")
		fl := v.flag.ro() | flag(Uint8) | flagIndir
		return Value{uint8Type, p, fl}
	}
	panic(&ValueError{"reflect.Value.Index", v.kind()})
}

// CanInt reports whether Int can be used without panicking.
func (v Value) CanInt() bool {
	switch v.kind() {
	case Int, Int8, Int16, Int32, Int64:
		return true
	default:
		return false
	}
}

// Int returns v's underlying value, as an int64.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64.
func (v Value) Int() int64 {
	k := v.kind()
	p := v.ptr
	switch k {
	case Int:
		return int64(*(*int)(p))
	case Int8:
		return int64(*(*int8)(p))
	case Int16:
		return int64(*(*int16)(p))
	case Int32:
		return int64(*(*int32)(p))
	case Int64:
		return *(*int64)(p)
	}
	panic(&ValueError{"reflect.Value.Int", v.kind()})
}

// CanInterface reports whether Interface can be used without panicking.
func (v Value) CanInterface() bool {
	if v.flag == 0 {
		panic(&ValueError{"reflect.Value.CanInterface", Invalid})
	}
	return v.flag&flagRO == 0
}

// Interface returns v's current value as an interface{}.
// It is equivalent to:
//	var i interface{} = (v's underlying value)
// It panics if the Value was obtained by accessing
// unexported struct fields.
func (v Value) Interface() (i any) {
	return valueInterface(v, true)
}

func valueInterface(v Value, safe bool) any {
	if v.flag == 0 {
		panic(&ValueError{"reflect.Value.Interface", Invalid})
	}
	if safe && v.flag&flagRO != 0 {
		// Do not allow access to unexported values via Interface,
		// because they might be pointers that should not be
		// writable or methods or function that should not be callable.
		panic("reflect.Value.Interface: cannot return value obtained from unexported field or method")
	}
	if v.flag&flagMethod != 0 {
		v = makeMethodValue("Interface", v)
	}

	if v.flag&flagMethodFn != 0 {
		if v.typ.Kind() != Func {
			panic("reflect: MethodFn of non-Func")
		}
		ft := (*funcType)(unsafe.Pointer(v.typ))
		if ft.in[0].Kind() != Ptr {
			v = makeValueMethod(v)
		}
	}

	if v.kind() == Interface {
		// Special case: return the element inside the interface.
		// Empty interface has one layout, all interfaces with
		// methods have a second layout.
		if v.NumMethod() == 0 {
			return *(*any)(v.ptr)
		}
		return *(*interface {
			M()
		})(v.ptr)
	}

	// TODO: pass safe to packEface so we don't need to copy if safe==true?
	return packEface(v)
}

// InterfaceData returns a pair of unspecified uintptr values.
// It panics if v's Kind is not Interface.
//
// In earlier versions of Go, this function returned the interface's
// value as a uintptr pair. As of Go 1.4, the implementation of
// interface values precludes any defined use of InterfaceData.
//
// Deprecated: The memory representation of interface values is not
// compatible with InterfaceData.
func (v Value) InterfaceData() [2]uintptr {
	v.mustBe(Interface)
	// We treat this as a read operation, so we allow
	// it even for unexported data, because the caller
	// has to import "unsafe" to turn it into something
	// that can be abused.
	// Interface value is always bigger than a word; assume flagIndir.
	return *(*[2]uintptr)(v.ptr)
}

// IsNil reports whether its argument v is nil. The argument must be
// a chan, func, interface, map, pointer, or slice value; if it is
// not, IsNil panics. Note that IsNil is not always equivalent to a
// regular comparison with nil in Go. For example, if v was created
// by calling ValueOf with an uninitialized interface variable i,
// i==nil will be true but v.IsNil will panic as v will be the zero
// Value.
func (v Value) IsNil() bool {
	k := v.kind()
	switch k {
	case Chan, Func, Map, Pointer, UnsafePointer:
		if v.flag&flagMethod != 0 {
			return false
		}
		ptr := v.ptr
		if v.flag&flagIndir != 0 {
			ptr = *(*unsafe.Pointer)(ptr)
		}
		return ptr == nil
	case Interface, Slice:
		// Both interface and slice are nil if first word is 0.
		// Both are always bigger than a word; assume flagIndir.
		return *(*unsafe.Pointer)(v.ptr) == nil
	}
	panic(&ValueError{"reflect.Value.IsNil", v.kind()})
}

// IsValid reports whether v represents a value.
// It returns false if v is the zero Value.
// If IsValid returns false, all other methods except String panic.
// Most functions and methods never return an invalid Value.
// If one does, its documentation states the conditions explicitly.
func (v Value) IsValid() bool {
	return v.flag != 0
}

// IsZero reports whether v is the zero value for its type.
// It panics if the argument is invalid.
func (v Value) IsZero() bool {
	switch v.kind() {
	case Bool:
		return !v.Bool()
	case Int, Int8, Int16, Int32, Int64:
		return v.Int() == 0
	case Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
		return v.Uint() == 0
	case Float32, Float64:
		return math.Float64bits(v.Float()) == 0
	case Complex64, Complex128:
		c := v.Complex()
		return math.Float64bits(real(c)) == 0 && math.Float64bits(imag(c)) == 0
	case Array:
		for i := 0; i < v.Len(); i++ {
			if !v.Index(i).IsZero() {
				return false
			}
		}
		return true
	case Chan, Func, Interface, Map, Pointer, Slice, UnsafePointer:
		return v.IsNil()
	case String:
		return v.Len() == 0
	case Struct:
		for i := 0; i < v.NumField(); i++ {
			if !v.Field(i).IsZero() {
				return false
			}
		}
		return true
	default:
		// This should never happens, but will act as a safeguard for
		// later, as a default value doesn't makes sense here.
		panic(&ValueError{"reflect.Value.IsZero", v.Kind()})
	}
}

// Kind returns v's Kind.
// If v is the zero Value (IsValid returns false), Kind returns Invalid.
func (v Value) Kind() Kind {
	return v.kind()
}

// Len returns v's length.
// It panics if v's Kind is not Array, Chan, Map, Slice, or String.
func (v Value) Len() int {
	k := v.kind()
	switch k {
	case Array:
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		return int(tt.len)
	case Chan:
		return chanlen(v.pointer())
	case Map:
		return maplen(v.pointer())
	case Slice:
		// Slice is bigger than a word; assume flagIndir.
		return (*unsafeheader.Slice)(v.ptr).Len
	case String:
		// String is bigger than a word; assume flagIndir.
		return (*unsafeheader.String)(v.ptr).Len
	}
	panic(&ValueError{"reflect.Value.Len", v.kind()})
}

// MapIndex returns the value associated with key in the map v.
// It panics if v's Kind is not Map.
// It returns the zero Value if key is not found in the map or if v represents a nil map.
// As in Go, the key's value must be assignable to the map's key type.
func (v Value) MapIndex(key Value) Value {
	v.mustBe(Map)
	tt := (*mapType)(unsafe.Pointer(v.typ))

	// Do not require key to be exported, so that DeepEqual
	// and other programs can use all the keys returned by
	// MapKeys as arguments to MapIndex. If either the map
	// or the key is unexported, though, the result will be
	// considered unexported. This is consistent with the
	// behavior for structs, which allow read but not write
	// of unexported fields.

	var e unsafe.Pointer
	if key.kind() == String && tt.key.Kind() == String && tt.elem.size <= maxValSize {
		k := *(*string)(key.ptr)
		e = mapaccess_faststr(v.typ, v.pointer(), k)
	} else {
		key = key.assignTo("reflect.Value.MapIndex", tt.key, nil)
		var k unsafe.Pointer
		if key.flag&flagIndir != 0 {
			k = key.ptr
		} else {
			k = unsafe.Pointer(&key.ptr)
		}
		e = mapaccess(v.typ, v.pointer(), k)
	}
	if e == nil {
		return Value{}
	}
	typ := tt.elem
	fl := (v.flag | key.flag).ro()
	fl |= flag(typ.Kind())
	return copyVal(typ, fl, e)
}

// MapKeys returns a slice containing all the keys present in the map,
// in unspecified order.
// It panics if v's Kind is not Map.
// It returns an empty slice if v represents a nil map.
func (v Value) MapKeys() []Value {
	v.mustBe(Map)
	tt := (*mapType)(unsafe.Pointer(v.typ))
	keyType := tt.key

	fl := v.flag.ro() | flag(keyType.Kind())

	m := v.pointer()
	mlen := int(0)
	if m != nil {
		mlen = maplen(m)
	}
	var it hiter
	mapiterinit(v.typ, m, &it)
	a := make([]Value, mlen)
	var i int
	for i = 0; i < len(a); i++ {
		key := mapiterkey(&it)
		if key == nil {
			// Someone deleted an entry from the map since we
			// called maplen above. It's a data race, but nothing
			// we can do about it.
			break
		}
		a[i] = copyVal(keyType, fl, key)
		mapiternext(&it)
	}
	return a[:i]
}

// hiter's structure matches runtime.hiter's structure.
// Having a clone here allows us to embed a map iterator
// inside type MapIter so that MapIters can be re-used
// without doing any allocations.
type hiter struct {
	key         unsafe.Pointer
	elem        unsafe.Pointer
	t           unsafe.Pointer
	h           unsafe.Pointer
	buckets     unsafe.Pointer
	bptr        unsafe.Pointer
	overflow    *[]unsafe.Pointer
	oldoverflow *[]unsafe.Pointer
	startBucket uintptr
	offset      uint8
	wrapped     bool
	B           uint8
	i           uint8
	bucket      uintptr
	checkBucket uintptr
}

func (h *hiter) initialized() bool {
	return h.t != nil
}

// A MapIter is an iterator for ranging over a map.
// See Value.MapRange.
type MapIter struct {
	m     Value
	hiter hiter
}

// Key returns the key of iter's current map entry.
func (iter *MapIter) Key() Value {
	if !iter.hiter.initialized() {
		panic("MapIter.Key called before Next")
	}
	iterkey := mapiterkey(&iter.hiter)
	if iterkey == nil {
		panic("MapIter.Key called on exhausted iterator")
	}

	t := (*mapType)(unsafe.Pointer(iter.m.typ))
	ktype := t.key
	return copyVal(ktype, iter.m.flag.ro()|flag(ktype.Kind()), iterkey)
}

// SetIterKey assigns to v the key of iter's current map entry.
// It is equivalent to v.Set(iter.Key()), but it avoids allocating a new Value.
// As in Go, the key must be assignable to v's type.
func (v Value) SetIterKey(iter *MapIter) {
	if !iter.hiter.initialized() {
		panic("reflect: Value.SetIterKey called before Next")
	}
	iterkey := mapiterkey(&iter.hiter)
	if iterkey == nil {
		panic("reflect: Value.SetIterKey called on exhausted iterator")
	}

	v.mustBeAssignable()
	var target unsafe.Pointer
	if v.kind() == Interface {
		target = v.ptr
	}

	t := (*mapType)(unsafe.Pointer(iter.m.typ))
	ktype := t.key

	key := Value{ktype, iterkey, iter.m.flag | flag(ktype.Kind()) | flagIndir}
	key = key.assignTo("reflect.MapIter.SetKey", v.typ, target)
	typedmemmove(v.typ, v.ptr, key.ptr)
}

// Value returns the value of iter's current map entry.
func (iter *MapIter) Value() Value {
	if !iter.hiter.initialized() {
		panic("MapIter.Value called before Next")
	}
	iterelem := mapiterelem(&iter.hiter)
	if iterelem == nil {
		panic("MapIter.Value called on exhausted iterator")
	}

	t := (*mapType)(unsafe.Pointer(iter.m.typ))
	vtype := t.elem
	return copyVal(vtype, iter.m.flag.ro()|flag(vtype.Kind()), iterelem)
}

// SetIterValue assigns to v the value of iter's current map entry.
// It is equivalent to v.Set(iter.Value()), but it avoids allocating a new Value.
// As in Go, the value must be assignable to v's type.
func (v Value) SetIterValue(iter *MapIter) {
	if !iter.hiter.initialized() {
		panic("reflect: Value.SetIterValue called before Next")
	}
	iterelem := mapiterelem(&iter.hiter)
	if iterelem == nil {
		panic("reflect: Value.SetIterValue called on exhausted iterator")
	}

	v.mustBeAssignable()
	var target unsafe.Pointer
	if v.kind() == Interface {
		target = v.ptr
	}

	t := (*mapType)(unsafe.Pointer(iter.m.typ))
	vtype := t.elem

	elem := Value{vtype, iterelem, iter.m.flag | flag(vtype.Kind()) | flagIndir}
	elem = elem.assignTo("reflect.MapIter.SetValue", v.typ, target)
	typedmemmove(v.typ, v.ptr, elem.ptr)
}

// Next advances the map iterator and reports whether there is another
// entry. It returns false when iter is exhausted; subsequent
// calls to Key, Value, or Next will panic.
func (iter *MapIter) Next() bool {
	if !iter.m.IsValid() {
		panic("MapIter.Next called on an iterator that does not have an associated map Value")
	}
	if !iter.hiter.initialized() {
		mapiterinit(iter.m.typ, iter.m.pointer(), &iter.hiter)
	} else {
		if mapiterkey(&iter.hiter) == nil {
			panic("MapIter.Next called on exhausted iterator")
		}
		mapiternext(&iter.hiter)
	}
	return mapiterkey(&iter.hiter) != nil
}

// Reset modifies iter to iterate over v.
// It panics if v's Kind is not Map and v is not the zero Value.
// Reset(Value{}) causes iter to not to refer to any map,
// which may allow the previously iterated-over map to be garbage collected.
func (iter *MapIter) Reset(v Value) {
	if v.IsValid() {
		v.mustBe(Map)
	}
	iter.m = v
	iter.hiter = hiter{}
}

// MapRange returns a range iterator for a map.
// It panics if v's Kind is not Map.
//
// Call Next to advance the iterator, and Key/Value to access each entry.
// Next returns false when the iterator is exhausted.
// MapRange follows the same iteration semantics as a range statement.
//
// Example:
//
//	iter := reflect.ValueOf(m).MapRange()
// 	for iter.Next() {
//		k := iter.Key()
//		v := iter.Value()
//		...
//	}
//
func (v Value) MapRange() *MapIter {
	v.mustBe(Map)
	return &MapIter{m: v}
}

// copyVal returns a Value containing the map key or value at ptr,
// allocating a new variable as needed.
func copyVal(typ *rtype, fl flag, ptr unsafe.Pointer) Value {
	if ifaceIndir(typ) {
		// Copy result so future changes to the map
		// won't change the underlying value.
		c := unsafe_New(typ)
		typedmemmove(typ, c, ptr)
		return Value{typ, c, fl | flagIndir}
	}
	return Value{typ, *(*unsafe.Pointer)(ptr), fl}
}

// Method returns a function value corresponding to v's i'th method.
// The arguments to a Call on the returned function should not include
// a receiver; the returned function will always use v as the receiver.
// Method panics if i is out of range or if v is a nil interface value.
func (v Value) Method(i int) Value {
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.Method", Invalid})
	}
	if v.flag&flagMethod != 0 || uint(i) >= uint(v.typ.NumMethod()) {
		panic("reflect: Method index out of range")
	}
	if v.typ.Kind() == Interface && v.IsNil() {
		panic("reflect: Method on nil interface value")
	}
	fl := v.flag.ro() | (v.flag & flagIndir)
	fl |= flag(Func)
	fl |= flag(i)<<flagMethodShift | flagMethod
	return Value{v.typ, v.ptr, fl}
}

// NumMethod returns the number of exported methods in the value's method set.
func (v Value) NumMethod() int {
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.NumMethod", Invalid})
	}
	if v.flag&flagMethod != 0 {
		return 0
	}
	return v.typ.NumMethod()
}

// MethodByName returns a function value corresponding to the method
// of v with the given name.
// The arguments to a Call on the returned function should not include
// a receiver; the returned function will always use v as the receiver.
// It returns the zero Value if no method was found.
func (v Value) MethodByName(name string) Value {
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.MethodByName", Invalid})
	}
	if v.flag&flagMethod != 0 {
		return Value{}
	}
	m, ok := v.typ.MethodByName(name)
	if !ok {
		return Value{}
	}
	return v.Method(m.Index)
}

// NumField returns the number of fields in the struct v.
// It panics if v's Kind is not Struct.
func (v Value) NumField() int {
	v.mustBe(Struct)
	tt := (*structType)(unsafe.Pointer(v.typ))
	return len(tt.fields)
}

// OverflowComplex reports whether the complex128 x cannot be represented by v's type.
// It panics if v's Kind is not Complex64 or Complex128.
func (v Value) OverflowComplex(x complex128) bool {
	k := v.kind()
	switch k {
	case Complex64:
		return overflowFloat32(real(x)) || overflowFloat32(imag(x))
	case Complex128:
		return false
	}
	panic(&ValueError{"reflect.Value.OverflowComplex", v.kind()})
}

// OverflowFloat reports whether the float64 x cannot be represented by v's type.
// It panics if v's Kind is not Float32 or Float64.
func (v Value) OverflowFloat(x float64) bool {
	k := v.kind()
	switch k {
	case Float32:
		return overflowFloat32(x)
	case Float64:
		return false
	}
	panic(&ValueError{"reflect.Value.OverflowFloat", v.kind()})
}

func overflowFloat32(x float64) bool {
	if x < 0 {
		x = -x
	}
	return math.MaxFloat32 < x && x <= math.MaxFloat64
}

// OverflowInt reports whether the int64 x cannot be represented by v's type.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64.
func (v Value) OverflowInt(x int64) bool {
	k := v.kind()
	switch k {
	case Int, Int8, Int16, Int32, Int64:
		bitSize := v.typ.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic(&ValueError{"reflect.Value.OverflowInt", v.kind()})
}

// OverflowUint reports whether the uint64 x cannot be represented by v's type.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64.
func (v Value) OverflowUint(x uint64) bool {
	k := v.kind()
	switch k {
	case Uint, Uintptr, Uint8, Uint16, Uint32, Uint64:
		bitSize := v.typ.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic(&ValueError{"reflect.Value.OverflowUint", v.kind()})
}

//go:nocheckptr
// This prevents inlining Value.Pointer when -d=checkptr is enabled,
// which ensures cmd/compile can recognize unsafe.Pointer(v.Pointer())
// and make an exception.

// Pointer returns v's value as a uintptr.
// It returns uintptr instead of unsafe.Pointer so that
// code using reflect cannot obtain unsafe.Pointers
// without importing the unsafe package explicitly.
// It panics if v's Kind is not Chan, Func, Map, Pointer, Slice, or UnsafePointer.
//
// If v's Kind is Func, the returned pointer is an underlying
// code pointer, but not necessarily enough to identify a
// single function uniquely. The only guarantee is that the
// result is zero if and only if v is a nil func Value.
//
// If v's Kind is Slice, the returned pointer is to the first
// element of the slice. If the slice is nil the returned value
// is 0.  If the slice is empty but non-nil the return value is non-zero.
//
// It's preferred to use uintptr(Value.UnsafePointer()) to get the equivalent result.
func (v Value) Pointer() uintptr {
	k := v.kind()
	switch k {
	case Pointer:
		if v.typ.ptrdata == 0 {
			val := *(*uintptr)(v.ptr)
			// Since it is a not-in-heap pointer, all pointers to the heap are
			// forbidden! See comment in Value.Elem and issue #48399.
			if !verifyNotInHeapPtr(val) {
				panic("reflect: reflect.Value.Pointer on an invalid notinheap pointer")
			}
			return val
		}
		fallthrough
	case Chan, Map, UnsafePointer:
		return uintptr(v.pointer())
	case Func:
		if v.flag&flagMethod != 0 {
			// As the doc comment says, the returned pointer is an
			// underlying code pointer but not necessarily enough to
			// identify a single function uniquely. All method expressions
			// created via reflect have the same underlying code pointer,
			// so their Pointers are equal. The function used here must
			// match the one used in makeMethodValue.
			return methodValueCallCodePtr()
		}
		p := v.pointer()
		// Non-nil func value points at data block.
		// First word of data block is actual code.
		if p != nil {
			p = *(*unsafe.Pointer)(p)
		}
		return uintptr(p)

	case Slice:
		return (*SliceHeader)(v.ptr).Data
	}
	panic(&ValueError{"reflect.Value.Pointer", v.kind()})
}

// Recv receives and returns a value from the channel v.
// It panics if v's Kind is not Chan.
// The receive blocks until a value is ready.
// The boolean value ok is true if the value x corresponds to a send
// on the channel, false if it is a zero value received because the channel is closed.
func (v Value) Recv() (x Value, ok bool) {
	v.mustBe(Chan)
	v.mustBeExported()
	return v.recv(false)
}

// internal recv, possibly non-blocking (nb).
// v is known to be a channel.
func (v Value) recv(nb bool) (val Value, ok bool) {
	tt := (*chanType)(unsafe.Pointer(v.typ))
	if ChanDir(tt.dir)&RecvDir == 0 {
		panic("reflect: recv on send-only channel")
	}
	t := tt.elem
	val = Value{t, nil, flag(t.Kind())}
	var p unsafe.Pointer
	if ifaceIndir(t) {
		p = unsafe_New(t)
		val.ptr = p
		val.flag |= flagIndir
	} else {
		p = unsafe.Pointer(&val.ptr)
	}
	selected, ok := chanrecv(v.pointer(), nb, p)
	if !selected {
		val = Value{}
	}
	return
}

// Send sends x on the channel v.
// It panics if v's kind is not Chan or if x's type is not the same type as v's element type.
// As in Go, x's value must be assignable to the channel's element type.
func (v Value) Send(x Value) {
	v.mustBe(Chan)
	v.mustBeExported()
	v.send(x, false)
}

// internal send, possibly non-blocking.
// v is known to be a channel.
func (v Value) send(x Value, nb bool) (selected bool) {
	tt := (*chanType)(unsafe.Pointer(v.typ))
	if ChanDir(tt.dir)&SendDir == 0 {
		panic("reflect: send on recv-only channel")
	}
	x.mustBeExported()
	x = x.assignTo("reflect.Value.Send", tt.elem, nil)
	var p unsafe.Pointer
	if x.flag&flagIndir != 0 {
		p = x.ptr
	} else {
		p = unsafe.Pointer(&x.ptr)
	}
	return chansend(v.pointer(), p, nb)
}

// Set assigns x to the value v.
// It panics if CanSet returns false.
// As in Go, x's value must be assignable to v's type.
func (v Value) Set(x Value) {
	v.mustBeAssignable()
	x.mustBeExported() // do not let unexported x leak
	var target unsafe.Pointer
	if v.kind() == Interface {
		target = v.ptr
	}
	x = x.assignTo("reflect.Set", v.typ, target)
	if x.flag&flagIndir != 0 {
		if x.ptr == unsafe.Pointer(&zeroVal[0]) {
			typedmemclr(v.typ, v.ptr)
		} else {
			typedmemmove(v.typ, v.ptr, x.ptr)
		}
	} else {
		*(*unsafe.Pointer)(v.ptr) = x.ptr
	}
}

// SetBool sets v's underlying value.
// It panics if v's Kind is not Bool or if CanSet() is false.
func (v Value) SetBool(x bool) {
	v.mustBeAssignable()
	v.mustBe(Bool)
	*(*bool)(v.ptr) = x
}

// SetBytes sets v's underlying value.
// It panics if v's underlying value is not a slice of bytes.
func (v Value) SetBytes(x []byte) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Uint8 {
		panic("reflect.Value.SetBytes of non-byte slice")
	}
	*(*[]byte)(v.ptr) = x
}

// setRunes sets v's underlying value.
// It panics if v's underlying value is not a slice of runes (int32s).
func (v Value) setRunes(x []rune) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Int32 {
		panic("reflect.Value.setRunes of non-rune slice")
	}
	*(*[]rune)(v.ptr) = x
}

// SetComplex sets v's underlying value to x.
// It panics if v's Kind is not Complex64 or Complex128, or if CanSet() is false.
func (v Value) SetComplex(x complex128) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetComplex", v.kind()})
	case Complex64:
		*(*complex64)(v.ptr) = complex64(x)
	case Complex128:
		*(*complex128)(v.ptr) = x
	}
}

// SetFloat sets v's underlying value to x.
// It panics if v's Kind is not Float32 or Float64, or if CanSet() is false.
func (v Value) SetFloat(x float64) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetFloat", v.kind()})
	case Float32:
		*(*float32)(v.ptr) = float32(x)
	case Float64:
		*(*float64)(v.ptr) = x
	}
}

// SetInt sets v's underlying value to x.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64, or if CanSet() is false.
func (v Value) SetInt(x int64) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetInt", v.kind()})
	case Int:
		*(*int)(v.ptr) = int(x)
	case Int8:
		*(*int8)(v.ptr) = int8(x)
	case Int16:
		*(*int16)(v.ptr) = int16(x)
	case Int32:
		*(*int32)(v.ptr) = int32(x)
	case Int64:
		*(*int64)(v.ptr) = x
	}
}

// SetLen sets v's length to n.
// It panics if v's Kind is not Slice or if n is negative or
// greater than the capacity of the slice.
func (v Value) SetLen(n int) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	s := (*unsafeheader.Slice)(v.ptr)
	if uint(n) > uint(s.Cap) {
		panic("reflect: slice length out of range in SetLen")
	}
	s.Len = n
}

// SetCap sets v's capacity to n.
// It panics if v's Kind is not Slice or if n is smaller than the length or
// greater than the capacity of the slice.
func (v Value) SetCap(n int) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	s := (*unsafeheader.Slice)(v.ptr)
	if n < s.Len || n > s.Cap {
		panic("reflect: slice capacity out of range in SetCap")
	}
	s.Cap = n
}

// SetMapIndex sets the element associated with key in the map v to elem.
// It panics if v's Kind is not Map.
// If elem is the zero Value, SetMapIndex deletes the key from the map.
// Otherwise if v holds a nil map, SetMapIndex will panic.
// As in Go, key's elem must be assignable to the map's key type,
// and elem's value must be assignable to the map's elem type.
func (v Value) SetMapIndex(key, elem Value) {
	v.mustBe(Map)
	v.mustBeExported()
	key.mustBeExported()
	tt := (*mapType)(unsafe.Pointer(v.typ))

	if key.kind() == String && tt.key.Kind() == String && tt.elem.size <= maxValSize {
		k := *(*string)(key.ptr)
		if elem.typ == nil {
			mapdelete_faststr(v.typ, v.pointer(), k)
			return
		}
		elem.mustBeExported()
		elem = elem.assignTo("reflect.Value.SetMapIndex", tt.elem, nil)
		var e unsafe.Pointer
		if elem.flag&flagIndir != 0 {
			e = elem.ptr
		} else {
			e = unsafe.Pointer(&elem.ptr)
		}
		mapassign_faststr(v.typ, v.pointer(), k, e)
		return
	}

	key = key.assignTo("reflect.Value.SetMapIndex", tt.key, nil)
	var k unsafe.Pointer
	if key.flag&flagIndir != 0 {
		k = key.ptr
	} else {
		k = unsafe.Pointer(&key.ptr)
	}
	if elem.typ == nil {
		mapdelete(v.typ, v.pointer(), k)
		return
	}
	elem.mustBeExported()
	elem = elem.assignTo("reflect.Value.SetMapIndex", tt.elem, nil)
	var e unsafe.Pointer
	if elem.flag&flagIndir != 0 {
		e = elem.ptr
	} else {
		e = unsafe.Pointer(&elem.ptr)
	}
	mapassign(v.typ, v.pointer(), k, e)
}

// SetUint sets v's underlying value to x.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64, or if CanSet() is false.
func (v Value) SetUint(x uint64) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetUint", v.kind()})
	case Uint:
		*(*uint)(v.ptr) = uint(x)
	case Uint8:
		*(*uint8)(v.ptr) = uint8(x)
	case Uint16:
		*(*uint16)(v.ptr) = uint16(x)
	case Uint32:
		*(*uint32)(v.ptr) = uint32(x)
	case Uint64:
		*(*uint64)(v.ptr) = x
	case Uintptr:
		*(*uintptr)(v.ptr) = uintptr(x)
	}
}

// SetPointer sets the unsafe.Pointer value v to x.
// It panics if v's Kind is not UnsafePointer.
func (v Value) SetPointer(x unsafe.Pointer) {
	v.mustBeAssignable()
	v.mustBe(UnsafePointer)
	*(*unsafe.Pointer)(v.ptr) = x
}

// SetString sets v's underlying value to x.
// It panics if v's Kind is not String or if CanSet() is false.
func (v Value) SetString(x string) {
	v.mustBeAssignable()
	v.mustBe(String)
	*(*string)(v.ptr) = x
}

// Slice returns v[i:j].
// It panics if v's Kind is not Array, Slice or String, or if v is an unaddressable array,
// or if the indexes are out of bounds.
func (v Value) Slice(i, j int) Value {
	var (
		cap  int
		typ  *sliceType
		base unsafe.Pointer
	)
	switch kind := v.kind(); kind {
	default:
		panic(&ValueError{"reflect.Value.Slice", v.kind()})

	case Array:
		if v.flag&flagAddr == 0 {
			panic("reflect.Value.Slice: slice of unaddressable array")
		}
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		cap = int(tt.len)
		typ = (*sliceType)(unsafe.Pointer(tt.slice))
		base = v.ptr

	case Slice:
		typ = (*sliceType)(unsafe.Pointer(v.typ))
		s := (*unsafeheader.Slice)(v.ptr)
		base = s.Data
		cap = s.Cap

	case String:
		s := (*unsafeheader.String)(v.ptr)
		if i < 0 || j < i || j > s.Len {
			panic("reflect.Value.Slice: string slice index out of bounds")
		}
		var t unsafeheader.String
		if i < s.Len {
			t = unsafeheader.String{Data: arrayAt(s.Data, i, 1, "i < s.Len"), Len: j - i}
		}
		return Value{v.typ, unsafe.Pointer(&t), v.flag}
	}

	if i < 0 || j < i || j > cap {
		panic("reflect.Value.Slice: slice index out of bounds")
	}

	// Declare slice so that gc can see the base pointer in it.
	var x []unsafe.Pointer

	// Reinterpret as *unsafeheader.Slice to edit.
	s := (*unsafeheader.Slice)(unsafe.Pointer(&x))
	s.Len = j - i
	s.Cap = cap - i
	if cap-i > 0 {
		s.Data = arrayAt(base, i, typ.elem.Size(), "i < cap")
	} else {
		// do not advance pointer, to avoid pointing beyond end of slice
		s.Data = base
	}

	fl := v.flag.ro() | flagIndir | flag(Slice)
	return Value{typ.common(), unsafe.Pointer(&x), fl}
}

// Slice3 is the 3-index form of the slice operation: it returns v[i:j:k].
// It panics if v's Kind is not Array or Slice, or if v is an unaddressable array,
// or if the indexes are out of bounds.
func (v Value) Slice3(i, j, k int) Value {
	var (
		cap  int
		typ  *sliceType
		base unsafe.Pointer
	)
	switch kind := v.kind(); kind {
	default:
		panic(&ValueError{"reflect.Value.Slice3", v.kind()})

	case Array:
		if v.flag&flagAddr == 0 {
			panic("reflect.Value.Slice3: slice of unaddressable array")
		}
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		cap = int(tt.len)
		typ = (*sliceType)(unsafe.Pointer(tt.slice))
		base = v.ptr

	case Slice:
		typ = (*sliceType)(unsafe.Pointer(v.typ))
		s := (*unsafeheader.Slice)(v.ptr)
		base = s.Data
		cap = s.Cap
	}

	if i < 0 || j < i || k < j || k > cap {
		panic("reflect.Value.Slice3: slice index out of bounds")
	}

	// Declare slice so that the garbage collector
	// can see the base pointer in it.
	var x []unsafe.Pointer

	// Reinterpret as *unsafeheader.Slice to edit.
	s := (*unsafeheader.Slice)(unsafe.Pointer(&x))
	s.Len = j - i
	s.Cap = k - i
	if k-i > 0 {
		s.Data = arrayAt(base, i, typ.elem.Size(), "i < k <= cap")
	} else {
		// do not advance pointer, to avoid pointing beyond end of slice
		s.Data = base
	}

	fl := v.flag.ro() | flagIndir | flag(Slice)
	return Value{typ.common(), unsafe.Pointer(&x), fl}
}

// String returns the string v's underlying value, as a string.
// String is a special case because of Go's String method convention.
// Unlike the other getters, it does not panic if v's Kind is not String.
// Instead, it returns a string of the form "<T value>" where T is v's type.
// The fmt package treats Values specially. It does not call their String
// method implicitly but instead prints the concrete values they hold.
func (v Value) String() string {
	switch k := v.kind(); k {
	case Invalid:
		return "<invalid Value>"
	case String:
		return *(*string)(v.ptr)
	}
	// If you call String on a reflect.Value of other type, it's better to
	// print something than to panic. Useful in debugging.
	return "<" + v.Type().String() + " Value>"
}

// TryRecv attempts to receive a value from the channel v but will not block.
// It panics if v's Kind is not Chan.
// If the receive delivers a value, x is the transferred value and ok is true.
// If the receive cannot finish without blocking, x is the zero Value and ok is false.
// If the channel is closed, x is the zero value for the channel's element type and ok is false.
func (v Value) TryRecv() (x Value, ok bool) {
	v.mustBe(Chan)
	v.mustBeExported()
	return v.recv(true)
}

// TrySend attempts to send x on the channel v but will not block.
// It panics if v's Kind is not Chan.
// It reports whether the value was sent.
// As in Go, x's value must be assignable to the channel's element type.
func (v Value) TrySend(x Value) bool {
	v.mustBe(Chan)
	v.mustBeExported()
	return v.send(x, true)
}

// Type returns v's type.
func (v Value) Type() Type {
	f := v.flag
	if f == 0 {
		panic(&ValueError{"reflect.Value.Type", Invalid})
	}
	if f&flagMethod == 0 {
		// Easy case
		return toType(v.typ)
	}

	// Method value.
	// v.typ describes the receiver, not the method type.
	i := int(v.flag) >> flagMethodShift
	if v.typ.Kind() == Interface {
		// Method on interface.
		tt := (*interfaceType)(unsafe.Pointer(v.typ))
		if uint(i) >= uint(len(tt.methods)) {
			panic("reflect: internal error: invalid method index")
		}
		m := &tt.methods[i]
		return toType(m.typ)
	}
	// Method on concrete type.
	ms := v.typ.exportedMethods()
	if uint(i) >= uint(len(ms)) {
		panic("reflect: internal error: invalid method index")
	}
	m := ms[i]
	return toType(m.mtyp)
}

// CanUint reports whether Uint can be used without panicking.
func (v Value) CanUint() bool {
	switch v.kind() {
	case Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
		return true
	default:
		return false
	}
}

// Uint returns v's underlying value, as a uint64.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64.
func (v Value) Uint() uint64 {
	k := v.kind()
	p := v.ptr
	switch k {
	case Uint:
		return uint64(*(*uint)(p))
	case Uint8:
		return uint64(*(*uint8)(p))
	case Uint16:
		return uint64(*(*uint16)(p))
	case Uint32:
		return uint64(*(*uint32)(p))
	case Uint64:
		return *(*uint64)(p)
	case Uintptr:
		return uint64(*(*uintptr)(p))
	}
	panic(&ValueError{"reflect.Value.Uint", v.kind()})
}

//go:nocheckptr
// This prevents inlining Value.UnsafeAddr when -d=checkptr is enabled,
// which ensures cmd/compile can recognize unsafe.Pointer(v.UnsafeAddr())
// and make an exception.

// UnsafeAddr returns a pointer to v's data, as a uintptr.
// It is for advanced clients that also import the "unsafe" package.
// It panics if v is not addressable.
//
// It's preferred to use uintptr(Value.Addr().UnsafePointer()) to get the equivalent result.
func (v Value) UnsafeAddr() uintptr {
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.UnsafeAddr", Invalid})
	}
	if v.flag&flagAddr == 0 {
		panic("reflect.Value.UnsafeAddr of unaddressable value")
	}
	return uintptr(v.ptr)
}

// UnsafePointer returns v's value as a unsafe.Pointer.
// It panics if v's Kind is not Chan, Func, Map, Pointer, Slice, or UnsafePointer.
//
// If v's Kind is Func, the returned pointer is an underlying
// code pointer, but not necessarily enough to identify a
// single function uniquely. The only guarantee is that the
// result is zero if and only if v is a nil func Value.
//
// If v's Kind is Slice, the returned pointer is to the first
// element of the slice. If the slice is nil the returned value
// is nil.  If the slice is empty but non-nil the return value is non-nil.
func (v Value) UnsafePointer() unsafe.Pointer {
	k := v.kind()
	switch k {
	case Pointer:
		if v.typ.ptrdata == 0 {
			// Since it is a not-in-heap pointer, all pointers to the heap are
			// forbidden! See comment in Value.Elem and issue #48399.
			if !verifyNotInHeapPtr(*(*uintptr)(v.ptr)) {
				panic("reflect: reflect.Value.UnsafePointer on an invalid notinheap pointer")
			}
			return *(*unsafe.Pointer)(v.ptr)
		}
		fallthrough
	case Chan, Map, UnsafePointer:
		return v.pointer()
	case Func:
		if v.flag&flagMethod != 0 {
			// As the doc comment says, the returned pointer is an
			// underlying code pointer but not necessarily enough to
			// identify a single function uniquely. All method expressions
			// created via reflect have the same underlying code pointer,
			// so their Pointers are equal. The function used here must
			// match the one used in makeMethodValue.
			code := methodValueCallCodePtr()
			return *(*unsafe.Pointer)(unsafe.Pointer(&code))
		}
		p := v.pointer()
		// Non-nil func value points at data block.
		// First word of data block is actual code.
		if p != nil {
			p = *(*unsafe.Pointer)(p)
		}
		return p

	case Slice:
		return (*unsafeheader.Slice)(v.ptr).Data
	}
	panic(&ValueError{"reflect.Value.UnsafePointer", v.kind()})
}

// StringHeader is the runtime representation of a string.
// It cannot be used safely or portably and its representation may
// change in a later release.
// Moreover, the Data field is not sufficient to guarantee the data
// it references will not be garbage collected, so programs must keep
// a separate, correctly typed pointer to the underlying data.
type StringHeader struct {
	Data uintptr
	Len  int
}

// SliceHeader is the runtime representation of a slice.
// It cannot be used safely or portably and its representation may
// change in a later release.
// Moreover, the Data field is not sufficient to guarantee the data
// it references will not be garbage collected, so programs must keep
// a separate, correctly typed pointer to the underlying data.
type SliceHeader struct {
	Data uintptr
	Len  int
	Cap  int
}

func typesMustMatch(what string, t1, t2 Type) {
	if !typeEqual(t1, t2) {
		panic(what + ": " + t1.String() + " != " + t2.String())
	}
}

// arrayAt returns the i-th element of p,
// an array whose elements are eltSize bytes wide.
// The array pointed at by p must have at least i+1 elements:
// it is invalid (but impossible to check here) to pass i >= len,
// because then the result will point outside the array.
// whySafe must explain why i < len. (Passing "i < len" is fine;
// the benefit is to surface this assumption at the call site.)
func arrayAt(p unsafe.Pointer, i int, eltSize uintptr, whySafe string) unsafe.Pointer {
	return add(p, uintptr(i)*eltSize, "i < len")
}

// grow grows the slice s so that it can hold extra more values, allocating
// more capacity if needed. It also returns the old and new slice lengths.
func grow(s Value, extra int) (Value, int, int) {
	i0 := s.Len()
	i1 := i0 + extra
	if i1 < i0 {
		panic("reflect.Append: slice overflow")
	}
	m := s.Cap()
	if i1 <= m {
		return s.Slice(0, i1), i0, i1
	}
	if m == 0 {
		m = extra
	} else {
		const threshold = 256
		for m < i1 {
			if i0 < threshold {
				m += m
			} else {
				m += (m + 3*threshold) / 4
			}
		}
	}
	t := MakeSlice(s.Type(), i1, m)
	Copy(t, s)
	return t, i0, i1
}

// Append appends the values x to a slice s and returns the resulting slice.
// As in Go, each x's value must be assignable to the slice's element type.
func Append(s Value, x ...Value) Value {
	s.mustBe(Slice)
	s, i0, i1 := grow(s, len(x))
	for i, j := i0, 0; i < i1; i, j = i+1, j+1 {
		s.Index(i).Set(x[j])
	}
	return s
}

// AppendSlice appends a slice t to a slice s and returns the resulting slice.
// The slices s and t must have the same element type.
func AppendSlice(s, t Value) Value {
	s.mustBe(Slice)
	t.mustBe(Slice)
	typesMustMatch("reflect.AppendSlice", s.Type().Elem(), t.Type().Elem())
	s, i0, i1 := grow(s, t.Len())
	Copy(s.Slice(i0, i1), t)
	return s
}

// Copy copies the contents of src into dst until either
// dst has been filled or src has been exhausted.
// It returns the number of elements copied.
// Dst and src each must have kind Slice or Array, and
// dst and src must have the same element type.
//
// As a special case, src can have kind String if the element type of dst is kind Uint8.
func Copy(dst, src Value) int {
	dk := dst.kind()
	if dk != Array && dk != Slice {
		panic(&ValueError{"reflect.Copy", dk})
	}
	if dk == Array {
		dst.mustBeAssignable()
	}
	dst.mustBeExported()

	sk := src.kind()
	var stringCopy bool
	if sk != Array && sk != Slice {
		stringCopy = sk == String && dst.typ.Elem().Kind() == Uint8
		if !stringCopy {
			panic(&ValueError{"reflect.Copy", sk})
		}
	}
	src.mustBeExported()

	de := dst.typ.Elem()
	if !stringCopy {
		se := src.typ.Elem()
		typesMustMatch("reflect.Copy", de, se)
	}

	var ds, ss unsafeheader.Slice
	if dk == Array {
		ds.Data = dst.ptr
		ds.Len = dst.Len()
		ds.Cap = ds.Len
	} else {
		ds = *(*unsafeheader.Slice)(dst.ptr)
	}
	if sk == Array {
		ss.Data = src.ptr
		ss.Len = src.Len()
		ss.Cap = ss.Len
	} else if sk == Slice {
		ss = *(*unsafeheader.Slice)(src.ptr)
	} else {
		sh := *(*unsafeheader.String)(src.ptr)
		ss.Data = sh.Data
		ss.Len = sh.Len
		ss.Cap = sh.Len
	}

	return typedslicecopy(de.common(), ds, ss)
}

// A runtimeSelect is a single case passed to rselect.
// This must match ../runtime/select.go:/runtimeSelect
type runtimeSelect struct {
	dir SelectDir      // SelectSend, SelectRecv or SelectDefault
	typ *rtype         // channel type
	ch  unsafe.Pointer // channel
	val unsafe.Pointer // ptr to data (SendDir) or ptr to receive buffer (RecvDir)
}

// rselect runs a select. It returns the index of the chosen case.
// If the case was a receive, val is filled in with the received value.
// The conventional OK bool indicates whether the receive corresponds
// to a sent value.
//go:noescape
func rselect([]runtimeSelect) (chosen int, recvOK bool)

// A SelectDir describes the communication direction of a select case.
type SelectDir int

// NOTE: These values must match ../runtime/select.go:/selectDir.

const (
	_             SelectDir = iota
	SelectSend              // case Chan <- Send
	SelectRecv              // case <-Chan:
	SelectDefault           // default
)

// A SelectCase describes a single case in a select operation.
// The kind of case depends on Dir, the communication direction.
//
// If Dir is SelectDefault, the case represents a default case.
// Chan and Send must be zero Values.
//
// If Dir is SelectSend, the case represents a send operation.
// Normally Chan's underlying value must be a channel, and Send's underlying value must be
// assignable to the channel's element type. As a special case, if Chan is a zero Value,
// then the case is ignored, and the field Send will also be ignored and may be either zero
// or non-zero.
//
// If Dir is SelectRecv, the case represents a receive operation.
// Normally Chan's underlying value must be a channel and Send must be a zero Value.
// If Chan is a zero Value, then the case is ignored, but Send must still be a zero Value.
// When a receive operation is selected, the received Value is returned by Select.
//
type SelectCase struct {
	Dir  SelectDir // direction of case
	Chan Value     // channel to use (for send or receive)
	Send Value     // value to send (for send)
}

// Select executes a select operation described by the list of cases.
// Like the Go select statement, it blocks until at least one of the cases
// can proceed, makes a uniform pseudo-random choice,
// and then executes that case. It returns the index of the chosen case
// and, if that case was a receive operation, the value received and a
// boolean indicating whether the value corresponds to a send on the channel
// (as opposed to a zero value received because the channel is closed).
// Select supports a maximum of 65536 cases.
func Select(cases []SelectCase) (chosen int, recv Value, recvOK bool) {
	if len(cases) > 65536 {
		panic("reflect.Select: too many cases (max 65536)")
	}
	// NOTE: Do not trust that caller is not modifying cases data underfoot.
	// The range is safe because the caller cannot modify our copy of the len
	// and each iteration makes its own copy of the value c.
	var runcases []runtimeSelect
	if len(cases) > 4 {
		// Slice is heap allocated due to runtime dependent capacity.
		runcases = make([]runtimeSelect, len(cases))
	} else {
		// Slice can be stack allocated due to constant capacity.
		runcases = make([]runtimeSelect, len(cases), 4)
	}

	haveDefault := false
	for i, c := range cases {
		rc := &runcases[i]
		rc.dir = c.Dir
		switch c.Dir {
		default:
			panic("reflect.Select: invalid Dir")

		case SelectDefault: // default
			if haveDefault {
				panic("reflect.Select: multiple default cases")
			}
			haveDefault = true
			if c.Chan.IsValid() {
				panic("reflect.Select: default case has Chan value")
			}
			if c.Send.IsValid() {
				panic("reflect.Select: default case has Send value")
			}

		case SelectSend:
			ch := c.Chan
			if !ch.IsValid() {
				break
			}
			ch.mustBe(Chan)
			ch.mustBeExported()
			tt := (*chanType)(unsafe.Pointer(ch.typ))
			if ChanDir(tt.dir)&SendDir == 0 {
				panic("reflect.Select: SendDir case using recv-only channel")
			}
			rc.ch = ch.pointer()
			rc.typ = &tt.rtype
			v := c.Send
			if !v.IsValid() {
				panic("reflect.Select: SendDir case missing Send value")
			}
			v.mustBeExported()
			v = v.assignTo("reflect.Select", tt.elem, nil)
			if v.flag&flagIndir != 0 {
				rc.val = v.ptr
			} else {
				rc.val = unsafe.Pointer(&v.ptr)
			}

		case SelectRecv:
			if c.Send.IsValid() {
				panic("reflect.Select: RecvDir case has Send value")
			}
			ch := c.Chan
			if !ch.IsValid() {
				break
			}
			ch.mustBe(Chan)
			ch.mustBeExported()
			tt := (*chanType)(unsafe.Pointer(ch.typ))
			if ChanDir(tt.dir)&RecvDir == 0 {
				panic("reflect.Select: RecvDir case using send-only channel")
			}
			rc.ch = ch.pointer()
			rc.typ = &tt.rtype
			rc.val = unsafe_New(tt.elem)
		}
	}

	chosen, recvOK = rselect(runcases)
	if runcases[chosen].dir == SelectRecv {
		tt := (*chanType)(unsafe.Pointer(runcases[chosen].typ))
		t := tt.elem
		p := runcases[chosen].val
		fl := flag(t.Kind())
		if ifaceIndir(t) {
			recv = Value{t, p, fl | flagIndir}
		} else {
			recv = Value{t, *(*unsafe.Pointer)(p), fl}
		}
	}
	return chosen, recv, recvOK
}

/*
 * constructors
 */

// implemented in package runtime
func unsafe_New(*rtype) unsafe.Pointer
func unsafe_NewArray(*rtype, int) unsafe.Pointer

// MakeSlice creates a new zero-initialized slice value
// for the specified slice type, length, and capacity.
func MakeSlice(typ Type, len, cap int) Value {
	if typ.Kind() != Slice {
		panic("reflect.MakeSlice of non-slice type")
	}
	if len < 0 {
		panic("reflect.MakeSlice: negative len")
	}
	if cap < 0 {
		panic("reflect.MakeSlice: negative cap")
	}
	if len > cap {
		panic("reflect.MakeSlice: len > cap")
	}

	s := unsafeheader.Slice{Data: unsafe_NewArray(typ.Elem().(*rtype), cap), Len: len, Cap: cap}
	return Value{typ.(*rtype), unsafe.Pointer(&s), flagIndir | flag(Slice)}
}

// MakeChan creates a new channel with the specified type and buffer size.
func MakeChan(typ Type, buffer int) Value {
	if typ.Kind() != Chan {
		panic("reflect.MakeChan of non-chan type")
	}
	if buffer < 0 {
		panic("reflect.MakeChan: negative buffer size")
	}
	if typ.ChanDir() != BothDir {
		panic("reflect.MakeChan: unidirectional channel type")
	}
	t := typ.(*rtype)
	ch := makechan(t, buffer)
	return Value{t, unsafe.Pointer(&ch), flag(Chan) | flagIndir}
}

// MakeMap creates a new map with the specified type.
func MakeMap(typ Type) Value {
	return MakeMapWithSize(typ, 0)
}

// MakeMapWithSize creates a new map with the specified type
// and initial space for approximately n elements.
func MakeMapWithSize(typ Type, n int) Value {
	if typ.Kind() != Map {
		panic("reflect.MakeMapWithSize of non-map type")
	}
	t := typ.(*rtype)
	m := makemap(t, n)
	return Value{t, unsafe.Pointer(&m), flag(Map) | flagIndir}
}

// Indirect returns the value that v points to.
// If v is a nil pointer, Indirect returns a zero Value.
// If v is not a pointer, Indirect returns v.
func Indirect(v Value) Value {
	if v.Kind() != Pointer {
		return v
	}
	return v.Elem()
}

// ValueOf returns a new Value initialized to the concrete value
// stored in the interface i. ValueOf(nil) returns the zero Value.
func ValueOf(i any) Value {
	if i == nil {
		return Value{}
	}

	// TODO: Maybe allow contents of a Value to live on the stack.
	// For now we make the contents always escape to the heap. It
	// makes life easier in a few places (see chanrecv/mapassign
	// comment below).
	escapes(i)

	return unpackEface(i)
}

// Zero returns a Value representing the zero value for the specified type.
// The result is different from the zero value of the Value struct,
// which represents no value at all.
// For example, Zero(TypeOf(42)) returns a Value with Kind Int and value 0.
// The returned value is neither addressable nor settable.
func Zero(typ Type) Value {
	if typ == nil {
		panic("reflect: Zero(nil)")
	}
	t := typ.(*rtype)
	fl := flag(t.Kind())
	if ifaceIndir(t) {
		var p unsafe.Pointer
		if t.size <= maxZero {
			p = unsafe.Pointer(&zeroVal[0])
		} else {
			p = unsafe_New(t)
		}
		return Value{t, p, fl | flagIndir}
	}
	return Value{t, nil, fl}
}

// must match declarations in runtime/map.go.
const maxZero = 1024

// Using linkname here doesn't work for gofrontend.
// //go:linkname zeroVal runtime.zeroVal
var zeroVal [maxZero]byte

// New returns a Value representing a pointer to a new zero value
// for the specified type. That is, the returned Value's Type is PointerTo(typ).
func New(typ Type) Value {
	if typ == nil {
		panic("reflect: New(nil)")
	}
	t := typ.(*rtype)
	pt := t.ptrTo()
	if ifaceIndir(pt) {
		// This is a pointer to a go:notinheap type.
		panic("reflect: New of type that may not be allocated in heap (possibly undefined cgo C type)")
	}
	ptr := unsafe_New(t)
	fl := flag(Pointer)
	return Value{pt, ptr, fl}
}

// NewAt returns a Value representing a pointer to a value of the
// specified type, using p as that pointer.
func NewAt(typ Type, p unsafe.Pointer) Value {
	fl := flag(Pointer)
	t := typ.(*rtype)
	return Value{t.ptrTo(), p, fl}
}

// assignTo returns a value v that can be assigned directly to typ.
// It panics if v is not assignable to typ.
// For a conversion to an interface type, target is a suggested scratch space to use.
// target must be initialized memory (or nil).
func (v Value) assignTo(context string, dst *rtype, target unsafe.Pointer) Value {
	if v.flag&flagMethod != 0 {
		v = makeMethodValue(context, v)
	}

	switch {
	case directlyAssignable(dst, v.typ):
		// Overwrite type so that they match.
		// Same memory layout, so no harm done.
		fl := v.flag&(flagAddr|flagIndir) | v.flag.ro()
		fl |= flag(dst.Kind())
		return Value{dst, v.ptr, fl}

	case implements(dst, v.typ):
		if target == nil {
			target = unsafe_New(dst)
		}
		if v.Kind() == Interface && v.IsNil() {
			// A nil ReadWriter passed to nil Reader is OK,
			// but using ifaceE2I below will panic.
			// Avoid the panic by returning a nil dst (e.g., Reader) explicitly.
			return Value{dst, nil, flag(Interface)}
		}
		x := valueInterface(v, false)
		if dst.NumMethod() == 0 {
			*(*any)(target) = x
		} else {
			ifaceE2I(dst, x, target)
		}
		return Value{dst, target, flagIndir | flag(Interface)}
	}

	// Failed.
	panic(context + ": value of type " + v.typ.String() + " is not assignable to type " + dst.String())
}

// Convert returns the value v converted to type t.
// If the usual Go conversion rules do not allow conversion
// of the value v to type t, or if converting v to type t panics, Convert panics.
func (v Value) Convert(t Type) Value {
	if v.flag&flagMethod != 0 {
		v = makeMethodValue("Convert", v)
	}
	op := convertOp(t.common(), v.typ)
	if op == nil {
		panic("reflect.Value.Convert: value of type " + v.typ.String() + " cannot be converted to type " + t.String())
	}
	return op(v, t)
}

// CanConvert reports whether the value v can be converted to type t.
// If v.CanConvert(t) returns true then v.Convert(t) will not panic.
func (v Value) CanConvert(t Type) bool {
	vt := v.Type()
	if !vt.ConvertibleTo(t) {
		return false
	}
	// Currently the only conversion that is OK in terms of type
	// but that can panic depending on the value is converting
	// from slice to pointer-to-array.
	if vt.Kind() == Slice && t.Kind() == Pointer && t.Elem().Kind() == Array {
		n := t.Elem().Len()
		if n > v.Len() {
			return false
		}
	}
	return true
}

// convertOp returns the function to convert a value of type src
// to a value of type dst. If the conversion is illegal, convertOp returns nil.
func convertOp(dst, src *rtype) func(Value, Type) Value {
	switch src.Kind() {
	case Int, Int8, Int16, Int32, Int64:
		switch dst.Kind() {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
			return cvtInt
		case Float32, Float64:
			return cvtIntFloat
		case String:
			return cvtIntString
		}

	case Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
		switch dst.Kind() {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
			return cvtUint
		case Float32, Float64:
			return cvtUintFloat
		case String:
			return cvtUintString
		}

	case Float32, Float64:
		switch dst.Kind() {
		case Int, Int8, Int16, Int32, Int64:
			return cvtFloatInt
		case Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
			return cvtFloatUint
		case Float32, Float64:
			return cvtFloat
		}

	case Complex64, Complex128:
		switch dst.Kind() {
		case Complex64, Complex128:
			return cvtComplex
		}

	case String:
		if dst.Kind() == Slice && dst.Elem().PkgPath() == "" {
			switch dst.Elem().Kind() {
			case Uint8:
				return cvtStringBytes
			case Int32:
				return cvtStringRunes
			}
		}

	case Slice:
		if dst.Kind() == String && src.Elem().PkgPath() == "" {
			switch src.Elem().Kind() {
			case Uint8:
				return cvtBytesString
			case Int32:
				return cvtRunesString
			}
		}
		// "x is a slice, T is a pointer-to-array type,
		// and the slice and array types have identical element types."
		if dst.Kind() == Pointer && dst.Elem().Kind() == Array && src.Elem() == dst.Elem().Elem() {
			return cvtSliceArrayPtr
		}

	case Chan:
		if dst.Kind() == Chan && specialChannelAssignability(dst, src) {
			return cvtDirect
		}
	}

	// dst and src have same underlying type.
	if haveIdenticalUnderlyingType(dst, src, false) {
		return cvtDirect
	}

	// dst and src are non-defined pointer types with same underlying base type.
	if dst.Kind() == Pointer && dst.Name() == "" &&
		src.Kind() == Pointer && src.Name() == "" &&
		haveIdenticalUnderlyingType(dst.Elem().common(), src.Elem().common(), false) {
		return cvtDirect
	}

	if implements(dst, src) {
		if src.Kind() == Interface {
			return cvtI2I
		}
		return cvtT2I
	}

	return nil
}

// makeInt returns a Value of type t equal to bits (possibly truncated),
// where t is a signed or unsigned int type.
func makeInt(f flag, bits uint64, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	switch typ.size {
	case 1:
		*(*uint8)(ptr) = uint8(bits)
	case 2:
		*(*uint16)(ptr) = uint16(bits)
	case 4:
		*(*uint32)(ptr) = uint32(bits)
	case 8:
		*(*uint64)(ptr) = bits
	}
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

// makeFloat returns a Value of type t equal to v (possibly truncated to float32),
// where t is a float32 or float64 type.
func makeFloat(f flag, v float64, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	switch typ.size {
	case 4:
		*(*float32)(ptr) = float32(v)
	case 8:
		*(*float64)(ptr) = v
	}
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

// makeFloat returns a Value of type t equal to v, where t is a float32 type.
func makeFloat32(f flag, v float32, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	*(*float32)(ptr) = v
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

// makeComplex returns a Value of type t equal to v (possibly truncated to complex64),
// where t is a complex64 or complex128 type.
func makeComplex(f flag, v complex128, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	switch typ.size {
	case 8:
		*(*complex64)(ptr) = complex64(v)
	case 16:
		*(*complex128)(ptr) = v
	}
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

func makeString(f flag, v string, t Type) Value {
	ret := New(t).Elem()
	ret.SetString(v)
	ret.flag = ret.flag&^flagAddr | f
	return ret
}

func makeBytes(f flag, v []byte, t Type) Value {
	ret := New(t).Elem()
	ret.SetBytes(v)
	ret.flag = ret.flag&^flagAddr | f
	return ret
}

func makeRunes(f flag, v []rune, t Type) Value {
	ret := New(t).Elem()
	ret.setRunes(v)
	ret.flag = ret.flag&^flagAddr | f
	return ret
}

// These conversion functions are returned by convertOp
// for classes of conversions. For example, the first function, cvtInt,
// takes any value v of signed int type and returns the value converted
// to type t, where t is any signed or unsigned int type.

// convertOp: intXX -> [u]intXX
func cvtInt(v Value, t Type) Value {
	return makeInt(v.flag.ro(), uint64(v.Int()), t)
}

// convertOp: uintXX -> [u]intXX
func cvtUint(v Value, t Type) Value {
	return makeInt(v.flag.ro(), v.Uint(), t)
}

// convertOp: floatXX -> intXX
func cvtFloatInt(v Value, t Type) Value {
	return makeInt(v.flag.ro(), uint64(int64(v.Float())), t)
}

// convertOp: floatXX -> uintXX
func cvtFloatUint(v Value, t Type) Value {
	return makeInt(v.flag.ro(), uint64(v.Float()), t)
}

// convertOp: intXX -> floatXX
func cvtIntFloat(v Value, t Type) Value {
	return makeFloat(v.flag.ro(), float64(v.Int()), t)
}

// convertOp: uintXX -> floatXX
func cvtUintFloat(v Value, t Type) Value {
	return makeFloat(v.flag.ro(), float64(v.Uint()), t)
}

// convertOp: floatXX -> floatXX
func cvtFloat(v Value, t Type) Value {
	if v.Type().Kind() == Float32 && t.Kind() == Float32 {
		// Don't do any conversion if both types have underlying type float32.
		// This avoids converting to float64 and back, which will
		// convert a signaling NaN to a quiet NaN. See issue 36400.
		return makeFloat32(v.flag.ro(), *(*float32)(v.ptr), t)
	}
	return makeFloat(v.flag.ro(), v.Float(), t)
}

// convertOp: complexXX -> complexXX
func cvtComplex(v Value, t Type) Value {
	return makeComplex(v.flag.ro(), v.Complex(), t)
}

// convertOp: intXX -> string
func cvtIntString(v Value, t Type) Value {
	s := "\uFFFD"
	if x := v.Int(); int64(rune(x)) == x {
		s = string(rune(x))
	}
	return makeString(v.flag.ro(), s, t)
}

// convertOp: uintXX -> string
func cvtUintString(v Value, t Type) Value {
	s := "\uFFFD"
	if x := v.Uint(); uint64(rune(x)) == x {
		s = string(rune(x))
	}
	return makeString(v.flag.ro(), s, t)
}

// convertOp: []byte -> string
func cvtBytesString(v Value, t Type) Value {
	return makeString(v.flag.ro(), string(v.Bytes()), t)
}

// convertOp: string -> []byte
func cvtStringBytes(v Value, t Type) Value {
	return makeBytes(v.flag.ro(), []byte(v.String()), t)
}

// convertOp: []rune -> string
func cvtRunesString(v Value, t Type) Value {
	return makeString(v.flag.ro(), string(v.runes()), t)
}

// convertOp: string -> []rune
func cvtStringRunes(v Value, t Type) Value {
	return makeRunes(v.flag.ro(), []rune(v.String()), t)
}

// convertOp: []T -> *[N]T
func cvtSliceArrayPtr(v Value, t Type) Value {
	n := t.Elem().Len()
	if n > v.Len() {
		panic("reflect: cannot convert slice with length " + itoa.Itoa(v.Len()) + " to pointer to array with length " + itoa.Itoa(n))
	}
	h := (*unsafeheader.Slice)(v.ptr)
	return Value{t.common(), h.Data, v.flag&^(flagIndir|flagAddr|flagKindMask) | flag(Pointer)}
}

// convertOp: direct copy
func cvtDirect(v Value, typ Type) Value {
	f := v.flag
	t := typ.common()
	ptr := v.ptr
	if f&flagAddr != 0 {
		// indirect, mutable word - make a copy
		c := unsafe_New(t)
		typedmemmove(t, c, ptr)
		ptr = c
		f &^= flagAddr
	}
	return Value{t, ptr, v.flag.ro() | f} // v.flag.ro()|f == f?
}

// convertOp: concrete -> interface
func cvtT2I(v Value, typ Type) Value {
	target := unsafe_New(typ.common())
	x := valueInterface(v, false)
	if typ.NumMethod() == 0 {
		*(*any)(target) = x
	} else {
		ifaceE2I(typ.(*rtype), x, target)
	}
	return Value{typ.common(), target, v.flag.ro() | flagIndir | flag(Interface)}
}

// convertOp: interface -> interface
func cvtI2I(v Value, typ Type) Value {
	if v.IsNil() {
		ret := Zero(typ)
		ret.flag |= v.flag.ro()
		return ret
	}
	return cvtT2I(v.Elem(), typ)
}

// implemented in ../runtime
func chancap(ch unsafe.Pointer) int
func chanclose(ch unsafe.Pointer)
func chanlen(ch unsafe.Pointer) int

// Note: some of the noescape annotations below are technically a lie,
// but safe in the context of this package. Functions like chansend
// and mapassign don't escape the referent, but may escape anything
// the referent points to (they do shallow copies of the referent).
// It is safe in this package because the referent may only point
// to something a Value may point to, and that is always in the heap
// (due to the escapes() call in ValueOf).

//go:noescape
func chanrecv(ch unsafe.Pointer, nb bool, val unsafe.Pointer) (selected, received bool)

//go:noescape
func chansend(ch unsafe.Pointer, val unsafe.Pointer, nb bool) bool

func makechan(typ *rtype, size int) (ch unsafe.Pointer)
func makemap(t *rtype, cap int) (m unsafe.Pointer)

//go:noescape
func mapaccess(t *rtype, m unsafe.Pointer, key unsafe.Pointer) (val unsafe.Pointer)

//go:noescape
func mapaccess_faststr(t *rtype, m unsafe.Pointer, key string) (val unsafe.Pointer)

//go:noescape
func mapassign(t *rtype, m unsafe.Pointer, key, val unsafe.Pointer)

//go:noescape
func mapassign_faststr(t *rtype, m unsafe.Pointer, key string, val unsafe.Pointer)

//go:noescape
func mapdelete(t *rtype, m unsafe.Pointer, key unsafe.Pointer)

//go:noescape
func mapdelete_faststr(t *rtype, m unsafe.Pointer, key string)

//go:noescape
func mapiterinit(t *rtype, m unsafe.Pointer, it *hiter)

//go:noescape
func mapiterkey(it *hiter) (key unsafe.Pointer)

//go:noescape
func mapiterelem(it *hiter) (elem unsafe.Pointer)

//go:noescape
func mapiternext(it *hiter)

//go:noescape
func maplen(m unsafe.Pointer) int

//go:linkname call runtime.reflectcall
func call(typ *funcType, fnaddr unsafe.Pointer, isInterface bool, isMethod bool, params *unsafe.Pointer, results *unsafe.Pointer)

func ifaceE2I(t *rtype, src any, dst unsafe.Pointer)

// memmove copies size bytes to dst from src. No write barriers are used.
//go:noescape
func memmove(dst, src unsafe.Pointer, size uintptr)

// typedmemmove copies a value of type t to dst from src.
//go:noescape
func typedmemmove(t *rtype, dst, src unsafe.Pointer)

// typedmemclr zeros the value at ptr of type t.
//go:noescape
func typedmemclr(t *rtype, ptr unsafe.Pointer)

// typedslicecopy copies a slice of elemType values from src to dst,
// returning the number of elements copied.
//go:noescape
func typedslicecopy(elemType *rtype, dst, src unsafeheader.Slice) int

//go:noescape
func typehash(t *rtype, p unsafe.Pointer, h uintptr) uintptr

func verifyNotInHeapPtr(p uintptr) bool

// Dummy annotation marking that the value x escapes,
// for use in cases where the reflect code is so clever that
// the compiler cannot follow.
func escapes(x any) {
	if dummy.b {
		dummy.x = x
	}
}

var dummy struct {
	b bool
	x any
}

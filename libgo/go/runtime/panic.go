// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

// Calling panic with one of the errors below will call errorString.Error
// which will call mallocgc to concatenate strings. That will fail if
// malloc is locked, causing a confusing error message. Throw a better
// error message instead.
func panicCheckMalloc(err error) {
	gp := getg()
	if gp != nil && gp.m != nil && gp.m.mallocing != 0 {
		throw(string(err.(errorString)))
	}
}

var indexError = error(errorString("index out of range"))

func panicindex() {
	panicCheckMalloc(indexError)
	panic(indexError)
}

var sliceError = error(errorString("slice bounds out of range"))

func panicslice() {
	panicCheckMalloc(sliceError)
	panic(sliceError)
}

var divideError = error(errorString("integer divide by zero"))

func panicdivide() {
	panicCheckMalloc(divideError)
	panic(divideError)
}

var overflowError = error(errorString("integer overflow"))

func panicoverflow() {
	panicCheckMalloc(overflowError)
	panic(overflowError)
}

var floatError = error(errorString("floating point error"))

func panicfloat() {
	panicCheckMalloc(floatError)
	panic(floatError)
}

var memoryError = error(errorString("invalid memory address or nil pointer dereference"))

func panicmem() {
	panicCheckMalloc(memoryError)
	panic(memoryError)
}

func throwreturn() {
	throw("no return at end of a typed function - compiler is broken")
}

func throwinit() {
	throw("recursive call during initialization - linker skew")
}

//go:nosplit
func canpanic(gp *g) bool {
	// Note that g is m->gsignal, different from gp.
	// Note also that g->m can change at preemption, so m can go stale
	// if this function ever makes a function call.
	_g_ := getg()
	_m_ := _g_.m

	// Is it okay for gp to panic instead of crashing the program?
	// Yes, as long as it is running Go code, not runtime code,
	// and not stuck in a system call.
	if gp == nil || gp != _m_.curg {
		return false
	}
	if _m_.locks-_m_.softfloat != 0 || _m_.mallocing != 0 || _m_.throwing != 0 || _m_.preemptoff != "" || _m_.dying != 0 {
		return false
	}
	status := readgstatus(gp)
	if status&^_Gscan != _Grunning || gp.syscallsp != 0 {
		return false
	}
	return true
}

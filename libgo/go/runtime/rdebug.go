// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import _ "unsafe" // for go:linkname

// Define maxstacksize here for gccgo. For gc it is defined in
// stack.go, but gccgo doesn't use that file. Or, for that matter,
// maxstacksize.
var maxstacksize uintptr = 1 << 20 // enough until runtime.main sets it for real

//go:linkname setMaxStack runtime_1debug.setMaxStack
func setMaxStack(in int) (out int) {
	out = int(maxstacksize)
	maxstacksize = uintptr(in)
	return out
}

//go:linkname setPanicOnFault runtime_1debug.setPanicOnFault
func setPanicOnFault(new bool) (old bool) {
	_g_ := getg()
	old = _g_.paniconfault
	_g_.paniconfault = new
	return old
}

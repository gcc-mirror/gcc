// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// GNU/Hurd library calls 386 specific derived from libcall_linux_386.go.
// Remove Iopl, iopl.

package syscall

//sys	Ioperm(from int, num int, on int) (err error)
//ioperm(from _C_long, num _C_long, on _C_int) _C_int

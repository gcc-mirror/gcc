// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Build on all systems other than solaris/386.
// 32-bit Solaris 2/x86 needs _nuname, handled in libcall_solaris_386.go.
//go:build !386 || !solaris
// +build !386 !solaris

package syscall

//sysnb	Uname(buf *Utsname) (err error)
//uname(buf *Utsname) _C_int

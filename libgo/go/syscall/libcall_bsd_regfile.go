// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin dragonfly freebsd openbsd solaris,amd64 solaris,sparc64

package syscall

//sys	sendfile(outfd int, infd int, offset *Offset_t, count int) (written int, err error)
//sendfile(outfd _C_int, infd _C_int, offset *Offset_t, count Size_t) Ssize_t

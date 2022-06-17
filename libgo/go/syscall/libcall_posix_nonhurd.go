// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build !hurd
// +build !hurd

package syscall

// Removed the mount call for GNU/Hurd, it exists but use translators.
// Functionality is not the same as descibed in Linux <sys/mount.h>.
// Removed the madvise call for GNU/Hurd, not yet implemented.

//sys	Mount(source string, target string, fstype string, flags uintptr, data string) (err error)
//mount(source *byte, target *byte, fstype *byte, flags _C_long, data *byte) _C_int

//sys Madvise(b []byte, advice int) (err error)
//madvise(addr *byte, len Size_t, advice _C_int) _C_int

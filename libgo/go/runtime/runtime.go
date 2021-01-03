// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/atomic"
	_ "unsafe" // for go:linkname
)

//go:generate go run wincallback.go
//go:generate go run mkduff.go
//go:generate go run mkfastlog2table.go

// For gccgo, while we still have C runtime code, use go:linkname to
// export some functions.
//
//go:linkname tickspersecond

var ticksLock mutex
var ticksVal uint64

// Note: Called by runtime/pprof in addition to runtime code.
func tickspersecond() int64 {
	r := int64(atomic.Load64(&ticksVal))
	if r != 0 {
		return r
	}
	lock(&ticksLock)
	r = int64(ticksVal)
	if r == 0 {
		t0 := nanotime()
		c0 := cputicks()
		usleep(100 * 1000)
		t1 := nanotime()
		c1 := cputicks()
		if t1 == t0 {
			t1++
		}
		r = (c1 - c0) * 1000 * 1000 * 1000 / (t1 - t0)
		if r == 0 {
			r++
		}
		atomic.Store64(&ticksVal, uint64(r))
	}
	unlock(&ticksLock)
	return r
}

var envs []string
var argslice []string

//go:linkname syscall_runtime_envs syscall.runtime__envs
func syscall_runtime_envs() []string { return append([]string{}, envs...) }

//go:linkname syscall_Getpagesize syscall.Getpagesize
func syscall_Getpagesize() int { return int(physPageSize) }

//go:linkname os_runtime_args os.runtime__args
func os_runtime_args() []string { return append([]string{}, argslice...) }

//go:linkname syscall_Exit syscall.Exit
//go:nosplit
func syscall_Exit(code int) {
	exit(int32(code))
}

// Temporary, for the gccgo runtime code written in C.
//go:linkname get_envs runtime_get_envs
func get_envs() []string { return envs }

//go:linkname get_args runtime_get_args
func get_args() []string { return argslice }

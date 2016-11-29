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
// rename some functions to themselves, so that the compiler will
// export them.
//
//go:linkname tickspersecond runtime.tickspersecond

var ticks struct {
	lock mutex
	pad  uint32 // ensure 8-byte alignment of val on 386
	val  uint64
}

// Note: Called by runtime/pprof in addition to runtime code.
func tickspersecond() int64 {
	r := int64(atomic.Load64(&ticks.val))
	if r != 0 {
		return r
	}
	lock(&ticks.lock)
	r = int64(ticks.val)
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
		atomic.Store64(&ticks.val, uint64(r))
	}
	unlock(&ticks.lock)
	return r
}

var envs []string
var argslice []string

//go:linkname syscall_runtime_envs syscall.runtime_envs
func syscall_runtime_envs() []string { return append([]string{}, envs...) }

//go:linkname os_runtime_args os.runtime_args
func os_runtime_args() []string { return append([]string{}, argslice...) }

// Temporary, for the gccgo runtime code written in C.
//go:linkname get_envs runtime_get_envs
func get_envs() []string { return envs }

//go:linkname get_args runtime_get_args
func get_args() []string { return argslice }

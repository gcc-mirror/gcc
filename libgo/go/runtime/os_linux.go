// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

type mOS struct {
	unused byte
}

func futex(addr unsafe.Pointer, op int32, val uint32, ts, addr2 unsafe.Pointer, val3 uint32) int32 {
	return int32(syscall(_SYS_futex, uintptr(addr), uintptr(op), uintptr(val), uintptr(ts), uintptr(addr2), uintptr(val3)))
}

// Linux futex.
//
//	futexsleep(uint32 *addr, uint32 val)
//	futexwakeup(uint32 *addr)
//
// Futexsleep atomically checks if *addr == val and if so, sleeps on addr.
// Futexwakeup wakes up threads sleeping on addr.
// Futexsleep is allowed to wake up spuriously.

const (
	_FUTEX_WAIT = 0
	_FUTEX_WAKE = 1
)

// Atomically,
//	if(*addr == val) sleep
// Might be woken up spuriously; that's allowed.
// Don't sleep longer than ns; ns < 0 means forever.
//go:nosplit
func futexsleep(addr *uint32, val uint32, ns int64) {
	var ts timespec

	// Some Linux kernels have a bug where futex of
	// FUTEX_WAIT returns an internal error code
	// as an errno. Libpthread ignores the return value
	// here, and so can we: as it says a few lines up,
	// spurious wakeups are allowed.
	if ns < 0 {
		futex(unsafe.Pointer(addr), _FUTEX_WAIT, val, nil, nil, 0)
		return
	}

	// It's difficult to live within the no-split stack limits here.
	// On ARM and 386, a 64-bit divide invokes a general software routine
	// that needs more stack than we can afford. So we use timediv instead.
	// But on real 64-bit systems, where words are larger but the stack limit
	// is not, even timediv is too heavy, and we really need to use just an
	// ordinary machine instruction.
	if sys.PtrSize == 8 {
		ts.set_sec(ns / 1000000000)
		ts.set_nsec(int32(ns % 1000000000))
	} else {
		ts.tv_nsec = 0
		ts.set_sec(int64(timediv(ns, 1000000000, (*int32)(unsafe.Pointer(&ts.tv_nsec)))))
	}
	futex(unsafe.Pointer(addr), _FUTEX_WAIT, val, unsafe.Pointer(&ts), nil, 0)
}

// If any procs are sleeping on addr, wake up at most cnt.
//go:nosplit
func futexwakeup(addr *uint32, cnt uint32) {
	ret := futex(unsafe.Pointer(addr), _FUTEX_WAKE, cnt, nil, nil, 0)
	if ret >= 0 {
		return
	}

	// I don't know that futex wakeup can return
	// EAGAIN or EINTR, but if it does, it would be
	// safe to loop and call futex again.
	systemstack(func() {
		print("futexwakeup addr=", addr, " returned ", ret, "\n")
	})

	*(*int32)(unsafe.Pointer(uintptr(0x1006))) = 0x1006
}

const (
	_AT_NULL   = 0  // End of vector
	_AT_PAGESZ = 6  // System physical page size
	_AT_RANDOM = 25 // introduced in 2.6.29
)

func sysargs(argc int32, argv **byte) {
	n := argc + 1

	// skip over argv, envp to get to auxv
	for argv_index(argv, n) != nil {
		n++
	}

	// skip NULL separator
	n++

	// now argv+n is auxv
	auxv := (*[1 << 28]uintptr)(add(unsafe.Pointer(argv), uintptr(n)*sys.PtrSize))
	for i := 0; auxv[i] != _AT_NULL; i += 2 {
		tag, val := auxv[i], auxv[i+1]
		switch tag {
		case _AT_RANDOM:
			// The kernel provides a pointer to 16-bytes
			// worth of random data.
			startupRandomData = (*[16]byte)(unsafe.Pointer(val))[:]

		case _AT_PAGESZ:
			// Check that the true physical page size is
			// compatible with the runtime's assumed
			// physical page size.
			if sys.PhysPageSize < val {
				print("runtime: kernel page size (", val, ") is larger than runtime page size (", sys.PhysPageSize, ")\n")
				exit(1)
			}
			if sys.PhysPageSize%val != 0 {
				print("runtime: runtime page size (", sys.PhysPageSize, ") is not a multiple of kernel page size (", val, ")\n")
				exit(1)
			}
		}

		// Commented out for gccgo for now.
		// archauxv(tag, val)
	}
}

// syscall.go -- Basic syscall interface.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This package contains an interface to the low-level operating system
// primitives.  The details vary depending on the underlying system.
// Its primary use is inside other packages that provide a more portable
// interface to the system, such as "os", "time" and "net".  Use those
// packages rather than this one if you can.
// For details of the functions and data types in this package consult
// the manuals for the appropriate operating system.
package syscall

import (
	"sync"
	"unsafe"
)

func libc_syscall32(trap int32, a1, a2, a3, a4, a5, a6 int32) int32 __asm__ ("syscall");
func libc_syscall64(trap int64, a1, a2, a3, a4, a5, a6 int64) int64 __asm__ ("syscall");

// Do a system call.  We look at the size of uintptr to see how to pass
// the arguments, so that we don't pass a 64-bit value when the function
// expects a 32-bit one.
func Syscall(trap, a1, a2, a3 uintptr) (r1, r2, err uintptr) {
  var r uintptr;
  if unsafe.Sizeof(r) == 4 {
    r1 := libc_syscall32(int32(trap), int32(a1), int32(a2), int32(a3), 0, 0, 0);
    r = uintptr(r1);
  } else {
    r1 := libc_syscall64(int64(trap), int64(a1), int64(a2), int64(a3), 0, 0, 0);
    r = uintptr(r1);
  }
  return r, 0, uintptr(GetErrno());
}

func Syscall6(trap, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2, err uintptr) {
  var r uintptr;
  if unsafe.Sizeof(r) == 4 {
    r1 := libc_syscall32(int32(trap), int32(a1), int32(a2), int32(a3),
    			 int32(a4), int32(a5), int32(a6));
    r = uintptr(r1);
  } else {
    r1 := libc_syscall64(int64(trap), int64(a1), int64(a2), int64(a3),
			 int64(a4), int64(a5), int64(a6));
    r = uintptr(r1);
  }
  return r, 0, uintptr(GetErrno());
}

// Mmap manager, for use by operating system-specific implementations.

type mmapper struct {
	sync.Mutex
	active map[*byte][]byte // active mappings; key is last byte in mapping
	mmap   func(addr, length uintptr, prot, flags, fd int, offset int64) (uintptr, int)
	munmap func(addr uintptr, length uintptr) int
}

func (m *mmapper) Mmap(fd int, offset int64, length int, prot int, flags int) (data []byte, errno int) {
	if length <= 0 {
		return nil, EINVAL
	}

	// Map the requested memory.
	addr, errno := m.mmap(0, uintptr(length), prot, flags, fd, offset)
	if errno != 0 {
		return nil, errno
	}

	// Slice memory layout
	var sl = struct {
		addr uintptr
		len  int
		cap  int
	}{addr, length, length}

	// Use unsafe to turn sl into a []byte.
	b := *(*[]byte)(unsafe.Pointer(&sl))

	// Register mapping in m and return it.
	p := &b[cap(b)-1]
	m.Lock()
	defer m.Unlock()
	m.active[p] = b
	return b, 0
}

func (m *mmapper) Munmap(data []byte) (errno int) {
	if len(data) == 0 || len(data) != cap(data) {
		return EINVAL
	}

	// Find the base of the mapping.
	p := &data[cap(data)-1]
	m.Lock()
	defer m.Unlock()
	b := m.active[p]
	if b == nil || &b[0] != &data[0] {
		return EINVAL
	}

	// Unmap the memory and update m.
	if errno := m.munmap(uintptr(unsafe.Pointer(&b[0])), uintptr(len(b))); errno != 0 {
		return errno
	}
	m.active[p] = nil, false
	return 0
}

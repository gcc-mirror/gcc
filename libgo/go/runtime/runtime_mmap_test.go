// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime_test

import (
	"runtime"
	"testing"
	"unsafe"
)

// Test that the error value returned by mmap is positive, as that is
// what the code in mem_bsd.go, mem_darwin.go, and mem_linux.go expects.
// See the uses of ENOMEM in sysMap in those files.
func TestMmapErrorSign(t *testing.T) {
	p := runtime.Mmap(nil, ^uintptr(0)&^(runtime.GetPhysPageSize()-1), 0, runtime.MAP_ANON|runtime.MAP_PRIVATE, -1, 0)

	// The runtime.mmap function is nosplit, but t.Errorf is not.
	// Reset the pointer so that we don't get an "invalid stack
	// pointer" error from t.Errorf if we call it.
	v := uintptr(p)
	p = nil

	err := runtime.Errno()
	if v != ^uintptr(0) || err != runtime.ENOMEM {
		t.Errorf("mmap = %v, %v, want %v", v, err, runtime.ENOMEM)
	}
}

func TestPhysPageSize(t *testing.T) {
	// Mmap fails if the address is not page aligned, so we can
	// use this to test if the page size is the true page size.
	ps := runtime.GetPhysPageSize()

	// Get a region of memory to play with. This should be page-aligned.
	b := uintptr(runtime.Mmap(nil, 2*ps, 0, runtime.MAP_ANON|runtime.MAP_PRIVATE, -1, 0))
	if b == ^uintptr(0) {
		t.Fatalf("Mmap: %v %v", b, runtime.Errno())
	}

	// Mmap should fail at a half page into the buffer.
	err := uintptr(runtime.Mmap(unsafe.Pointer(uintptr(b)+ps/2), ps, 0, runtime.MAP_ANON|runtime.MAP_PRIVATE|runtime.MAP_FIXED, -1, 0))
	if err != ^uintptr(0) {
		t.Errorf("Mmap should have failed with half-page alignment %d, but succeeded: %v", ps/2, err)
	}

	// Mmap should succeed at a full page into the buffer.
	err = uintptr(runtime.Mmap(unsafe.Pointer(uintptr(b)+ps), ps, 0, runtime.MAP_ANON|runtime.MAP_PRIVATE|runtime.MAP_FIXED, -1, 0))
	if err == ^uintptr(0) {
		t.Errorf("Mmap at full-page alignment %d failed: %v %v", ps, err, runtime.Errno())
	}
}

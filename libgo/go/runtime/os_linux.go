// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

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

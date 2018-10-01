// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

var executablePath string

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
	sysauxv(auxv[:])
}

const (
	_AT_NULL         = 0    // Terminates the vector
	_AT_PAGESZ       = 6    // Page size in bytes
	_AT_SUN_EXECNAME = 2014 // exec() path name
)

func sysauxv(auxv []uintptr) {
	for i := 0; auxv[i] != _AT_NULL; i += 2 {
		tag, val := auxv[i], auxv[i+1]
		switch tag {
		case _AT_PAGESZ:
			physPageSize = val
		case _AT_SUN_EXECNAME:
			executablePath = gostringnocopy((*byte)(unsafe.Pointer(val)))
		}
	}
}

//go:linkname solarisExecutablePath os.solarisExecutablePath

// solarisExecutablePath is called from the os package to fetch the
// saved executable path.
func solarisExecutablePath() string {
	return executablePath
}

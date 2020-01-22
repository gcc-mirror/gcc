// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import _ "unsafe"

var executablePath string

//extern getexecname
func getexecname() *byte

//extern getpagesize
func getPageSize() int32

//extern sysconf
func sysconf(int32) _C_long

func osinit() {
	ncpu = getncpu()
	if physPageSize == 0 {
		physPageSize = uintptr(getPageSize())
	}
}

func sysargs(argc int32, argv **byte) {
	executablePath = gostringnocopy(getexecname())
}

//go:linkname solarisExecutablePath os.solarisExecutablePath

// solarisExecutablePath is called from the os package to fetch the
// saved executable path.
func solarisExecutablePath() string {
	return executablePath
}

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import _ "unsafe"

var executablePath string

//extern getexecname
func getexecname() *byte

//extern getpagesize
func getpagesize() int32

func sysargs(argc int32, argv **byte) {
	physPageSize = uintptr(getpagesize())
	executablePath = gostringnocopy(getexecname())
}

//go:linkname solarisExecutablePath os.solarisExecutablePath

// solarisExecutablePath is called from the os package to fetch the
// saved executable path.
func solarisExecutablePath() string {
	return executablePath
}

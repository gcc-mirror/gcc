// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build linux
// +build mips mipsle

package runtime

var randomNumber uint32

func archauxv(tag, val uintptr) {
	switch tag {
	case _AT_RANDOM:
		// sysargs filled in startupRandomData, but that
		// pointer may not be word aligned, so we must treat
		// it as a byte array.
		randomNumber = uint32(startupRandomData[4]) | uint32(startupRandomData[5])<<8 |
			uint32(startupRandomData[6])<<16 | uint32(startupRandomData[7])<<24
	}
}

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

const (
	_AT_PLATFORM = 15 //  introduced in at least 2.6.11

	_HWCAP_VFP   = 1 << 6  // introduced in at least 2.6.11
	_HWCAP_VFPv3 = 1 << 13 // introduced in 2.6.30
	_HWCAP_IDIVA = 1 << 17
)

var randomNumber uint32
var armArch uint8 = 6 // we default to ARMv6
var hwcap uint32      // set by archauxv
var hardDiv bool      // set if a hardware divider is available

func archauxv(tag, val uintptr) {
	switch tag {
	case _AT_RANDOM:
		// sysargs filled in startupRandomData, but that
		// pointer may not be word aligned, so we must treat
		// it as a byte array.
		randomNumber = uint32(startupRandomData[4]) | uint32(startupRandomData[5])<<8 |
			uint32(startupRandomData[6])<<16 | uint32(startupRandomData[7])<<24

	case _AT_PLATFORM: // v5l, v6l, v7l
		t := *(*uint8)(unsafe.Pointer(val + 1))
		if '5' <= t && t <= '7' {
			armArch = t - '0'
		}

	case _AT_HWCAP: // CPU capability bit flags
		hwcap = uint32(val)
		hardDiv = (hwcap & _HWCAP_IDIVA) != 0
	}
}

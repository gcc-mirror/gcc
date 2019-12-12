// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "internal/cpu"

const (
	// bit masks taken from bits/hwcap.h
	_HWCAP_S390_ZARCH  = 2
	_HWCAP_S390_STFLE  = 4
	_HWCAP_S390_MSA    = 8
	_HWCAP_S390_LDISP  = 16
	_HWCAP_S390_EIMM   = 32
	_HWCAP_S390_DFP    = 64
	_HWCAP_S390_ETF3EH = 256
	_HWCAP_S390_VX     = 2048 // vector facility
	_HWCAP_S390_VXE    = 8192
)

func archauxv(tag, val uintptr) {
	switch tag {
	case _AT_HWCAP: // CPU capability bit flags
		cpu.S390X.HasZARCH = val&_HWCAP_S390_ZARCH != 0
		cpu.S390X.HasSTFLE = val&_HWCAP_S390_STFLE != 0
		cpu.S390X.HasLDISP = val&_HWCAP_S390_LDISP != 0
		cpu.S390X.HasEIMM = val&_HWCAP_S390_EIMM != 0
		cpu.S390X.HasDFP = val&_HWCAP_S390_DFP != 0
		cpu.S390X.HasETF3EH = val&_HWCAP_S390_ETF3EH != 0
		cpu.S390X.HasMSA = val&_HWCAP_S390_MSA != 0
		cpu.S390X.HasVX = val&_HWCAP_S390_VX != 0
		cpu.S390X.HasVXE = val&_HWCAP_S390_VXE != 0
	}
}

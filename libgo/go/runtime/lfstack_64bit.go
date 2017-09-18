// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build amd64 arm64 mips64 mips64le ppc64 ppc64le s390x arm64be alpha sparc64 ia64

package runtime

import "unsafe"

const (
	// addrBits is the number of bits needed to represent a virtual address.
	//
	// In Linux the user address space for each architecture is limited as
	// follows (taken from the processor.h file for the architecture):
	//
	// Architecture  Name              Maximum Value (exclusive)
	// ---------------------------------------------------------------------
	// arm64         TASK_SIZE_64      Depends on configuration.
	// ppc64{,le}    TASK_SIZE_USER64  0x400000000000UL (46 bit addresses)
	// mips64{,le}   TASK_SIZE64       0x010000000000UL (40 bit addresses)
	// s390x         TASK_SIZE         0x020000000000UL (41 bit addresses)
	//
	// These values may increase over time.
	//
	// On AMD64, virtual addresses are 48-bit numbers sign extended to 64.
	// We shift the address left 16 to eliminate the sign extended part and make
	// room in the bottom for the count.
	addrBits = 48

	// In addition to the 16 bits taken from the top, we can take 3 from the
	// bottom, because node must be pointer-aligned, giving a total of 19 bits
	// of count.
	cntBits = 64 - addrBits + 3

	// On sparc64-linux, user addresses are 52-bit numbers sign extended to 64.
	// We shift the address left 12 to eliminate the sign extended part and make
	// room in the bottom for the count.
	sparcLinuxAddrBits = 52
	sparcLinuxCntBits  = 64 - sparcLinuxAddrBits + 3

	// On IA64, the virtual address space is devided into 8 regions, with
	// 52 address bits each (with 64k page size).
	ia64AddrBits = 55
	ia64CntBits  = 64 - ia64AddrBits + 3

	// On AIX, 64-bit addresses are split into 36-bit segment number and 28-bit
	// offset in segment.  Segment numbers in the range 0x070000000-0x07FFFFFFF
	// and 0x0A0000000-0x0AFFFFFFF(LSA) are available for mmap.
	// We assume all lfnode addresses are from memory allocated with mmap.
	// We use one bit to distinguish between the two ranges.
	aixAddrBits = 57
	aixCntBits  = 64 - aixAddrBits + 3
)

func lfstackPack(node *lfnode, cnt uintptr) uint64 {
	if GOARCH == "sparc64" && GOOS == "linux" {
		return uint64(uintptr(unsafe.Pointer(node)))<<(64-sparcLinuxAddrBits) | uint64(cnt&(1<<sparcLinuxCntBits-1))
	}
	if GOARCH == "ia64" {
		// Top three bits are the region number
		val := uint64(uintptr(unsafe.Pointer(node)))
		return (val<<(64-ia64AddrBits))&(1<<(64-3)-1) | val&^(1<<(64-3)-1) | uint64(cnt&(1<<ia64CntBits-1))
	}
	if GOARCH == "ppc64" && GOOS == "aix" {
		return uint64(uintptr(unsafe.Pointer(node)))<<(64-aixAddrBits) | uint64(cnt&(1<<aixCntBits-1))
	}
	return uint64(uintptr(unsafe.Pointer(node)))<<(64-addrBits) | uint64(cnt&(1<<cntBits-1))
}

func lfstackUnpack(val uint64) *lfnode {
	if GOARCH == "amd64" || GOOS == "solaris" {
		// amd64 or Solaris systems can place the stack above the VA hole, so we need to sign extend
		// val before unpacking.
		return (*lfnode)(unsafe.Pointer(uintptr(int64(val) >> cntBits << 3)))
	}
	if GOARCH == "sparc64" && GOOS == "linux" {
		return (*lfnode)(unsafe.Pointer(uintptr(int64(val) >> sparcLinuxCntBits << 3)))
	}
	if GOARCH == "ia64" {
		return (*lfnode)(unsafe.Pointer(uintptr((val>>ia64CntBits<<3)&(1<<(64-3)-1) | val&^(1<<(64-3)-1))))
	}
	if GOARCH == "ppc64" && GOOS == "aix" {
		if val&(1<<63) != 0 {
			return (*lfnode)(unsafe.Pointer(uintptr((val >> aixCntBits << 3) | 0x7<<56)))
		} else {
			return (*lfnode)(unsafe.Pointer(uintptr((val >> aixCntBits << 3) | 0xa<<56)))
		}
	}
	return (*lfnode)(unsafe.Pointer(uintptr(val >> cntBits << 3)))
}

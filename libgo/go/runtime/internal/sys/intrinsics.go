// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package sys

//extern __builtin_ctz
func builtinCtz32(uint32) int32

//extern __builtin_ctzll
func builtinCtz64(uint64) int32

//go:nosplit

// Ctz64 counts trailing (low-order) zeroes,
// and if all are zero, then 64.
func Ctz64(x uint64) uint64 {
	if x == 0 {
		return 64
	}
	return uint64(builtinCtz64(x))
}

//go:nosplit

// Ctz32 counts trailing (low-order) zeroes,
// and if all are zero, then 32.
func Ctz32(x uint32) uint32 {
	if x == 0 {
		return 32
	}
	return uint32(builtinCtz32(x))
}

//extern __builtin_bswap64
func bswap64(uint64) uint64

//go:nosplit

// Bswap64 returns its input with byte order reversed
// 0x0102030405060708 -> 0x0807060504030201
func Bswap64(x uint64) uint64 {
	return bswap64(x)
}

//extern __builtin_bswap32
func bswap32(uint32) uint32

//go:nosplit

// Bswap32 returns its input with byte order reversed
// 0x01020304 -> 0x04030201
func Bswap32(x uint32) uint32 {
	return bswap32(x)
}

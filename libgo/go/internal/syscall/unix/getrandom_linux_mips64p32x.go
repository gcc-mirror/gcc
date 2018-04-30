// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build mips64p32 mips64p32le

package unix

// Linux getrandom system call number.
// See GetRandom in getrandom_linux.go.
const randomTrap uintptr = 6317

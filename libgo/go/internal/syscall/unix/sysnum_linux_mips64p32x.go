// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build mips64p32 mips64p32le

package unix

const (
	getrandomTrap     uintptr = 6317
	copyFileRangeTrap uintptr = 6324
)

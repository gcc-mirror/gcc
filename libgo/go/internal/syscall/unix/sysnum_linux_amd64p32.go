// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package unix

const (
	getrandomTrap     uintptr = 0x40000000 + 318
	copyFileRangeTrap uintptr = 0x40000000 + 326
)

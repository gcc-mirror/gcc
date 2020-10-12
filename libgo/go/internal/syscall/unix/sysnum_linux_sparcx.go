// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build sparc sparc64

package unix

const (
	getrandomTrap     uintptr = 347
	copyFileRangeTrap uintptr = 357
)

// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build sh shbe

package unix

const (
	getrandomTrap     uintptr = 373
	copyFileRangeTrap uintptr = 380
)

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix

package os

import "syscall"

//extern closedir64
func libc_closedir(*syscall.DIR) int

//extern fdopendir64
func libc_fdopendir(int32) *syscall.DIR

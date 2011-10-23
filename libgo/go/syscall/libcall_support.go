// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Declarations for functions which are actually written in C.

package syscall

func entersyscall()
func exitsyscall()
func GetErrno() int
func SetErrno(int)

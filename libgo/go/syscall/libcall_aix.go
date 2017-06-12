// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix

package syscall

func raw_ptrace(request int, pid int, addr *byte, data *byte) Errno {
	return ENOSYS
}

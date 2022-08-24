// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "internal/oserror"

// An Errno is an unsigned number describing an error condition.
// It implements the error interface. The zero Errno is by convention
// a non-error, so code to convert from Errno to error should use:
//
//	err = nil
//	if errno != 0 {
//		err = errno
//	}
//
// Errno values can be tested against error values from the os package
// using errors.Is. For example:
//
//	_, _, err := syscall.Syscall(...)
//	if errors.Is(err, fs.ErrNotExist) ...
type Errno uintptr

func (e Errno) Error() string {
	return Errstr(int(e))
}

func (e Errno) Is(target error) bool {
	switch target {
	case oserror.ErrPermission:
		return e == EACCES || e == EPERM
	case oserror.ErrExist:
		return e == EEXIST || e == ENOTEMPTY
	case oserror.ErrNotExist:
		return e == ENOENT
	}
	return false
}

func (e Errno) Temporary() bool {
	return e == EINTR || e == EMFILE || e == ENFILE || e.Timeout()
}

func (e Errno) Timeout() bool {
	return e == EAGAIN || e == EWOULDBLOCK || e == ETIMEDOUT
}

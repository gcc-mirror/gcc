// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build (aix || darwin || dragonfly || freebsd || hurd || (linux && !android) || netbsd || openbsd || solaris) && cgo
// +build aix darwin dragonfly freebsd hurd linux,!android netbsd openbsd solaris
// +build cgo

// Package pty is a simple pseudo-terminal package for Unix systems,
// implemented by calling C functions via cgo.
// This is only used for testing the os/signal package.
package pty

import (
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

//extern posix_openpt
func posix_openpt(int32) int32

//extern grantpt
func grantpt(int32) int32

//extern unlockpt
func unlockpt(int32) int32

//extern ptsname
func ptsname(int32) *byte

//extern close
func close(int32) int32

const _O_RDWR = 2

type PtyError struct {
	FuncName    string
	ErrorString string
	Errno       syscall.Errno
}

func ptyError(name string, err error) *PtyError {
	return &PtyError{name, err.Error(), err.(syscall.Errno)}
}

func (e *PtyError) Error() string {
	return fmt.Sprintf("%s: %s", e.FuncName, e.ErrorString)
}

func (e *PtyError) Unwrap() error { return e.Errno }

// Open returns a control pty and the name of the linked process tty.
func Open() (pty *os.File, processTTY string, err error) {
	m := posix_openpt(_O_RDWR)
	if m < 0 {
		return nil, "", ptyError("posix_openpt", syscall.GetErrno())
	}
	if grantpt(m) < 0 {
		errno := syscall.GetErrno()
		close(m)
		return nil, "", ptyError("grantpt", errno)
	}
	if unlockpt(m) < 0 {
		errno := syscall.GetErrno()
		close(m)
		return nil, "", ptyError("unlockpt", errno)
	}
	p := ptsname(m)
	s := (*[32000]byte)(unsafe.Pointer(p))[:]
	for i, v := range s {
		if v == 0 {
			s = s[:i:i]
			break
		}
	}
	processTTY = string(s)
	return os.NewFile(uintptr(m), "pty"), processTTY, nil
}

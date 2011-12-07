// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// gccgo specific implementation of syslog for Solaris.  Solaris uses
// STREAMS to communicate with syslogd.  That is enough of a pain that
// we just call the libc function.

package syslog

import (
	"fmt"
	"syscall"
)

func unixSyslog() (conn serverConn, err error) {
	return libcConn(0), nil
}

type libcConn int

func syslog_c(int, *byte)

func (libcConn) writeBytes(p Priority, prefix string, b []byte) (int, error) {
	syslog_c(int(p), syscall.StringBytePtr(fmt.Sprintf("%s: %s", prefix, b)))
	return len(b), nil
}

func (libcConn) writeString(p Priority, prefix string, s string) (int, error) {
	syslog_c(int(p), syscall.StringBytePtr(fmt.Sprintf("%s: %s", prefix, s)))
	return len(s), nil
}

func (libcConn) close() error {
	return nil
}

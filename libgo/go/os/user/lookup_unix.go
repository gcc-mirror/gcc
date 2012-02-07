// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin freebsd linux
// +build cgo

package user

import (
	"fmt"
	"strings"
	"syscall"
	"unsafe"
)

/*
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <stdlib.h>

static int mygetpwuid_r(int uid, struct passwd *pwd,
	char *buf, size_t buflen, struct passwd **result) {
 return getpwuid_r(uid, pwd, buf, buflen, result);
}
*/

//extern getpwnam_r
func libc_getpwnam_r(name *byte, pwd *syscall.Passwd, buf *byte, buflen syscall.Size_t, result **syscall.Passwd) int

//extern getpwuid_r
func libc_getpwuid_r(uid syscall.Uid_t, pwd *syscall.Passwd, buf *byte, buflen syscall.Size_t, result **syscall.Passwd) int

// bytePtrToString takes a NUL-terminated array of bytes and convert
// it to a Go string.
func bytePtrToString(p *byte) string {
	a := (*[10000]byte)(unsafe.Pointer(p))
	i := 0
	for a[i] != 0 {
		i++
	}
	return string(a[:i])
}

func init() {
	implemented = true
}

// Lookup looks up a user by username. If the user cannot be found,
// the returned error is of type UnknownUserError.
func Lookup(username string) (*User, error) {
	return lookup(-1, username, true)
}

// LookupId looks up a user by userid. If the user cannot be found,
// the returned error is of type UnknownUserIdError.
func LookupId(uid int) (*User, error) {
	return lookup(uid, "", false)
}

func lookup(uid int, username string, lookupByName bool) (*User, error) {
	var pwd syscall.Passwd
	var result *syscall.Passwd

	// FIXME: Should let buf grow if necessary.
	const bufSize = 1024
	buf := make([]byte, bufSize)
	if lookupByName {
		rv := libc_getpwnam_r(syscall.StringBytePtr(username),
			&pwd,
			&buf[0],
			bufSize,
			&result)
		if rv != 0 {
			return nil, fmt.Errorf("user: lookup username %s: %s", username, syscall.GetErrno())
		}
		if result == nil {
			return nil, UnknownUserError(username)
		}
	} else {
		rv := libc_getpwuid_r(syscall.Uid_t(uid),
			&pwd,
			&buf[0],
			bufSize,
			&result)
		if rv != 0 {
			return nil, fmt.Errorf("user: lookup userid %d: %s", uid, syscall.GetErrno())
		}
		if result == nil {
			return nil, UnknownUserIdError(uid)
		}
	}
	u := &User{
		Uid:      int(pwd.Pw_uid),
		Gid:      int(pwd.Pw_gid),
		Username: bytePtrToString((*byte)(unsafe.Pointer(pwd.Pw_name))),
		Name:     bytePtrToString((*byte)(unsafe.Pointer(pwd.Pw_gecos))),
		HomeDir:  bytePtrToString((*byte)(unsafe.Pointer(pwd.Pw_dir))),
	}
	// The pw_gecos field isn't quite standardized.  Some docs
	// say: "It is expected to be a comma separated list of
	// personal data where the first item is the full name of the
	// user."
	if i := strings.Index(u.Name, ","); i >= 0 {
		u.Name = u.Name[:i]
	}
	return u, nil
}
